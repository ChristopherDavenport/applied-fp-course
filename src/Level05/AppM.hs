{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
module Level05.AppM where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad          (join)

import           Data.Text              (Text)

import           Level05.Types          (Error)

import           Data.Bifunctor         (first)

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will create a newtype `AppM` that is a shorthand way of
-- describing the return type of a function that may contain an error.
--
-- Our `AppM` type will work in the same manner as the Functor/Applicative/Monad 
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value. With the added bonus of allowing us to perform `IO` actions!
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     alsoMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (IO (Either Error a))
-- This structure allows us to start writing our functions in terms of
-- constraints. As an example, if we wanted to abstract over IO and indicate
-- that instead of the concrete type we wanted a constraint that allows for IO
-- actions. Our AppM would look more like this:
--
-- AppM m a = AppM ( m (Either Error a) )
--
-- Then our functions would look like:
--
-- foo :: MonadIO m => Int -> AppM m a
--
-- Or we could not use a concrete type for Error
--
-- AppM e m a = AppM ( m (Either e a) )

runAppM :: AppM a -> IO (Either Error a)
runAppM (AppM m) = m

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  fmap f (AppM a) = AppM ( fmap (\e -> fmap f e) a )

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM (pure (Right a))

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) (AppM ioef) (AppM ioea) = AppM iob
    where
    iob = do
      ef <- ioef
      ea <- ioea
      pure (ea >>= (\a -> fmap (\f -> f(a)) ef ))

instance Monad AppM where
  return :: a -> AppM a
  return a = AppM (pure (Right a))

  (>>=) :: forall a b. AppM a -> (a -> AppM b) -> AppM b
  (>>=)  (AppM ioea) f =
    let
      eitherManage :: Either Error a -> IO (Either Error (Either Error b))
      eitherManage ea = runAppM (traverse f ea)
      ioStackedEither :: IO (Either Error (Either Error b))
      ioStackedEither = ioea >>= eitherManage
      collapsedIO :: IO (Either Error b)
      collapsedIO = fmap join ioStackedEither
    in 
      AppM collapsedIO

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO ioa = AppM (fmap (\a -> Right a) ioa)

instance MonadError Error AppM where
  throwError :: forall a. Error -> AppM a
  throwError e =
    let
      io :: IO (Either Error a)
      io = pure $ Left e
    in AppM io

  catchError :: forall a. AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM ioea) f = 
    let
      intermediate :: IO (Either Error a)
      intermediate = do
        ea <- ioea
        either (\e -> runAppM (f e)) (\a -> pure $ Right a) ea
    in
      AppM intermediate

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither e =
  either throwError pure e
