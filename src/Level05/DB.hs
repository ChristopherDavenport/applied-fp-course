{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)
import           Level05.DB.Types                   (DbComment, DBCommentInsert(..), DBTopic(..))

import           Level05.AppM                       (AppM(..), liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB:: (a -> Either Error b) -> IO a -> AppM b
runDB f ioa = AppM $ fmap f ioa

getComments:: FirstAppDB -> Topic -> AppM [Comment]
getComments db t =
    let
      sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      conn = dbConn db
      dbCommentIO = Sql.query conn sql (Sql.Only $ getTopic t) :: IO [DbComment]
    in do
      dbComment <- liftIO dbCommentIO
      liftEither $ traverse fromDbComment dbComment

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic db topic ct =
    let
      sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      conn = dbConn db
      topicText = getTopic topic
      commentText = getCommentText ct
    in do
      time <- liftIO getCurrentTime
      _ <- liftIO $ Sql.execute conn sql $ DBCommentInsert topicText commentText time
      return ()

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics db =
    let
      sql = "SELECT DISTINCT topic FROM comments"
      conn = dbConn db
    in do
      topics <- liftIO (Sql.query_ conn sql :: IO [DBTopic])
      liftEither $ traverse (\dbTopic -> mkTopic $ dbTopicName dbTopic) topics

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic db t =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    conn = dbConn db
  in do
    _ <- liftIO $ Sql.execute conn sql $ Sql.Only $ getTopic t 
    pure ()
