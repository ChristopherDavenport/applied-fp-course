{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types           (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse s ct bs = responseLBS s [("content-type" , (renderContentType ct))] bs

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 ct bs = responseLBS status200 [("content-type", (renderContentType ct))] bs

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 ct bs = responseLBS status404 [("content-type", (renderContentType ct))] bs

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 ct bs = responseLBS status400 [("content-type", (renderContentType ct))] bs

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest t bs = do
  comment <- (mkCommentText . lazyByteStringToStrictText) bs
  topic <- mkTopic t
  return $ AddRq topic comment
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest :: Text -> Either Error RqType
mkViewRequest t = fmap ViewRq (mkTopic t)

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

--  EmptyTopic | EmptyCommentText | MissingTopic
mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopic        = resp404 PlainText "EmptyTpic"
mkErrorResponse EmptyCommentText  = resp404 PlainText "EmptyCommentText"
mkErrorResponse MissingTopic      = resp400 PlainText "Missing Topic"
mkErrorResponse UnmatchedRequest  = resp400 PlainText "Unmatched Request"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest request = case ((requestMethod request), (pathInfo request)) of
  ("POST", topic:["add"]) -> 
    fmap (\body -> mkAddRequest topic body) (strictRequestBody request) 
  ("GET", topic:["view"]) -> pure $ mkViewRequest topic
  ("GET", ["list"]) -> pure mkListRequest
  _ -> pure $ Left UnmatchedRequest
  

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest (AddRq t ct)  = Right $ resp200 PlainText "AddRq not implemented yet"
handleRequest (ViewRq t)    = Right $ resp200 PlainText "ViewRq not implemented yet"
handleRequest ListRq        = Right $ resp200 PlainText "ListRq not implemented yet"


-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app req cb = do
  rqType <- mkRequest req
  cb $ either mkErrorResponse (\rqType -> either mkErrorResponse (\a -> a) (handleRequest rqType)) rqType

runApp :: IO ()
runApp = run 3000 app
