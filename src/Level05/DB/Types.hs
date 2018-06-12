module Level05.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import           Database.SQLite.Simple.ToRow   (ToRow (toRow))

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.
data DbComment = DbComment
  { dbCommentId      :: Int
  , dbCommentTopic   :: Text
  , dbCommentComment :: Text
  , dbCommentTime    :: UTCTime
  }
  deriving Show

-- This type class instance comes from our DB package and tells the DB package
-- how to decode a single row from the database into a single representation of
-- our type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DbComment where
  fromRow = DbComment
            -- field :: FromField a => RowParser a
            <$> field
            <*> field
            <*> field
            <*> field

data DBCommentInsert = DBCommentInsert Text Text UTCTime

instance ToRow DBCommentInsert where
  toRow (DBCommentInsert topic body time) = toRow (topic, body, time)

data DBTopic = DBTopic {
  dbTopicName :: Text
}
  
instance FromRow DBTopic where
  fromRow = fmap DBTopic field
