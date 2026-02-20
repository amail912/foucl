{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Model (
  NoteContent(..), StorageId(..)
  , ChecklistContent(..), ChecklistItem(..)
  , Content
  , Identifiable(..), hash
  , mkIdentifiable
  ) where


import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import Data.ByteString.Base64.Lazy (encode)
import           Data.ByteString.Lazy.Char8  as BL
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import           Data.Digest.Pure.SHA        (bytestringDigest, sha256)
import           GHC.Generics                (Generic)

data StorageId = StorageId { id      :: !String
                           , version :: !String
                           } deriving (Show, Generic, Eq)

instance ToJSON   StorageId
instance FromJSON StorageId

class (Show a, Eq a, ToJSON a, FromJSON a) => Content a where
  hash :: a -> String

data Content a => Identifiable a =
    Identifiable { storageId :: !StorageId, content :: !a } deriving (Eq, Show)

instance Content a => ToJSON (Identifiable a) where
  toJSON (Identifiable {..}) =
    object [ "storageId" .= storageId, "content" .= content ]

instance Content a => FromJSON (Identifiable a) where
  parseJSON = withObject "Identifiable" $ \value -> Identifiable
    <$> value .: "storageId"
    <*> value .: "content"

mkIdentifiable :: (Content a) => a -> IO (Identifiable a)
mkIdentifiable content = do
    uuid <- liftIO $ fmap toString nextRandom
    return $ Identifiable (StorageId { id = uuid, version = hash content }) content

-- ===================== Note =============================================

data NoteContent = NoteContent { title       :: Maybe String
                               , noteContent :: String
                               } deriving (Show, Generic, Eq)

instance Content NoteContent where
  hash content = base64Sha256 $ show content

instance FromJSON NoteContent
instance ToJSON   NoteContent

-- ======================= CHECKLIST =======================================

data ChecklistContent = ChecklistContent { name  :: String
                                         , items :: [ChecklistItem]
                                         } deriving (Show, Generic, Eq)

data ChecklistItem    = ChecklistItem    { label   :: String
                                         , checked :: Bool
                                         } deriving (Show, Generic, Eq)

instance FromJSON ChecklistContent
instance ToJSON   ChecklistContent

instance FromJSON ChecklistItem
instance ToJSON   ChecklistItem

instance Content ChecklistContent where
  hash checklistContent = base64Sha256 $ show checklistContent

-- =============================== Utils ==========================================

base64Sha256 :: String -> String
base64Sha256 contentToHash = BL.unpack . encode . bytestringDigest . sha256 $ BL.pack contentToHash
