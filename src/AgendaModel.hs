{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AgendaModel
  ( ItemType(..)
  , ItemStatus(..)
  , RecurrenceRule(..)
  , CalendarItemContent(..)
  , CalendarItem(..)
  , ValidateRequest(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), (.!=), withObject, object, Value(..))
import Data.Aeson.Types (Parser)
import Data.Aeson.KeyMap (insert)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)

data ItemType = Intention | ScheduledBlock
  deriving (Show, Eq, Generic)

instance FromJSON ItemType where
  parseJSON (String s) =
    case s of
      "INTENTION" -> pure Intention
      "BLOC_PLANIFIE" -> pure ScheduledBlock
      _ -> fail "Unknown ItemType value"
  parseJSON _ = fail "ItemType must be a JSON string"

instance ToJSON ItemType where
  toJSON Intention = String "INTENTION"
  toJSON ScheduledBlock = String "BLOC_PLANIFIE"

data ItemStatus = Todo | EnCours | Fait | Annule
  deriving (Show, Eq, Generic)

instance FromJSON ItemStatus where
  parseJSON (String s) =
    case s of
      "TODO" -> pure Todo
      "EN_COURS" -> pure EnCours
      "FAIT" -> pure Fait
      "ANNULE" -> pure Annule
      _ -> fail "Unknown ItemStatus value"
  parseJSON _ = fail "ItemStatus must be a JSON string"

instance ToJSON ItemStatus where
  toJSON Todo = String "TODO"
  toJSON EnCours = String "EN_COURS"
  toJSON Fait = String "FAIT"
  toJSON Annule = String "ANNULE"

data RecurrenceRule
  = RecurrenceDaily
  | RecurrenceWeekly
  | RecurrenceMonthly
  | RecurrenceYearly
  | RecurrenceEveryXDays Int
  deriving (Show, Eq, Generic)

instance FromJSON RecurrenceRule where
  parseJSON = withObject "RecurrenceRule" $ \v -> do
    kind <- v .: "type"
    case (kind :: String) of
      "DAILY" -> pure RecurrenceDaily
      "WEEKLY" -> pure RecurrenceWeekly
      "MONTHLY" -> pure RecurrenceMonthly
      "YEARLY" -> pure RecurrenceYearly
      "EVERY_X_DAYS" -> RecurrenceEveryXDays <$> v .: "interval_days"
      _ -> fail "Unknown RecurrenceRule type"

instance ToJSON RecurrenceRule where
  toJSON rule =
    case rule of
      RecurrenceDaily -> object ["type" .= String "DAILY"]
      RecurrenceWeekly -> object ["type" .= String "WEEKLY"]
      RecurrenceMonthly -> object ["type" .= String "MONTHLY"]
      RecurrenceYearly -> object ["type" .= String "YEARLY"]
      RecurrenceEveryXDays interval ->
        object ["type" .= String "EVERY_X_DAYS", "interval_days" .= interval]

data CalendarItemContent = CalendarItemContent
  { itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , status :: ItemStatus
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe Int
  , category :: Maybe String
  , recurrenceRule :: Maybe RecurrenceRule
  , recurrenceExceptionDates :: [String]
  } deriving (Show, Eq, Generic)

instance FromJSON CalendarItemContent where
  parseJSON = withObject "CalendarItemContent" $ \v -> do
    itemType <- v .: "type"
    title <- v .: "titre"
    windowStart <- v .: "fenetre_debut"
    windowEnd <- v .: "fenetre_fin"
    status <- v .: "statut"
    sourceItemId <- v .:? "source_item_id"
    actualDurationMinutes <- v .:? "duree_reelle_minutes"
    category <- v .:? "categorie"
    recurrenceRule <- v .:? "recurrence_rule"
    recurrenceExceptionDates <- v .:? "recurrence_exception_dates" .!= []
    pure CalendarItemContent {..}

instance ToJSON CalendarItemContent where
  toJSON CalendarItemContent {..} =
    object $ baseFields <> optionalFields
    where
      baseFields =
        [ "type" .= itemType
        , "titre" .= title
        , "fenetre_debut" .= windowStart
        , "fenetre_fin" .= windowEnd
        , "statut" .= status
        ]
      optionalFields =
        catMaybes
          [ ("source_item_id" .=) <$> sourceItemId
          , ("duree_reelle_minutes" .=) <$> actualDurationMinutes
          , ("categorie" .=) <$> category
          ] <>
        case recurrenceRule of
          Just rule -> [ "recurrence_rule" .= rule, "recurrence_exception_dates" .= recurrenceExceptionDates ]
          Nothing -> []

data CalendarItem
  = NewCalendarItem { content :: CalendarItemContent }
  | ServerCalendarItem { content :: CalendarItemContent, itemId :: String }
  deriving (Show, Eq, Generic)

instance FromJSON CalendarItem where
  parseJSON value@(Object v) = do
    content <- parseJSON value :: Parser CalendarItemContent
    mId <- v .:? "id"
    case mId of
      Just itemId -> pure $ ServerCalendarItem { content = content, itemId = itemId }
      Nothing -> pure $ NewCalendarItem { content = content }
  parseJSON _ = fail "CalendarItem must be a JSON object"

instance ToJSON CalendarItem where
  toJSON (NewCalendarItem {content}) = toJSON content
  toJSON (ServerCalendarItem {content, itemId}) =
    case toJSON content of
      Object obj -> Object (insert "id" (toJSON itemId) obj)
      other -> other

newtype ValidateRequest = ValidateRequest
  { validateDurationMinutes :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON ValidateRequest where
  parseJSON = withObject "ValidateRequest" $ \v ->
    ValidateRequest <$> v .: "duree_reelle_minutes"
