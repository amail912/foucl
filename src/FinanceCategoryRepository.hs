{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceCategoryRepository
  ( FinanceCategoryRepository(..)
  , FinanceCategory(..)
  , FinanceCategoryWriteRequest(..)
  , FinanceCategoryOwner(..)
  , financeCategoryOwnerText
  , normalizeFinanceCategoryName
  , postgresFinanceCategoryRepository
  , financeCategoryPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), object, withObject, (.:), (.:?), (.=))
import Data.List (find)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , execute
  , query
  , query_
  , withTransaction
  )
import Helpers (mapSqlReadException, mapSqlWriteException, tryExcept, withPoolExceptHandled)
import Repository (RepositoryError(..))

data FinanceCategoryRepository = FinanceCategoryRepository
  { repoListFinanceCategories :: !(String -> ExceptT RepositoryError IO [FinanceCategory])
  , repoCreateFinanceCategory :: !(String -> FinanceCategoryWriteRequest -> ExceptT RepositoryError IO FinanceCategory)
  , repoUpdateFinanceCategory :: !(String -> String -> FinanceCategoryWriteRequest -> ExceptT RepositoryError IO FinanceCategory)
  , repoDeleteFinanceCategory :: !(String -> String -> ExceptT RepositoryError IO ())
  }

data FinanceCategoryWriteRequest = FinanceCategoryWriteRequest
  { financeCategoryWriteName :: !String
  , financeCategoryWriteParentId :: !(Maybe String)
  }

data FinanceCategoryOwner
  = FinanceCategoryBuiltIn
  | FinanceCategoryUser
  deriving (Eq, Show)

data FinanceCategory = FinanceCategory
  { financeCategoryId :: !String
  , financeCategoryName :: !String
  , financeCategoryParentId :: !(Maybe String)
  , financeCategoryOwner :: !FinanceCategoryOwner
  , financeCategorySelectable :: !Bool
  } deriving (Eq, Show)

data FinanceCategoryMeta = FinanceCategoryMeta
  { financeCategoryMetaId :: !String
  , financeCategoryMetaUserId :: !(Maybe String)
  , financeCategoryMetaParentId :: !(Maybe String)
  , financeCategoryMetaOwner :: !FinanceCategoryOwner
  }

instance FromJSON FinanceCategoryWriteRequest where
  parseJSON = withObject "FinanceCategoryWriteRequest" $ \value ->
    FinanceCategoryWriteRequest
      <$> value .: "name"
      <*> value .:? "parentId"

instance ToJSON FinanceCategory where
  toJSON FinanceCategory
    { financeCategoryId
    , financeCategoryName
    , financeCategoryParentId
    , financeCategoryOwner
    , financeCategorySelectable
    } =
      object
        [ "id" .= financeCategoryId
        , "name" .= financeCategoryName
        , "parentId" .= financeCategoryParentId
        , "owner" .= financeCategoryOwnerText financeCategoryOwner
        , "selectable" .= financeCategorySelectable
        ]

financeCategoryOwnerText :: FinanceCategoryOwner -> Text
financeCategoryOwnerText FinanceCategoryBuiltIn = "built_in"
financeCategoryOwnerText FinanceCategoryUser = "user"

normalizeFinanceCategoryName :: String -> Maybe String
normalizeFinanceCategoryName raw =
  let trimmed = trim raw
  in if null trimmed then Nothing else Just trimmed

postgresFinanceCategoryRepository :: Pool Connection -> FinanceCategoryRepository
postgresFinanceCategoryRepository pool =
  FinanceCategoryRepository
    { repoListFinanceCategories = pgListFinanceCategories pool
    , repoCreateFinanceCategory = pgCreateFinanceCategory pool
    , repoUpdateFinanceCategory = pgUpdateFinanceCategory pool
    , repoDeleteFinanceCategory = pgDeleteFinanceCategory pool
    }

financeCategoryPostgresHealthChecks :: Connection -> ExceptT String IO ()
financeCategoryPostgresHealthChecks conn = do
  tryExcept
    (query_ conn
      "SELECT category_id, user_id, name, parent_id, owner, selectable FROM finance_categories LIMIT 0"
      :: IO [(String, Maybe String, String, Maybe String, String, Bool)])
    (\err -> "Finance schema check failed for finance_categories: " ++ show err)
  pure ()

pgListFinanceCategories :: Pool Connection -> String -> ExceptT RepositoryError IO [FinanceCategory]
pgListFinanceCategories pool userId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept
      (query conn
        "SELECT category_id, name, parent_id, owner, selectable \
        \FROM finance_categories \
        \WHERE user_id IS NULL OR user_id = ? \
        \ORDER BY CASE WHEN user_id IS NULL THEN 0 ELSE 1 END, category_id ASC"
        (Only userId))
      mapSqlReadException
    mapM decodeFinanceCategory rows

pgCreateFinanceCategory :: Pool Connection -> String -> FinanceCategoryWriteRequest -> ExceptT RepositoryError IO FinanceCategory
pgCreateFinanceCategory pool userId FinanceCategoryWriteRequest
  { financeCategoryWriteName
  , financeCategoryWriteParentId
  } =
    withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
      categoryId <- liftIO (toString <$> nextRandom)
      validateAccessibleParent conn userId financeCategoryWriteParentId Nothing
      _ <- tryExcept
        (withTransaction conn $
          execute conn
            "INSERT INTO finance_categories (category_id, user_id, name, parent_id, owner, selectable) VALUES (?, ?, ?, ?, ?, ?)"
            ( categoryId
            , userId
            , financeCategoryWriteName
            , financeCategoryWriteParentId
            , financeCategoryOwnerText FinanceCategoryUser
            , True
            ))
        mapSqlWriteException
      pure FinanceCategory
        { financeCategoryId = categoryId
        , financeCategoryName = financeCategoryWriteName
        , financeCategoryParentId = financeCategoryWriteParentId
        , financeCategoryOwner = FinanceCategoryUser
        , financeCategorySelectable = True
        }

pgUpdateFinanceCategory :: Pool Connection -> String -> String -> FinanceCategoryWriteRequest -> ExceptT RepositoryError IO FinanceCategory
pgUpdateFinanceCategory pool userId categoryId FinanceCategoryWriteRequest
  { financeCategoryWriteName
  , financeCategoryWriteParentId
  } =
    withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
      target <- loadOwnedMutableCategory conn userId categoryId
      accessibleCategories <- loadAccessibleCategoryMeta conn userId
      validateAccessibleParent conn userId financeCategoryWriteParentId (Just (financeCategoryMetaId target))
      if wouldCreateCycle accessibleCategories categoryId financeCategoryWriteParentId
        then throwError WriteFailure
        else do
          _ <- tryExcept
            (withTransaction conn $
              execute conn
                "UPDATE finance_categories SET name = ?, parent_id = ? WHERE category_id = ? AND user_id = ?"
                (financeCategoryWriteName, financeCategoryWriteParentId, categoryId, userId))
            mapSqlWriteException
          pure ()
      pure FinanceCategory
        { financeCategoryId = categoryId
        , financeCategoryName = financeCategoryWriteName
        , financeCategoryParentId = financeCategoryWriteParentId
        , financeCategoryOwner = FinanceCategoryUser
        , financeCategorySelectable = True
        }

pgDeleteFinanceCategory :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteFinanceCategory pool userId categoryId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    target <- loadOwnedMutableCategory conn userId categoryId
    hasChildrenRows <- tryExcept
      (query conn
        "SELECT category_id FROM finance_categories WHERE parent_id = ? LIMIT 1"
        (Only (financeCategoryMetaId target))
        :: IO [Only String])
      mapSqlReadException
    result <-
      if not (null hasChildrenRows)
        then pure False
        else do
          deletedCount <- tryExcept
            (withTransaction conn $
              execute conn
                "DELETE FROM finance_categories WHERE category_id = ? AND user_id = ?"
                (categoryId, userId))
            mapSqlWriteException
          pure (deletedCount == 1)
    case result of
      False -> throwError AlreadyExists
      True -> pure ()

loadOwnedMutableCategory :: Connection -> String -> String -> ExceptT RepositoryError IO FinanceCategoryMeta
loadOwnedMutableCategory conn userId categoryId = do
  rows <- tryExcept
    (query conn
      "SELECT category_id, user_id, parent_id, owner FROM finance_categories WHERE category_id = ?"
      (Only categoryId))
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [row] ->
      case decodeFinanceCategoryMeta row of
        Left err -> throwError err
        Right meta
          | financeCategoryMetaOwner meta == FinanceCategoryBuiltIn -> throwError AlreadyExists
          | financeCategoryMetaUserId meta /= Just userId -> throwError NotFound
          | otherwise -> pure meta
    _ -> throwError ReadFailure

validateAccessibleParent :: Connection -> String -> Maybe String -> Maybe String -> ExceptT RepositoryError IO ()
validateAccessibleParent _ _ Nothing _ = pure ()
validateAccessibleParent _ _ (Just parentId) (Just targetId)
  | parentId == targetId = throwError WriteFailure
validateAccessibleParent conn userId (Just parentId) _ = do
  rows <- tryExcept
    (query conn
      "SELECT category_id FROM finance_categories WHERE category_id = ? AND (user_id IS NULL OR user_id = ?)"
      (parentId, userId)
      :: IO [Only String])
    mapSqlReadException
  case rows of
    [Only _] -> pure ()
    _ -> throwError WriteFailure

loadAccessibleCategoryMeta :: Connection -> String -> ExceptT RepositoryError IO [FinanceCategoryMeta]
loadAccessibleCategoryMeta conn userId = do
  rows <- tryExcept
    (query conn
      "SELECT category_id, user_id, parent_id, owner FROM finance_categories WHERE user_id IS NULL OR user_id = ?"
      (Only userId))
    mapSqlReadException
  either throwError pure (mapM decodeFinanceCategoryMeta rows)

wouldCreateCycle :: [FinanceCategoryMeta] -> String -> Maybe String -> Bool
wouldCreateCycle _ _ Nothing = False
wouldCreateCycle categories targetId (Just parentId) = go (Just parentId)
  where
    go Nothing = False
    go (Just currentId)
      | currentId == targetId = True
      | otherwise =
          case find (\category -> financeCategoryMetaId category == currentId) categories of
            Nothing -> False
            Just category -> go (financeCategoryMetaParentId category)

decodeFinanceCategory :: (String, String, Maybe String, String, Bool) -> ExceptT RepositoryError IO FinanceCategory
decodeFinanceCategory (categoryId, name, parentId, ownerText, selectable) =
  case parseFinanceCategoryOwner ownerText of
    Nothing -> throwError ReadFailure
    Just financeCategoryOwner ->
      pure FinanceCategory
        { financeCategoryId = categoryId
        , financeCategoryName = name
        , financeCategoryParentId = parentId
        , financeCategoryOwner = financeCategoryOwner
        , financeCategorySelectable = selectable
        }

decodeFinanceCategoryMeta :: (String, Maybe String, Maybe String, String) -> Either RepositoryError FinanceCategoryMeta
decodeFinanceCategoryMeta (categoryId, userId, parentId, ownerText) =
  case parseFinanceCategoryOwner ownerText of
    Nothing -> Left ReadFailure
    Just financeCategoryOwner ->
      Right FinanceCategoryMeta
        { financeCategoryMetaId = categoryId
        , financeCategoryMetaUserId = userId
        , financeCategoryMetaParentId = parentId
        , financeCategoryMetaOwner = financeCategoryOwner
        }

parseFinanceCategoryOwner :: String -> Maybe FinanceCategoryOwner
parseFinanceCategoryOwner "built_in" = Just FinanceCategoryBuiltIn
parseFinanceCategoryOwner "user" = Just FinanceCategoryUser
parseFinanceCategoryOwner _ = Nothing

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
