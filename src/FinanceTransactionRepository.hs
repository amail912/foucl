{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceTransactionRepository
  ( FinanceTransactionRepository(..)
  , FinanceTransaction(..)
  , FinanceTransactionLinkRequest(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransactionTransfer(..)
  , FinanceTransactionCreateRequest(..)
  , FinanceTransactionCategorizeRequest(..)
  , FinanceTransactionSplitRequest(..)
  , FinanceTransactionSplitWriteRow(..)
  , FinanceTransactionWriteRequest(..)
  , FinanceTransactionDirection(..)
  , financeTransactionDirectionText
  , postgresFinanceTransactionRepository
  , financeTransactionPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value, object, withObject, (.:), (.:?), (.=))
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
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

data FinanceTransactionRepository = FinanceTransactionRepository
  { repoCreateFinanceTransaction :: !(String -> FinanceTransactionWriteRequest -> ExceptT RepositoryError IO FinanceTransaction)
  , repoLoadFinanceTransactionById :: !(String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoListFinanceTransactions :: !(String -> Maybe String -> Maybe UTCTime -> Maybe UTCTime -> ExceptT RepositoryError IO [FinanceTransaction])
  , repoCategorizeFinanceTransaction :: !(String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoSplitFinanceTransaction :: !(String -> String -> [FinanceTransactionSplitWriteRow] -> ExceptT RepositoryError IO FinanceTransaction)
  , repoLinkFinanceTransactions :: !(String -> String -> String -> String -> ExceptT RepositoryError IO (FinanceTransaction, FinanceTransaction))
  }

data FinanceTransactionCreateRequest = FinanceTransactionCreateRequest
  { financeTransactionCreateAccountId :: !String
  , financeTransactionCreateAmount :: !Int
  , financeTransactionCreateOccurredAt :: !(Maybe String)
  }

data FinanceTransactionLinkRequest = FinanceTransactionLinkRequest
  { financeTransactionLinkSourceTransactionId :: !String
  , financeTransactionLinkTargetTransactionId :: !String
  , financeTransactionLinkType :: !String
  }

data FinanceTransactionCategorizeRequest = FinanceTransactionCategorizeRequest
  { financeTransactionCategorizeCategory :: !String
  }

data FinanceTransactionSplitRequest = FinanceTransactionSplitRequest
  { financeTransactionSplitRows :: ![FinanceTransactionSplitWriteRow]
  }

data FinanceTransactionSplitWriteRow = FinanceTransactionSplitWriteRow
  { financeTransactionSplitWriteAmount :: !Int
  , financeTransactionSplitWriteCategory :: !String
  } deriving (Eq, Show)

data FinanceTransactionWriteRequest = FinanceTransactionWriteRequest
  { financeTransactionWriteIdempotencyKey :: !String
  , financeTransactionWriteDirection :: !FinanceTransactionDirection
  , financeTransactionWriteAccountId :: !String
  , financeTransactionWriteAmount :: !Int
  , financeTransactionWriteOccurredAt :: !UTCTime
  , financeTransactionWriteOccurredAtSupplied :: !Bool
  }

data FinanceTransactionDirection
  = FinanceTransactionSent
  | FinanceTransactionReceived
  deriving (Eq, Show)

data FinanceTransactionSplitRow = FinanceTransactionSplitRow
  { financeTransactionSplitAmount :: !Int
  , financeTransactionSplitCategory :: !String
  } deriving (Eq, Show)

data FinanceTransaction = FinanceTransaction
  { financeTransactionId :: !String
  , financeTransactionDirection :: !FinanceTransactionDirection
  , financeTransactionAccountId :: !String
  , financeTransactionAmount :: !Int
  , financeTransactionOccurredAt :: !UTCTime
  , financeTransactionRecordedAt :: !UTCTime
  , financeTransactionTransfer :: !(Maybe FinanceTransactionTransfer)
  , financeTransactionCategory :: !(Maybe String)
  , financeTransactionSplits :: ![FinanceTransactionSplitRow]
  } deriving (Eq, Show)

data FinanceTransactionTransfer = FinanceTransactionTransfer
  { financeTransactionTransferLinkType :: !String
  , financeTransactionTransferPeerTransactionId :: !String
  , financeTransactionTransferPeerAccountId :: !String
  , financeTransactionTransferPeerAmount :: !Int
  , financeTransactionTransferLinkedAt :: !UTCTime
  } deriving (Eq, Show)

instance FromJSON FinanceTransactionCreateRequest where
  parseJSON = withObject "FinanceTransactionCreateRequest" $ \value ->
    FinanceTransactionCreateRequest
      <$> value .: "accountId"
      <*> value .: "amount"
      <*> value .:? "occurredAt"

instance FromJSON FinanceTransactionLinkRequest where
  parseJSON = withObject "FinanceTransactionLinkRequest" $ \value ->
    FinanceTransactionLinkRequest
      <$> value .: "sourceTransactionId"
      <*> value .: "targetTransactionId"
      <*> value .: "linkType"

instance FromJSON FinanceTransactionCategorizeRequest where
  parseJSON = withObject "FinanceTransactionCategorizeRequest" $ \value ->
    FinanceTransactionCategorizeRequest <$> value .: "category"

instance FromJSON FinanceTransactionSplitRequest where
  parseJSON = withObject "FinanceTransactionSplitRequest" $ \value ->
    FinanceTransactionSplitRequest <$> value .: "splits"

instance FromJSON FinanceTransactionSplitWriteRow where
  parseJSON = withObject "FinanceTransactionSplitWriteRow" $ \value ->
    FinanceTransactionSplitWriteRow
      <$> value .: "amount"
      <*> value .: "category"

instance ToJSON FinanceTransactionSplitRow where
  toJSON FinanceTransactionSplitRow { financeTransactionSplitAmount, financeTransactionSplitCategory } =
    object
      [ "amount" .= financeTransactionSplitAmount
      , "category" .= financeTransactionSplitCategory
      ]

instance ToJSON FinanceTransactionTransfer where
  toJSON FinanceTransactionTransfer
    { financeTransactionTransferLinkType
    , financeTransactionTransferPeerTransactionId
    , financeTransactionTransferPeerAccountId
    , financeTransactionTransferPeerAmount
    , financeTransactionTransferLinkedAt
    } =
      object
        [ "linkType" .= financeTransactionTransferLinkType
        , "peerTransactionId" .= financeTransactionTransferPeerTransactionId
        , "peerAccountId" .= financeTransactionTransferPeerAccountId
        , "peerAmount" .= financeTransactionTransferPeerAmount
        , "linkedAt" .= financeTransactionTransferLinkedAt
        ]

instance ToJSON FinanceTransaction where
  toJSON FinanceTransaction
    { financeTransactionId
    , financeTransactionDirection
    , financeTransactionAccountId
    , financeTransactionAmount
    , financeTransactionOccurredAt
    , financeTransactionRecordedAt
    , financeTransactionTransfer
    , financeTransactionCategory
    , financeTransactionSplits
    } =
      object
        [ "id" .= financeTransactionId
        , "direction" .= financeTransactionDirectionText financeTransactionDirection
        , "accountId" .= financeTransactionAccountId
        , "amount" .= financeTransactionAmount
        , "occurredAt" .= financeTransactionOccurredAt
        , "recordedAt" .= financeTransactionRecordedAt
        , "transfer" .= financeTransactionTransfer
        , "category" .= financeTransactionCategory
        , "splits" .= financeTransactionSplits
        , "notes" .= ([] :: [Value])
        ]

financeTransactionDirectionText :: FinanceTransactionDirection -> Text
financeTransactionDirectionText FinanceTransactionSent = "sent"
financeTransactionDirectionText FinanceTransactionReceived = "received"

postgresFinanceTransactionRepository :: Pool Connection -> FinanceTransactionRepository
postgresFinanceTransactionRepository pool =
  FinanceTransactionRepository
    { repoCreateFinanceTransaction = pgCreateFinanceTransaction pool
    , repoLoadFinanceTransactionById = pgLoadFinanceTransactionById pool
    , repoListFinanceTransactions = pgListFinanceTransactions pool
    , repoCategorizeFinanceTransaction = pgCategorizeFinanceTransaction pool
    , repoSplitFinanceTransaction = pgSplitFinanceTransaction pool
    , repoLinkFinanceTransactions = pgLinkFinanceTransactions pool
    }

financeTransactionPostgresHealthChecks :: Connection -> ExceptT String IO ()
financeTransactionPostgresHealthChecks conn = do
  tryExcept
    (query_ conn
      "SELECT event_id, user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at FROM finance_transaction_events LIMIT 0"
      :: IO [(String, String, String, String, String, Int64, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at FROM finance_transactions LIMIT 0"
      :: IO [(String, String, String, String, Int64, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_transactions: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, idempotency_key, direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id FROM finance_transaction_idempotency LIMIT 0"
      :: IO [(String, String, String, String, Int64, Bool, UTCTime, String)])
    (\err -> "Finance schema check failed for finance_transaction_idempotency: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT event_id, user_id, transaction_id, event_type, category, splits_payload, recorded_at FROM finance_transaction_classification_events LIMIT 0"
      :: IO [(String, String, String, String, Maybe String, Maybe Value, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_classification_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, category, updated_at FROM finance_transaction_categories LIMIT 0"
      :: IO [(String, String, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_categories: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, split_index, amount, category, updated_at FROM finance_transaction_splits LIMIT 0"
      :: IO [(String, String, Int, Int64, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_splits: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT event_id, user_id, source_transaction_id, target_transaction_id, link_type, recorded_at FROM finance_transaction_link_events LIMIT 0"
      :: IO [(String, String, String, String, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_link_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, peer_transaction_id, link_type, linked_at FROM finance_transaction_links LIMIT 0"
      :: IO [(String, String, String, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_links: " ++ show err)
  pure ()

pgCreateFinanceTransaction :: Pool Connection -> String -> FinanceTransactionWriteRequest -> ExceptT RepositoryError IO FinanceTransaction
pgCreateFinanceTransaction pool userId request@FinanceTransactionWriteRequest
  { financeTransactionWriteIdempotencyKey
  , financeTransactionWriteDirection
  , financeTransactionWriteAccountId
  , financeTransactionWriteAmount
  , financeTransactionWriteOccurredAt
  , financeTransactionWriteOccurredAtSupplied
  } =
    withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
      existingRows <- tryExcept
        (query conn
          "SELECT direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id FROM finance_transaction_idempotency WHERE user_id = ? AND idempotency_key = ?"
          (userId, financeTransactionWriteIdempotencyKey))
        mapSqlReadException
      case existingRows of
        [(storedDirection, storedAccountId, storedAmount, storedOccurredAtSupplied, storedOccurredAt, transactionId)] ->
          if idempotencyMatches request storedDirection storedAccountId storedAmount storedOccurredAtSupplied storedOccurredAt
            then pgLoadFinanceTransactionByIdInConn conn userId transactionId
            else throwError AlreadyExists
        [] -> do
          transactionId <- liftIO (toString <$> nextRandom)
          eventId <- liftIO (toString <$> nextRandom)
          _ <- tryExcept
            (withTransaction conn $ do
              recordedRows <- query conn
                "INSERT INTO finance_transaction_events (event_id, user_id, transaction_id, account_id, direction, amount, occurred_at) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING recorded_at"
                (eventId, userId, transactionId, financeTransactionWriteAccountId, financeTransactionDirectionText financeTransactionWriteDirection, financeTransactionWriteAmount, financeTransactionWriteOccurredAt)
                :: IO [Only UTCTime]
              recordedAt <- case recordedRows of
                [Only rowRecordedAt] -> pure rowRecordedAt
                _ -> fail "Unexpected recorded_at row count for finance transaction event insert"
              _ <- execute conn
                "INSERT INTO finance_transactions (user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (userId, transactionId, financeTransactionWriteAccountId, financeTransactionDirectionText financeTransactionWriteDirection, financeTransactionWriteAmount, financeTransactionWriteOccurredAt, recordedAt)
              _ <- execute conn
                "INSERT INTO finance_transaction_idempotency (user_id, idempotency_key, direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                ( userId
                , financeTransactionWriteIdempotencyKey
                , financeTransactionDirectionText financeTransactionWriteDirection
                , financeTransactionWriteAccountId
                , financeTransactionWriteAmount
                , financeTransactionWriteOccurredAtSupplied
                , financeTransactionWriteOccurredAt
                , transactionId
                )
              pure ())
            mapSqlWriteException
          pgLoadFinanceTransactionByIdInConn conn userId transactionId
        _ -> throwError ReadFailure

pgLoadFinanceTransactionById :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgLoadFinanceTransactionById pool userId transactionId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn ->
    pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgLoadFinanceTransactionByIdInConn :: Connection -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgLoadFinanceTransactionByIdInConn conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId)
      :: IO [(String, Text, String, Int64, UTCTime, UTCTime)])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [row] -> do
      base <- decodeBaseTransaction row
      hydrateTransactionDetails conn userId base
    _ -> throwError ReadFailure

pgListFinanceTransactions :: Pool Connection -> String -> Maybe String -> Maybe UTCTime -> Maybe UTCTime -> ExceptT RepositoryError IO [FinanceTransaction]
pgListFinanceTransactions pool userId mAccountId mFrom mTo =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept (runListQuery conn) mapSqlReadException
    baseRows <- mapM decodeBaseTransaction rows
    mapM (hydrateTransactionDetails conn userId) baseRows
  where
    runListQuery conn =
      case (mAccountId, mFrom, mTo) of
        (Nothing, Nothing, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? ORDER BY occurred_at DESC, transaction_id ASC"
            (Only userId)
        (Just accountId, Nothing, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId)
        (Nothing, Just fromTs, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND occurred_at >= ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, fromTs)
        (Nothing, Nothing, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, toTs)
        (Just accountId, Just fromTs, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at >= ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, fromTs)
        (Just accountId, Nothing, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, toTs)
        (Nothing, Just fromTs, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND occurred_at >= ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, fromTs, toTs)
        (Just accountId, Just fromTs, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at >= ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, fromTs, toTs)

pgCategorizeFinanceTransaction :: Pool Connection -> String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgCategorizeFinanceTransaction pool userId transactionId categorySlug =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireTransactionAmount conn userId transactionId
    validateCategorySlug conn userId categorySlug
    activeSplitRows <- tryExcept
      (query conn
        "SELECT split_index FROM finance_transaction_splits WHERE user_id = ? AND transaction_id = ? LIMIT 1"
        (userId, transactionId)
        :: IO [Only Int])
      mapSqlReadException
    if not (null activeSplitRows)
      then throwError AlreadyExists
      else do
        eventId <- liftIO (toString <$> nextRandom)
        _ <- tryExcept
          (withTransaction conn $ do
            _ <- execute conn
              "INSERT INTO finance_transaction_classification_events (event_id, user_id, transaction_id, event_type, category, splits_payload) VALUES (?, ?, ?, ?, ?, ?)"
              (eventId, userId, transactionId, ("TransactionCategorized" :: String), Just categorySlug, Nothing :: Maybe Value)
            _ <- execute conn
              "DELETE FROM finance_transaction_categories WHERE user_id = ? AND transaction_id = ?"
              (userId, transactionId)
            _ <- execute conn
              "INSERT INTO finance_transaction_categories (user_id, transaction_id, category) VALUES (?, ?, ?)"
              (userId, transactionId, categorySlug)
            pure ())
          mapSqlWriteException
        pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgSplitFinanceTransaction :: Pool Connection -> String -> String -> [FinanceTransactionSplitWriteRow] -> ExceptT RepositoryError IO FinanceTransaction
pgSplitFinanceTransaction pool userId transactionId splitRows =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    transactionAmount <- requireTransactionAmount conn userId transactionId
    validateSplitRows conn userId transactionAmount splitRows
    eventId <- liftIO (toString <$> nextRandom)
    _ <- tryExcept
      (withTransaction conn $ do
        _ <- execute conn
          "INSERT INTO finance_transaction_classification_events (event_id, user_id, transaction_id, event_type, category, splits_payload) VALUES (?, ?, ?, ?, ?, ?)"
          (eventId, userId, transactionId, ("TransactionSplit" :: String), Nothing :: Maybe String, Nothing :: Maybe Value)
        _ <- execute conn
          "DELETE FROM finance_transaction_categories WHERE user_id = ? AND transaction_id = ?"
          (userId, transactionId)
        _ <- execute conn
          "DELETE FROM finance_transaction_splits WHERE user_id = ? AND transaction_id = ?"
          (userId, transactionId)
        insertSplitRows conn userId transactionId splitRows
        pure ())
      mapSqlWriteException
    pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgLinkFinanceTransactions :: Pool Connection -> String -> String -> String -> String -> ExceptT RepositoryError IO (FinanceTransaction, FinanceTransaction)
pgLinkFinanceTransactions pool userId sourceTransactionId targetTransactionId linkType =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    if linkType /= "transfer"
      then throwError WriteFailure
      else do
        source <- loadTransactionFacts conn userId sourceTransactionId
        target <- loadTransactionFacts conn userId targetTransactionId
        validateTransferPair source target
        sourceLinked <- transactionHasLink conn userId sourceTransactionId
        targetLinked <- transactionHasLink conn userId targetTransactionId
        if sourceLinked || targetLinked
          then throwError AlreadyExists
          else do
            eventId <- liftIO (toString <$> nextRandom)
            _ <- tryExcept
              (withTransaction conn $ do
                _ <- execute conn
                  "INSERT INTO finance_transaction_link_events (event_id, user_id, source_transaction_id, target_transaction_id, link_type) VALUES (?, ?, ?, ?, ?)"
                  (eventId, userId, sourceTransactionId, targetTransactionId, linkType)
                _ <- execute conn
                  "INSERT INTO finance_transaction_links (user_id, transaction_id, peer_transaction_id, link_type) VALUES (?, ?, ?, ?)"
                  (userId, sourceTransactionId, targetTransactionId, linkType)
                _ <- execute conn
                  "INSERT INTO finance_transaction_links (user_id, transaction_id, peer_transaction_id, link_type) VALUES (?, ?, ?, ?)"
                  (userId, targetTransactionId, sourceTransactionId, linkType)
                pure ())
              mapSqlWriteException
            sourceTransaction <- pgLoadFinanceTransactionByIdInConn conn userId sourceTransactionId
            targetTransaction <- pgLoadFinanceTransactionByIdInConn conn userId targetTransactionId
            pure (sourceTransaction, targetTransaction)

data TransactionFacts = TransactionFacts
  { transactionFactsId :: !String
  , transactionFactsDirection :: !FinanceTransactionDirection
  , transactionFactsAccountId :: !String
  , transactionFactsAmount :: !Int
  }

loadTransactionFacts :: Connection -> String -> String -> ExceptT RepositoryError IO TransactionFacts
loadTransactionFacts conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id, direction, account_id, amount FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId)
      :: IO [(String, Text, String, Int64)])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [(loadedId, directionText, accountId, amount)] ->
      case directionFromText directionText of
        Nothing -> throwError ReadFailure
        Just parsedDirection ->
          pure TransactionFacts
            { transactionFactsId = loadedId
            , transactionFactsDirection = parsedDirection
            , transactionFactsAccountId = accountId
            , transactionFactsAmount = fromIntegral amount
            }
    _ -> throwError ReadFailure

transactionHasLink :: Connection -> String -> String -> ExceptT RepositoryError IO Bool
transactionHasLink conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id FROM finance_transaction_links WHERE user_id = ? AND transaction_id = ? LIMIT 1"
      (userId, transactionId)
      :: IO [Only String])
    mapSqlReadException
  pure (not (null rows))

validateTransferPair :: TransactionFacts -> TransactionFacts -> ExceptT RepositoryError IO ()
validateTransferPair source target
  | transactionFactsId source == transactionFactsId target = throwError WriteFailure
  | transactionFactsAccountId source == transactionFactsAccountId target = throwError WriteFailure
  | transactionFactsDirection source == transactionFactsDirection target = throwError WriteFailure
  | transactionFactsAmount source /= transactionFactsAmount target = throwError WriteFailure
  | otherwise = pure ()

insertSplitRows :: Connection -> String -> String -> [FinanceTransactionSplitWriteRow] -> IO ()
insertSplitRows _ _ _ [] = pure ()
insertSplitRows conn userId transactionId rows =
  mapM_ insertOne (zip [0 :: Int ..] rows)
  where
    insertOne (splitIndex, FinanceTransactionSplitWriteRow amount categorySlug) = do
      _ <- execute conn
        "INSERT INTO finance_transaction_splits (user_id, transaction_id, split_index, amount, category) VALUES (?, ?, ?, ?, ?)"
        (userId, transactionId, splitIndex, amount, categorySlug)
      pure ()

requireTransactionAmount :: Connection -> String -> String -> ExceptT RepositoryError IO Int
requireTransactionAmount conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT amount FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId)
      :: IO [Only Int64])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [Only amount] -> pure (fromIntegral amount)
    _ -> throwError ReadFailure

validateSplitRows :: Connection -> String -> Int -> [FinanceTransactionSplitWriteRow] -> ExceptT RepositoryError IO ()
validateSplitRows conn userId transactionAmount splitRows = do
  if length splitRows < 2
    then throwError WriteFailure
    else pure ()
  let amounts = map financeTransactionSplitWriteAmount splitRows
  if any (<= 0) amounts
    then throwError WriteFailure
    else pure ()
  if sum amounts /= transactionAmount
    then throwError WriteFailure
    else pure ()
  mapM_ (validateCategorySlug conn userId . financeTransactionSplitWriteCategory) splitRows

validateCategorySlug :: Connection -> String -> String -> ExceptT RepositoryError IO ()
validateCategorySlug conn userId categorySlug = do
  rows <- tryExcept
    (query conn
      "SELECT selectable FROM finance_categories WHERE category_id = ? AND (user_id IS NULL OR user_id = ?)"
      (categorySlug, userId)
      :: IO [Only Bool])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [Only selectable] ->
      if selectable
        then pure ()
        else throwError WriteFailure
    _ -> throwError ReadFailure

hydrateTransactionDetails :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateTransactionDetails conn userId transaction = do
  withTransfer <- hydrateTransferState conn userId transaction
  hydrateClassificationState conn userId withTransfer

hydrateTransferState :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateTransferState conn userId transaction = do
  rows <- tryExcept
    (query conn
      "SELECT peer_transaction_id, link_type, linked_at FROM finance_transaction_links WHERE user_id = ? AND transaction_id = ?"
      (userId, financeTransactionId transaction)
      :: IO [(String, String, UTCTime)])
    mapSqlReadException
  case rows of
    [] -> pure transaction { financeTransactionTransfer = Nothing }
    [(peerTransactionId, linkType, linkedAt)] -> do
      peerRows <- tryExcept
        (query conn
          "SELECT account_id, amount FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
          (userId, peerTransactionId)
          :: IO [(String, Int64)])
        mapSqlReadException
      case peerRows of
        [(peerAccountId, peerAmount)] ->
          pure transaction
            { financeTransactionTransfer =
                Just FinanceTransactionTransfer
                  { financeTransactionTransferLinkType = linkType
                  , financeTransactionTransferPeerTransactionId = peerTransactionId
                  , financeTransactionTransferPeerAccountId = peerAccountId
                  , financeTransactionTransferPeerAmount = fromIntegral peerAmount
                  , financeTransactionTransferLinkedAt = linkedAt
                  }
            }
        [] -> throwError ReadFailure
        _ -> throwError ReadFailure
    _ -> throwError ReadFailure

hydrateClassificationState :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateClassificationState conn userId transaction = do
  splitRows <- tryExcept
    (query conn
      "SELECT amount, category FROM finance_transaction_splits WHERE user_id = ? AND transaction_id = ? ORDER BY split_index ASC"
      (userId, financeTransactionId transaction)
      :: IO [(Int64, String)])
    mapSqlReadException
  if not (null splitRows)
    then do
      let decodedSplits =
            [ FinanceTransactionSplitRow
                { financeTransactionSplitAmount = fromIntegral amount
                , financeTransactionSplitCategory = categorySlug
                }
            | (amount, categorySlug) <- splitRows
            ]
      pure transaction
        { financeTransactionCategory = Nothing
        , financeTransactionSplits = decodedSplits
        }
    else do
      categoryRows <- tryExcept
        (query conn
          "SELECT category FROM finance_transaction_categories WHERE user_id = ? AND transaction_id = ?"
          (userId, financeTransactionId transaction)
          :: IO [Only String])
        mapSqlReadException
      case categoryRows of
        [] -> pure transaction { financeTransactionCategory = Nothing, financeTransactionSplits = [] }
        [Only categorySlug] -> pure transaction { financeTransactionCategory = Just categorySlug, financeTransactionSplits = [] }
        _ -> throwError ReadFailure

decodeBaseTransaction :: (String, Text, String, Int64, UTCTime, UTCTime) -> ExceptT RepositoryError IO FinanceTransaction
decodeBaseTransaction (transactionId, directionText, accountId, amount, occurredAt, recordedAt) =
  case directionFromText directionText of
    Nothing -> throwError ReadFailure
    Just financeTransactionDirection ->
      pure FinanceTransaction
        { financeTransactionId = transactionId
        , financeTransactionDirection = financeTransactionDirection
        , financeTransactionAccountId = accountId
        , financeTransactionAmount = fromIntegral amount
        , financeTransactionOccurredAt = occurredAt
        , financeTransactionRecordedAt = recordedAt
        , financeTransactionTransfer = Nothing
        , financeTransactionCategory = Nothing
        , financeTransactionSplits = []
        }

idempotencyMatches :: FinanceTransactionWriteRequest -> Text -> String -> Int64 -> Bool -> UTCTime -> Bool
idempotencyMatches FinanceTransactionWriteRequest
  { financeTransactionWriteDirection
  , financeTransactionWriteAccountId
  , financeTransactionWriteAmount
  , financeTransactionWriteOccurredAt
  , financeTransactionWriteOccurredAtSupplied
  }
  storedDirection
  storedAccountId
  storedAmount
  storedOccurredAtSupplied
  storedOccurredAt =
    financeTransactionDirectionText financeTransactionWriteDirection == storedDirection
      && financeTransactionWriteAccountId == storedAccountId
      && fromIntegral financeTransactionWriteAmount == storedAmount
      && financeTransactionWriteOccurredAtSupplied == storedOccurredAtSupplied
      && (not financeTransactionWriteOccurredAtSupplied || financeTransactionWriteOccurredAt == storedOccurredAt)

directionFromText :: Text -> Maybe FinanceTransactionDirection
directionFromText "sent" = Just FinanceTransactionSent
directionFromText "received" = Just FinanceTransactionReceived
directionFromText _ = Nothing
