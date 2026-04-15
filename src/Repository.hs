module Repository
  ( RepositoryError(..)
  ) where

data RepositoryError
  = AlreadyExists
  | NotFound
  | ReadFailure
  | WriteFailure
  | StorageFailure
  deriving (Eq, Show)
