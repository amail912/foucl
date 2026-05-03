module Lib
    ( runApp
    , Backend(..)
    , makePostgresSessionStore
    , makePostgresCalendarRepository
    , makePostgresTripSharingRepository
    , makePostgresFinanceAccountRepository
    , makePostgresNoteRepository
    , makePostgresChecklistRepository
    , DatabaseConfig(..)
    , startupMigrationDomainsForBackend
    ) where

import Lib.Config
import Lib.Startup
