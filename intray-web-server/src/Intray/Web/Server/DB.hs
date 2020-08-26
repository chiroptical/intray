{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Intray.Web.Server.DB where

import Database.Persist.Sql
import Database.Persist.TH
import Intray.Client
import Intray.Web.Server.Persistence ()

share
  [mkPersist sqlSettings, mkMigrate "migrateLoginCache"]
  [persistLowerCase|

UserToken
    name Username
    token Token

    UniqueUserToken name

    deriving Show
    deriving Eq
    deriving Generic
|]
