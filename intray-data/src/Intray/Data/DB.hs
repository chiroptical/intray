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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.DB where

import Data.Set (Set)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Intray.Data.AccessKeyUUID
import Intray.Data.AccountUUID
import Intray.Data.HashedPassword
import Intray.Data.Import
import Intray.Data.ItemType
import Intray.Data.ItemUUID
import Intray.Data.Permission
import Intray.Data.Stripe ()
import Intray.Data.Username
import qualified Web.Stripe.Types as Stripe

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
    identifier AccountUUID
    username Username
    hashedPassword HashedPassword
    createdTimestamp UTCTime
    lastLogin UTCTime Maybe

    UniqueUserIdentifier identifier
    UniqueUsername username

    deriving Show
    deriving Eq
    deriving Generic


Customer
    user AccountUUID
    stripeCustomer Stripe.CustomerId
    UniqueCustomerUser user
    UniqueUserCustomer stripeCustomer
    deriving Show
    deriving Eq
    deriving Generic


StripeEvent
    event Stripe.EventId
    error Text Maybe
    UniqueStripeEvent event
    deriving Show
    deriving Eq
    deriving Generic


IntrayItem
    identifier ItemUUID
    userId AccountUUID
    type ItemType
    contents ByteString
    created UTCTime

    UniqueItemIdentifier identifier

    deriving Show
    deriving Eq
    deriving Generic


AccessKey
    identifier AccessKeyUUID
    user AccountUUID
    name Text
    hashedKey HashedPassword
    createdTimestamp UTCTime
    permissions (Set Permission)

    UniqueAccessKeyIdentifier identifier

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity IntrayItem

instance Validity User

instance Validity AccessKey
