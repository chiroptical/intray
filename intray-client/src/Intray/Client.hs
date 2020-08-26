{-# LANGUAGE DataKinds #-}

module Intray.Client
  ( module Intray.Client
  , module Intray.API
  , module Servant.API
  , module Servant.Client
  , module Servant.Auth.Client
  , module Data.Mergeless
  , module Data.UUID.Typed
  ) where

import Data.Mergeless
import qualified Data.UUID.Typed
import Import
import Intray.API
import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Client

clientGetShowItem :: Token -> ClientM (Maybe (ItemInfo TypedItem))
clientGetSize :: Token -> ClientM Int
clientGetItemUUIDs :: Token -> ClientM [ItemUUID]
clientGetItems :: Token -> ClientM [ItemInfo TypedItem]
clientPostAddItem :: Token -> TypedItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientPostSync ::
     Token
  -> SyncRequest ClientId ItemUUID (AddedItem TypedItem)
  -> ClientM (SyncResponse ClientId ItemUUID (AddedItem TypedItem))
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientPostChangePassphrase :: Token -> ChangePassphrase -> ClientM NoContent
clientDeleteAccount :: Token -> ClientM NoContent
clientPostAddAccessKey :: Token -> AddAccessKey -> ClientM AccessKeyCreated
clientGetAccessKey :: Token -> AccessKeyUUID -> ClientM AccessKeyInfo
clientGetAccessKeys :: Token -> ClientM [AccessKeyInfo]
clientDeleteAccessKey :: Token -> AccessKeyUUID -> ClientM NoContent
clientGetPermissions :: Token -> ClientM (Set Permission)
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin :: LoginForm -> ClientM (Headers '[ Header "Set-Cookie" Text] NoContent)
clientGetDocs :: ClientM GetDocsResponse
clientGetPricing :: ClientM (Maybe Pricing)
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> AccountUUID -> ClientM NoContent
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientGetShowItem :<|> clientGetSize :<|> clientGetItemUUIDs :<|> clientGetItems :<|> clientPostAddItem :<|> clientGetItem :<|> clientDeleteItem :<|> clientPostSync :<|> clientGetAccountInfo :<|> clientPostChangePassphrase :<|> clientDeleteAccount :<|> clientPostAddAccessKey :<|> clientGetAccessKey :<|> clientGetAccessKeys :<|> clientDeleteAccessKey :<|> clientGetPermissions :<|> clientPostRegister :<|> clientPostLogin :<|> clientGetDocs :<|> clientGetPricing :<|> clientAdminGetStats :<|> clientAdminDeleteAccount :<|> clientAdminGetAccounts =
  client (flatten intrayAPI)
