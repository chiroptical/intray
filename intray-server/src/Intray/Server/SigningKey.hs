module Intray.Server.SigningKey
  ( loadSigningKey
  ) where

import Crypto.JOSE.JWK (JWK)
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Import
import Servant.Auth.Server as Auth

storeSigningKey :: Path Abs File -> JWK -> IO ()
storeSigningKey skf key_ = LB.writeFile (toFilePath skf) (JSON.encodePretty key_)

loadSigningKey :: Path Abs File -> IO JWK
loadSigningKey skf = do
  mErrOrKey <- forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath skf)
  case mErrOrKey of
    Nothing -> do
      key_ <- Auth.generateKey
      storeSigningKey skf key_
      pure key_
    Just (Left err) ->
      die $ unlines ["Failed to load signing key from file", fromAbsFile skf, "with error:", err]
    Just (Right r) -> pure r
