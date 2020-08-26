{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Foundation
  ( module Intray.Web.Server.Foundation
  , module Intray.Web.Server.Widget
  , module Intray.Web.Server.Static
  , module Intray.Web.Server.Constants
  , module Intray.Web.Server.DB
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql
import Import
import Intray.Client
import Intray.Web.Server.Constants
import Intray.Web.Server.DB
import Intray.Web.Server.Static
import Intray.Web.Server.Widget
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Text.Hamlet
import Web.Cookie
import Yesod hiding (Header)
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.EmbeddedStatic

type IntrayWidget = IntrayWidget' ()

type IntrayWidget' = WidgetFor App

type IntrayHandler = HandlerFor App

type IntrayAuthHandler a = AuthHandler App a

data App =
  App
    { appHttpManager :: Http.Manager
    , appStatic :: EmbeddedStatic
    , appTracking :: Maybe Text
    , appVerification :: Maybe Text
    , appAPIBaseUrl :: BaseUrl
    , appConnectionPool :: ConnectionPool
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $(widgetFile "default-body")
    app <- getYesod
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
  authRoute _ = Just $ AuthR LoginR
  maximumContentLengthIO s =
    \case
      Just AddR -> pure Nothing
      r -> pure $ maximumContentLength s r
  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) "client_session_key.aes"

instance PathPiece Username where
  fromPathPiece = parseUsername
  toPathPiece = usernameText

instance YesodAuth App where
  type AuthId App = Username
  loginDest _ = AddR
  logoutDest _ = HomeR
  authHttpManager = getsYesod appHttpManager
  authenticate creds =
    if credsPlugin creds == intrayAuthPluginName
      then case parseUsername $ credsIdent creds of
             Nothing -> pure $ UserError Msg.InvalidLogin
             Just un -> pure $ Authenticated un
      else pure $ ServerError $ T.unwords ["Unknown authentication plugin:", credsPlugin creds]
  authPlugins _ = [intrayAuthPlugin]
  maybeAuthId =
    runMaybeT $ do
      s <- MaybeT $ lookupSession credsKey
      MaybeT $ return $ fromPathPiece s

intrayAuthPluginName :: Text
intrayAuthPluginName = "intray-auth-plugin"

intrayAuthPlugin :: AuthPlugin App
intrayAuthPlugin = AuthPlugin intrayAuthPluginName dispatch loginWidget
  where
    dispatch :: Text -> [Text] -> IntrayAuthHandler TypedContent
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["register"] = getNewAccountR >>= sendResponse
    dispatch "POST" ["register"] = postNewAccountR >>= sendResponse
    dispatch "GET" ["change-password"] = getChangePasswordR >>= sendResponse
    dispatch "POST" ["change-password"] = postChangePasswordR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget :: (Route Auth -> Route App) -> IntrayWidget
    loginWidget _ = do
      token <- genToken
      msgs <- getMessages
      $(widgetFile "auth/login")

data LoginData =
  LoginData
    { loginUserkey :: Text
    , loginPassword :: Text
    }
  deriving (Show)

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR intrayAuthPluginName ["login"]

postLoginR :: IntrayAuthHandler TypedContent
postLoginR = do
  let loginInputForm = LoginData <$> ireq textField "userkey" <*> ireq passwordField "passphrase"
  result <- runInputPostResult loginInputForm
  muser <-
    case result of
      FormMissing -> invalidArgs ["Form is missing"]
      FormFailure _ -> return $ Left Msg.InvalidLogin
      FormSuccess (LoginData ukey pwd) ->
        case parseUsername ukey of
          Nothing -> pure $ Left Msg.InvalidUsernamePass
          Just un -> do
            liftHandler $ login LoginForm {loginFormUsername = un, loginFormPassword = pwd}
            pure $ Right un
  case muser of
    Left err -> loginErrorMessageI LoginR err
    Right un -> setCredsRedirect $ Creds intrayAuthPluginName (usernameText un) []

registerR :: AuthRoute
registerR = PluginR intrayAuthPluginName ["register"]

getNewAccountR :: IntrayAuthHandler Html
getNewAccountR = do
  token <- genToken
  msgs <- getMessages
  liftHandler $ defaultLayout $(widgetFile "auth/register")

data NewAccount =
  NewAccount
    { newAccountUsername :: Username
    , newAccountPassword1 :: Text
    , newAccountPassword2 :: Text
    }
  deriving (Show)

postNewAccountR :: IntrayAuthHandler TypedContent
postNewAccountR = do
  let newAccountInputForm =
        NewAccount <$>
        ireq
          (checkMMap
             (\t ->
                pure $
                case parseUsernameWithError t of
                  Left err -> Left (T.pack $ unwords ["Invalid username:", show t ++ ";", err])
                  Right un -> Right un)
             usernameText
             textField)
          "username" <*>
        ireq passwordField "passphrase" <*>
        ireq passwordField "passphrase-confirm"
  mr <- liftHandler getMessageRender
  result <- liftHandler $ runInputPostResult newAccountInputForm
  mdata <-
    case result of
      FormMissing -> invalidArgs ["Form is incomplete"]
      FormFailure msgs -> pure $ Left msgs
      FormSuccess d ->
        pure $
        if newAccountPassword1 d == newAccountPassword2 d
          then Right
                 Registration
                   { registrationUsername = newAccountUsername d
                   , registrationPassword = newAccountPassword1 d
                   }
          else Left [mr Msg.PassMismatch]
  case mdata of
    Left errs -> do
      setMessage $ toHtml $ T.concat errs
      liftHandler $ redirect $ AuthR registerR
    Right reg -> do
      errOrOk <- liftHandler $ runClient $ clientPostRegister reg
      case errOrOk of
        Left err -> do
          case err of
            FailureResponse _ resp ->
              case Http.statusCode $ responseStatusCode resp of
                409 -> setMessage "An account with this username already exists"
                _ -> setMessage "Failed to register for unknown reasons."
            _ -> setMessage "Failed to register for unknown reasons."
          liftHandler $ redirect $ AuthR registerR
        Right NoContent ->
          liftHandler $ do
            login
              LoginForm
                { loginFormUsername = registrationUsername reg
                , loginFormPassword = registrationPassword reg
                }
            setCredsRedirect $
              Creds intrayAuthPluginName (usernameText $ registrationUsername reg) []

changePasswordTargetR :: AuthRoute
changePasswordTargetR = PluginR intrayAuthPluginName ["change-password"]

data ChangePassword =
  ChangePassword
    { changePasswordOldPassword :: Text
    , changePasswordNewPassword1 :: Text
    , changePasswordNewPassword2 :: Text
    }
  deriving (Show)

getChangePasswordR :: IntrayAuthHandler Html
getChangePasswordR = do
  token <- genToken
  msgs <- getMessages
  liftHandler $ defaultLayout $(widgetFile "auth/change-password")

postChangePasswordR :: IntrayAuthHandler Html
postChangePasswordR = do
  ChangePassword {..} <-
    liftHandler $
    runInputPost $
    ChangePassword <$> ireq passwordField "old" <*> ireq passwordField "new1" <*>
    ireq passwordField "new2"
  unless (changePasswordNewPassword1 == changePasswordNewPassword2) $
    invalidArgs ["Passwords do not match."]
  liftHandler $
    withLogin $ \t -> do
      let cpp =
            ChangePassphrase
              { changePassphraseOld = changePasswordOldPassword
              , changePassphraseNew = changePasswordNewPassword1
              }
      NoContent <- runClientOrErr $ clientPostChangePassphrase t cpp
      redirect AccountR

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance PathPiece (UUID a) where
  fromPathPiece = parseUUID
  toPathPiece = uuidText

withNavBar :: WidgetFor App () -> HandlerFor App Html
withNavBar widget = do
  mauth <- maybeAuthId
  msgs <- getMessages
  defaultLayout $(widgetFile "with-nav-bar")

genToken :: MonadHandler m => m Html
genToken = do
  alreadyExpired
  req <- getRequest
  let tokenKey = defaultCsrfParamName
  pure $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

runClient :: ClientM a -> Handler (Either ClientError a)
runClient func = do
  man <- getsYesod appHttpManager
  burl <- getsYesod appAPIBaseUrl
  let cenv = ClientEnv man burl Nothing
  liftIO $ runClientM func cenv

runClientOrErr :: ClientM a -> Handler a
runClientOrErr func = do
  errOrRes <- runClient func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp -> sendResponseStatus Http.status500 $ show resp
    Right r -> pure r

runClientOrDisallow :: ClientM a -> Handler (Maybe a)
runClientOrDisallow func = do
  errOrRes <- runClient func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then pure Nothing
          else sendResponseStatus Http.status500 $ show resp
    Right r -> pure $ Just r

handleStandardServantErrs :: ClientError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
  case err of
    FailureResponse _ resp -> func resp
    ConnectionError e -> redirect $ ErrorAPIDownR $ T.pack $ show e
    e -> sendResponseStatus Http.status500 $ unwords ["Error while calling API:", show e]

login :: LoginForm -> Handler ()
login form = do
  errOrRes <- runClient $ clientPostLogin form
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then do
            addMessage "error" "Unable to login"
            redirect $ AuthR LoginR
          else sendResponseStatus Http.status500 $ show resp
    Right (Headers NoContent (HCons sessionHeader HNil)) ->
      case sessionHeader of
        Header session -> recordLoginToken (loginFormUsername form) session
        _ ->
          sendResponseStatus Http.status500 $
          unwords ["The server responded but with an invalid header for login", show sessionHeader]

withLogin :: ToTypedContent a => (Token -> Handler a) -> Handler a
withLogin func = do
  un <- requireAuthId
  mLoginToken <- lookupToginToken un
  case mLoginToken of
    Nothing -> redirect $ AuthR LoginR
    Just token -> func token

lookupToginToken :: Username -> Handler (Maybe Token)
lookupToginToken un = runDb $ fmap (userTokenToken . entityVal) <$> getBy (UniqueUserToken un)

recordLoginToken :: Username -> Text -> Handler ()
recordLoginToken un session = do
  let token = Token $ setCookieValue $ parseSetCookie $ TE.encodeUtf8 session
  void $
    runDb $ upsert UserToken {userTokenName = un, userTokenToken = token} [UserTokenToken =. token]

runDb :: SqlPersistT IO a -> Handler a
runDb func = do
  pool <- getsYesod appConnectionPool
  liftIO $ runSqlPool func pool

addInfoMessage :: Html -> Handler ()
addInfoMessage = addMessage ""

addNegativeMessage :: Html -> Handler ()
addNegativeMessage = addMessage "negative"

addPositiveMessage :: Html -> Handler ()
addPositiveMessage = addMessage "positive"
