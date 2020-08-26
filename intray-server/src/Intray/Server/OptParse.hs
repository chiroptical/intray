{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Server.OptParse
  ( module Intray.Server.OptParse
  , module Intray.Server.OptParse.Types
  ) where

import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import qualified Env
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Looper
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import qualified System.Environment as System
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe
import qualified YamlParse.Applicative as YamlParse

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let port = fromMaybe 8001 $ serveFlagPort <|> envPort <|> mc confPort
  let host =
        T.pack $ fromMaybe ("localhost:" <> show port) $ serveFlagHost <|> envHost <|> mc confHost
  let logLevel = fromMaybe LevelInfo $ serveFlagLogLevel <|> envLogLevel <|> mc confLogLevel
  signingKeyFile <-
    case serveFlagSigningKeyFile <|> envSigningKeyFile <|> mc confSigningKeyFile of
      Nothing -> resolveFile' "signing-key.json"
      Just skf -> resolveFile' skf
  let connInfo =
        mkSqliteConnectionInfo $ fromMaybe "intray.db" (serveFlagDb <|> envDb <|> mc confDb)
  admins <-
    forM (serveFlagAdmins ++ fromMaybe [] (mc confAdmins)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
        Just u -> pure u
  freeloaders <-
    forM (serveFlagFreeloaders ++ fromMaybe [] (mc confFreeloaders)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid freeloader username:", s]
        Just u -> pure u
  mmSets <-
    do let mmc :: (MonetisationConfiguration -> Maybe a) -> Maybe a
           mmc func = mc confMonetisationConfig >>= func
       let plan =
             Stripe.PlanId . T.pack <$>
             (serveFlagStripePlan <|> envStripePlan <|> mmc monetisationConfStripePlan)
       let config =
             (\sk ->
                StripeConfig
                  { Stripe.secretKey = StripeKey $ TE.encodeUtf8 $ T.pack sk
                  , stripeEndpoint = Nothing
                  }) <$>
             (serveFlagStripeSecretKey <|> envStripeSecretKey <|>
              mmc monetisationConfStripeSecretKey)
       let publicKey =
             T.pack <$>
             (serveFlagStripePublishableKey <|> envStripePublishableKey <|>
              mmc monetisationConfStripePublishableKey)
       let fetcherSets =
             deriveLooperSettings
               (seconds 0)
               (minutes 1)
               serveFlagLooperStripeEventsFetcher
               envLooperStripeEventsFetcher
               (mmc monetisationConfStripeEventsFetcher)
       let retrierSets =
             deriveLooperSettings
               (seconds 30)
               (hours 24)
               serveFlagLooperStripeEventsRetrier
               envLooperStripeEventsRetrier
               (mmc monetisationConfStripeEventsRetrier)
       let maxItemsFree =
             fromMaybe 5 $
             serveFlagMaxItemsFree <|> envMaxItemsFree <|> mmc monetisationConfMaxItemsFree
       pure $
         MonetisationSettings <$> (StripeSettings <$> plan <*> config <*> publicKey) <*>
         pure fetcherSets <*>
         pure retrierSets <*>
         pure maxItemsFree
  pure
    ( DispatchServe
        ServeSettings
          { serveSetHost = host
          , serveSetPort = port
          , serveSetLogLevel = logLevel
          , serveSetSigningKeyFile = signingKeyFile
          , serveSetConnectionInfo = connInfo
          , serveSetAdmins = admins
          , serveSetFreeloaders = freeloaders
          , serveSetMonetisationSettings = mmSets
          }
    , Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  cp <-
    case flagConfigFile <|> envConfigFile of
      Nothing -> getDefaultConfigFile
      Just cf -> resolveFile' cf
  YamlParse.readConfigFile cp

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  configDir <- getXdgDir XdgConfig (Just [reldir|intray|])
  resolveFile configDir "config.yaml"

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_SERVER_" $
  Environment <$>
  Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.help "Config file") <*>
  Env.var (fmap Just . Env.str) "HOST" (Env.def Nothing <> Env.help "host to run the api server on") <*>
  Env.var
    (fmap Just . Env.auto)
    "PORT"
    (Env.def Nothing <> Env.help "port to run the api server on") <*>
  Env.var (fmap Just . Env.str) "DATABASE" (Env.def Nothing <> Env.help "database file") <*>
  Env.var
    (fmap Just . Env.auto)
    "LOG_LEVEL"
    (Env.def Nothing <> Env.help "minimal severity of log messages") <*>
  Env.var
    (fmap Just . Env.auto)
    "SIGNING_KEY_FILE"
    (Env.def Nothing <> Env.help "the file to store the signing key in") <*>
  Env.var
    (fmap Just . Env.auto)
    "STRIPE_PLAN"
    (Env.def Nothing <> Env.help "stripe plan id for subscriptions") <*>
  Env.var
    (fmap Just . Env.auto)
    "STRIPE_SECRET_KEY"
    (Env.def Nothing <> Env.help "stripe secret key") <*>
  Env.var
    (fmap Just . Env.auto)
    "STRIPE_PUBLISHABLE_KEY"
    (Env.def Nothing <> Env.help "stripe publishable key") <*>
  looperVarEnv "STRIPE_EVENTS_FETCHER" <*>
  looperVarEnv "STRIPE_EVENTS_RETRIER" <*>
  Env.var
    (fmap Just . Env.auto)
    "MAX_ITEMS_FREE"
    (Env.def Nothing <> Env.help "maximum items that a free user can have")

looperVarEnv :: String -> Env.Parser Env.Error LooperEnvironment
looperVarEnv n = Env.prefixed "LOOPER_" $ looperEnvironmentParser n

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser
        , ""
        , "Configuration file format:"
        , T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser = CommandServe <$> parseServeFlags
    modifier = fullDesc <> progDesc "Serve requests" <> YamlParse.confDesc @Configuration

parseServeFlags :: Parser ServeFlags
parseServeFlags =
  ServeFlags <$>
  option
    (Just <$> str)
    (mconcat [long "api-host", value Nothing, metavar "HOST", help "the host to serve on"]) <*>
  option
    (Just <$> auto)
    (mconcat [long "api-port", value Nothing, metavar "PORT", help "the port to serve on"]) <*>
  option
    (Just . T.pack <$> str)
    (mconcat
       [ long "database"
       , value Nothing
       , metavar "DATABASE_CONNECTION_STRING"
       , help "The sqlite connection string"
       ]) <*>
  many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin"])) <*>
  many (strOption (mconcat [long "freeloader", metavar "USERNAME", help "A freeloader"])) <*>
  option
    (Just <$> auto)
    (mconcat
       [ long "log-level"
       , metavar "LOG_LEVEL"
       , value Nothing
       , help $
         "the log level, possible values: " <> show [LevelDebug, LevelInfo, LevelWarn, LevelError]
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "signing-key-file"
       , value Nothing
       , metavar "FILEPATH"
       , help "the file to store the signing key in"
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "stripe-plan"
       , value Nothing
       , metavar "PLAN_ID"
       , help "The product pricing plan for stripe"
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "stripe-secret-key"
       , value Nothing
       , metavar "SECRET_KEY"
       , help "The secret key for stripe"
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "stripe-publishable-key"
       , value Nothing
       , metavar "PUBLISHABLE_KEY"
       , help "The publishable key for stripe"
       ]) <*>
  getLooperFlags "stripe-events-fetcher" <*>
  getLooperFlags "stripe-events-retrier" <*>
  option
    (Just <$> auto)
    (mconcat
       [ long "max-items-free"
       , value Nothing
       , metavar "INT"
       , help "How many items a user can sync in the free plan"
       ])

parseFlags :: Parser Flags
parseFlags =
  Flags <$>
  option
    (Just <$> str)
    (mconcat [long "config-file", value Nothing, metavar "FILEPATH", help "The config file"])
