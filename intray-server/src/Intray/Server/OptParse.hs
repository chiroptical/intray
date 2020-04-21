{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.OptParse
  ( module Intray.Server.OptParse
  , module Intray.Server.OptParse.Types
  ) where

import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Looper
import Options.Applicative
import qualified System.Environment as System
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

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
  let connInfo =
        mkSqliteConnectionInfo $ fromMaybe "intray.db" (serveFlagDb <|> envDb <|> mc confDb)
  admins <-
    forM (serveFlagAdmins ++ fromMaybe [] (mc confAdmins)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
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
          , serveSetConnectionInfo = connInfo
          , serveSetAdmins = admins
          , serveSetMonetisationSettings = mmSets
          }
    , Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <-
    case flagConfigFile <|> envConfigFile of
      Nothing -> getDefaultConfigFile
      Just cf -> resolveFile' cf
  mContents <- forgivingAbsence $ SB.readFile (fromAbsFile configFile)
  forM mContents $ \contents ->
    case Yaml.decodeEither' contents of
      Left err ->
        die $
        unlines
          [ unwords ["Failed to read config file:", fromAbsFile configFile]
          , Yaml.prettyPrintParseException err
          ]
      Right res -> pure res

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  configDir <- getXdgDir XdgConfig (Just [reldir|intray|])
  resolveFile configDir "config.yaml"

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let mv k = lookup ("INTRAY_SERVER_" <> k) env
      mr :: Read a => String -> IO (Maybe a)
      mr k =
        forM (mv k) $ \s ->
          case readMaybe s of
            Nothing -> die $ "Un-Read-able value: " <> s
            Just val -> pure val
      le n = readLooperEnvironment "INTRAY_SERVER_LOOPER_" n env
  let envConfigFile = mv "CONFIG_FILE"
  envPort <- mr "PORT"
  let envHost = mv "HOST"
  let envDb = T.pack <$> mv "DATABASE"
  envLogLevel <- mr "LOG_LEVEL"
  let envStripePlan = mv "STRIPE_PLAN"
  let envStripeSecretKey = mv "STRIPE_SECRET_KEY"
  let envStripePublishableKey = mv "STRIPE_PUBLISHABLE_KEY"
  let envLooperStripeEventsFetcher = le "STRIPE_EVENTS_FETCHER"
  let envLooperStripeEventsRetrier = le "STRIPE_EVENTS_RETRIER"
  envMaxItemsFree <- mr "MAX_ITEMS_FREE"
  pure Environment {..}

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Intray server"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser = CommandServe <$> parseServeFlags
    modifier = fullDesc <> progDesc "Command example."

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
  many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin to use"])) <*>
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
