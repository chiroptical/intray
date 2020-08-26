{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Web.Server.OptParse
  ( getInstructions
  , Instructions
  , Dispatch(..)
  , Settings(..)
  , ServeSettings(..)
  ) where

import qualified Data.Text as T
import qualified Env
import Import
import qualified Intray.Server.OptParse as API
import Intray.Web.Server.OptParse.Types
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import qualified System.Environment as System
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
  (API.DispatchServe apiSets, API.Settings) <-
    API.combineToInstructions
      (API.CommandServe serveFlagAPIFlags)
      flagAPIFlags
      envAPIEnvironment
      (confAPIConfiguration <$> mConf)
  let port = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  when (API.serveSetPort apiSets == port) $
    die $
    unlines ["Web server port and API port must not be the same.", "They are both: " ++ show port]
  pure
    ( DispatchServe
        ServeSettings
          { serveSetAPISettings = apiSets
          , serveSetPort = port
          , serveSetTracking = serveFlagTracking <|> envTracking <|> mc confTracking
          , serveSetVerification = serveFlagVerification <|> envVerification <|> mc confVerification
          , serveSetLoginCacheFile =
              fromMaybe "intray-web-server.db" $
              serveFlagLoginCacheFile <|> envLoginCacheFile <|> mc confLoginCacheFile
          }
    , Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <-
    case API.flagConfigFile flagAPIFlags <|> API.envConfigFile envAPIEnvironment of
      Nothing -> API.getDefaultConfigFile
      Just cf -> resolveFile' cf
  YamlParse.readConfigFile configFile

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  (\apiEnv (a, b, c, d) -> Environment apiEnv a b c d) <$> API.environmentParser <*>
  Env.prefixed
    "INTRAY_WEB_SERVER_"
    ((,,,) <$>
     Env.var
       (fmap Just . Env.auto)
       "PORT"
       (Env.def Nothing <> Env.help "port to run the web server on") <*>
     Env.var
       (fmap Just . Env.auto)
       "ANALYTICS_TRACKING_ID"
       (Env.def Nothing <> Env.help "google analytics tracking id") <*>
     Env.var
       (fmap Just . Env.auto)
       "SEARCH_CONSOLE_VERIFICATION"
       (Env.def Nothing <> Env.help "google search console verification id") <*>
     Env.var
       (fmap Just . Env.str)
       "LOGIN_CACHE_FILE"
       (Env.def Nothing <> Env.help "google search console verification id"))

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
    parser =
      CommandServe <$>
      (ServeFlags <$> API.parseServeFlags <*>
       option
         (Just <$> auto)
         (mconcat [long "web-port", metavar "PORT", value Nothing, help "the port to serve on"]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "analytics-tracking-id"
            , value Nothing
            , metavar "TRACKING_ID"
            , help "The google analytics tracking ID"
            ]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "search-console-verification"
            , value Nothing
            , metavar "VERIFICATION_TAG"
            , help "The contents of the google search console verification tag"
            ]) <*>
       option
         (Just <$> str)
         (mconcat
            [ long "login-cache-file"
            , value Nothing
            , metavar "FILEPATH"
            , help "The file to store the login cache database in"
            ]))
    modifier = fullDesc <> progDesc "Serve requests" <> YamlParse.confDesc @Configuration

parseFlags :: Parser Flags
parseFlags = Flags <$> API.parseFlags
