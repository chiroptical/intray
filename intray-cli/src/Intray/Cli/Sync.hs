module Intray.Cli.Sync
    ( withStoreAndSync
    , modifyStoreAndSync
    , syncAndGet
    , syncAndReturn
    ) where

import Import

import Intray.Client
import Intray.Client.Store

import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store

withStoreAndSync :: (Store -> CliM Store) -> CliM ()
withStoreAndSync func = do
    before <- readStoreOrEmpty
    processed <- func before
    let req = makeSyncRequest processed
    strat <- asks setSyncStrategy
    mErrOrStore <-
        case strat of
            NeverSync -> pure Nothing
            AlwaysSync ->
                withToken $ \t -> runSingleClient $ clientPostSync t req
    after <-
        case mErrOrStore of
            Nothing -> pure processed
            Just errOrStore ->
                case errOrStore of
                    Left err -> do
                        liftIO $
                            putStrLn $
                            unlines
                                [ "Sync failed, but store still modified succesfully:"
                                , show err
                                ]
                        pure processed
                    Right r -> pure $ mergeStore before r
    writeStore after

modifyStoreAndSync :: (Store -> Store) -> CliM ()
modifyStoreAndSync func = withStoreAndSync (pure . func)

syncAndGet :: (Store -> CliM a) -> CliM a
syncAndGet func = do
    before <- readStoreOrEmpty
    let req = makeSyncRequest before
    strat <- asks setSyncStrategy
    mErrOrStore <-
        case strat of
            NeverSync -> pure Nothing
            AlwaysSync ->
                withToken $ \t -> runSingleClient $ clientPostSync t req
    case mErrOrStore of
        Nothing -> func before
        Just errOrStore ->
            case errOrStore of
                Left err -> do
                    liftIO $
                        putStrLn $
                        unlines
                            [ "Sync failed, but still fetched succesfully:"
                            , show err
                            ]
                    func before
                Right r -> do
                    let after = mergeStore before r
                    writeStore after
                    func after

syncAndReturn :: (Store -> a) -> CliM a
syncAndReturn func = syncAndGet $ pure . func
