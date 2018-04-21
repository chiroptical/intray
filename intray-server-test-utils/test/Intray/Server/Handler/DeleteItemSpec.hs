module Intray.Server.Handler.DeleteItemSpec
    ( spec
    ) where

import TestImport

import Network.HTTP.Types.Status
import Servant.Client

import Intray.Client

import Intray.Client.Gen ()
import Intray.Data.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    describe "DeleteItem" $
    it "succesfully manages to delete the item that was just added" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
                errOrItem <-
                    runClient cenv $ do
                        uuid <- clientPostAddItem token t
                        void $ clientDeleteItem token uuid
                        clientGetItem token uuid
                case errOrItem of
                    Left err ->
                        case err of
                            FailureResponse {} ->
                                statusCode (responseStatus err) `shouldBe` 404
                            _ ->
                                expectationFailure $
                                unwords ["Unexpected error:", show err]
                    Right _ -> expectationFailure "Should not have succeeded."
