module DrvGraph.Core.ErrorTest
    ( unit_renderSingleError
    , unit_renderAttachedContextError
    , unit_checkpointAppError
    ) where

import Data.Function ((&))
import Data.Text qualified as Text
import Test.Tasty.HUnit ((@?=))

import DrvGraph.Core.Error (addAppErrorContext, appError, checkpointAppError, renderAppError, throwAppErrorText, tryAppError)

unit_renderSingleError :: IO ()
unit_renderSingleError = do
    let err = appError "Test error"
    renderAppError err @?= Text.unlines ["error:     Test error"]

unit_renderAttachedContextError :: IO ()
unit_renderAttachedContextError = do
    let err =
            appError "Root error"
                & addAppErrorContext "Intermediate error"
                & addAppErrorContext "Direct error"
    renderAppError err
        @?= Text.unlines
            [ "error:     Direct error"
            , "caused by: Intermediate error"
            , "caused by: Root error"
            ]

unit_checkpointAppError :: IO ()
unit_checkpointAppError = do
    let result = tryAppError $ do
            checkpointAppError "Direct error" $ do
                checkpointAppError "Intermediate error" $ do
                    throwAppErrorText "Root error"
    Left err <- result
    renderAppError err
        @?= Text.unlines
            [ "error:     Direct error"
            , "caused by: Intermediate error"
            , "caused by: Root error"
            ]
