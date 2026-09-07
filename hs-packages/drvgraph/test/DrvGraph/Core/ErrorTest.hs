module DrvGraph.Core.ErrorTest
    ( unit_renderSingleError
    , unit_renderAttachedContextError
    , unit_withErrorContext
    ) where

import Data.Function ((&))
import Data.Text qualified as Text
import Test.Tasty.HUnit ((@?=))

import DrvGraph.Core.Error (addErrContext, appError, renderErr, withErrContext)

unit_renderSingleError :: IO ()
unit_renderSingleError = do
    let err = appError "Test error"
    renderErr err @?= Text.unlines ["error:     Test error"]

unit_renderAttachedContextError :: IO ()
unit_renderAttachedContextError = do
    let err =
            appError "Root error"
                & addErrContext "Intermediate error"
                & addErrContext "Direct error"
    renderErr err
        @?= Text.unlines
            [ "error:     Direct error"
            , "caused by: Intermediate error"
            , "caused by: Root error"
            ]

unit_withErrorContext :: IO ()
unit_withErrorContext = do
    let Left err = withErrContext "Direct error" $ do
            withErrContext "Intermediate error" $ do
                Left $ appError "Root error"
    renderErr err
        @?= Text.unlines
            [ "error:     Direct error"
            , "caused by: Intermediate error"
            , "caused by: Root error"
            ]
