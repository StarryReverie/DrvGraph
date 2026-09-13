module DrvGraph.Core.Error
    ( AppError
    , AppEither
    , AppExceptT
    , appError
    , exceptionToAppError
    , addErrContext
    , liftErr
    , withErrContext
    , renderErr
    , unwrapRight
    ) where

import Control.Exception (Exception (displayException))
import Control.Monad.Except (ExceptT, MonadError (throwError), withError)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB

-- | Application-wide error type for reporting a chain of error messages.
newtype AppError = AppError (NonEmpty Text)
    deriving (Eq, Show)

-- | A convenient wrapper of @Either@ with @AppError@ as its error type.
type AppEither = Either AppError

-- | A convenient wrapper of @ExceptT@ with @AppError@ as its error type.
type AppExceptT m = ExceptT AppError m

-- | Make a new error.
appError :: Text -> AppError
appError = AppError . (:| [])

-- | Convert an @Exception e@ to @AppError@.
exceptionToAppError :: (Exception e) => e -> AppError
exceptionToAppError = appError . Text.pack . displayException

-- | Attach an additional layer of error message to the error type.
addErrContext :: Text -> AppError -> AppError
addErrContext msg (AppError errs) = AppError (msg <| errs)

-- | Lift a @Either@ into a @MonadError@.
liftErr :: (MonadError AppError m) => AppEither a -> m a
liftErr = either throwError pure

-- | Attach an additional layer of error message to the error monad.
withErrContext :: (MonadError AppError m) => Text -> m a -> m a
withErrContext msg = withError (addErrContext msg)

-- | Pretty print this error type in multiple lines.
renderErr :: AppError -> Text
renderErr (AppError (direct :| indirects)) = firstMsg <> otherMsg
  where
    firstMsg = "error:     " <> direct <> "\n"
    otherMsg = TL.toStrict . TB.toLazyText $ builder
    builder = foldMap (\e -> "caused by: " <> TB.fromText e <> "\n") indirects

-- | Unwrap @Right@ or error on @Left@.
unwrapRight :: AppEither a -> a
unwrapRight = either (error . Text.unpack . renderErr) id
