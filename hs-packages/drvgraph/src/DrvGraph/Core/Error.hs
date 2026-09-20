module DrvGraph.Core.Error
    ( AppError
    , AppEither
    , appError
    , exceptionToAppError
    , addAppErrorContext
    , throwAppError
    , throwAppErrorText
    , throwAppEither
    , throwEitherAsAppError
    , rethrowAsAppError
    , tryAppError
    , catchAppError
    , handleAppError
    , checkpointAppError
    , renderAppError
    , unwrapRight
    ) where

import Control.Exception.Safe (Exception (displayException), MonadCatch, MonadThrow, catch, handle, throwM, try)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as LazyTextBuilder

-- | Application-wide error type for reporting a chain of error messages.
newtype AppError = AppError (NonEmpty Text)
    deriving (Eq, Show)

instance Exception AppError where
    displayException = Text.unpack . Text.strip . renderAppError

-- | A convenient wrapper of @Either@ with @AppError@ as its error type.
type AppEither = Either AppError

-- | Make a new error.
appError :: Text -> AppError
appError = AppError . (:| [])

-- | Convert an @Exception e@ to @AppError@.
exceptionToAppError :: (Exception e) => e -> AppError
exceptionToAppError = appError . Text.pack . displayException

-- | Attach an additional layer of error message to the error type.
addAppErrorContext :: Text -> AppError -> AppError
addAppErrorContext msg (AppError errs) = AppError (msg <| errs)

-- | Throw an @AppError@ as an exception.
throwAppError :: (MonadThrow m) => AppError -> m a
throwAppError = throwM

-- | Make a new error and throw it.
throwAppErrorText :: (MonadThrow m) => Text -> m a
throwAppErrorText = throwM . appError

-- | Lift a pure @AppEither@ into a monad that can throw @AppError@.
throwAppEither :: (MonadThrow m) => AppEither a -> m a
throwAppEither = either throwM pure

-- | Throw a pure @Either@ carrying a foreign exception as @AppError@.
throwEitherAsAppError :: (Exception e, MonadThrow m) => Either e a -> m a
throwEitherAsAppError = either (throwM . exceptionToAppError) pure

-- | Catch a foreign exception and rethrow it as @AppError@.
rethrowAsAppError :: forall e m a. (Exception e, MonadCatch m) => m a -> m a
rethrowAsAppError action = action `catch` handler
  where
    handler :: e -> m a
    handler = throwM . exceptionToAppError

-- | Catch an @AppError@ thrown in the monad, returning it as a pure value.
tryAppError :: (MonadCatch m) => m a -> m (AppEither a)
tryAppError = try

-- | Catch an @AppError@ thrown in the monad and handle it.
catchAppError :: (MonadCatch m) => m a -> (AppError -> m a) -> m a
catchAppError = catch

-- | Handle an @AppError@ thrown in the monad.
handleAppError :: (MonadCatch m) => (AppError -> m a) -> m a -> m a
handleAppError = handle

-- | Attach an additional layer of error message to the error monad.
checkpointAppError :: (MonadCatch m) => Text -> m a -> m a
checkpointAppError msg action = catchAppError action (throwAppError . addAppErrorContext msg)

-- | Pretty print this error type in multiple lines.
renderAppError :: AppError -> Text
renderAppError (AppError (direct :| indirects)) = firstMsg <> otherMsg
  where
    firstMsg = "error:     " <> direct <> "\n"
    otherMsg = LazyText.toStrict . LazyTextBuilder.toLazyText $ builder
    builder = foldMap (\e -> "caused by: " <> LazyTextBuilder.fromText e <> "\n") indirects

-- | Unwrap @Right@ or error on @Left@.
unwrapRight :: AppEither a -> a
unwrapRight = either (error . Text.unpack . renderAppError) id
