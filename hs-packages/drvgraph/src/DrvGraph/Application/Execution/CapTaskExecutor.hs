module DrvGraph.Application.Execution.CapTaskExecutor
    ( withTaskExecutorImpl
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Sequence (ViewL (EmptyL, (:<)), (|>))
import Data.Sequence qualified as Seq
import UnliftIO.IORef qualified as IORef

withTaskExecutorImpl :: (MonadIO m) => ((m a -> m (), m (Maybe a)) -> m r) -> m r
withTaskExecutorImpl action = do
    queue <- IORef.newIORef Seq.empty
    action (submit queue, await queue)
  where
    submit queue task = do
        IORef.atomicModifyIORef' queue (\q -> (q |> task, ()))
    await queue = do
        q <- IORef.readIORef queue
        case Seq.viewl q of
            EmptyL -> pure Nothing
            task :< newQueue -> do
                IORef.atomicModifyIORef' queue (const (newQueue, ()))
                Just <$> task
