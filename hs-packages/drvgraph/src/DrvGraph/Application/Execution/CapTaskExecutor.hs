module DrvGraph.Application.Execution.CapTaskExecutor
    ( withTaskExecutorImpl
    ) where

import Control.Exception.Safe (MonadCatch, MonadMask, SomeException, bracket, throw, tryAny)
import UnliftIO (MonadIO, MonadUnliftIO)
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent qualified as Concurrent
import UnliftIO.STM (TQueue, TVar)
import UnliftIO.STM qualified as Stm

withTaskExecutorImpl :: (MonadMask m, MonadUnliftIO m) => ((m a -> m (), m (Maybe a)) -> m r) -> m r
withTaskExecutorImpl action = do
    taskTx <- Stm.atomically Stm.newTQueue
    resultRx <- Stm.atomically Stm.newTQueue
    unfinishedTasks <- Stm.atomically $ Stm.newTVar 0

    num <- Concurrent.getNumCapabilities
    bracket
        (traverse Async.async . replicate num $ taskWorker taskTx resultRx unfinishedTasks)
        (traverse Async.cancel)
        (const $ action (submit taskTx unfinishedTasks, await resultRx unfinishedTasks))
  where
    submit taskTx unfinishedTasks task = do
        Stm.atomically $ do
            Stm.writeTQueue taskTx task
            Stm.modifyTVar' unfinishedTasks (+ 1)

    await resultRx unfinishedTasks = do
        res <- Stm.atomically $ do
            hasUnfinished <- (> 0) <$> Stm.readTVar unfinishedTasks
            if hasUnfinished
                then Just <$> Stm.readTQueue resultRx
                else Stm.tryReadTQueue resultRx

        case res of
            Nothing -> pure Nothing
            Just (Left ex) -> throw ex
            Just (Right val) -> pure $ Just val

taskWorker
    :: (MonadCatch m, MonadIO m)
    => TQueue (m a) -> TQueue (Either SomeException a) -> TVar Int -> m ()
taskWorker taskRx resultTx unfinishedTasks = loop
  where
    loop = do
        front <- Stm.atomically $ Stm.readTQueue taskRx
        res <- tryAny front
        Stm.atomically $ do
            Stm.writeTQueue resultTx res
            Stm.modifyTVar' unfinishedTasks (\x -> x - 1)
        loop
