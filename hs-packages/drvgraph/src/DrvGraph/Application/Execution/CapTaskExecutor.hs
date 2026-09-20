module DrvGraph.Application.Execution.CapTaskExecutor
    ( withTaskExecutorImpl
    ) where

import Control.Concurrent.Async.Warden (Warden)
import Control.Concurrent.Async.Warden qualified as Warden
import Control.Exception.Safe (MonadCatch, SomeException, throw, tryAny)
import Control.Monad (replicateM_)
import Control.Monad.Reader (MonadReader, asks)
import Optics ((^.))
import UnliftIO (MonadIO, MonadUnliftIO (withRunInIO))
import UnliftIO.STM (TQueue, TVar)
import UnliftIO.STM qualified as Stm

import DrvGraph.Application.Execution.Environment (AppEnvironment)

withTaskExecutorImpl
    :: (MonadCatch m, MonadReader AppEnvironment m, MonadUnliftIO m)
    => ((m a -> m (), m (Maybe a)) -> m r) -> m r
withTaskExecutorImpl action = do
    taskTx <- Stm.atomically Stm.newTQueue
    resultRx <- Stm.atomically Stm.newTQueue
    unfinishedTasks <- Stm.atomically $ Stm.newTVar 0

    num <- asks (^. #numMaxJobs)
    withWarden $ \warden -> do
        replicateM_ num $ spawn_ warden $ taskWorker taskTx resultRx unfinishedTasks
        action (submit taskTx unfinishedTasks, await resultRx unfinishedTasks)
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

withWarden :: (MonadUnliftIO m) => (Warden -> m a) -> m a
withWarden action = withRunInIO $ \runInIO -> do
    Warden.withWarden $ runInIO . action

spawn_ :: (MonadUnliftIO m) => Warden -> m () -> m ()
spawn_ warden action = withRunInIO $ \runInIO -> do
    Warden.spawn_ warden $ runInIO action
