{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
-- | [Process Binder]
-- 
-- This is a frontend to easily use Cloud Haskell inside other monads.
-- If you have a complex system and want to make it distributed withouth
-- having to deal with monad transformers or complex pipeing to connect
-- Cloud Haskell and the rest of the application, this package can 
-- easliy abstract away the hassle by handling the communication
-- with Cloud Haskell inside the IO Monad.
module Control.Distributed.Process.Binder.Types ( 
  ProcessBinder,
  newProcessQueue,
  runAction,
  newChan
  )where

import Control.Concurrent.STM
import qualified Control.Distributed.Process as P
import Data.Binary (Binary)
import Data.Typeable (Typeable,cast)
import Control.Monad (forever)
import Control.Distributed.Process.Node (LocalNode,forkProcess)

data Holder = forall a. Typeable a => Holder{internalValue :: a}

data ProcessRunOption = RunNonBlocking | RunBlocking

data QueuedProcess = BlockingProcess QueuedProcessBody | NonBlockingProcess QueuedProcessBody

data QueuedProcessBody = QueuedProcessBody{
  queuedAction :: P.Process Holder,
  resultHolder :: TMVar Holder
  }
  

data ProcessBinder = ProcessBinder{
  binderProcessId :: P.ProcessId,
  processQueue :: TChan QueuedProcess
}                     
                     

class ProcessRunner runner where
  queueProcess :: runner -> a -> IO ()
  readResult :: runner -> IO a
                     
castHolder :: Typeable a => Holder -> Maybe a
castHolder (Holder val) = cast val
    
newChan :: (Typeable a, Binary a) => ProcessBinder -> IO (P.SendPort a, P.ReceivePort a)
newChan binder = newChan' Nothing
  where
    newChan' Nothing = runBlockingAction binder P.newChan >>= newChan'
    newChan' (Just chan) = return chan

processWrapper :: Typeable a => P.Process a -> P.Process Holder
processWrapper proc = proc >>= return.Holder
                           
processRunner :: TChan QueuedProcess -> P.Process ()
processRunner chan = forever $ do 
  qAction <- P.liftIO $ atomically $ readTChan chan
  case qAction of
    BlockingProcess qAction' -> runBlockingProcess qAction'
    NonBlockingProcess qAction' -> runNonBlockingProcess qAction'
    
  where
    runNonBlockingProcess qAction = do
      P.spawnLocal $ runBlockingProcess qAction
      return ()
    runBlockingProcess qAction = do      
      let
        action = queuedAction qAction
        resultVar = resultHolder qAction  
      result <- action
      P.liftIO $ atomically $ putTMVar resultVar result
  
  
runAction :: Typeable a => ProcessBinder -> P.Process a -> IO (Maybe a)
runAction = runAction' RunNonBlocking

runBlockingAction :: Typeable a => ProcessBinder -> P.Process a -> IO (Maybe a)
runBlockingAction = runAction' RunBlocking

runAction' :: Typeable a => ProcessRunOption -> ProcessBinder -> P.Process a -> IO (Maybe a)
runAction' runOption binder action = do
  resultVar <- newEmptyTMVarIO
  let
    actionWrapper = QueuedProcessBody{queuedAction=processWrapper action,
                                      resultHolder=resultVar}
    actionRunner = case runOption of
      RunNonBlocking -> NonBlockingProcess actionWrapper
      RunBlocking -> BlockingProcess actionWrapper
  atomically $ writeTChan (processQueue binder) actionRunner
  result <- atomically $ takeTMVar (resultHolder actionWrapper)
  return $ castHolder result
                     
newProcessQueue :: LocalNode -> IO ProcessBinder
newProcessQueue localNode = do
  queue <- newTChanIO
  blockingChan <- newTChanIO
  pid <- forkProcess localNode $ processRunner queue
  return $ ProcessBinder{binderProcessId=pid,processQueue=queue}