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
  runAction
  )where

import Control.Concurrent.STM
import Control.Distributed.Process (Process,ProcessId,liftIO)
import Data.Typeable (Typeable,cast)
import Control.Monad (forever)
import Control.Distributed.Process.Node (LocalNode,forkProcess)

data Holder = forall a. Typeable a => Holder{internalValue :: a}

data QueuedProcess = QueuedProcess{
  queuedAction :: Process Holder,
  resultHolder :: TMVar Holder
  }
  

data ProcessBinder = ProcessBinder{
  binderProcessId :: ProcessId,
  processQueue :: TChan QueuedProcess
  }
                     
                     
castHolder :: Typeable a => Holder -> Maybe a
castHolder (Holder val) = cast val
    
processWrapper :: Typeable a => Process a -> Process Holder
processWrapper proc = proc >>= return.Holder
                     
processRunner :: TChan QueuedProcess -> Process ()
processRunner chan = forever $ do 
  qAction <- liftIO $ atomically $ readTChan chan
  let
    action = queuedAction qAction
    resultVar = resultHolder qAction
  result <- action
  liftIO $ atomically $ putTMVar resultVar result
  
runAction :: Typeable a => ProcessBinder -> Process a -> IO (Maybe a)
runAction binder action = do
  resultVar <- newEmptyTMVarIO
  let
    actionWrapper = QueuedProcess{queuedAction=processWrapper action,
                                  resultHolder=resultVar}
  atomically $ writeTChan (processQueue binder) actionWrapper
  result <- atomically $ takeTMVar (resultHolder actionWrapper)
  return $ castHolder result
                     
newProcessQueue :: LocalNode -> IO ProcessBinder
newProcessQueue localNode = do
  queue <- newTChanIO
  pid <- forkProcess localNode $ processRunner queue
  return $ ProcessBinder{binderProcessId=pid,processQueue=queue}