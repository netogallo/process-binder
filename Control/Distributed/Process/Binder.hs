-- | [Process Binder]
-- 
-- This is a frontend to easily use Cloud Haskell inside other monads.
-- If you have a complex system and want to make it distributed withouth
-- having to deal with monad transformers or complex pipeing to connect
-- Cloud Haskell and the rest of the application, this package can 
-- easliy abstract away the hassle by handling the communication
-- with Cloud Haskell inside the IO Monad.

module Control.Distributed.Process.Binder where

import Control.Distributed.Process.Binder.Types
