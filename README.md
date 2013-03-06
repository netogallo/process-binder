Process Binder
===

A Cloud Haskell Wrapper for the IO Monad.

## Description

This package is a wrapper for Cloud Haskell with the purpose of easing the interaction with Cloud Haskell in other monads. Cloud Haskell is a distributed computing framework for the Haskell Programming Language which easily lets you distribute applications across multiple computers. Integrating Cloud Haskell existing systems or combining it with other frameworks (like Yesod) can be a difficult task. Process Binder eases this task by providing communication channels with Cloud Haskell and wrapping this functionality in the IO Monad. As a consequence, Cloud Haskell actions can be run inside the IO Monad easily.