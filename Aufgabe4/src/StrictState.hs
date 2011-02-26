-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Strict
-- Copyright   :  (c) Andy Gill 2001,
--           (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Strict state monads.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
--
-- See below for examples.

-----------------------------------------------------------------------------

-- shortend version to make this compile with jhc.

module StrictState (
    State(..)
  ) where

-- ---------------------------------------------------------------------------
-- | A parameterizable state monad where /s/ is the type of the state
-- to carry and /a/ is the type of the /return value/.

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> case runState m s of
                                 (a, s') -> runState (k a) s'
