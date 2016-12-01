{-# LANGUAGE
  DataKinds,
  FlexibleContexts,
  NoMonomorphismRestriction,
  TypeOperators,
  ScopedTypeVariables
 #-}

module Book where

import Prelude hiding ((^), (<*>), (+))
import Language.LLC
import Language.GV

serverEq =
  llam $ \c ->
    recv c          `bind` (llp $ \x c ->
    recv c          `bind` (llp $ \y c ->
    send (constant (==) $$$ x $$$ y) c))

clientEq =
   llam $ \c ->
    send (constant 42) c `bind` (llam $ \c ->
    send (constant 53) c `bind` (llam $ \c ->
    recv c    `bind` (llp $ \r c ->
    wait c    `bind` (llz $
    ret r))))

example =
    defnGV $ fork serverEq `bind` clientEq