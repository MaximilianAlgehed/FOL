module E where

import GHC.IO.Handle
import System.Process

import FOL
import Backend

eprover :: [Prop] -> Prop -> IO (Handle, Handle, Handle, ProcessHandle) 
eprover = backend "eprover"
