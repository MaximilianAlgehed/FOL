module Vampire where

import GHC.IO.Handle
import System.Process

import FOL
import Backend

vampire :: [Prop] -> Prop -> IO (Handle, Handle, Handle, ProcessHandle) 
vampire = backend "vampire" 
