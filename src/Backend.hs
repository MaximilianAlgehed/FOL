module Backend where

import GHC.IO.Handle
import System.Process

import FOL

backend :: String -> [Prop] -> Prop -> IO (Handle, Handle, Handle, ProcessHandle) 
backend s ps c = do
  let theory = tptp ps c
  runInteractiveCommand $ "echo " ++ show theory ++ " | " ++ s
