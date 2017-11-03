module Backend where

import System.Process

import FOL

backend :: String -> [Prop] -> Prop -> IO ()
backend s ps c = do
  let theory = tptp ps c
  callCommand $ "echo " ++ show theory ++ " | " ++ s
