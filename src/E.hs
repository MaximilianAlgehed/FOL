module E where

import FOL
import Backend

eprover :: [Prop] -> Prop -> IO ()
eprover = backend "eprover"
