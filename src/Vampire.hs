module Vampire where

import FOL
import Backend

vampire :: [Prop] -> Prop -> IO ()
vampire = backend "vampire" 
