module Main where

import System.Environment   ( getArgs )
import Controller           ( route   )

main :: IO ()
main = do getArgs >>= route
