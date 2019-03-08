module Main where

import System.Environment   ( getArgs   )
import Controller           ( routeArg  )

main :: IO ()
main = getArgs >>= mapM_ routeArg
