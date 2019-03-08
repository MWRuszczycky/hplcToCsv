module Main where

import Data.List            ( intercalate )
import System.Environment   ( getArgs     )
import Controller           ( routeArg    )

main :: IO ()
main = do results <- getArgs >>= mapM routeArg
          putStrLn . intercalate "\n\n" $ results
