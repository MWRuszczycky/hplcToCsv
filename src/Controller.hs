module Controller
    ( routeArg
    ) where

import Control.Monad.State  ( evalStateT    )
import System.Directory     ( doesFileExist )
import Types                ( Chrom  (..)
                            , Parser (..)   )
import Paths_hplcToCsv      ( version       )
import Data.Version         ( showVersion   )
import Text.Printf          ( printf        )
import Model                ( changeName
                            , summarize
                            , toCsv
                            , parse         )

---------------------------------------------------------------------
-- Main controller

routeArg :: String -> IO ()
routeArg "--help"    = dispHelp
routeArg "-h"        = dispHelp
routeArg "--version" = dispVersion
routeArg "-v"        = dispVersion
routeArg fp          = do
    exists <- doesFileExist fp
    if exists
       then convert fp
       else dispErr $ "file '" ++ fp ++ "' does not exist"
    putStrLn ""

---------------------------------------------------------------------

convert :: FilePath -> IO ()
convert fn = do
    xs <- lines <$> readFile fn
    putStrLn $ "File: " ++ fn
    case evalStateT parse xs of
         Left err -> dispErr err
         Right c  -> do let csvfn = changeName fn
                        putStrLn $ "file converted to: " ++ csvfn
                        putStrLn . summarize $ c
                        writeFile csvfn . toCsv $ c

dispHelp :: IO ()
dispHelp = do
    dispVersion
    putStrLn "usage:"
    putStrLn "  hplcToCsv filename [filename]...\n"
    putStrLn "options:"
    putStrLn "  -h, --help:    Show this help"
    putStrLn "  -v, --version: Display the version number"

dispVersion :: IO ()
dispVersion = putStrLn $ "hplcToCsv-" ++ showVersion version

dispErr :: String -> IO ()
dispErr err = putStrLn $ "Error: " ++ err
