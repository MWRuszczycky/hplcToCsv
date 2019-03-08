module Controller
    ( routeArg
    ) where

import Control.Monad.State  ( evalStateT        )
import System.Directory     ( doesFileExist     )
import Types                ( Chrom  (..)
                            , Parser (..)       )
import Paths_hplcToCsv      ( version           )
import Data.Version         ( showVersion       )
import Text.Printf          ( printf            )
import Model                ( changeName
                            , formatError
                            , missingFileError
                            , conversionInfo
                            , toCsv
                            , parse             )

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
       then convert fp >>= putStrLn
       else putStrLn . missingFileError $ fp

---------------------------------------------------------------------

convert :: FilePath -> IO String
convert fn = do
    xs <- lines <$> readFile fn
    putStrLn $ "File: " ++ fn
    case evalStateT parse xs of
         Left err -> pure . formatError $ err
         Right c  -> do let csvfn = changeName fn
                        writeFile csvfn . toCsv $ c
                        pure $ conversionInfo csvfn c

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
