module Controller
    ( routeArg
    ) where

import Control.Monad.State  ( evalStateT            )
import System.Directory     ( doesFileExist         )
import Control.Exception    ( SomeException, catch  )
import Data.List            ( intercalate           )
import Types                ( Chrom  (..)
                            , Parser (..)           )
import Paths_hplcToCsv      ( version               )
import Data.Version         ( showVersion           )
import Text.Printf          ( printf                )
import Model                ( changeName
                            , formatError
                            , toCsv
                            , toInfo
                            , parse                 )

-- =============================================================== --
-- Main controller

routeArg :: String -> IO String
routeArg "--help"    = pure helpStr
routeArg "-h"        = pure helpStr
routeArg "--version" = pure versionStr
routeArg "-v"        = pure versionStr
routeArg fp          = do
    exists <- doesFileExist fp
    if exists
       then convert fp
       else pure . formatError fp $ "File does not exist"

-- =============================================================== --
-- Run modes

---------------------------------------------------------------------
-- Conversion of files

convert :: FilePath -> IO String
convert fn = do
    xs <- lines <$> readFile fn
    case evalStateT parse xs of
         Left err -> pure . formatError fn $ err
         Right c  -> do let csvfn = changeName fn
                        writeFile csvfn . toCsv $ c
                        pure $ toInfo fn csvfn c

tryReadFile :: FilePath -> IO (Either String String)
tryReadFile fp = catch ( Right <$> readFile fp ) handler
    where handler :: SomeException -> IO (Either String String)
          handler = pure . Left . show

---------------------------------------------------------------------
-- Display of help and information strings

helpStr :: String
helpStr = intercalate "\n" hs
    where hs = [ "usage:"
               , "  hplcToCsv filename [filename]..."
               , "options:"
               , "  -h, --help:    Show this help"
               , "  -v, --version: Display the version number"
               ]

versionStr :: String
versionStr = "hplcToCsv-" ++ showVersion version
