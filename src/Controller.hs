module Controller
    ( route
    , convert
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

route :: [String] -> IO ()
route ("--help":_)    = putStr helpStr
route ("-h":_)        = putStr helpStr
route ("--version":_) = putStrLn versionStr
route ("-v":_)        = putStrLn versionStr
route xs              = mapM convert xs >>= putStr . intercalate "\n"

-- =============================================================== --
-- Run modes

---------------------------------------------------------------------
-- Conversion of files

convert :: FilePath -> IO String
convert fp = tryReadFile fp >>= either err run
    where run = finish fp . evalStateT parse . lines
          err = pure . formatError fp

finish :: FilePath -> Either String Chrom -> IO String
finish fp (Left  e) = pure . formatError fp $ e
finish fp (Right c) = let csvFp = changeName fp
                      in  do writeFile csvFp . toCsv $ c
                             pure . toInfo fp csvFp  $ c

tryReadFile :: FilePath -> IO (Either String String)
tryReadFile fp = catch ( Right <$> readFile fp ) handler
    where handler :: SomeException -> IO (Either String String)
          handler = pure . Left . show

---------------------------------------------------------------------
-- Display of help and information strings

helpStr :: String
helpStr = unlines hs
    where hs = [ "usage:"
               , "  hplcToCsv filename [filename]..."
               , "options:"
               , "  -h, --help:    Show this help"
               , "  -v, --version: Display the version number"
               ]

versionStr :: String
versionStr = "hplcToCsv-" ++ showVersion version
