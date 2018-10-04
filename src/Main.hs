module Main where

import System.Environment   ( getArgs       )
import System.Directory     ( doesFileExist )
import Data.List            ( intercalate   )
import Text.Read            ( readMaybe     )
import Text.Printf          ( printf        )
import Paths_hplcToCsv      ( version       )
import Data.Version         ( showVersion   )
import Control.Monad.State  ( StateT
                            , StateT (..)
                            , evalStateT    )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Types

data Chrom = Chrom { sampid   :: String
                   , method   :: String
                   , aqdate   :: String
                   , tunits   :: String
                   , sunits   :: String
                   , srate    :: Double
                   , ntimes   :: Int
                   , tmult    :: Double
                   , smult    :: Double
                   , signals  :: [Double]
                   }

instance Show Chrom where
    show c = intercalate "\n" xs
        where trs = "  time range:   %0.1f--%0.1f " ++ tunits c
              xs  = [ "Chromatogram summary:"
                    , "  sample id:    " ++ sampid c
                    , "  method file:  " ++ method c
                    , "  date & time:  " ++ aqdate c
                    , "  signal units: " ++ sunits c
                    , printf trs (0 :: Double) (timeMax c) ]

type Parser a = StateT [String] (Either String) a

---------------------------------------------------------------------
---------------------------------------------------------------------
-- IO

main :: IO ()
main = getArgs >>= mapM_ handleArg

handleArg :: String -> IO ()
handleArg "--help"    = dispHelp
handleArg "-h"        = dispHelp
handleArg "--version" = dispVersion
handleArg "-v"        = dispVersion
handleArg fp          = do
    exists <- doesFileExist fp
    if exists
       then convert fp
       else dispErr $ "file '" ++ fp ++ "' does not exist"
    putStrLn ""

convert :: FilePath -> IO ()
convert fn = do
    xs <- lines <$> readFile fn
    putStrLn $ "File: " ++ fn
    case evalStateT parse xs of
         Left err -> dispErr err
         Right c  -> do let csvfn = changeName fn
                        putStrLn $ "file converted to: " ++ csvfn
                        print c
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

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Pure

---------------------------------------------------------------------
-- Helper functions

changeName :: FilePath -> FilePath
changeName f = takeWhile (/= '.') f ++ ".csv"

timeMax :: Chrom -> Double
timeMax c = (fromIntegral . ntimes $ c) * tmult c / srate c

cleanString :: String -> String
cleanString = unwords . words

---------------------------------------------------------------------
-- CSV Conversion

toCsv :: Chrom -> String
toCsv c = intercalate "\n" hdr
    where hdr = [ "# Sample ID:    " ++ sampid c
                , "# Method file:  " ++ method c
                , "# Date & time:  " ++ aqdate c
                , "# Time units:   " ++ tunits c
                , "# Signal units: " ++ sunits c
                , plotChrom c ]

getTimes :: Chrom -> [Double]
getTimes c = take (ntimes c) . iterate ( + tstp ) $ 0
    where tstp = tmult c / srate c

plotChrom :: Chrom -> String
plotChrom c = intercalate "\n" . zipWith go (getTimes c) $ (signals c)
    where go t s = show t ++ "," ++ ( show . (* smult c) $ s )

---------------------------------------------------------------------
-- Parser

parse :: Parser Chrom
parse = do
    sid <- readParStr "Sample ID"
    mt  <- readParStr "Method"
    dt  <- readParStr "Acquisition Date and Time"
    rt  <- readParVal "Sampling Rate"
    np  <- readParVal "Total Data Points"
    xt  <- readParStr "X Axis Title"
    yt  <- readParStr "Y Axis Title"
    xm  <- readParVal "X Axis Multiplier"
    ym  <- readParVal "Y Axis Multiplier"
    as  <- readAbsorbances
    return Chrom { sampid  = sid
                 , method  = mt
                 , aqdate  = dt
                 , tunits  = xt
                 , sunits  = yt
                 , srate   = rt
                 , ntimes  = np
                 , tmult   = xm
                 , smult   = ym
                 , signals = as }

readParStr :: String -> Parser String
readParStr s = StateT go
    where s'         = s ++ ":"
          go []      = Left $ "Cannot parse out: " ++ s
          go (x:xs') | u == s'   = Right (cleanString v, xs')
                     | otherwise = go xs'
                     where (u,v) = splitAt (length s') x

readParVal :: Read a => String -> Parser a
readParVal s = StateT go
    where s'         = s ++ ":"
          go []      = Left $ "Cannot parse out: " ++ s
          go (x:xs') | u == s'   = f . readMaybe . head . words $ v
                     | otherwise = go xs'
                     where (u,v)      = splitAt (length s') x
                           f Nothing  = Left $ "Cannot read value for: " ++ s
                           f (Just w) = Right (w, xs')

readAbsorbances :: Parser [Double]
readAbsorbances = StateT $ go . mapM readMaybe
    where go Nothing   = Left "Cannot read the absorbances"
          go (Just xs) = Right (xs, [])
