module Main where

import System.Environment   ( getArgs       )
import System.Directory     ( doesFileExist )
import Data.List            ( intercalate   )
import Text.Read            ( readMaybe     )
import Text.Printf          ( printf        )
import Paths_hplcToCsv      ( version       )
import Data.Version         ( showVersion   )

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

type ParseState = ([String], Chrom)
type Parser     = Either String ParseState
type Setter a   = a -> Chrom -> Chrom

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
    xs <- readFile fn
    putStrLn $ "File: " ++ fn
    case parse xs of
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
-- Helper functions and setters for the Chrom type

changeName :: FilePath -> FilePath
changeName f = takeWhile (/= '.') f ++ ".csv"

timeMax :: Chrom -> Double
timeMax c = (fromIntegral . ntimes $ c) * tmult c / srate c

setSampId, setMethod, setAqDate, setTunits, setSunits :: Setter String
setSampId s c = c { sampid = s }
setMethod s c = c { method = s }
setAqDate s c = c { aqdate = s }
setTunits s c = c { tunits = s }
setSunits s c = c { sunits = s }

setSRate, setTMult, setSMult :: Setter Double
setSRate  x c = c { srate  = x }
setTMult  x c = c { tmult  = x }
setSMult  x c = c { smult  = x }

setNTimes :: Setter Int
setNTimes x c = c { ntimes = x }

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

parse :: String -> Either String Chrom
parse x =
    initialize x
    >>= readParStr "Sample ID" setSampId
    >>= readParStr "Method" setMethod
    >>= readParStr "Acquisition Date and Time" setAqDate
    >>= readParVal "Sampling Rate" setSRate
    >>= readParVal "Total Data Points" setNTimes
    >>= readParStr "X Axis Title" setTunits
    >>= readParStr "Y Axis Title" setSunits
    >>= readParVal "X Axis Multiplier" setTMult
    >>= readParVal "Y Axis Multiplier" setSMult
    >>= readAbsorbances
    >>= return . snd

initialize :: String -> Parser
initialize x = Right (lines x, c0)
    where c0 = Chrom { sampid  = []
                     , method  = []
                     , aqdate  = []
                     , tunits  = []
                     , sunits  = []
                     , srate   = 0
                     , ntimes  = 0
                     , tmult   = 0
                     , smult   = 0
                     , signals = [] }

cleanString :: String -> String
cleanString = unwords . words

readParStr :: String -> Setter String -> ParseState -> Parser
readParStr p0 _ ([], _)   = Left $ "Cannot parse out: " ++ p0
readParStr p0 f (x:xs, c)
    | u == p1   = Right ( xs, f (cleanString v) c )
    | otherwise = readParStr p0 f (xs, c)
    where p1    = p0 ++ ":"
          (u,v) = splitAt (length p1) x

readParVal :: Read a => String -> Setter a -> ParseState -> Parser
readParVal p0 _ ([], _)   = Left $ "Cannot parse out: " ++ p0
readParVal p0 f (x:xs, c)
    | u == p1   = result
    | otherwise = readParVal p0 f (xs, c)
    where p1     = p0 ++ ":"
          (u, v) = splitAt (length p1) x
          result = case readMaybe . head . words $ v of
                        Nothing -> Left $ "Cannot read value for: " ++ p0
                        Just n  -> Right ( xs, f n c )

readAbsorbances :: ParseState -> Parser
readAbsorbances (xs, c) = case mapM readMaybe xs of
                               Nothing -> Left "Cannot read the absorbances"
                               Just as -> Right ([], c { signals = as } )
