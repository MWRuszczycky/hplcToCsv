module Model
    ( changeName
    , toCsv
    , parse
    , conversionInfo
    , formatError
    , missingFileError
    ) where

import Control.Monad.State  ( StateT (..)   )
import Data.List            ( intercalate   )
import Text.Printf          ( printf        )
import Text.Read            ( readMaybe     )
import Types                ( Chrom  (..)
                            , Parser (..)   )

---------------------------------------------------------------------
-- Helper functions

changeName :: FilePath -> FilePath
changeName f = takeWhile (/= '.') f ++ ".csv"

timeMax :: Chrom -> Double
timeMax c = (fromIntegral . ntimes $ c) * tmult c / srate c

cleanString :: String -> String
cleanString = unwords . words

---------------------------------------------------------------------
-- Output formatting

summarize :: Chrom -> String
-- ^Summarize a chromatogram for display
summarize c= intercalate "\n" xs
    where trs = "  time range:   %0.1f--%0.1f " ++ tunits c
          xs  = [ "Chromatogram summary:"
                , "  sample id:    " ++ sampid c
                , "  method file:  " ++ method c
                , "  date & time:  " ++ aqdate c
                , "  signal units: " ++ sunits c
                , printf trs (0 :: Double) (timeMax c) ]

conversionInfo :: FilePath -> Chrom -> String
conversionInfo fp c = intercalate "\n" hs
    where hs = [ "File converted to: " ++ fp
               , summarize c
               ]

formatError :: String -> String
-- ^Format an error string.
formatError = (++) "Error: "

missingFileError :: String -> String
missingFileError fp =formatError $ "file '" ++ fp ++ "' does not exist"

---------------------------------------------------------------------
-- CSV Conversion

toCsv :: Chrom -> String
toCsv c = intercalate "\n" hdr ++ "\n"
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
