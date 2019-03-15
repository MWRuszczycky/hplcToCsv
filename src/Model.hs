module Model
    ( changeName
    , toCsv
    , parse
    , conversionInfo
    , formatError
    ) where

import Control.Monad.State  ( StateT (..)
                            , lift          )
import Data.List            ( intercalate   )
import Text.Printf          ( printf        )
import Text.Read            ( readMaybe     )
import Types                ( Chrom  (..)
                            , Parser (..)   )

---------------------------------------------------------------------
-- Helper functions

changeName :: FilePath -> FilePath
-- ^Replace the file extension with '.csv' ensuring that the new name
-- does not conflict with the old name.
changeName f
    | null name    = f ++ ".csv"
    | ext == "csv" = f ++ ".csv"
    | otherwise    = name ++ "csv"
    where (rExt, rName) = break (== '.') . reverse $ f
          (name, ext)   = ( reverse rName, reverse rExt )

timeMax :: Chrom -> Double
timeMax c = (fromIntegral . nTimes $ c) * xMult c / rate c

cleanString :: String -> String
cleanString = unwords . words

---------------------------------------------------------------------
-- Output formatting

summarize :: Chrom -> String
-- ^Summarize a chromatogram for display
summarize c= intercalate "\n" xs
    where trs = "  time range:   %0.1f--%0.1f " ++ xUnits c
          xs  = [ "Chromatogram summary:"
                , "  sample id:    " ++ sampleID c
                , "  method file:  " ++ method   c
                , "  date & time:  " ++ date     c
                , "  signal units: " ++ yUnits   c
                , printf trs (0 :: Double) (timeMax c) ]

conversionInfo :: FilePath -> FilePath -> Chrom -> String
conversionInfo fpOld fpNew c = intercalate "\n" hs
    where hs = [ "File input: " ++ fpOld
               , "File converted to: " ++ fpNew
               , summarize c
               ]

formatError :: FilePath -> String -> String
-- ^Format an error string.
formatError fp err = intercalate "\n" hs
    where hs = [ "File input: " ++ fp
               , "  Error: "    ++ err
               ]

---------------------------------------------------------------------
-- CSV Conversion

toCsv :: Chrom -> String
toCsv c = intercalate "\n" hdr ++ "\n"
    where hdr = [ "# Sample ID:    " ++ sampleID c
                , "# Method file:  " ++ method   c
                , "# Date & time:  " ++ date     c
                , "# Time units:   " ++ xUnits   c
                , "# Signal units: " ++ yUnits   c
                , plotChrom c ]

getTimes :: Chrom -> [Double]
getTimes c = take (nTimes c) . iterate ( + step ) $ 0
    where step = xMult c / rate c

plotChrom :: Chrom -> String
plotChrom c = intercalate "\n" . zipWith go (getTimes c) $ (signals c)
    where go x y = show x ++ "," ++ ( show . (* yMult c) $ y )

---------------------------------------------------------------------
-- Parser

parse :: Parser Chrom
parse = readChrom >>= checkData
    where readChrom = Chrom <$> readParStr "Sample ID"
                            <*> readParStr "Method"
                            <*> readParStr "Acquisition Date and Time"
                            <*> readParVal "Sampling Rate"
                            <*> readParVal "Total Data Points"
                            <*> readParStr "X Axis Title"
                            <*> readParStr "Y Axis Title"
                            <*> readParVal "X Axis Multiplier"
                            <*> readParVal "Y Axis Multiplier"
                            <*> readAbsorbances

checkData :: Chrom -> Parser Chrom
checkData c
    | nt == ns  = pure c
    | otherwise = lift . Left $ err
    where nt  = nTimes c
          ns  = length . signals $ c
          err = "Total data points does not equal absorbance count."

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
