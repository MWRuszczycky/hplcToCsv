module Model
    ( changeName
    , formatError
    , toCsv
    , toInfo
    , parse
    ) where

import Control.Monad.State  ( StateT (..)
                            , get, put, lift )
import Data.List            ( intercalate    )
import Text.Printf          ( printf         )
import Text.Read            ( readMaybe      )
import Types                ( Chrom  (..)
                            , Parser (..)
                            , Parameter
                            , readParam      )

-- =============================================================== --
-- Output formatting

-- Exported

toInfo :: FilePath -> FilePath -> Chrom -> String
toInfo fpOld fpNew c = unlines hs
    where hs = [ "File input: " ++ fpOld
               , "File converted to: " ++ fpNew
               , summarize c
               ]

toCsv :: Chrom -> String
toCsv c = unlines hdr
    where hdr = [ "# Sample ID:    " ++ sampleID c
                , "# Method file:  " ++ method   c
                , "# Date & time:  " ++ date     c
                , "# Time units:   " ++ xUnits   c
                , "# Signal units: " ++ yUnits   c
                , plotChrom c ]

formatError :: FilePath -> String -> String
-- ^Format an error string.
formatError fp err = unlines hs
    where hs = [ "File input: " ++ fp
               , "  Error: "    ++ err
               ]

-- Unexported

summarize :: Chrom -> String
-- ^Summarize a chromatogram for display
summarize c= unlines xs
    where trs = "  time range:   %0.1f--%0.1f " ++ xUnits c
          xs  = [ "Chromatogram summary:"
                , "  sample id:    " ++ sampleID c
                , "  method file:  " ++ method   c
                , "  date & time:  " ++ date     c
                , "  signal units: " ++ yUnits   c
                , printf trs (0 :: Double) (timeMax c) ]

-- =============================================================== --
-- Accessors and formatters

-- Exported

changeName :: FilePath -> FilePath
-- ^Replace the file extension with '.csv' ensuring that the new name
-- does not conflict with the old name.
changeName f
    | null name    = f ++ ".csv"
    | ext == "csv" = f ++ ".csv"
    | otherwise    = name ++ "csv"
    where (rExt, rName) = break (== '.') . reverse $ f
          (name, ext)   = ( reverse rName, reverse rExt )

-- Unexported

timeMax :: Chrom -> Double
timeMax c = (fromIntegral . nTimes $ c) * xMult c / rate c

getTimes :: Chrom -> [Double]
getTimes c = take (nTimes c) . iterate ( + step ) $ 0
    where step = xMult c / rate c

plotChrom :: Chrom -> String
plotChrom c = intercalate "\n" . zipWith go (getTimes c) $ (signals c)
    where go x y = show x ++ "," ++ ( show . (* yMult c) $ y )

-- =============================================================== --
-- Parser

-- Exported

parse :: Parser Chrom
parse = readChrom >>= checkData
    where readChrom = Chrom <$> parameter "Sample ID"
                            <*> parameter "Method"
                            <*> parameter "Acquisition Date and Time"
                            <*> parameter "Sampling Rate"
                            <*> parameter "Total Data Points"
                            <*> parameter "X Axis Title"
                            <*> parameter "Y Axis Title"
                            <*> parameter "X Axis Multiplier"
                            <*> parameter "Y Axis Multiplier"
                            <*> absorbances

-- Unexported

parameter :: Parameter a => String -> Parser a
parameter s = do
    (p,v) <- breakNext s ':'
    if p == s
       then maybe (parseFail s) pure . readParam $ v
       else parameter s

breakNext :: String -> Char -> Parser (String, String)
breakNext s c = get >>= go
    where go []     = parseFail s
          go (t:ts) = do put ts
                         case break (== c) t of
                              (x,[]) -> pure (x, []     )
                              (x,xs) -> pure (x, tail xs)

absorbances :: Parser [Double]
absorbances = StateT $ go . mapM readMaybe
    where go Nothing   = Left "Cannot read the absorbances"
          go (Just xs) = Right (xs, [])

---------------------------------------------------------------------
-- Parse checkers and error handling

checkData :: Chrom -> Parser Chrom
checkData c
    | timePointsMatch = pure c
    | otherwise       = lift . Left $ err
    where timePointsMatch = nTimes c == ( length . signals ) c
          err = "Total data points does not equal absorbance count."

parseFail :: String -> Parser a
parseFail s = lift . Left $ "Cannot parse parameter: " ++ s
