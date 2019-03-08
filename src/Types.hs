module Types
    ( Chrom  (..)
    , Parser (..)
    ) where

import Control.Monad.State  ( StateT (..)   )
import Data.List            ( intercalate   )
import Text.Printf          ( printf        )


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
                   } deriving ( Show )

-- instance Show Chrom where
--     show c = intercalate "\n" xs
--         where trs = "  time range:   %0.1f--%0.1f " ++ tunits c
--               xs  = [ "Chromatogram summary:"
--                     , "  sample id:    " ++ sampid c
--                     , "  method file:  " ++ method c
--                     , "  date & time:  " ++ aqdate c
--                     , "  signal units: " ++ sunits c
--                     , printf trs (0 :: Double) (timeMax c) ]

type Parser a = StateT [String] (Either String) a
