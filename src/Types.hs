module Types
    ( Chrom  (..)
    , Parser (..)
    ) where

import Control.Monad.State  ( StateT (..)   )
import Data.List            ( intercalate   )

data Chrom = Chrom { sampid   :: String
                   , method   :: String
                   , aqdate   :: String
                   , srate    :: Double
                   , ntimes   :: Int
                   , tunits   :: String
                   , sunits   :: String
                   , tmult    :: Double
                   , smult    :: Double
                   , signals  :: [Double]
                   } deriving ( Show )

type Parser a = StateT [String] (Either String) a
