module Types
    ( Chrom  (..)
    , Parser (..)
    ) where

import Control.Monad.State  ( StateT (..)   )
import Data.List            ( intercalate   )

data Chrom = Chrom { sampleID :: String
                   , method   :: String
                   , date     :: String
                   , rate     :: Double
                   , nTimes   :: Int
                   , xUnits   :: String
                   , yUnits   :: String
                   , xMult    :: Double
                   , yMult    :: Double
                   , signals  :: [Double]
                   } deriving ( Show )

type ErrString = String
type Parser a  = StateT [String] (Either ErrString) a
