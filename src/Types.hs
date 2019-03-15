{-# LANGUAGE FlexibleInstances #-}

module Types
    ( Chrom     (..)
    , Parser    (..)
    , ErrString (..)
    , Parameter
    , readParam
    ) where

import Control.Monad.State  ( StateT (..)   )
import Text.Read            ( readMaybe     )
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

class Parameter a where
    readParam :: String -> Maybe a

instance Parameter [Char] where
    readParam = Just . unwords . words

instance Parameter Int where
    readParam = formatNumPar

instance Parameter Double where
    readParam = formatNumPar

formatNumPar :: Read a => String -> Maybe a
formatNumPar s = case words s of
                      []  -> Nothing
                      x:_ -> readMaybe x

type ErrString = String
type Parser a  = StateT [String] (Either ErrString) a
