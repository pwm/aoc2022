module AoC.Core.Date
  ( Date (..),
    mkDate,
    displayDate,
    Year,
    mkYear,
    getYear,
    yearError,
    Day,
    mkDay,
    getDay,
    dayError,
  )
where

import Text.Printf (printf)
import Prelude

-- Date

data Date = MkDate
  { year :: Year,
    day :: Day
  }
  deriving stock (Show, Eq, Ord)

mkDate :: Int -> Int -> Either String Date
mkDate yearInt dayInt = MkDate <$> mkYear yearInt <*> mkDay dayInt

displayDate :: Date -> String
displayDate date = "Y" <> show (getYear date.year) <> "D" <> printf "%02d" (getDay date.day)

-- Year

newtype Year = UnsafeMkYear Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum)

instance Bounded Year where
  minBound, maxBound :: Year
  minBound = UnsafeMkYear 2015
  maxBound = UnsafeMkYear 2099

mkYear :: Int -> Either String Year
mkYear i
  | i `elem` [getYear minBound .. getYear maxBound] = Right $ UnsafeMkYear i
  | otherwise = Left yearError

getYear :: Year -> Int
getYear (UnsafeMkYear i) = i

yearError :: String
yearError =
  "Valid years are integers in the range ["
    <> show (getYear minBound)
    <> ".."
    <> show (getYear maxBound)
    <> "]"

-- Day

newtype Day = UnsafeMkDay Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum)

instance Bounded Day where
  minBound, maxBound :: Day
  minBound = UnsafeMkDay 1
  maxBound = UnsafeMkDay 25

mkDay :: Int -> Either String Day
mkDay i
  | i `elem` [getDay minBound .. getDay maxBound] = Right $ UnsafeMkDay i
  | otherwise = Left dayError

getDay :: Day -> Int
getDay (UnsafeMkDay i) = i

dayError :: String
dayError =
  "Valid days are integers in the range ["
    <> show (getDay minBound)
    <> ".."
    <> show (getDay maxBound)
    <> "]"
