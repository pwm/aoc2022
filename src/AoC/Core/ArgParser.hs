module AoC.Core.ArgParser
  ( Command (..),
    execParser,
    opts,
  )
where

import AoC.Core.Date
import Control.Monad ((<=<))
import Data.Either.Combinators
import Options.Applicative hiding (execParser)
import Options.Applicative qualified as OptParse
import Text.Read
import Prelude

execParser :: ParserInfo a -> IO a
execParser = OptParse.execParser

data Command
  = Fetch Date
  | Solve Date
  deriving stock (Show)

opts :: ParserInfo Command
opts = info (helper <*> hsubparser (mconcat cmds)) (progDesc "Fetch and solve AoC puzzles")
  where
    cmds :: [Mod CommandFields Command]
    cmds =
      [ command "fetch" (info (Fetch <$> dateP) (progDesc "Fetch puzzle input")),
        command "solve" (info (Solve <$> dateP) (progDesc "Solve puzzle"))
      ]

dateP :: Parser Date
dateP = liftA2 MkDate yearP dayP

yearP :: Parser Year
yearP = option (eitherReader (mkYear <=< yearIntP)) mods
  where
    yearIntP :: String -> Either String Int
    yearIntP = mapLeft (const yearError) . readEither
    mods :: Mod OptionFields Year
    mods = long "year" <> short 'y' <> metavar "YEAR" <> help "Which year"

dayP :: Parser Day
dayP = option (eitherReader (mkDay <=< dayIntP)) mods
  where
    dayIntP :: String -> Either String Int
    dayIntP = mapLeft (const dayError) . readEither
    mods :: Mod OptionFields Day
    mods = long "day" <> short 'd' <> metavar "DAY" <> help "Which day"
