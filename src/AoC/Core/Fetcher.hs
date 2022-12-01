module AoC.Core.Fetcher
  ( fetchPuzzle,
  )
where

import AoC.Core.Date
import AoC.Core.File
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Req as Req
import Network.HTTP.Types.Status
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.Exit (exitFailure, exitSuccess)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Prelude

fetchPuzzle :: Date -> IO ()
fetchPuzzle date = do
  fp <- inputPath date
  alreadyFetched <- doesFileExist fp
  when alreadyFetched $
    print ("Input file " <> inputName date <> " is already present in " <> fp) >> exitFailure
  inputFile <- fetchRemoteFile date `catch` (errorWithoutStackTrace . handleException)
  writeFile fp (T.unpack inputFile)
  print ("Successfully fetched input file " <> inputName date) >> exitSuccess

fetchRemoteFile :: Date -> IO Text
fetchRemoteFile date = do
  let host = "adventofcode.com"
      url =
        Req.https (T.pack host)
          /: T.pack (show (getYear date.year))
          /: "day"
          /: T.pack (show (getDay date.day))
          /: "input"
  now <- getCurrentTime
  session <- getEnv "AOC_SESSION"
  bs <-
    Req.runReq Req.defaultHttpConfig $
      Req.req Req.GET url Req.NoReqBody Req.bsResponse $
        Req.cookieJar (mkSessionCookie host session now)
  pure $ decodeUtf8 $ Req.responseBody bs

handleException :: Req.HttpException -> String
handleException (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException rsp _)))
  | responseStatus rsp == notFound404 = "Input file not found. Could it be for a future puzzle?"
handleException e = show e

mkSessionCookie :: String -> String -> UTCTime -> CookieJar
mkSessionCookie host session now =
  createCookieJar $
    maybeToList $
      generateCookie
        defaultSetCookie
          { setCookieName = "session",
            setCookieValue = BS.pack session
          }
        defaultRequest {host = BS.pack host}
        now
        True
