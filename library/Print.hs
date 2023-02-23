{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Print where

import Control.Concurrent (threadDelay) -- delay in microseconds
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Text as T
import Network.HTTP.Simple
import System.Random (randomRIO)
import Types

aamulehtiDownload :: Magazine -> NetworkOperation CompleteAamulehti
aamulehtiDownload Magazine {..} = do
  let spreadNumbers = zip [0, 2 ..] spreads
  CompleteAamulehti <$> concat <$> mapM (\(pnr, spr) -> aamulehtiSpread pnr baseurl spr) spreadNumbers

aamulehtiSpread :: PageNumber -> T.Text -> Spread -> NetworkOperation [AamulehtiPage]
aamulehtiSpread pnr baseUrl Spread {..} = do
  leftSide <- mapM (dlPage baseUrl) leftPage
  rightSide <- mapM (dlPage baseUrl) rightPage
  humaneWait (T.pack $ "Haetaan sivut " <> show pnr <> " ja " <> show (pnr + 1) <> ".") ()
  return $
    catMaybes
      [ fmap (buildPage pnr) leftSide,
        fmap (buildPage $ pnr + 1) rightSide
      ]
  where
    buildPage pageN (p, o) = AamulehtiPage p o pageN

dlPage :: T.Text -> (T.Text, T.Text) -> NetworkOperation (B.ByteString, B.ByteString)
dlPage baseUrl (page, overlay) =
  (,) <$> download (T.concat [baseUrl, "/", page]) <*> download (T.concat [baseUrl, "/", overlay])
  where
    download :: T.Text -> NetworkOperation B.ByteString
    download url = do
      req <- parseRequest (T.unpack url)
      getResponseBody <$> httpBS req

mimicAHumanAndWaitBetweenSpreads :: T.Text -> IO ()
mimicAHumanAndWaitBetweenSpreads msg = do
  die2 <- randomRIO (1, 6) :: IO Int
  die3 <- unNegateDie <$> randomRIO (-10, 10) :: IO Int
  die4 <- unNegateDie <$> randomRIO (-360, 75) :: IO Int
  let total =
        1 + die2 + die3
          + die4
  putStrLn $ T.unpack msg
  putStrLn ("Matkitaan ihmistä ja odotellaan " ++ show total ++ "s.")
  threadDelay (total * 1000000)
  where
    unNegateDie die = if die < 0 then 0 else die

humaneWait :: T.Text -> a -> NetworkOperation a
humaneWait msg x = liftIO (mimicAHumanAndWaitBetweenSpreads msg) >> return x

aamulehtiPrint :: CompleteAamulehti -> FileOperation FilePath
aamulehtiPrint CompleteAamulehti {..} = do
  mapM_ writePages pages
  -- tehdään ulkoinen imagemagick/pdf .sh kutsu?
  return "./aamulehti.pdf"
  where
    writePages :: AamulehtiPage -> FileOperation ()
    writePages AamulehtiPage {..} = do
      let pnr p = T.unpack (T.justifyRight 3 '0' (T.pack $ show p))
      liftIO $ B.writeFile ("pages/page_" ++ pnr pageNumber ++ ".jpg") page
      liftIO $ B.writeFile ("pages/page_" ++ pnr pageNumber ++ "_overlay.png") overlay
