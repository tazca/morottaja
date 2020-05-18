{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}

module Print where

import Control.Concurrent (threadDelay) -- delay in microseconds
import qualified Data.ByteString as B
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Text as T
import Network.HTTP.Simple
import System.Random (randomRIO)

import Types

getMagazine :: Magazine -> IO CompleteAamulehti
getMagazine Magazine{..} = do
  let numberedSpreads = zip [0, 2..] spreads
  completeAamulehti <- mapM (\(pnr, spr) -> getSpread pnr baseurl spr) numberedSpreads
  return $ CompleteAamulehti
    (concatMap (\(a, b) -> catMaybes [a, b]) completeAamulehti)

getSpread :: PageNumber -> T.Text -> Spread -> IO (Maybe AamulehtiPage,
                                                   Maybe AamulehtiPage)
getSpread pnr baseUrl Spread{..} = do
  leftSide <- mapM (getPage baseUrl) leftPage
  rightSide <- mapM (getPage baseUrl) rightPage
  mimicAHumanAndWaitBetweenSpreads
  return (if isJust leftSide
          then Just $ AamulehtiPage (head $ fromJust leftSide) (head $ tail $ fromJust leftSide) (pnr)
          else Nothing,
          if isJust rightSide
          then Just $ AamulehtiPage (head $ fromJust rightSide) (head $ tail $ fromJust rightSide) (pnr + 1)
          else Nothing) -- TODO: list <-> tuple wankkaus pois

getPage :: T.Text -> (T.Text, T.Text) -> IO [B.ByteString]
getPage baseUrl (page, overlay) = sequence [dlUrl (T.concat [baseUrl, "/", page]),
                                            dlUrl (T.concat [baseUrl, "/", overlay])]


dlUrl :: T.Text -> IO B.ByteString
dlUrl pageUrl = do
  req <- parseRequest $ T.unpack pageUrl
  getResponseBody <$> httpBS req

mimicAHumanAndWaitBetweenSpreads :: IO ()
mimicAHumanAndWaitBetweenSpreads = do
  die2 <- randomRIO (1, 6) :: IO Int
  die3 <- unNegateDie <$> randomRIO (-10, 10) :: IO Int
  die4 <- unNegateDie <$> randomRIO (-360, 75) :: IO Int
  let total = 1 + die2 + die3
        + die4
  putStrLn ("Sivun kääntämiseen menee " ++ show total ++ "s.")
  threadDelay (total * 1000000)
  where
    unNegateDie die = if die < 0 then 0 else die

printMagazine :: CompleteAamulehti -> IO FilePath
printMagazine CompleteAamulehti{..} = do
  mapM_ writePages pages
  -- tehdään PDF-kutsu
  return "aamulehti.pdf"
  where
    writePages :: AamulehtiPage -> IO ()
    writePages AamulehtiPage{..} =
      let pnr p
            | p < 100 && p >= 10 = "0" ++ show p
            | p < 10 = "00" ++ show p
            | otherwise = show p
      in do
        B.writeFile ("pages/page_" ++ pnr pageNumber ++ ".jpg") page
        B.writeFile ("pages/page_" ++ pnr pageNumber ++ "_overlay.png") overlay
