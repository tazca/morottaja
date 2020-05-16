{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}

module Print where

import Network.HTTP.Conduit (simpleHttp)

import Types

getMagazine :: Magazine -> IO CompleteAamulehti
getMagazine = undefined

printMagazine :: CompleteAamulehti -> IO FilePath
printMagazine = undefined
