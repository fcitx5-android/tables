{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_, void)
import Data.List.Extra (find)
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, SrcLoc (srcLocFile), callStack, getCallStack, withFrozenCallStack)
import System.Directory.Extra (canonicalizePath)

extraTableNames :: [String]
extraTableNames =
  [ "boshiamy",
    "zhengma",
    "cangjie3",
    "cangjie5",
    "cangjie-large",
    "quick3",
    "quick5",
    "quick-classic",
    "scj6",
    "easy-large",
    "wu",
    "cantonese",
    "cantonhk",
    "jyutping-table",
    "stroke5",
    "array30",
    "array30-large",
    "wubi-large",
    "zhengma-large",
    "zhengma-pinyin",
    "t9",
    "wubi98",
    "wubi98-pinyin",
    "wubi98-single"
  ]

otherTableNames :: [String]
otherTableNames =
  [ "am/amharic",
    "ar/arabic",
    "ml/malayalam-phonetic",
    "ta/tamil-remington",
    "other/cns11643",
    "other/compose",
    "other/emoji",
    "other/ipa-x-sampa",
    "other/latex",
    "uk/translit-ua",
    "ru/rustrad",
    "ru/translit",
    "ru/yawerty",
    "th/thai",
    "vi/viqr"
  ]

--------------------------------------------------------------------------------
tablesVersion :: String
tablesVersion = "1"

main :: IO ()
main = shakeArgs shakeOptions {shakeReport = ["report.html"], shakeVersion = tablesVersion} $ do
  mainPathRule
  forM_ extraTableNames $ tableIMRule "fcitx5-table-extra"
  forM_ otherTableNames $ tableIMRule "fcitx5-table-other"
  "fcitx5-table-extra" ~> need [name <.> "zip" | name <- extraTableNames]
  "fcitx5-table-other" ~> need [takeBaseName name <.> "zip" | name <- otherTableNames]
  "everything" ~> need ["fcitx5-table-extra", "fcitx5-table-other"]

--------------------------------------------------------------------------------

tableIMRule :: FilePath -> String -> Rules ()
tableIMRule fp name = do
  -- @tableName@ is the name of the table
  -- while @name@ is the path to the table relative to fcitx5-table-extra or fcitx5-table-other
  let tableName = takeBaseName name
      conf = tableName <.> "conf"
      dict = tableName <.> "main" <.> "dict"
      packaged = tableName <.> "zip"
  conf %> \out -> do
    src <- getCanonicalizedRootSrc $ fp </> "tables" </> name <.> "conf" <.> "in"
    poDir <- getCanonicalizedRootSrc $ fp </> "po"
    _ <- getDirectoryFiles poDir ["*.po"]
    need [src]
    cmd_ "msgfmt" "-d" poDir "--desktop" "--template" src "-o" out
  dict %> \out -> do
    src <- getCanonicalizedRootSrc $ fp </> "tables" </> name <.> "txt"
    need [src]
    cmd_ "libime_tabledict" src out
  packaged %> \_ -> do
    need [dict, conf]
    cmd_ "zip" tableName dict conf

--------------------------------------------------------------------------------

data MainPath = MainPath
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult MainPath = FilePath

getCanonicalizedRootSrc :: FilePath -> Action FilePath
getCanonicalizedRootSrc fp = do
  root <- askOracle MainPath
  liftIO . canonicalizePath $ root </> fp

mainPathRule :: Rules ()
mainPathRule = void $
  addOracleCache $
    \MainPath -> takeDirectory <$> liftIO getMainPath

getMainPath :: HasCallStack => IO FilePath
getMainPath =
  withFrozenCallStack
    $ canonicalizePath
      . srcLocFile
      . snd
      . fromJust
      . find ((== "getMainPath") . fst)
      . getCallStack
    $ callStack

--------------------------------------------------------------------------------
