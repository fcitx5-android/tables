{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath

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
    "wubi98-single",
    "wubi98-large"
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

outputDir :: FilePath
outputDir = "build"

main :: IO ()
main = shakeArgs
  shakeOptions
    { shakeReport = ["report.html"],
      shakeVersion = tablesVersion,
      shakeFiles = outputDir
    }
  $ do
    forM_ extraTableNames $ tableIMRule "fcitx5-table-extra"
    forM_ otherTableNames $ tableIMRule "fcitx5-table-other"
    "fcitx5-table-extra" ~> need [outputDir </> name <.> "zip" | name <- extraTableNames]
    "fcitx5-table-other" ~> need [outputDir </> takeBaseName name <.> "zip" | name <- otherTableNames]
    "build" ~> need ["fcitx5-table-extra", "fcitx5-table-other"]
    "clean" ~> do
      removeFilesAfter outputDir ["//*"]

--------------------------------------------------------------------------------

tableIMRule :: FilePath -> String -> Rules ()
tableIMRule fp name = do
  -- @tableName@ is the name of the table
  -- while @name@ is the path to the table relative to fcitx5-table-extra or fcitx5-table-other
  let tableName = takeBaseName name
      conf = outputDir </> tableName <.> "conf"
      dict = outputDir </> tableName <.> "main" <.> "dict"
      packaged = outputDir </> tableName <.> "zip"
  conf %> \out -> do
    let src = fp </> "tables" </> name <.> "conf" <.> "in"
        poDir = fp </> "po"
    _ <- getDirectoryFiles poDir ["*.po"]
    need [src]
    cmd_ "msgfmt" "-d" poDir "--desktop" "--template" src "-o" out
  dict %> \out -> do
    let src = fp </> "tables" </> name <.> "txt"
    need [src]
    cmd_ "libime_tabledict" src out
  packaged %> \_ -> do
    need [dict, conf]
    cmd_ (Cwd outputDir) "zip" tableName (takeFileName dict) (takeFileName conf)
