#!/usr/bin/env stack
{- stack --resolver lts-10.7 --install-ghc runghc
    --package base
    --package bytestring
    --package lens
    --package megaparsec
    --package prettyprinter
    --package text
    --package wreq
    --
    -hide-all-packages
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where



import           Control.Lens
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Network.Wreq
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec                       as P
import           Text.Megaparsec.Char                  as P



newtype LtsVersion = LtsVersion Text deriving (Eq, Ord, Show)
newtype Name = Name Text deriving (Eq, Ord, Show)
data Version = Version Text | NoVersion deriving (Eq, Ord, Show)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ltsFlag : ltsVersion : [] | ltsFlag == "--lts"
            -> generateScript (LtsVersion (T.pack ltsVersion))
        _other -> do
            T.hPutStrLn stderr "Usage: ./Generate.hs --lts <version>"
            T.hPutStrLn stderr "Version: e.g. 10.5 for LTS-10.5"
            exitWith (ExitFailure 1)

downloadCabalConstraints :: LtsVersion -> IO Text
downloadCabalConstraints (LtsVersion lts) = do
    response <- get ("https://www.stackage.org/lts-" ++ T.unpack lts ++ "/cabal.config")
    pure (T.decodeUtf8 (BSL.toStrict (view responseBody response)))

generateScript
    :: LtsVersion
    -> IO ()
generateScript lts@(LtsVersion ltsVersion) = do
    T.hPutStrLn stderr ("Downloading constraints for LTS-" <> ltsVersion)
    contents <- downloadCabalConstraints lts
    T.hPutStrLn stderr "Parsing constraints file"
    case parse cabalConstraintFileP "" contents of
        Left err -> T.putStrLn "Parse error: " >> print err
        Right (Packages packages) ->
            let render = renderIO stdout
                layout = layoutPretty defaultLayoutOptions
            in (render . layout . vcat)
                [ "#!/usr/bin/env bash"
                , ""
                , "set -euo pipefail"
                , ""
                , hang 4 (backslashedLineBreaks
                    (pretty ("stack build --dry-run --prefetch --resolver lts-" <> ltsVersion)
                    : [pretty pkgName | (Name pkgName, _) <- packages
                                      , pkgName /= "ghc" -- Tends to misbehave so we exclude it here
                                      , pkgName /= "rts" -- Dito
                                      ]))
                , ""
                ]

backslashedLineBreaks :: [Doc ann] -> Doc ann
backslashedLineBreaks = concatWith (\x y -> x <> " \\" <> line <> y)

-- | A list of packages and their respective versions.
newtype Packages = Packages [(Name, Version)]
    deriving (Eq, Ord, Show)

-- | Ad-hoc parser for handling Stackage provided Cabal constraint files.
cabalConstraintFileP :: Parsec () Text Packages
cabalConstraintFileP = manyTill anyChar (try (string "constraints:"))
     *> P.space
     *> packagesP
     <* manyTill anyChar eof

  where

    packagesP :: Parsec () Text Packages
    packagesP = fmap Packages (sepBy packageP comma')
      where
        comma' = char ',' *> P.space

    packageP :: Parsec () Text (Name, Version)
    packageP = do
        name <- do n <- some (alphaNumChar <|> char '-')
                   (pure . Name . T.pack) n
        P.space
        version <- do
            let versionNumber = do
                    _ <- string "=="
                    v <- some (digitChar <|> char '.')
                    pure (Version (T.pack v) )
                merelyInstalled = NoVersion <$ string "installed"
            versionNumber <|> merelyInstalled
        P.space
        pure (name, version)
