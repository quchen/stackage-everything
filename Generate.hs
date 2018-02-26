#!/usr/bin/env stack
{- stack --resolver lts-9.0 --install-ghc runghc
    --package megaparsec
    --package text
    --package containers
    --package wreq
    --package lens
    --package bytestring
    --package optparse-generic
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where



import           Control.Lens
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           Network.Wreq
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Text



newtype Name = Name Text deriving (Eq, Ord, Show)
data Version = Version Text | NoVersion deriving (Eq, Ord, Show)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ltsFlag : ltsVersion : [] | ltsFlag == "--lts"
            -> generateScript (T.pack ltsVersion)
        _other -> do
            T.hPutStrLn stderr "Usage: ./Generate.hs --lts <version>"
            T.hPutStrLn stderr "Version: e.g. 10.5 for LTS-10.5"
            exitWith (ExitFailure 1)

downloadCabalConstraints :: Text -> IO Text
downloadCabalConstraints lts = do
    response <- get ("https://www.stackage.org/lts-" ++ T.unpack lts ++ "/cabal.config")
    pure (T.decodeUtf8 (BSL.toStrict (view responseBody response)))

generateScript
    :: Text     -- ^ Stackage LTS version to target.
    -> IO ()
generateScript ltsVersion = do
    T.hPutStrLn stderr ("Downloading constraints for LTS-" <> ltsVersion)
    contents <- downloadCabalConstraints ltsVersion
    T.hPutStrLn stderr "Parsing constraints file"
    case parse cabalConstraintFileP "" contents of
        Left err -> T.putStrLn "Parse error: " >> print err
        Right (Packages packages) -> do
            T.putStrLn "#!/usr/bin/env bash"
            T.putStrLn ""
            for_ packages (\(Name pkgName, _pkgVersion) ->
                T.putStrLn ("stack build --dry-run --prefetch --resolver lts-" <> ltsVersion <> " " <> pkgName) )

-- | A list of packages and their respective versions.
newtype Packages = Packages [(Name, Version)]
    deriving (Eq, Ord, Show)

-- | Ad-hoc parser for handling Stackage provided Cabal constraint files.
cabalConstraintFileP :: Parser Packages
cabalConstraintFileP = manyTill anyChar (try (string "constraints:"))
     *> space
     *> packagesP
     <* manyTill anyChar eof

  where

    packagesP :: Parser Packages
    packagesP = fmap Packages (sepBy packageP comma)
      where
        comma = char ',' *> space

    packageP :: Parser (Name, Version)
    packageP = do
        name <- do n <- some (alphaNumChar <|> char '-')
                   (pure . Name . T.pack) n
        space
        version <- do
            let versionNumber = do
                    _ <- string "=="
                    v <- some (digitChar <|> char '.')
                    pure (Version (T.pack v) )
                merelyInstalled = NoVersion <$ string "installed"
            versionNumber <|> merelyInstalled
        space
        pure (name, version)
