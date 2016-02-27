#!/usr/bin/env stack
-- stack --resolver lts-5.0 --install-ghc runghc --package megaparsec --package text --package containers

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Text.Megaparsec
import           Text.Megaparsec.Text



ltsVersion :: Text
ltsVersion = "5.4"

-- | Version of the Cabal package to be generated.
--
-- Decoupling this from 'ltsVersion' is useful if something went wrong and
-- we need a new point release for the generated package.
generatedPackageVersion :: Text
generatedPackageVersion = ltsVersion

-- | Source to read the constraints from.
stackageFilename :: FilePath
stackageFilename = "lts-5.4"



main :: IO ()
main = do
    contents <- T.readFile stackageFilename
    case parse fileP stackageFilename contents of
        Left err -> T.putStrLn "Parse error: " >> print err
        Right packages -> generateOutputFiles packages

-- | A list of packages and their respective versions.
newtype Packages = Packages [(Text, Maybe Text)] -- Name/version
    deriving (Eq, Ord, Show)

fileP :: Parser Packages
fileP = manyTill anyChar (try (string "constraints:"))
     *> space
     *> packagesP
     <* manyTill anyChar eof

  where

    packagesP :: Parser Packages
    packagesP = fmap Packages (sepBy packageP comma)
      where
        comma = char ',' *> space

    packageP :: Parser (Text, Maybe Text)
    packageP = do
        name <- T.pack <$> some (alphaNumChar <|> char '-')
        space
        let versionNumber = do
                _ <- string "=="
                v <- some (digitChar <|> char '.')
                pure (Just (T.pack v) )
            merelyInstalled = Nothing <$ string "installed"
        version <- versionNumber <|> merelyInstalled
        space
        pure (name, version)



generateOutputFiles :: Packages -> IO ()
generateOutputFiles packages = do
    T.writeFile "output/stackage-everything.cabal"
                (renderCabalFile packages)
    T.writeFile "output/stack.yaml"
                renderStackYaml
    T.writeFile "output/README.md"
                renderReadme
    T.writeFile "output/Setup.hs"
                renderSetupHs



-- | Template to generate the .cabal file
renderCabalFile
    :: Packages
    -> Text
renderCabalFile packages = T.intercalate "\n"
    [ "name:          stackage-everything"
    , "version:       " <> generatedPackageVersion
    , "synopsis:      Meta-package to depend on all of Stackage LTS " <> ltsVersion <> "."
    , "description:"
    , "    This meta-package depends on the entirety of Stackage."
    , "    ."
    , "    See README.md for further details."
    , ""
    , "license:       PublicDomain"
    , "author:        David Luposchainsky <dluposchainsky(λ)gmail.com>"
    , "maintainer:    David Luposchainsky <dluposchainsky(λ)gmail.com>"
    , "build-type:    Simple"
    , "homepage:      https://github.com/quchen/stackage-everything"
    , "bug-reports:   https://github.com/quchen/stackage-everything/issues"
    , "category:      Development"
    , "cabal-version: >=1.10"
    , ""
    , "source-repository head"
    , "    type:     git"
    , "    location: https://github.com/quchen/stackage-everything"
    , ""
    , "library"
    , "    exposed-modules:  Stack.Everything.Dummy"
    , "    hs-source-dirs:   src"
    , "    default-language: Haskell2010"
    , "    build-depends:    " <> renderPackages packages ]

  where

    renderPackages :: Packages -> Text
    renderPackages (Packages p) = T.intercalate separator (foldMap renderPackage p)
      where
        separator = "\n" <> T.replicate 20 " " <> ", "
        renderPackage (name, _)
            | name == "base" = ["base < 127"]
        renderPackage (name, Just version) = [name <> " == " <> version ]
        renderPackage (name, Nothing) = [name]



-- | Template to generate the stack.yaml file
renderStackYaml :: Text
renderStackYaml = T.intercalate "\n"
    [ "resolver: lts-" <> ltsVersion
    , ""
    , "packages:"
    , "    - '.'"
    , ""
    , "extra-deps: []"
    , ""
    , "flags: {}"
    , ""
    , "extra-package-dbs: []" ]



renderReadme :: Text
renderReadme = T.unlines
    [ "stackage-everything"
    , "-------------------"
    , ""
    , "This meta package depends on the entirety of Stackage."
    , ""
    , "The purpose of this is making Stackage available offline, which can be useful"
    , "if you're in an area with poor or no internet connectivity, such as airplanes"
    , "or rural areas."
    , ""
    , "In order to"
    , ""
    , "```bash"
    , "stack build --prefetch --dry-run --only-dependencies"
    , "```"
    , ""
    , "This package is a useful version of [acme-everything], is a joke package, and fails to build due to all the incompatibilities."
    , ""
    , ""
    , "[acme-everything]: http://hackage.haskell.org/package/acme-everything" ]



renderSetupHs :: Text
renderSetupHs = T.unlines
    [ "import Distribution.Simple"
    , "main = defaultMain" ]
