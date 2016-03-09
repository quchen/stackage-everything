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



newtype Name = Name Text deriving (Eq, Ord, Show)
data Version = Version Text | NoVersion deriving (Eq, Ord, Show)

main :: IO ()
main = generatePackage "5.6"
                       "5.6"
                       "input/stackage-lts-5.6.cabal"

generatePackage
    :: Text     -- ^ Stackage LTS version to target.
    -> Text     -- ^ Version of the Cabal package to be generated.
                --
                -- Decoupling this from the LTS version parameter is useful if
                -- something went wrong and we need a new point release for the
                -- generated package.
    -> FilePath -- ^ Source to read the constraints from.
    -> IO ()
generatePackage ltsVersion generatedPackageVersion stackageFilename = do
    putStrLn "Reading input file"
    contents <- T.readFile stackageFilename
    putStrLn "Parsing input"
    case parse cabalConstraintFileP stackageFilename contents of
        Left err -> T.putStrLn "Parse error: " >> print err
        Right packages -> generateOutputFiles packages ltsVersion generatedPackageVersion

-- | A list of packages and their respective versions.
newtype Packages = Packages [(Name, Version)] -- Name/version
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



generateOutputFiles :: Packages -> Text -> Text -> IO ()
generateOutputFiles packages ltsVersion generatedPackageVersion = do
    putStrLn "Generating output files"
    T.writeFile "output/stackage-everything.cabal"
                (renderCabalFile packages ltsVersion generatedPackageVersion)
    T.writeFile "output/stack.yaml"
                (renderStackYaml ltsVersion)
    T.writeFile "output/README.md"
                renderReadme
    T.writeFile "output/Setup.hs"
                renderSetupHs



-- | Template to generate the .cabal file
renderCabalFile
    :: Packages
    -> Text
    -> Text
    -> Text
renderCabalFile packages ltsVersion generatedPackageVersion = T.intercalate "\n"
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
    , "extra-source-files: README.md"
    , ""
    , "source-repository head"
    , "    type:     git"
    , "    location: https://github.com/quchen/stackage-everything"
    , ""
    , "library"
    , "    exposed-modules:    Development.Stack.Everything.Dummy"
    , "    hs-source-dirs:     src"
    , "    default-language:   Haskell2010"
    , "    build-depends:      " <> renderPackages packages ]

  where

    renderPackages :: Packages -> Text
    renderPackages (Packages p) = T.intercalate separator (foldMap renderPackage p)
      where
        separator = "\n" <> T.replicate 22 " " <> ", "
        renderPackage (Name n, _)
            | n == "base" = ["base < 127"] -- To make `cabal check` happy
        renderPackage (Name n, Version v) = [n <> " == " <> v ]
        renderPackage (Name n, NoVersion) = [n]



-- | Template to generate the stack.yaml file
renderStackYaml :: Text -> Text
renderStackYaml ltsVersion = T.intercalate "\n"
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
    , "Use `stack build` with appropriate parameters to make use of it."
    , "For example, to download all the source files so they can be installed"
    , "without an internet connection later, run"
    , ""
    , "```bash"
    , "stack build --prefetch --dry-run --only-dependencies"
    , "```" ]


renderSetupHs :: Text
renderSetupHs = T.unlines
    [ "import Distribution.Simple"
    , "main = defaultMain" ]
