module VersionSAT.PackageDescription.Satisfiable (
    libSatisfiable,
) where

import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)

import Distribution.PackageDescription

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import VersionSAT
import VersionSAT.PackageDescription.Common

-- | Checks that library is satisfiable
libSatisfiable :: GenericPackageDescription -> IO (Maybe (Model Identity))
libSatisfiable gpd = case condLibrary gpd of
    Nothing -> return Nothing
    Just lib -> runSATMaybe $ do
        st <- emptyS
        prop <- componentProp st manualFlags lib
        addProp prop
        model <- readModel st
        solve model
  where
    manualFlags :: Map.Map FlagName Bool
    manualFlags = Map.fromList
        [ (fn, v)
        | MkPackageFlag { flagName = fn, flagDefault = v, flagManual = True } <- genPackageFlags gpd
        ]

_test :: FilePath -> IO ()
_test fp = do
    contents <- BS.readFile fp
    case parseGenericPackageDescriptionMaybe contents of
        Nothing  -> fail "cannot parse"
        Just gpd -> libSatisfiable gpd >>= traverse_ prettyModel
