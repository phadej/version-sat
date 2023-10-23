module VersionSAT.PackageDescription.Revision (
    compareRevision,
    POrdering (..),
) where

import Control.Monad.IO.Class
import Data.Functor.Identity (Identity (..))

import Distribution.PackageDescription

import qualified Data.Map        as Map

import VersionSAT
import VersionSAT.PackageDescription.Common

data POrdering a
  = PEQ
  | PLT a
  | PGT a
  | PNC
  deriving (Functor, Foldable, Traversable, Show)

compareRevision :: GenericPackageDescription -> GenericPackageDescription -> IO (POrdering (Model Identity))
compareRevision gpd1 gpd2 =
     case condLibrary gpd1 of
        Nothing -> return PNC
        Just lib1 -> case condLibrary gpd2 of
            Nothing -> return PNC
            Just lib2 -> compareComponents lib1 lib2

compareComponents :: CondTree ConfVar [Dependency] Library -> CondTree ConfVar [Dependency] Library -> IO (POrdering (Model Identity))
compareComponents comp1 comp2 = do
    le <- implies comp1 comp2
    ge <- implies comp2 comp1

    return $ case (le, ge) of
        (Just _,  Just _)  -> PEQ
        (Just x,  Nothing) -> PGT x
        (Nothing, Just y)  -> PLT y
        (Nothing, Nothing) -> PNC

  where
    implies c1 c2 = runSATMaybe $ do
            st <- emptyS
            prop1 <- componentProp st Map.empty c1
            prop2 <- componentProp st Map.empty c2

            addProp (neg $ prop1 --> prop2)

            model <- readModel st
            solve model
