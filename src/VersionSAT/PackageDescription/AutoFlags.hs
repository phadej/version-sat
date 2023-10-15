module VersionSAT.PackageDescription.AutoFlags (
    nonDisjointAutoFlags,
) where

import Control.Monad.IO.Class                 (liftIO)
import Data.Foldable.WithIndex                (ifor_)
import Data.Functor.Identity                  (Identity (..))
import Data.IORef                             (writeIORef)
import Data.Map                               (Map)
import Data.Traversable.WithIndex             (ifor)
import Distribution.PackageDescription.Parsec
       (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty                    (prettyShow)

import Distribution.PackageDescription

import qualified Data.ByteString as BS
import qualified Data.Map        as Map

import VersionSAT
import VersionSAT.PackageDescription.Common

nonDisjointAutoFlags :: GenericPackageDescription -> IO (Map FlagName (Model Identity))
nonDisjointAutoFlags gpd = case condLibrary gpd of
    Nothing  -> return Map.empty
    Just lib -> fmap (Map.mapMaybe id) $ ifor autoFlags $ \fn _ -> do
        runSATMaybe $ do
            st <- emptyS
            propTrue  <- componentProp st (Map.insert fn True  manualFlags) lib
            liftIO $ writeIORef (litFlag st) Map.empty -- reset automatic flags.
            propFalse <- componentProp st (Map.insert fn False manualFlags) lib
            addProp (propTrue /\ propFalse)
            model <- readModel st
            solve model
  where
    manualFlags :: Map.Map FlagName Bool
    manualFlags = Map.fromList
        [ (fn, v)
        | MkPackageFlag { flagName = fn, flagDefault = v, flagManual = True } <- genPackageFlags gpd
        ]

    autoFlags :: Map.Map FlagName Bool
    autoFlags = Map.fromList
        [ (fn, v)
        | MkPackageFlag { flagName = fn, flagDefault = v, flagManual = False } <- genPackageFlags gpd
        ]

_test :: FilePath -> IO ()
_test fp = do
    contents <- BS.readFile fp
    case parseGenericPackageDescriptionMaybe contents of
        Nothing -> fail "cannot parse"
        Just gpd -> do
            res <- nonDisjointAutoFlags gpd
            ifor_ res $ \fn model -> do
                putStrLn $ prettyShow fn ++ " NOT disjoint"
                prettyModel model
