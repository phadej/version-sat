module Main (main) where

import Text.Printf (printf)
import Distribution.Pretty (prettyShow)
import System.Environment (getArgs)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Data.Foldable (traverse_)
import Data.Foldable.WithIndex (ifor_)
import Distribution.PackageDescription
import Data.IORef
import System.IO

import qualified Cabal.Config as I
import qualified Data.Map as Map
import qualified Cabal.Index as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar

import VersionSAT.PackageDescription

main :: IO ()
main = do
    args <- getArgs
    case args of
        "revision" : x : y : [] -> revision x y
        "non-disjoint" : [] -> nonDisjointPackages
        _ -> unsatisfiablePackages

getHackageIndexPath :: IO FilePath
getHackageIndexPath = do
    cfg <- I.readConfig
    indexPath <- maybe
        (fail "no hackage")
        return
        (I.cfgRepoIndex cfg I.hackageHaskellOrg)

    printf "Hackage index at %s\n" indexPath

    return indexPath

revision :: FilePath -> FilePath -> IO ()
revision fp1 fp2 = do
    gpd1 <- readGPD fp1
    gpd2 <- readGPD fp2
    mres <- compareRevision gpd1 gpd2
    case mres of
        Nothing -> putStrLn "no libraries"
        Just res -> case res of
            PNC -> putStrLn "not comparable"
            PEQ -> putStrLn "equal"
            PLT m -> do
                putStrLn "relaxing, allowing e.g."
                prettyModel m
            PGT m -> do
                putStrLn "tightening, disallowing e.g."
                prettyModel m
  where
    readGPD fp = do
        bs <- BS.readFile fp
        case parseGenericPackageDescriptionMaybe bs of
            Nothing -> fail $ "cannot parse " ++ fp
            Just gpd -> return gpd

nonDisjointPackages :: IO ()
nonDisjointPackages = do
    (path, meta) <- I.cachedHackageMetadata

    totalRef <- newIORef (0 :: Int)
    unsatRef <- newIORef (0 :: Int)

    withFile path ReadMode $ \hdl ->
        ifor_ meta $ \pn pi -> do
            ifor_ (I.piVersions pi) $ \ver ri -> do
                entry <- Tar.hReadEntry hdl (I.riTarOffset ri)
                case Tar.entryContent entry of
                    Tar.NormalFile lbs _size -> do
                        let bs = LBS.toStrict lbs
                        case parseGenericPackageDescriptionMaybe bs of
                            Nothing -> fail $ "cannot parse " ++ prettyShow pn ++ " " ++ prettyShow ver
                            Just gpd -> do
                                modifyIORef' totalRef (1 +)

                                flags <- nonDisjointAutoFlags gpd
                                if null flags
                                then return ()
                                else do
                                    modifyIORef' unsatRef (1 +)
                                    printf "package %s %s: %s\n" (prettyShow pn) (prettyShow ver) (unwords (map prettyShow (Map.keys flags)))
                                    {-
                                    ifor_ flags $ \fn model -> do
                                        printf "flag %s:\n" (prettyShow fn)
                                        prettyModel model
                                    -}

                    _ -> fail "expecting normal file tar entry"

    total <- readIORef totalRef
    unsat <- readIORef unsatRef

    printf "%d of %d libraries with non-disjoint automatic flags (%.02f%%)\n" unsat total (100 * fromIntegral unsat / fromIntegral total :: Double)


unsatisfiablePackages :: IO ()
unsatisfiablePackages = do
    (path, meta) <- I.cachedHackageMetadata

    totalRef <- newIORef (0 :: Int)
    unsatRef <- newIORef (0 :: Int)

    withFile path ReadMode $ \hdl ->
        ifor_ meta $ \pn pi -> do
            ifor_ (I.piVersions pi) $ \ver ri -> do
                entry <- Tar.hReadEntry hdl (I.riTarOffset ri)
                case Tar.entryContent entry of
                    Tar.NormalFile lbs _size -> do
                        let bs = LBS.toStrict lbs
                        case parseGenericPackageDescriptionMaybe bs of
                            Nothing -> fail $ "cannot parse " ++ prettyShow pn ++ " " ++ prettyShow ver
                            Just gpd -> do
                                case condLibrary gpd of
                                    Nothing -> return ()
                                    Just _ -> do
                                        model <- libSatisfiable gpd
                                        case model of
                                            Nothing -> do
                                                printf "package %s %s\n" (prettyShow pn) (prettyShow ver)
                                                modifyIORef' totalRef (1 +)
                                                modifyIORef' unsatRef (1 +)
                                            Just _  -> do
                                                modifyIORef' totalRef (1 +)

                    _ -> fail "expecting normal file tar entry"

    total <- readIORef totalRef
    unsat <- readIORef unsatRef

    printf "%d of %d libraries unsatisfiable (%.02f%%)\n" unsat total (100 * fromIntegral unsat / fromIntegral total :: Double)
