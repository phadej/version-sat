{-# LANGUAGE UndecidableInstances #-}
module VersionSAT.PackageDescription.Common (
    S (..),
    emptyS,
    Model (..),
    prettyModel,
    readModel,
    componentProp,
) where

import Control.Applicative      (liftA2)
import Control.Monad            (unless, when)
import Control.Monad.IO.Class   (liftIO)
import Data.Bitraversable       (bitraverse)
import Data.Foldable            (foldl', for_)
import Data.Functor.Identity    (Identity (..))
import Data.HKD
       (FFoldable (..), FFunctor (..), FTraversable (..), ffmapDefault,
       ffoldMapDefault)
import Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import Data.Map                 (Map)
import Distribution.Compat.Lens (view)
import Distribution.Compiler    (CompilerFlavor)
import Distribution.Pretty      (prettyShow)
import Distribution.System      (Arch, OS)

import Distribution.PackageDescription
import Distribution.Version

import qualified Data.Map                          as Map
import qualified Distribution.Types.BuildInfo.Lens as L

import VersionSAT

-------------------------------------------------------------------------------
-- S
-------------------------------------------------------------------------------

data S s = S
    { litOS   :: !(IORef (Map OS (Lit s Bool)))
    , litArch :: !(IORef (Map Arch (Lit s Bool)))
    , litFlag :: !(IORef (Map FlagName (Lit s Bool)))
    , litComp :: !(IORef (Map CompilerFlavor (Lit s Bool, Lit s Version)))
    , litDeps :: !(IORef (Map PackageName (Lit s Bool, Lit s Version)))
    }

emptyS :: SAT s (S s)
emptyS = liftIO $ do
    litOS   <- newIORef Map.empty
    litArch <- newIORef Map.empty
    litFlag <- newIORef Map.empty
    litComp <- newIORef Map.empty
    litDeps <- newIORef Map.empty
    return S {..}

boolLiteral :: Ord k => IORef (Map k (Lit s Bool)) -> k -> SAT s (Lit s Bool)
boolLiteral ref k = do
    m <- liftIO $ readIORef ref
    case Map.lookup k m of
        Just l -> return l
        Nothing -> do
            l <- newBoolLit
            liftIO $ writeIORef ref $ Map.insert k l m
            return l

versionLiteral :: Ord k => IORef (Map k (Lit s Bool, Lit s Version)) -> k -> SAT s (Lit s Bool, Lit s Version)
versionLiteral ref k = do
    m <- liftIO $ readIORef ref
    case Map.lookup k m of
        Just ls -> return ls
        Nothing -> do
            lb <- newBoolLit
            lv <- newVersionLit
            let ls = (lb, lv)
            liftIO $ writeIORef ref $ Map.insert k ls m
            return ls

literalOS :: S s -> OS -> SAT s (Lit s Bool)
literalOS S {..} = boolLiteral litOS

literalArch :: S s -> Arch -> SAT s (Lit s Bool)
literalArch S {..} = boolLiteral litArch

literalFlagName :: S s -> FlagName -> SAT s (Lit s Bool)
literalFlagName S {..} = boolLiteral litFlag

literalCompilerFlavor :: S s -> CompilerFlavor -> SAT s (Lit s Bool, Lit s Version)
literalCompilerFlavor S {..} = versionLiteral litComp

literalDependencyVersion :: S s -> PackageName -> SAT s (Lit s Bool, Lit s Version)
literalDependencyVersion S {..} = versionLiteral litDeps

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data Model f = Model
    { modelFlag :: Map FlagName (f Bool)
    , modelArch :: Map Arch (f Bool)
    , modelOS   :: Map OS (f Bool)
    , modelComp :: Map CompilerFlavor (f Bool, f Version)
    , modelDeps :: Map PackageName (f Bool, f Version)
    }

prettyModel :: Model Identity -> IO ()
prettyModel Model {..} = do
    unless (null modelFlag) $ putStrLn $ "flags: " ++ unwords
        [ (if v then '+' else '-') : prettyShow fn
        | (fn, Identity v) <- Map.toList modelFlag
        ]

    unless (null modelArch) $ putStrLn $ "arch: " ++ unwords
        [ (if v then '+' else '-') : prettyShow fn
        | (fn, Identity v) <- Map.toList modelArch
        ]

    unless  (null modelOS) $ putStrLn $ "os: " ++ unwords
        [ (if v then '+' else '-') : prettyShow fn
        | (fn, Identity v) <- Map.toList modelOS
        ]

    for_ (Map.toList modelComp) $ \(pn, (Identity b, Identity v)) ->
        if b
        then putStrLn $ "compiler " ++ prettyShow pn ++ " " ++ prettyShow v
        else putStrLn $ "compiler not " ++ prettyShow pn

    -- dependencies
    for_ (Map.toList modelDeps) $ \(pn, (Identity b, Identity v)) -> when b $
        putStrLn $ prettyShow pn ++ " " ++ prettyShow v

instance FFunctor Model where ffmap = ffmapDefault
instance FFoldable Model where ffoldMap = ffoldMapDefault
instance FTraversable Model where
    ftraverse f Model {..} = pure Model
        <*> traverse f modelFlag
        <*> traverse f modelArch
        <*> traverse f modelOS
        <*> traverse (bitraverse f f) modelComp
        <*> traverse (bitraverse f f) modelDeps

readModel :: S s -> SAT s (Model (Lit s))
readModel S {..} = liftIO $ do
    modelFlag <- readIORef litFlag
    modelArch <- readIORef litArch
    modelOS   <- readIORef litOS
    modelComp <- readIORef litComp
    modelDeps <- readIORef litDeps
    return Model {..}

deriving instance (Show (f Bool), Show (f Version)) => Show (Model f)

-------------------------------------------------------------------------------
-- Conversiin
-------------------------------------------------------------------------------

componentProp
    :: forall s bi. L.HasBuildInfo bi
    => S s
    -> Map FlagName Bool                 -- ^ flag assignment
    -> CondTree ConfVar [Dependency] bi  -- ^ component build-info tree
    -> SAT s (Prop s)
componentProp st manualFlags = convert
  where
    convert :: CondTree ConfVar [Dependency] bi -> SAT s (Prop s)
    convert (CondNode n _ ns) = do
        n' <- convertLib n
        ns' <- traverse convertBranch ns

        return $ foldl' (/\) n' ns'

    convertCondition :: Condition ConfVar -> SAT s (Prop s)
    convertCondition (Var c)    = convertConfVar c
    convertCondition (Lit b)    = return $ if b then true else false
    convertCondition (CNot c)   = neg <$> convertCondition c
    convertCondition (COr x y)  = liftA2 (\/) (convertCondition x) (convertCondition y)
    convertCondition (CAnd x y) = liftA2 (/\) (convertCondition x) (convertCondition y)

    convertConfVar :: ConfVar -> SAT s (Prop s)
    convertConfVar (OS os) = lit <$> literalOS st os
    convertConfVar (PackageFlag f)
        | Just v <- Map.lookup f manualFlags = return $ if v then true else false
    convertConfVar (PackageFlag f) = lit <$> literalFlagName st f
    convertConfVar (Arch arch) = lit <$> literalArch st arch
    convertConfVar (Impl c vr) = do
        (lb, lv) <- literalCompilerFlavor st c
        return (lit lb /\ inRange lv vr)

    convertDependency :: Dependency -> SAT s (Prop s)
    convertDependency (Dependency pn vr _ls) = do
        (lb, lv) <- literalDependencyVersion st pn
        return (lit lb /\ inRange lv vr)

    convertLib :: bi -> SAT s (Prop s)
    convertLib component = do
        let bi = view L.buildInfo component
        deps <- traverse convertDependency (targetBuildDepends bi)
        return $ foldl' (/\) true deps

    convertBranch :: CondBranch ConfVar [Dependency] bi -> SAT s (Prop s)
    convertBranch (CondBranch c t f) = do
        c' <- convertCondition c
        t' <- convert t
        f' <- maybe (return true) convert f
        return $ ite c' t' f'
