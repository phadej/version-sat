{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module VersionSAT (
    -- * SAT Monad
    SAT,
    runSAT,
    runSATMaybe,
    UnsatException (..),
    -- * Literals
    Lit,
    newBoolLit,
    newVersionLit,
    Neg (..),
    -- * Propositional formulas
    Prop,
    true, false,
    lit, inRange,
    (\/), (/\), (<->), (-->), xor, ite,
    addDefinition,
    addProp,
    -- * Solving
    solve,
    solve_,
    -- * Simplification
    simplify,
) where

import Control.Applicative    (liftA2)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.SAT
       (Neg (..), SAT, UnsatException (..), runSAT, runSATMaybe, simplify,
       solve_)
import Data.Foldable          (foldl')
import Data.Functor.Identity
import Data.HKD
import Data.IORef
import Data.Traversable       (fmapDefault, foldMapDefault)
import Distribution.Version   (Version, VersionRange, anyVersion, nullVersion)

import qualified Control.Monad.SAT                         as SAT
import qualified Distribution.Types.VersionInterval.Legacy as L

import VersionExtras

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

-- | Literal.
--
-- To negate literate use 'neg'.
data Lit s a where
    LB :: SAT.Lit s -> Lit s Bool
    LV :: IORef [(L.VersionIntervals, SAT.Lit s)] -> Lit s Version

instance Show (Lit s a) where
    showsPrec d (LB l) = showsPrec d l
    showsPrec _ (LV _) = showString "v"

instance a ~ Bool => Neg (Lit s a) where
    neg (LB l) = LB (neg l)

newBoolLit :: SAT s (Lit s Bool)
newBoolLit = LB <$> SAT.newLit

newVersionLit :: SAT s (Lit s Version)
newVersionLit = do
    l <- SAT.trueLit
    ref <- liftIO $ newIORef [(L.toVersionIntervals anyVersion, l)]
    return (LV ref)

-------------------------------------------------------------------------------
-- Prop
-------------------------------------------------------------------------------

-- | Propositional formula.
data Prop s where
    PTrue   :: Prop s
    PFalse  :: Prop s
    (:&&:)  :: Prop s -> Prop s -> Prop s
    (:||:)  :: Prop s -> Prop s -> Prop s
    Not     :: Prop s -> Prop s
    Lit     :: Lit s Bool -> Prop s
    InRange :: Lit s Version -> VersionRange -> Prop s

deriving instance Show (Prop s)

infixr 5 \/
infixr 6 /\

-- | True 'Prop'.
true :: Prop s
true = PTrue

-- | False 'Prop'.
false :: Prop s
false = PFalse

-- | Make 'Prop' that version literal is in a version range
inRange :: Lit s Version -> VersionRange -> Prop s
inRange = InRange

-- | Make 'Prop' from a boolean literal.
lit :: Lit s Bool -> Prop s
lit = Lit

-- | Disjunction of propositional formulas, or.
(\/) :: Prop s -> Prop s -> Prop s
(\/) = (:||:)

-- | Conjunction of propositional formulas, and.
(/\) :: Prop s -> Prop s -> Prop s
(/\) = (:&&:)

-- | Implication of propositional formulas.
(-->) :: Prop s -> Prop s -> Prop s
x --> y = neg x \/ y

-- | Equivalence of propositional formulas.
(<->) :: Prop s -> Prop s -> Prop s
x <-> y = (x --> y) /\ (y --> x)

-- | Exclusive or, not equal of propositional formulas.
xor :: Prop s -> Prop s -> Prop s
xor x y = x <-> neg y

-- | If-then-else.
--
-- Semantics of @'ite' c t f@ are @ (c '/\' t) '\/' ('neg' c '/\' f)@.
--
ite :: Prop s -> Prop s -> Prop s -> Prop s
-- ite c t f = (c /\ t) \/ (neg c /\ f)
ite c t f = (c \/ f) /\ (neg c \/ t) /\ (t \/ f) -- this encoding makes (t == f) case propagate even when c is yet undecided.

-- | Negation of propositional formulas.
instance Neg (Prop s) where
    neg PTrue   = PFalse
    neg PFalse  = PTrue
    neg (Not p) = p
    neg p       = Not p

-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

-- | Assert that given 'Prop' is true.
addProp :: Prop s -> SAT s ()
addProp p = do
    p' <- convertProp p
    SAT.addProp p'

-- | Add definition of 'Prop'. The resulting literal is equivalent to the argument 'Prop'.
--
addDefinition :: Prop s -> SAT s (Lit s Bool)
addDefinition p = do
    p' <- convertProp p
    l <- SAT.addDefinition p'
    return (LB l)

convertProp :: Prop s -> SAT s (SAT.Prop s)
convertProp PTrue        = return SAT.true
convertProp PFalse       = return SAT.false
convertProp (p :&&: q)   = liftA2 (SAT./\) (convertProp p) (convertProp q)
convertProp (p :||: q)   = liftA2 (SAT.\/) (convertProp p) (convertProp q)
convertProp (Not p)      = SAT.neg <$> convertProp p
convertProp (Lit (LB l)) = return (SAT.lit l)
convertProp (InRange (LV ref) vr) = do
    lits <- representVersionRange ref vr
    return (foldl' (SAT.\/) SAT.false (map SAT.lit lits))

invariant :: [(L.VersionIntervals, SAT.Lit s)] -> SAT s ()
invariant is = do
    let is' = map fst is
    -- liftIO $ print is'
    let i = foldr1 L.unionVersionIntervals is'
    if i == L.toVersionIntervals anyVersion
    then return ()
    else liftIO $ fail $ show i

representVersionRange :: forall s. IORef [(L.VersionIntervals, SAT.Lit s)] -> VersionRange -> SAT s [SAT.Lit s]
representVersionRange ref vr = do
    let vi = L.toVersionIntervals vr
    if nullVersionIntervals vi
    then return []
    else do
        xs <- liftIO (readIORef ref)
        -- liftIO $ print ("represent", xs, vi)
        aux [] id vi xs
  where
    aux :: [SAT.Lit s]
        -> ([(L.VersionIntervals, SAT.Lit s)] -> [(L.VersionIntervals, SAT.Lit s)])
        -> L.VersionIntervals
        -> [(L.VersionIntervals, SAT.Lit s)]
        -> SAT s [SAT.Lit s]
    aux _   new x []                 = error $ "panic! how this happened? " ++ show (x, new [])
    aux acc new x (part@(y,ly) : ys) = case wedge x y of
        Disjoint  ->
            aux acc (new |> part) x ys

        Equal     -> do
            let new' = new (part : ys)
            invariant new'
            liftIO $ writeIORef ref new'
            return (ly : acc)

        -- x = y + x'
        XDiffY x' -> do
            aux (ly : acc) (new |> part) x' ys

        -- y = x + y'
        YDiffX y' -> do
            lx  <- SAT.newLit
            ly' <- SAT.newLit

            SAT.addDisjDefinition ly [lx, ly']
            SAT.assertAtMostOne [lx, ly']

            let new' = (new $ (x, lx) : (y', ly') : ys)
            liftIO $ writeIORef ref new'

            return (lx : acc)

        -- x = i + x'
        -- y = i + y'
        Tri x' i y' -> do
            li  <- SAT.newLit
            ly' <- SAT.newLit

            SAT.addDisjDefinition ly [li, ly']
            SAT.assertAtMostOne [li, ly']

            aux (li : acc)
                (new |> (i, li) |> (y', ly'))
                x'
                ys

(|>) :: ([a] -> [a]) -> a -> ([a] -> [a])
xs |> x = xs . (x :)

-------------------------------------------------------------------------------
-- Solving
-------------------------------------------------------------------------------

solve :: FTraversable model => model (Lit s) -> SAT s (model Identity)
solve m = do
    m1 <- ftraverse freezeLit m
    Comp m2 <- SAT.solve (Comp m1)
    return (ffmap thawLit m2)

data FrozenLit b a where
    FLB :: a -> FrozenLit a Bool
    FLV :: [(L.VersionIntervals, a)] -> FrozenLit a Version

newtype Comp model a = Comp (model (FrozenLit a))

firstA :: Applicative f => (a -> f b) -> FrozenLit a c -> f (FrozenLit b c)
firstA f (FLB x)  = FLB <$> f x
firstA f (FLV xs) = FLV <$> traverse (traverse f) xs

instance FTraversable model => Functor (Comp model) where fmap = fmapDefault
instance FTraversable model => Foldable (Comp model) where foldMap = foldMapDefault

instance FTraversable model => Traversable (Comp model) where
    traverse f (Comp m) = fmap Comp $ ftraverse (firstA f) m

freezeLit :: Lit s a -> SAT s (FrozenLit (SAT.Lit s) a)
freezeLit (LB l) = return (FLB l)
freezeLit (LV ref) = do
    xs <- liftIO (readIORef ref)
    return (FLV xs)

thawLit :: FrozenLit Bool a -> Identity a
thawLit (FLB x)  = Identity x
thawLit (FLV xs) = Identity (go xs) where
    go :: [(L.VersionIntervals, Bool)] -> Version
    go []                = nullVersion
    go ((_,  False) : ys) = go ys
    go ((vr, True)  : _)  = sampleVersion vr
