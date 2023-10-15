module PackageDescriptionExtras (
    simplifyCondTreeEx,
) where

import Distribution.PackageDescription
import Data.Foldable (foldl')
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)

import qualified Distribution.PackageDescription   as C

simplifyCondTreeEx
    :: forall a d u v. (Semigroup a, Semigroup d)
    => (v -> Either u Bool)
    -> CondTree v d a
    -> CondTree u d a
simplifyCondTreeEx env (CondNode a d ifs) = do
    let (ad1, ifs1) = partitionEithers $ map simplifyIf ifs
    let (ad2, ifs2) = unzipWith (\(CondNode a' d' ifs') -> ((a',d'), ifs')) (catMaybes ad1)
    let (a', d') = foldl' ((<>)) (a,d) ad2
    CondNode a' d' (concat ifs2 ++ ifs1)
  where
    simplifyIf :: CondBranch v d a -> Either (Maybe (CondTree u d a)) (CondBranch u d a)
    simplifyIf (CondBranch cnd t me) =
        case C.simplifyCondition cnd env of
          (C.Lit True,  _) -> Left $ Just $ simplifyCondTreeEx env t
          (C.Lit False, _) -> Left $ fmap (simplifyCondTreeEx env) me
          (cnd',      _)   -> Right (CondBranch cnd' (simplifyCondTreeEx env t) (fmap (simplifyCondTreeEx env) me))

unzipWith :: (c -> (a, b)) -> [c] -> ([a], [b])
unzipWith f xs = unzip (map f xs)
