module VersionExtras where

import Distribution.Version (Version, mkVersion, nullVersion, versionNumbers)

import qualified Distribution.Types.VersionInterval.Legacy as L

data Wedge
    = Disjoint
    | Equal
    
    | XDiffY L.VersionIntervals
    | YDiffX L.VersionIntervals

    | Tri L.VersionIntervals L.VersionIntervals L.VersionIntervals
  deriving Show

nullVersionIntervals :: L.VersionIntervals -> Bool
nullVersionIntervals = null . L.versionIntervals

differenceVersionIntervals :: L.VersionIntervals -> L.VersionIntervals -> L.VersionIntervals
differenceVersionIntervals x y = L.intersectVersionIntervals x (L.invertVersionIntervals y)

wedge :: L.VersionIntervals -> L.VersionIntervals -> Wedge
wedge x y
    | nullVersionIntervals inter
    = Disjoint

    | x == y
    = Equal

    | nullVersionIntervals yDiffX
    = XDiffY xDiffY

    | nullVersionIntervals xDiffY
    = YDiffX yDiffX

    | otherwise
    = Tri xDiffY inter yDiffX

  where
    inter = L.intersectVersionIntervals x y
    xDiffY = differenceVersionIntervals x y
    yDiffX = differenceVersionIntervals y x

sampleVersion :: L.VersionIntervals -> Version
sampleVersion is = case L.versionIntervals is of
    [] -> nullVersion
    (L.LowerBound v L.InclusiveBound, _) : _ -> v
    (L.LowerBound v L.ExclusiveBound, _) : _ -> nextVersion v

nextVersion :: Version -> Version
nextVersion v = mkVersion (versionNumbers v ++ [0])
