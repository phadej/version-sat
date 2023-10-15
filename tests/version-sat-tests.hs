module Main (main) where

import Test.Tasty (TestName, defaultMain, testGroup, TestTree)
import Data.HKD (Element (..))
import Data.Functor.Identity (Identity (..))
import Data.Foldable (traverse_)
import Test.Tasty.HUnit
import Distribution.Version
import Distribution.Pretty (prettyShow)

import VersionSAT

main :: IO ()
main = defaultMain $ testGroup "version-sat"
    [ satVersionProp "true" (mkVersion [0]) $ \_ -> singleton true
    , satVersionProp "major" (mkVersion [1]) $ \l -> singleton $
        inRange l (majorBoundVersion $ mkVersion [1])

    , satVersionProp "example1" (mkVersion [1]) $ \l -> singleton $
        inRange l (orLaterVersion $ mkVersion [1]) /\
        inRange l (earlierVersion $ mkVersion [2])

    , satVersionProp "example2" (mkVersion [1]) $ \l ->
        [ inRange l (orLaterVersion $ mkVersion [1])
        , inRange l (earlierVersion $ mkVersion [2])
        ]

{-
    , satVersionProp "example3" (mkVersion [0]) $ \l ->
        -- VersionIntervals [(LowerBound (mkVersion [4,9]) InclusiveBound,UpperBound (mkVersion [4,10]) ExclusiveBound)]
        -- VersionIntervals [(LowerBound (mkVersion [4,9]) InclusiveBound,UpperBound (mkVersion [4,10]) ExclusiveBound)])
        [ inRange l (unionVersionsRanges (orLaterVersion $ mkVersion))]
-}

    , unsatVersionProp "unsat01" $ \l -> singleton $
        inRange l (orLaterVersion $ mkVersion [2]) /\
        inRange l (earlierVersion $ mkVersion [1])

    , unsatVersionProp "unsat02" $ \l ->
        [ inRange l (orLaterVersion $ mkVersion [2])
        , inRange l (earlierVersion $ mkVersion [1])
        ]
    ]

singleton :: x -> [x]
singleton x = [x]

satVersionProp :: TestName -> Version -> (forall s. Lit s Version -> [Prop s]) -> TestTree
satVersionProp name expected f = testCase name $ do
    res <- runSATMaybe $ do
        l <- newVersionLit
        traverse_ addProp (f l)
        Element (Identity v) <- solve (Element l)
        return v

    case res of
        Nothing -> assertFailure "unsat"
        Just v  -> v @?= expected

unsatVersionProp :: TestName -> (forall s. Lit s Version -> [Prop s]) -> TestTree
unsatVersionProp name f = testCase name $ do
    res <- runSATMaybe $ do
        l <- newVersionLit
        traverse_ addProp (f l)
        Element (Identity v) <- solve (Element l)
        return v

    case res of
        Nothing -> return ()
        Just v  -> assertFailure $ prettyShow v
