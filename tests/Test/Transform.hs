{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

 -- Many of the tests match on a specific expected value,the other patterns should trigger a fail
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wall #-}
module Test.Transform where

import Language.Haskell.GHC.ExactPrint.Utils ( ss2range )

import GHC as GHC
    ( SrcSpan, GenLocated(L), RdrName, locA, LocatedN )
import GHC.Types.Name.Occurrence as GHC ( mkVarOcc )
import GHC.Types.Name.Reader as GHC ( mkRdrUnqual )

import Data.Generics as SYB ( Data, mkT, everywhere )

import System.FilePath ( (</>), (<.>) )

import Test.Common
    ( Changer,
      ReportType(Success),
      ParseFailure(ParseFailure),
      RoundtripReport(status, debugTxt),
      testPrefix,
      genTest )

import Test.HUnit ( assertBool, Test(..) )

transformTests :: FilePath -> Test
transformTests libdir = TestLabel "transformation tests" $ TestList
  [
    TestLabel "Low level transformations"
       (TestList (transformLowLevelTests libdir))
  ]

transformLowLevelTests :: FilePath -> [Test]
transformLowLevelTests libdir = [
    mkTestModChange libdir changeRenameCase1 "RenameCase1.hs"
  ]

mkTestModChange :: FilePath -> Changer -> FilePath -> Test
mkTestModChange libdir f file = mkTestMod libdir "expected" "transform" f file

changeRenameCase1 :: Changer
changeRenameCase1 _libdir parsed = return (rename "bazLonger" [((3,15),(3,18))] parsed)

mkTestMod :: FilePath -> String -> FilePath -> Changer -> FilePath ->  Test
mkTestMod libdir suffix dir f fp =
  let basename       = testPrefix </> dir </> fp
      expected       = basename <.> suffix
      writeFailure   = writeFile (basename <.> "out")
  in
    TestCase (do r <- either (\(ParseFailure s) -> error (s ++ basename)) id
                        <$> genTest libdir f basename expected
                 writeFailure (debugTxt r)
                 assertBool fp (status r == Success))


rename :: (Data a) => String -> [((Int, Int), (Int, Int))] -> a -> a
rename newNameStr spans' a
  = everywhere (mkT replaceRdr) a
  where
    newName = mkRdrUnqual (mkVarOcc newNameStr)

    cond :: SrcSpan -> Bool
    cond ln = ss2range ln `elem` spans'

    replaceRdr :: LocatedN RdrName -> LocatedN RdrName
    replaceRdr (L ln _)
        | cond (locA ln) = L ln newName
    replaceRdr x = x
