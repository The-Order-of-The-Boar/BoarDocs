module Main where
import Test.HUnit
import Chunks
import Data.Text
import qualified System.Exit as Exit
 
sameCodeTypeLit:: Test
sameCodeTypeLit = TestCase (assertEqual "Lit should be equal Lit" True (sameCodeType (Lit, pack("a")) (Lit, pack("b")) ) )

sameCodeTypeCfg:: Test
sameCodeTypeCfg = TestCase (assertEqual "Cfg should be equal Cfg" True (sameCodeType (Cfg, pack("b")) (Cfg, pack("a")) ) )

sameCodeTypeCode:: Test
sameCodeTypeCode = TestCase (assertEqual "Code should be equal Code" True (sameCodeType (Code, pack("d")) (Code, pack("e")) ) )
 
sameCodeTypeTests :: Test
sameCodeTypeTests = TestList [TestLabel "Lit" sameCodeTypeLit, TestLabel "Cfg" sameCodeTypeCfg, TestLabel "Code" sameCodeTypeCode]
 
main :: IO ()
main = do
    result <- runTestTT sameCodeTypeTests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess