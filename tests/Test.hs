import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Data.Text as T
import Text.Megaparsec hiding (State)
import Parsers.SbyLog.SbyLog
import Data.Void

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [sbyLogTests]

sbyLogTests :: TestTree
sbyLogTests = testGroup "Sby Log Parser Tests" [coverSbyLogTest, proveSbyLogTest]

coverSbyLogTest :: TestTree
coverSbyLogTest =
  testCase  "Cover Sby Log Parse"
  $ assertParseSbyLog
  $    "SBY 18:50:12 [ram_cover] Copy '/mnt/c/git/atg.mdadapter/src/mocks/memory_mock.vhd' to '/mnt/c/git/atg.mdadapter/verify_build/ram_cover/src/memory_mock.vhd'.\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: smtbmc\n"
    ++ "SBY 18:50:13 [ram_cover] base: starting process \"cd ram_cover/src; yosys -m ghdl -ql ../model/design.log ../model/design.ys\"\n"
    ++ "SBY 18:50:13 [ram_cover] base: ram.vhd:19:10:note: found RAM \"memory\", width: 8 bits, depth: 8\n"
    ++ "SBY 18:50:13 [ram_cover] base: ^\n"
    ++ "SBY 18:50:13 [ram_cover] base: finished (returncode=0)\n"
    ++ "SBY 18:50:13 [ram_cover] smt2: starting process \"cd ram_cover/model; yosys -m ghdl -ql design_smt2.log design_smt2.ys\"\n"
    ++ "SBY 18:50:13 [ram_cover] smt2: finished (returncode=0)\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: starting process \"cd ram_cover; yosys-smtbmc --presat --unroll -c --noprogress -t 20  --append 0 --dump-vcd engine_0/trace%.vcd --dump-vlogtb engine_0/trace%_tb.v --dump-smtc engine_0/trace%.smtc model/design_smt2.smt2\"\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Solver: yices\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Checking cover reachability in step 0..\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Reached cover statement at ram_vu.get.cover in step 0.\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Writing trace to VCD file: engine_0/trace0.vcd\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Writing trace to Verilog testbench: engine_0/trace0_tb.v\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Writing trace to constraints file: engine_0/trace0.smtc\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: ##   0:00:00  Status: passed\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: finished (returncode=0)\n"
    ++ "SBY 18:50:13 [ram_cover] engine_0: Status returned by engine: pass\n"
    ++ "SBY 18:50:13 [ram_cover] summary: Elapsed clock time [H:MM:SS (secs)]: 0:00:00 (0)\n"
    ++ "SBY 18:50:13 [ram_cover] summary: engine_0 (smtbmc) returned pass\n"
    ++ "SBY 18:50:13 [ram_cover] summary: trace: ram_cover/engine_0/trace0.vcd\n"
    ++ "SBY 18:50:13 [ram_cover] DONE (PASS, rc=0)\n"

proveSbyLogTest :: TestTree
proveSbyLogTest =
  testCase "Prove Sby Log Parse"
  $ assertParseSbyLog
  $    "SBY 18:50:13 [ram_prove] Copy '/mnt/c/git/atg.mdadapter/src/mocks/memory_mock.vhd' to '/mnt/c/git/atg.mdadapter/verify_build/ram_prove/src/memory_mock.vhd'.\n"
    ++ "SBY 18:50:13 [ram_prove] engine_0: smtbmc\n"
    ++ "SBY 18:50:13 [ram_prove] base: starting process \"cd ram_prove/src; yosys -m ghdl -ql ../model/design.log ../model/design.ys\"\n"
    ++ "SBY 18:50:13 [ram_prove] base: ram.vhd:19:10:note: found RAM \"memory\", width: 8 bits, depth: 8\n"
    ++ "SBY 18:50:13 [ram_prove] base: ^\n"
    ++ "SBY 18:50:13 [ram_prove] base: finished (returncode=0)\n"
    ++ "SBY 18:50:13 [ram_prove] smt2: starting process \"cd ram_prove/model; yosys -m ghdl -ql design_smt2.log design_smt2.ys\"\n"
    ++ "SBY 18:50:13 [ram_prove] smt2: finished (returncode=0)\n"
    ++ "SBY 18:50:13 [ram_prove] engine_0.basecase: starting process \"cd ram_prove; yosys-smtbmc --presat --unroll --noprogress -t 20  --append 0 --dump-vcd engine_0/trace.vcd --dump-vlogtb engine_0/trace_tb.v --dump-smtc engine_0/trace.smtc model/design_smt2.smt2\"\n"
    ++ "SBY 18:50:13 [ram_prove] engine_0.induction: starting process \"cd ram_prove; yosys-smtbmc --presat --unroll -i --noprogress -t 20  --append 0 --dump-vcd engine_0/trace_induct.vcd --dump-vlogtb engine_0/trace_induct_tb.v --dump-smtc engine_0/trace_induct.smtc model/design_smt2.smt2\"\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.basecase: ##   0:00:00  Solver: yices\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.induction: ##   0:00:00  Solver: yices\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.basecase: ##   0:00:00  Checking assumptions in step 0..\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.basecase: ##   0:00:00  Checking assertions in step 0..\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.induction: ##   0:00:00  Trying induction in step 20..\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.induction: ##   0:00:00  Temporal induction successful.\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.induction: ##   0:00:00  Status: passed\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.induction: finished (returncode=0)\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0: Status returned by engine for induction: pass\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.basecase: ##   0:00:00  Status: passed\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0.basecase: finished (returncode=0)\n"
    ++ "SBY 18:50:14 [ram_prove] engine_0: Status returned by engine for basecase: pass\n"
    ++ "SBY 18:50:14 [ram_prove] summary: Elapsed clock time [H:MM:SS (secs)]: 0:00:00 (0)\n"
    ++ "SBY 18:50:14 [ram_prove] summary: engine_0 (smtbmc) returned pass for induction\n"
    ++ "SBY 18:50:14 [ram_prove] summary: engine_0 (smtbmc) returned pass for basecase\n"
    ++ "SBY 18:50:14 [ram_prove] summary: successful proof by k-induction.\n"
    ++ "SBY 18:50:14 [ram_prove] DONE (PASS, rc=0)\n"

assertParseSbyLog :: String -> IO ()
assertParseSbyLog log = 
  case runParser pSbyLog "" . T.pack $ log of
    Left error -> ioError (userError $ errorBundlePretty error)
    Right result -> return ()