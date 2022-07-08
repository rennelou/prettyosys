module ParserTests (
  parseTests
) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Data.Text as T
import Text.Megaparsec hiding (State)

import Utils.Parsers.TextParser
import Utils.Parsers.SbyLog
import Utils.Parsers.PSL
import Data.Void

parseTests :: TestTree
parseTests = testGroup "Parser Tests" [sbyLogTests, pslLogTests]

sbyLogTests :: TestTree
sbyLogTests = testGroup "Sby Log" [coverSbyLogTest, proveSbyLogTest, assertionParseTest]

pslLogTests :: TestTree
pslLogTests = testGroup "PSL Parser" [pslTest]



coverSbyLogTest :: TestTree
coverSbyLogTest =
  testCase  "Cover"
  $ assertParser (pSbyLog "") . T.pack
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
  testCase "Prove"
  $ assertParser (pSbyLog "") . T.pack
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

assertionParseTest :: TestTree
assertionParseTest =
  testCase "Assertion Line"
  $  assertParser (pAssertion "" "") . T.pack 
  $  "engine_0.basecase: ##   0:00:00  Checking assertions in step 0..\n"




pslTest :: TestTree
pslTest =
  testCase "PSL"
  $ assertParser pPSL . T.pack
  $     "vunit linked_list_vu(linked_list(linked_list_rtl))\n"
    ++  "{\n"
    ++  "default clock is rising_edge(clk);\n"
    ++  "\n"
    ++  "assume rst = '1';\n"
    ++  "\n"
    ++  "attribute anyconst : boolean;\n"
    ++  "\n"
    ++  "signal any_value : std_logic_vector(CONTENT_SIZE - 1 downto 0);\n"
    ++  "attribute anyconst of any_value : signal is true;\n"
    ++  "assume any_value /= std_logic_vector(to_unsigned(0, CONTENT_SIZE));\n"
    ++  "\n"
    ++  "HAPPY_SHORTER_PATH : cover {\n"
    ++  "rst = '0' and done = '1' and start = '1' and opcode = append_command and new_value = any_value and new_node_address = any_address;\n"
    ++  "rst = '0' [+];\n"
    ++  "done = '1'\n"
    ++  "};\n"
    ++  "\n"
    ++  "------------------ DONE_START PROTOCOL ASSERTIONS ------------------------\n"
    ++  "IDLE_DONE_A : assert always (state_reg = idle -> done = '1') abort rst;\n"
    ++  "IDLE_DONE_B : assert always (done = '1' -> state_reg = idle) abort rst;\n"
    ++  "\n"
    ++  "IDLE_TO_OTHERS_STATES : assert always (\n"
    ++  "done = '1' and start = '1' and opcode < nop -> next(not stable(state_reg))\n"
    ++  ") abort rst;\n"
    ++  "\n"
    ++  "IDLE_HOLDS : assert always (\n"
    ++  "done = '1' and (start = '0' or opcode >= nop) -> next(stable(state_reg))\n"
    ++  ") abort rst;\n"
    ++  "--------------------------------------------------------------------------\n"
    ++  "}"

assertParser :: TextParser a -> T.Text -> IO ()
assertParser parserc s =
  case runParser parserc "" s of
    Left  error -> ioError (userError $ errorBundlePretty error)
    Right _     -> return ()