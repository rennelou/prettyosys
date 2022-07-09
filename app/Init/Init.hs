module Init.Init (
  InitArgs(..),
  initProject
) where

import System.IO
import System.Directory
import qualified Data.Text as T 

import Settings.Settings

data InitArgs = InitArgs {
  workDirArg    :: String,
  srcDirArg     :: String,
  vunitsDirArgs :: String,
  dephtArg      :: Int
}

initProject :: InitArgs -> IO ()
initProject initArgs = do
  createDirectoryIfMissing True (srcDirArg initArgs)
  createDirectoryIfMissing True (vunitsDirArgs initArgs)
  writeFile settingsFilename (T.unpack $ createSettings (convertToSettings initArgs)) 

convertToSettings :: InitArgs -> Settings
convertToSettings initArgs = 
  Settings 
    (T.pack $ workDirArg initArgs)
    (T.pack $ srcDirArg initArgs)
    (T.pack $ vunitsDirArgs initArgs)
    (dephtArg initArgs)

counterRTLFilename = "counter.vhd"

counterRTL =
  "library IEEE;\n\
  \use IEEE.std_logic_1164.all;\n\
  \use IEEE.numeric_std.all;\n \
  \\n \
  \entity counter is\n \
  \  port(\n \
  \      clk   : in std_logic;\n \
  \      rst   : in std_logic;\n \
  \      value : out std_logic_vector(7 downto 0)\n \
  \  );\n \
  \end entity counter;\n \
  \\n \
  \architecture counter_rtl of counter  is\n \
  \\n \
  \  signal counter_reg, counter_next : unsigned(7 downto 0) := (others => '0');\n \
  \\n \
  \begin\n \
  \\n \
  \  TICK: process (clk)\n \
  \  begin\n \
  \      if rising_edge(clk) then\n \
  \          if rst = '1' then\n \
  \              counter_reg <= (others => '0');\n \
  \          else\n \
  \              counter_reg <= counter_next;\n \
  \          end if;\n \
  \      end if;\n \
  \  end process TICK;\n \
  \\n \
  \  counter_next <= counter_reg + 1\n \
  \                  when counter_reg < 15\n \
  \                  else (others => '0');\n \
  \\n \
  \  value <= std_logic_vector(counter_reg);\n \
  \\n \
  \end architecture counter_rtl;"

counterPropertiesFilename = "counter.psl"

counterProperties =
  "vunit counter_vu(counter(counter_rtl))\n\
  \{\n\
  \  default clock is rising_edge(clk);\n\
  \\n\
  \  assume rst = '1';\n\
  \\n\
  \  COUNT_TO_FIVE : cover {\n\
  \    value = std_logic_vector(to_unsigned(5, 8))\n\
  \  };\n\
  \}"