
-- Testbench for elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity testbench is
	constant N_ctr : integer := 2;
end testbench; 

architecture tb of testbench is

-- DUT component
component elevator is
port(
  reset: in std_logic;
  clk: in std_logic;

  floor_request_up: in std_logic;
  floor_request_down: in std_logic;

--  x_in: in signed (7 downto 0);
--  y_out: out signed (15 downto 0)
  current_floor: out std_logic;
  moving_direction_up: out std_logic;
  moving_direction_down: out std_logic;
  door_open: out std_logic
);
end component;

component down_counter is
generic(
	N: integer
);
port(
  reset: in std_logic;
  clk: in std_logic;

  count: out std_logic_vector(N-1 downto 0)
);
end component;

-- testbench helpers
signal reset: std_logic := '1';
signal clk: std_logic := '1';

signal count : std_logic_vector(N_ctr-1 downto 0);

begin

--  DUT: fir_4tap port map(reset_in,
--    tap0, tap1, tap2, tap3,
--    clk_in, x_in, y_out);
--		DUT_COUNTER: down_counter
--			generic map(N_ctr)
--			port map(reset, clk, count);
  process
--		variable ctr_expected : unsigned (N_ctr-1 downto 0) := (others => '1');
  begin
  
    -- test counter
--    wait for 1ns;
--    reset <= '0';
--
--    for i in 0 to (10) loop
--      clk <= not clk;
--      wait for 5 ns;
--      clk <= not clk;
--      wait for 5 ns;
--
--			ctr_expected := ctr_expected - 1;
--	    assert (count = std_logic_vector(ctr_expected)) report
--				"#" & integer'image(i) &
--				": Expected " &
--				integer'image(to_integer(unsigned(ctr_expected))) &
--				" but got " &
--				integer'image(to_integer(unsigned(count))) severity error;
--    end loop;
--    assert false report "counter test complete" severity note;

    -- test elevator - 2 levels
    wait for 1ns;
    reset <= '0';

    for i in 0 to (10) loop
      clk <= not clk;
      wait for 5 ns;
      clk <= not clk;
      wait for 5 ns;


    end loop;

    assert false report "elevator test complete" severity note;
    wait;
  end process;
end tb;
