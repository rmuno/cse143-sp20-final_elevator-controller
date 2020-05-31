
-- Testbench for elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity testbench is
-- empty
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

-- testbench helpers
signal reset: std_logic := '1';
signal clk: std_logic := '1';

begin

--  DUT: fir_4tap port map(reset_in,
--    tap0, tap1, tap2, tap3,
--    clk_in, x_in, y_out);
  process
  begin
  
    -- setup
    wait for 1ns;
    reset <= '0';
  	
    for i in 1 to (10) loop

      clk <= not clk;
      wait for 5 ns;
      clk <= not clk;
      wait for 5 ns;
    end loop;

    assert false report "elevator test complete" severity note;
    wait;
  end process;
end tb;
