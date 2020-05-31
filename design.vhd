
-- elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity elevator is
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
end elevator;

architecture behavior_1 of elevator is
begin
  process(reset, clk) is
	begin
		if (reset = '1') then
			current_floor <= '0';
		  moving_direction_up <= '0';
		  moving_direction_down <= '0';
			door_open <= '0';
		end if;
  end process;
end behavior_1;
