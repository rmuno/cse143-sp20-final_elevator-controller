
-- simple 2:1 mux to replace certain if/else statements
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity mux_2_1 is
generic( N: integer );
port(
	a_in: in std_logic;
	b_in: in std_logic;
	sel_in: in std_logic;
  sel_out: out std_logic
);
end mux_2_1;

architecture behavior_1 of mux_2_1 is
begin
  process(sel_in) is
	begin
		if (sel_in = '0') then
			sel_out <= a_in;
		else
			sel_out <= b_in;
		end if;
  end process;
end behavior_1;
