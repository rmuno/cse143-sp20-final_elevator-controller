
-- Down counter to simulate motion (doors opening)
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
generic( N: integer );
port(
  reset_in: in std_logic;
  clk_in: in std_logic;

  count_out: out std_logic_vector(N-1 downto 0)
);
end counter;

architecture behavior_1 of counter is
	signal internal_count: std_logic_vector(N-1 downto 0) := (others=> '0');
begin
  process(reset_in, clk_in) is
		variable internal_count_next: std_logic_vector(N-1 downto 0) := (others => '0');
	begin
		-- approach from alternative machine problem 2 - PWM
		internal_count_next := std_logic_vector( unsigned(internal_count) + 1 );
		if (reset_in = '1') then
			-- reset counter
			internal_count <= (others => '0');
			count_out <= (others => '0');
		elsif (rising_edge(clk_in)) then
			-- incremenet counter
			internal_count <= internal_count_next;
			count_out <= internal_count_next;
		end if;
  end process;
end behavior_1;