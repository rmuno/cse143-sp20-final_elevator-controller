
-- Down counter to simulate motion (doors opening)
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity down_counter is
generic(
	N: integer
);
port(
  reset: in std_logic;
  clk: in std_logic;

  count: out std_logic_vector(N-1 downto 0)
);
end down_counter;

architecture behavior_1 of down_counter is
	signal internal_count: std_logic_vector(N-1 downto 0) := (others=> '1');
begin
  process(reset, clk) is
		variable internal_count_next: std_logic_vector(N-1 downto 0) := (others => '1');
	begin
		if (reset = '1') then
			-- reset counter
			internal_count <= (others => '1');
			internal_count_next := (others => '1');
			count <= (others => '1');
		else
			if (rising_edge(clk)) then
				-- reset internal clock
				if (internal_count = std_logic_vector(to_unsigned(0, N))) then
					internal_count_next := (others => '1');
				-- update internal clock
				else
					internal_count_next := std_logic_vector(unsigned(internal_count) - 1);
				end if;
				internal_count <= internal_count_next;
				count <= internal_count_next;
			end if;
		end if;
  end process;
end behavior_1;