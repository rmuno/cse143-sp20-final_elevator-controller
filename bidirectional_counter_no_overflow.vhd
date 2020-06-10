
-- Bidirectional counter with no overflow
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

-- bidirectional counter with no overflow
entity bidirectional_counter_no_overflow is
generic( N: integer );
port(
  reset_in: in std_logic;
  clk_in: in std_logic;
	
	-- up or down?
	count_up_in: in std_logic;

  count_out: out std_logic_vector(N-1 downto 0)
);
end bidirectional_counter_no_overflow;

architecture behavior_1 of bidirectional_counter_no_overflow is
	signal internal_count: std_logic_vector(N-1 downto 0) := (others=> '0');
begin
  process(reset_in, clk_in) is
		variable internal_count_next: std_logic_vector(N-1 downto 0) := (others => '0');
		-- helpers to prevent overflow
		constant all_ones: std_logic_vector(N-1 downto 0) := (others => '1');
		constant all_zeros: std_logic_vector(N-1 downto 0) := (others => '0');
	begin
		-- approach from alternative machine problem 2 - PWM
		if (count_up_in = '1') then
			internal_count_next := std_logic_vector( unsigned(internal_count) + 1 );
		else
			internal_count_next := std_logic_vector( unsigned(internal_count) - 1 );
		end if;

		if (reset_in = '1') then
			-- reset counter
			internal_count <= (others => '0');
			if (count_up_in = '1') then
				count_out <= (others => '0');
			else
				count_out <= (others => '1');
			end if;
		elsif (rising_edge(clk_in)) then
			-- prevent overflow to zero
			if ((count_up_in = '1' and internal_count = all_ones) or
					(count_up_in = '0' and internal_count = all_zeros)
			) then
				internal_count <= internal_count;
		 		count_out <= internal_count;
			else
				internal_count <= internal_count_next;
		 		count_out <= internal_count_next;
			end if;
		end if;
  end process;
end behavior_1;