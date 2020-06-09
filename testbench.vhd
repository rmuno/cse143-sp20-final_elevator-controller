
-- Testbench for elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity testbench is
	constant N_ctr : integer := 2;
end testbench; 

architecture tb of testbench is

-- elevator
component elevator_controller is
generic(
	CTR_SIZE : integer
);
port(
  reset_in: in std_logic;
  clk_in: in std_logic;

  floor_request_up_in: in std_logic;
  floor_request_down_in: in std_logic;

  current_floor_out: out std_logic;
  moving_direction_up_out: out std_logic;
  moving_direction_down_out: out std_logic;
  door_open_out: out std_logic
);
end component;

-- testbench helpers
signal reset: std_logic := '1';
signal clk: std_logic := '1';
--signal count_up: std_logic := '1';

--signal count : std_logic_vector(N_ctr-1 downto 0);
signal current_floor: std_logic;
signal moving_direction_up: std_logic;
signal moving_direction_down: std_logic;
signal door_open: std_logic;

signal floor_request_up: std_logic := '0';
signal floor_request_down: std_logic := '0';

constant CTR_SIZE : integer := 4;
--constant BOOP : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(8, CTR_SIZE));


--function clk_tick(clk: std_logic) return st

begin

  DUT: elevator_controller
		generic map(
			CTR_SIZE
		)
		port map(
			-- global inputs
	    reset, clk,
			-- elevator inputs
			floor_request_up, floor_request_down,

			-- elevator outputs
			current_floor,
			moving_direction_up,
			moving_direction_down,
			door_open
		);
--		DUT_COUNTER: counter
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
    wait for 1 ns;
    reset <= '0';

    clk <= not clk; wait for 5 ns;
		-- elevator should attempt to close door on reset
    for i in 1 to 3 loop
      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			assert (current_floor = '0') report "current_floor not zero" severity note;
			assert (moving_direction_up = '0') report "moving_direction_up not zero" severity note;
			assert (moving_direction_down = '0') report "moving_direction_down not zero" severity note;
			assert (door_open = '0') report "door_open not zero" severity note;
    end loop;

		-- straight-forward test: press button up
		-- simulate temporary button press
		floor_request_up <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
--		floor_request_up <= '0';
			floor_request_down <= '1';		
		-- elevator should move up for 8 cycles after initial button press
		for i in 1 to 8 loop

      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			assert (moving_direction_up = '1') report "elevator should be moving up!" severity error;
			assert (door_open = '0') report "Door should remain closed while in motion!" severity error;
    end loop;
--		floor_request_down <= '0';	

		
		-- elevator level should now be '1', and doors should begin to open
		-- door opening
		for i in 1 to 3 loop
      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			assert (current_floor = '1') report "Elevator current floor should be '1'!" severity error;
			assert (door_open = '0') report "Door should still be closed (but opening)" severity error;
    end loop;

		-- door open
		for i in 1 to 5 loop
      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			assert (door_open = '1') report "Door should be open!" severity error;
    end loop;

		-- door opening
		for i in 0 to (2) loop
      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			assert (door_open = '0') report "Door should be closing!" severity error;
    end loop;


--		floor_request_down <= '1';
      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
--		floor_request_down <= '0';

		for i in 0 to (25) loop
      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
    end loop;


    assert false report "elevator test complete" severity note;
    wait;
  end process;
end tb;
