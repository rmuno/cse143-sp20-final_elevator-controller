
-- Testbench for elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity testbench is
	constant CTR_SIZE : integer := 6;
	constant DELAY_LEVEL_CHANGE: integer := 8;
	constant DELAY_PASSENGER_LOADING: integer := 5;
	constant DELAY_DOOR_OPENCLOSE: integer := 3;

	constant ZERO : std_logic := '0';
	constant ONE : std_logic := '1';

	constant RUN_TEST1 : STD_LOGIC := '1';
	constant RUN_TEST2 : STD_LOGIC := '1';
	constant RUN_TEST3 : STD_LOGIC := '1';
	constant RUN_TEST4 : STD_LOGIC := '1';
	constant RUN_TEST5 : STD_LOGIC := '1';

--	constant CLK_DELAY : integer := 5;
end testbench; 

architecture tb of testbench is

-- elevator
component elevator_controller is
generic(
	CTR_SIZE : integer;
	DELAY_LEVEL_CHANGE: integer;
	DELAY_PASSENGER_LOADING: integer;
	DELAY_DOOR_OPENCLOSE: integer
);
port(
  reset_in: in std_logic;
  clk_in: in std_logic;

  floor_request_up_in: in std_logic;
  floor_request_down_in: in std_logic;
	door_request_open_in: in std_logic;
	door_request_close_in: in std_logic;
	door_sensor_in: in std_logic;

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

signal door_sensor: std_logic := '0';
signal floor_request_up: std_logic := '0';
signal floor_request_down: std_logic := '0';
signal door_request_open: std_logic := '0';
signal door_request_close: std_logic := '0';


--signal test_ctr_count_out: std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');
--signal test_ctr_count_up: std_logic := '1';
--

--function clk_tick(clk: std_logic) return st

-- :(
--impure function test_door_open return integer is
--begin
---- door opening
--		for i in 1 to DELAY_DOOR_OPENCLOSE loop
--      clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
--			assert (door_open = '0') report "Door should be closing!" severity error;
--    end loop;
--end function;

-- procedure help source: https://www.nandland.com/vhdl/examples/example-procedure-simple.html
procedure test_elevator_outputs(
	moving_up: std_logic; moving_down: std_logic;
	floor: std_logic; door_is_open: std_logic
) is
begin
	assert (moving_direction_up = moving_up) report "moving_direction_up not " & std_logic'image(moving_up) severity error;
	assert (current_floor = floor) report "current_floor not " & std_logic'image(floor) severity error;
	assert (moving_direction_down = moving_down) report "moving_direction_down not " & std_logic'image(moving_down) severity error;
	assert (door_open = door_is_open) report "door_open not " & std_logic'image(door_is_open) severity error;
end procedure;


--procedure clk_half is
--begin
--	clk <= not clk; wait for 5 ns;
--end procedure;
--
--procedure clk_tick is
--begin
--	clk <= not clk; wait for 5 ns;
--	clk <= not clk; wait for 5 ns;
--end procedure;

--procedure test_door_opening(
--	current_floor: std_logic
--) is
--begin
--	for i in 1 to DELAY_DOOR_OPENCLOSE loop
--		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
--		test_elevator_outputs(ZERO, ZERO, current_floor, ZERO);
--  end loop;
--end procedure;


begin
	
--	dut_counter : bidirectional_counter_no_overflow
--	generic map( CTR_SIZE )
--	port map(
--	  reset, clk,
--		
--		-- up or down?
--		test_ctr_count_up,
--	
--	  test_ctr_count_out
--	);
--

  DUT: elevator_controller
		generic map(
			CTR_SIZE,
			DELAY_LEVEL_CHANGE,
			DELAY_PASSENGER_LOADING,
			DELAY_DOOR_OPENCLOSE
		)
		port map(
			-- global inputs
	    reset, clk,
			-- elevator inputs
			floor_request_up, floor_request_down,
			door_request_open, door_request_close,
			door_sensor,

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

		-- ****************** TEST #1 - basic functionality: reset, go up, go down
		if (RUN_TEST1 = '1') then
		clk <= not clk; wait for 5 ns;
		-- elevator should attempt to close door on reset
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, current_floor, ZERO);
	  end loop;

		-- straight-forward test: press button up
		-- simulate temporary button press
		floor_request_up <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_up <= '0';	

		-- elevator should move up for 8 cycles after initial button press
		for i in 1 to DELAY_LEVEL_CHANGE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ONE, ZERO, ZERO, ZERO);
    end loop;
		-- floor reached
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
		
		-- elevator level should now be '1', and doors should begin to open
		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
    end loop;

		-- door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ONE);
    end loop;

		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
    end loop;

		floor_request_down <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_down <= '0';

		for i in 1 to DELAY_LEVEL_CHANGE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ONE, ONE, ZERO);
    end loop;
		-- floor reached
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);

		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;

		-- door closing
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- do nothing
		for i in 1 to 5 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
		end loop;
		clk <= not clk; wait for 5 ns;
    assert false report "elevator test 1 (basic functionality) complete" severity note;
		-- end test 1
		end if;


		-- ****************** TEST #2 door_request_open
		if (RUN_TEST2 = '1') then
		-- Test 1. Open door, hold open & let close.
		clk <= not clk; wait for 5 ns;
--		assert false report "elevator should be idle" severity note;
		-- elevator should be idle
		for i in 1 to DELAY_DOOR_OPENCLOSE+1 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	  end loop;

--		assert false report "elevator should begin opening door" severity note;
		door_request_open <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		door_request_open <= '0';
		
		-- elevator level should now be '0', and doors should begin to open
		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- door open
--		assert false report "elevator door should be open" severity note;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;

    -- 2. Open door, press & hold "open" while door closing
		door_request_open <= '1';
		-- hold door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_request_open <= '0';

		-- door closing
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		end loop;
	

		-- Test 2. press door open button at the last cycle of closing - should start to open door
		door_request_open <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		door_request_open <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		-- load passengers
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		-- close door
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- Test 3. open door & hold for some time, and door_open_request should interrupt closing state
		door_request_open <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING*3 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_request_open <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		door_request_open <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		door_request_open <= '1';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_request_open <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- Test 4. open button should do nothing while in motion (GOING UP)
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_up <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_up <= '0';

		door_request_open <= '1';
		for i in 1 to DELAY_LEVEL_CHANGE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ONE, ZERO, ZERO, ZERO);
    end loop;
		-- floor reached
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
		door_request_open <= '0';

		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ONE);
    end loop;
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
    end loop;


		-- Test 5. open button should do nothing while in motion (GOING DOWN)
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_down <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_down <= '0';

		door_request_open <= '1';
		for i in 1 to DELAY_LEVEL_CHANGE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ONE, ONE, ZERO);
    end loop;
		-- floor reached
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
		door_request_open <= '0';
		
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- arbitrary additional clock cycles
		for i in 1 to 5 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		end loop;

		clk <= not clk; wait for 5 ns;

    assert false report "elevator test 2 (door_request_open) complete" severity note;
		-- end test 2
		end if;
 

		-- ****************** TEST #3 - door_sensor: this is a repeat of test #2 for door_request_open, but
		-- using door_sensor
		if (RUN_TEST3 = '1') then
		-- Test 1. Open door, hold open & let close.
		clk <= not clk; wait for 5 ns;
--		assert false report "elevator should be idle" severity note;
		-- elevator should be idle
		for i in 1 to DELAY_DOOR_OPENCLOSE+1 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	  end loop;

--		assert false report "elevator should begin opening door" severity note;
		door_sensor <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		door_sensor <= '0';
		
		-- elevator level should now be '0', and doors should begin to open
				-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- door open
--		assert false report "elevator door should be open" severity note;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;

    -- 2. Open door, press & hold "open" while door closing
		door_sensor <= '1';
		-- hold door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_sensor <= '0';

		-- door closing
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		end loop;

-- Test 2. press door open button at the last cycle of closing - should start to open door
		door_sensor <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		door_sensor <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		-- load passengers
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		-- close door
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- Test 3. open door & hold for some time, and door_open_request should interrupt closing state
		door_sensor <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING*3 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_sensor <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		door_sensor <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		door_sensor <= '1';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_sensor <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- Test 4. open button should do nothing while in motion (GOING UP)
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_up <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_up <= '0';

		door_sensor <= '1';
		for i in 1 to DELAY_LEVEL_CHANGE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ONE, ZERO, ZERO, ZERO);
    end loop;
		-- floor reached
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
		door_sensor <= '0';

		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ONE);
    end loop;
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ONE, ZERO);
    end loop;


		-- Test 5. open button should do nothing while in motion (GOING DOWN)
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_down <= '1';
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		floor_request_down <= '0';

		door_sensor <= '1';
		for i in 1 to DELAY_LEVEL_CHANGE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ONE, ONE, ZERO);
    end loop;
		-- floor reached
		clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
		door_sensor <= '0';
		
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		for i in 1 to DELAY_DOOR_OPENCLOSE loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;



		-- arbitrary additional clock cycles
		for i in 1 to 5 loop
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
		end loop;

    assert false report "elevator test 3 (door_sensor) complete" severity note;
		-- end test 3
		end if;
 
   
		
		-- ****************** TEST #4 - door_request_close: test the "close doors" button
		-- using door_sensor
		if (RUN_TEST4 = '1') then
			-- Test 1. Open door & attempt to close
			clk <= not clk; wait for 5 ns;
	
	--		assert false report "elevator should begin opening door" severity note;
			door_request_open <= '1';
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			door_request_open <= '0';
			door_request_close <= '1';
			
			-- door_close should have no effect on door_opening state
			-- door opening
			for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
				test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	    end loop;

			-- release "close door" button before gate opens
--			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
--			door_request_close <= '0';
	
			-- door open
	--		assert false report "elevator door should be open for 1 cycle" severity note;
			for i in 1 to 1 loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
				test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
	    end loop;
	

			-- door should be closing
			for i in 1 to DELAY_DOOR_OPENCLOSE loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
				test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	    end loop;
			door_request_close <= '0';

			-- wait a few extra cycles
			for i in 1 to 5 loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			end loop;

    assert false report "elevator test 4 (door_request_close) complete" severity note;
		-- test 4
		end if;

				-- ****************** TEST #4 - door_request_open, door_sensor and door_request_close: test conflicting cases
		-- using door_sensor
		if (RUN_TEST5 = '1') then
			-- Test 1. Open door & attempt to close
			clk <= not clk; wait for 5 ns;
	
	--		assert false report "elevator should begin opening door" severity note;
			door_request_open <= '1';
			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			door_request_open <= '0';
			door_sensor <= '1';
			door_request_close <= '1';
			
			for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
				test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	    end loop;

			-- release "close door" button before gate opens
--			clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
--			door_request_close <= '0';
	
			-- door_close should have no effect with door_open or door_sensor enabled
			for i in 1 to DELAY_PASSENGER_LOADING*3 loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
				test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
	    end loop;
			door_sensor <= '0';
			door_request_close <= '0';

			-- door should be closing
			for i in 1 to DELAY_DOOR_OPENCLOSE loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
				test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	    end loop;


			-- wait a few extra cycles
			for i in 1 to 5 loop
				clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;
			end loop;

    assert false report "elevator test 5 (door_request_open, door_sensor and door_request_close) complete" severity note;
		-- test 5
		end if;

		wait;
  end process;
end tb;
