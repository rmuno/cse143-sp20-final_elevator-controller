
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

	constant CLK_DELAY : time := 5 ns;
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

signal current_floor: std_logic;
signal moving_direction_up: std_logic;
signal moving_direction_down: std_logic;
signal door_open: std_logic;

signal door_sensor: std_logic := '0';
signal floor_request_up: std_logic := '0';
signal floor_request_down: std_logic := '0';
signal door_request_open: std_logic := '0';
signal door_request_close: std_logic := '0';


-- helpers for managing clock cycles with simple known statements (e.g. "tick(clk, clk)"
-- instead of "tick_half(clk, clk); clk <= not clk; wait for 5 ns")
procedure tick_half(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	clkout <= not clkin; wait for CLK_DELAY;
end procedure;
procedure tick(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	tick_half(clkin, clkout);
	tick_half(clkin, clkout);
end procedure;

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


procedure test_elevator_openclose(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	for i in 1 to DELAY_DOOR_OPENCLOSE loop
		tick(clkin, clkout);
		test_elevator_outputs(ZERO, ZERO, current_floor, ZERO);
  end loop;
end procedure;

procedure test_elevator_levelchange(signal clkin : in std_logic; signal clkout : out std_logic;
	going_up: std_logic) is
	variable level_current : std_logic;
	variable level_next : std_logic;
begin
	if (going_up = '1') then
		level_current := '0';
		level_next := '1';
	else
		level_current := '1';
		level_next := '0';
	end if;

	for i in 1 to DELAY_LEVEL_CHANGE loop
			tick(clkin, clkout);
			test_elevator_outputs(going_up, not going_up, level_current, ZERO);
    end loop;
		-- floor reached
		tick(clkin, clkout);
		test_elevator_outputs(ZERO, ZERO, level_next, ZERO);
end procedure;

procedure arbitrary_additional_clock_cycles(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	for i in 1 to 5 loop
		tick(clkin, clkout);
	end loop;
end procedure;


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
		tick_half(clk, clk);
		-- elevator should attempt to close door on reset
		test_elevator_openclose(clk, clk);

		-- straight-forward test: press button up
		-- simulate temporary button press
		floor_request_up <= '1';
		tick(clk, clk);
		floor_request_up <= '0';	

		-- elevator should move up for 8 cycles after initial button press
		test_elevator_levelchange(clk, clk, ONE);
		
		-- elevator level should now be '1', and doors should begin to open
		-- door opening
		test_elevator_openclose(clk, clk);

		-- door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ONE, ONE);
    end loop;

		-- door opening
		test_elevator_openclose(clk, clk);

		floor_request_down <= '1';
		tick(clk, clk);
		floor_request_down <= '0';

		test_elevator_levelchange(clk, clk, ZERO);

		-- door opening
		test_elevator_openclose(clk, clk);

		-- door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;

		-- door closing
		test_elevator_openclose(clk, clk);

		-- do nothing
		for i in 1 to 5 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
		end loop;
		tick_half(clk, clk);
    assert false report "elevator test 1 (basic functionality) complete" severity note;
		-- end test 1
		end if;


		-- ****************** TEST #2 door_request_open
		if (RUN_TEST2 = '1') then
		-- Test 1. Open door, hold open & let close.
		tick_half(clk, clk);
    
		-- elevator should be idle
		for i in 1 to DELAY_DOOR_OPENCLOSE+1 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	  end loop;

		door_request_open <= '1';
		tick(clk, clk);
		door_request_open <= '0';
		
		-- elevator level should now be '0', and doors should begin to open
		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;

    -- 2. Open door, press & hold "open" while door closing
		door_request_open <= '1';
		-- hold door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_request_open <= '0';

		-- door closing
		test_elevator_openclose(clk, clk);
	

		-- Test 2. press door open button at the last cycle of closing - should start to open door
		door_request_open <= '1';
		tick(clk, clk);
		door_request_open <= '0';
		test_elevator_openclose(clk, clk);
		-- load passengers
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		-- close door
		test_elevator_openclose(clk, clk);

		-- Test 3. open door & hold for some time, and door_open_request should interrupt closing state
		door_request_open <= '1';
		tick(clk, clk);
		test_elevator_openclose(clk, clk);
    
    -- door should remain open
		for i in 1 to DELAY_PASSENGER_LOADING*3 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_request_open <= '0';
    -- door should now begin to close
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
    -- but right before it closes completely, let's open it again
		door_request_open <= '1';
		tick(clk, clk);
		door_request_open <= '1';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
    -- and finally let it close
		door_request_open <= '0';

		test_elevator_openclose(clk, clk);

		-- Test 4. open button should do nothing while in motion (GOING UP)
		tick(clk, clk);
		floor_request_up <= '1';
		tick(clk, clk);
		floor_request_up <= '0';

		door_request_open <= '1';
		test_elevator_levelchange(clk, clk, ONE);
		door_request_open <= '0';

		test_elevator_openclose(clk, clk);
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ONE, ONE);
    end loop;
		test_elevator_openclose(clk, clk);

		-- Test 5. open button should do nothing while in motion (GOING DOWN)
		tick(clk, clk);
		floor_request_down <= '1';
		tick(clk, clk);
		floor_request_down <= '0';

		door_request_open <= '1';
		test_elevator_levelchange(clk, clk, ZERO);
		door_request_open <= '0';
		
		test_elevator_openclose(clk, clk);
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		test_elevator_openclose(clk, clk);

		-- arbitrary additional clock cycles
		arbitrary_additional_clock_cycles(clk, clk);

		tick_half(clk, clk);

    assert false report "elevator test 2 (door_request_open) complete" severity note;
		-- end test 2
		end if;
 

		-- ****************** TEST #3 - door_sensor: this is a repeat of test #2 for door_request_open, but
		-- using door_sensor
		if (RUN_TEST3 = '1') then
		-- Test 1. Open door, hold open & let close.
		tick_half(clk, clk);
		-- elevator should be idle
		for i in 1 to DELAY_DOOR_OPENCLOSE+1 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	  end loop;

		door_sensor <= '1';
		tick(clk, clk);
		door_sensor <= '0';
		
		-- elevator level should now be '0', and doors should begin to open
		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;

		-- door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;

    -- 2. Open door, press & hold "open" while door closing
		door_sensor <= '1';
		-- hold door open
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_sensor <= '0';

		-- door closing
		test_elevator_openclose(clk, clk);

    -- Test 2. press door open button at the last cycle of closing - should start to open door
		door_sensor <= '1';
		tick(clk, clk);
		door_sensor <= '0';
		test_elevator_openclose(clk, clk);
		-- load passengers
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		-- close door
		test_elevator_openclose(clk, clk);

		-- Test 3. open door & hold for some time, and door_open_request should interrupt closing state
		door_sensor <= '1';
		tick(clk, clk);
		test_elevator_openclose(clk, clk);
		for i in 1 to DELAY_PASSENGER_LOADING*3 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_sensor <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		door_sensor <= '1';
		tick(clk, clk);
		door_sensor <= '1';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
    end loop;
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		door_sensor <= '0';
		test_elevator_openclose(clk, clk);

		-- Test 4. open button should do nothing while in motion (GOING UP)
		tick(clk, clk);
		floor_request_up <= '1';
		tick(clk, clk);
		floor_request_up <= '0';

		door_sensor <= '1';
		test_elevator_levelchange(clk, clk, ONE);
		door_sensor <= '0';

		test_elevator_openclose(clk, clk);
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ONE, ONE);
    end loop;
		test_elevator_openclose(clk, clk);


		-- Test 5. open button should do nothing while in motion (GOING DOWN)
		tick(clk, clk);
		floor_request_down <= '1';
		tick(clk, clk);
		floor_request_down <= '0';

		door_sensor <= '1';
		test_elevator_levelchange(clk, clk, ZERO);
		door_sensor <= '0';
		
		test_elevator_openclose(clk, clk);
		for i in 1 to DELAY_PASSENGER_LOADING loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
    end loop;
		test_elevator_openclose(clk, clk);



		-- arbitrary additional clock cycles
		arbitrary_additional_clock_cycles(clk, clk);

    assert false report "elevator test 3 (door_sensor) complete" severity note;
		-- end test 3
		end if;
 
   
		
		-- ****************** TEST #4 - door_request_close: test the "close doors" button
		-- using door_sensor
		if (RUN_TEST4 = '1') then
			-- Test 1. Open door & attempt to close
			tick_half(clk, clk);
	
			door_request_open <= '1';
			tick(clk, clk);
			door_request_open <= '0';
			door_request_close <= '1';
			
			-- door_close should have no effect on door_opening state
			-- door opening
			for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	    end loop;
	
			-- door open
			for i in 1 to 1 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
	    end loop;
	

			-- door should be closing
			test_elevator_openclose(clk, clk);
			door_request_close <= '0';

			-- wait a few extra cycles
			arbitrary_additional_clock_cycles(clk, clk);

    assert false report "elevator test 4 (door_request_close) complete" severity note;
		-- test 4
		end if;

		-- ****************** TEST #4 - door_request_open, door_sensor and door_request_close: test conflicting cases
		if (RUN_TEST5 = '1') then
			-- Test 1. Open door & attempt to close
			tick_half(clk, clk);

			door_request_open <= '1';
			tick(clk, clk);
			door_request_open <= '0';
			door_sensor <= '1';
			door_request_close <= '1';
			
			for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, ZERO, ZERO);
	    end loop;

	
			-- door_close should have no effect with door_open or door_sensor enabled
			for i in 1 to DELAY_PASSENGER_LOADING*3 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, ZERO, ONE);
	    end loop;
			door_sensor <= '0';
			door_request_close <= '0';

			-- door should be closing
			test_elevator_openclose(clk, clk);


			-- wait a few extra cycles
			arbitrary_additional_clock_cycles(clk, clk);

    assert false report "elevator test 5 (door_request_open, door_sensor and door_request_close) complete" severity note;
		-- test 5
		end if;

		wait;
  end process;
end tb;
