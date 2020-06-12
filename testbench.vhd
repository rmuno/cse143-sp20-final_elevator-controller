
-- Testbench for elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity testbench is
	constant CTR_SIZE : integer := 6;
	constant DELAY_LEVEL_CHANGE: integer := 8;
	constant DELAY_PASSENGER_LOADING: integer := 5;
	constant DELAY_DOOR_OPENCLOSE: integer := 3;

	constant FLOOR_CTR_SIZE: integer := 2;
	constant FLOOR_MAX     : integer := 4;

	constant ZERO : std_logic := '0';
	constant ONE : std_logic := '1';

	constant RUN_TEST1 : STD_LOGIC := '1';
	constant RUN_TEST2 : STD_LOGIC := '0';
	constant RUN_TEST3 : STD_LOGIC := '0';
	constant RUN_TEST4 : STD_LOGIC := '0';
	constant RUN_TEST5 : STD_LOGIC := '0';

	constant CLK_DELAY : time := 5 ns;
end testbench; 

architecture tb of testbench is

-- elevator
component elevator_controller is
generic(
	CTR_SIZE : integer;
	DELAY_LEVEL_CHANGE: integer;
	DELAY_PASSENGER_LOADING: integer;
	DELAY_DOOR_OPENCLOSE: integer;

	FLOOR_CTR_SIZE: integer;
	FLOOR_MAX: integer
);
port(
  reset_in: in std_logic;
  clk_in: in std_logic;

  floor_request_up_in: in std_logic_vector(FLOOR_MAX-1 downto 0);
  floor_request_down_in: in std_logic_vector(FLOOR_MAX-1 downto 0);
	door_request_open_in: in std_logic;
	door_request_close_in: in std_logic;
	door_sensor_in: in std_logic;

  current_floor_out: out std_logic_vector(FLOOR_CTR_SIZE-1 downto 0);
  moving_direction_up_out: out std_logic;
  moving_direction_down_out: out std_logic;
  door_open_out: out std_logic;
  door_opening_out: out std_logic;
  door_closing_out: out std_logic
);
end component;

-- testbench helpers
signal reset: std_logic := '1';
signal clk: std_logic := '1';
--signal count_up: std_logic := '1';

--signal count : std_logic_vector(N_ctr-1 downto 0);
signal current_floor: std_logic_vector(FLOOR_CTR_SIZE-1 downto 0);
signal moving_direction_up: std_logic;
signal moving_direction_down: std_logic;
signal door_open: std_logic;
signal door_opening: std_logic;
signal door_closing: std_logic;

signal door_sensor: std_logic := '0';
signal floor_request_up: std_logic_vector(FLOOR_MAX-1 downto 0) := (others => '0');
signal floor_request_down: std_logic_vector(FLOOR_MAX-1 downto 0) := (others => '0');
signal door_request_open: std_logic := '0';
signal door_request_close: std_logic := '0';


-- helpers for managing clock cycles with simple known statements (e.g. "tick(clk, clk)"
-- instead of "clk <= not clk; wait for 5 ns; clk <= not clk; wait for 5 ns;")
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
	floor: std_logic_vector(FLOOR_CTR_SIZE-1 downto 0);
	door_is_open: std_logic; door_is_opening: std_logic; door_is_closing: std_logic
) is
begin
	assert (moving_direction_up = moving_up) report "moving_direction_up not " & std_logic'image(moving_up) severity error;
	assert (current_floor = floor) report "current_floor not " & integer'image(to_integer(unsigned(floor))) severity error;
	assert (moving_direction_down = moving_down) report "moving_direction_down not " & std_logic'image(moving_down) severity error;
	assert (door_open = door_is_open) report "door_open not " & std_logic'image(door_is_open) severity error;
	assert (door_is_opening = door_opening) report "door_opening not " & std_logic'image(door_is_opening) severity error;
	assert (door_is_closing = door_closing) report "door_closing not " & std_logic'image(door_is_closing) severity error;
end procedure;

subtype floor_size is std_logic_vector(FLOOR_CTR_SIZE-1 downto 0);

-- integer to std_logic_vector (for floor levels)
function i2lvf(level : integer)
	return floor_size is
begin
	return std_logic_vector(to_unsigned(level, FLOOR_CTR_SIZE));
end function;

procedure test_elevator_idle_single(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	tick(clkin, clkout);
	test_elevator_outputs(ZERO, ZERO, current_floor, ZERO, ZERO, ZERO);
end procedure;

procedure test_elevator_idle(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	for i in 1 to 5 loop
		tick(clkin, clkout);
		test_elevator_outputs(ZERO, ZERO, current_floor, ZERO, ZERO, ZERO);
	end loop;
end procedure;

procedure test_elevator_openclose(signal clkin : in std_logic; signal clkout : out std_logic; is_opening : std_logic) is
begin
	for i in 1 to DELAY_DOOR_OPENCLOSE loop
		tick(clkin, clkout);
		test_elevator_outputs(ZERO, ZERO, current_floor, ZERO, is_opening, not is_opening);
  end loop;
end procedure;

procedure test_elevator_passengerload(signal clkin : in std_logic; signal clkout : out std_logic;
	floor: integer) is
begin
	for i in 1 to DELAY_PASSENGER_LOADING loop
		tick(clkin, clkout);
		test_elevator_outputs(ZERO, ZERO, i2lvf(floor), ONE, ZERO, ZERO);
   end loop;
end procedure;

procedure test_elevator_open_load_close(signal clkin : in std_logic; signal clkout : out std_logic;
	floor: integer) is
begin
			-- door opening
		test_elevator_openclose(clkin, clkout, ONE);

		-- door open
		test_elevator_passengerload(clkin, clkout, floor);

		-- door closing
		test_elevator_openclose(clkin, clkout, ZERO);
end procedure;

procedure test_elevator_levelchange(signal clkin : in std_logic; signal clkout : out std_logic;
	level_current: floor_size; going_up: std_logic; should_keep_going: std_logic ) is
	variable level_next : floor_size;
begin
	if (going_up = '1') then
		level_next := std_logic_vector(unsigned(level_current) + 1);
	else
		level_next := std_logic_vector(unsigned(level_current) - 1);
	end if;

	for i in 1 to DELAY_LEVEL_CHANGE loop
			tick(clkin, clkout);
			test_elevator_outputs(going_up, not going_up, level_current, ZERO, ZERO, ZERO);
    end loop;
		-- floor reached
		tick(clkin, clkout);
		test_elevator_outputs(should_keep_going and going_up, should_keep_going and not going_up, level_next, ZERO, ZERO, ZERO);
end procedure;

procedure arbitrary_additional_clock_cycles(signal clkin : in std_logic; signal clkout : out std_logic) is
begin
	for i in 1 to 5 loop
		tick(clkin, clkout);
	end loop;
end procedure;

--procedure test_door_opening(
--	current_floor: std_logic
--) is
--begin
--	for i in 1 to DELAY_DOOR_OPENCLOSE loop
--		tick(clk, clk);
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
			DELAY_DOOR_OPENCLOSE,
			FLOOR_CTR_SIZE,
			FLOOR_MAX
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
			moving_direction_up, moving_direction_down,
			door_open, door_opening, door_closing
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
--		tick(clk, clk);
		-- elevator should attempt to close door on reset
		test_elevator_openclose(clk, clk, ZERO);

		-- straight-forward test: press button up
		-- simulate temporary button press
		floor_request_down(1) <= '1';
		tick(clk, clk);
		floor_request_down(1) <= '0';	

		-- elevator should move up for 8 cycles after initial button press
		test_elevator_levelchange(clk, clk, i2lvf(0), ONE, ZERO);
		
		-- elevator level should now be '1', and doors should begin to open
		-- door opening
		test_elevator_openclose(clk, clk, ONE);

		-- door open
		test_elevator_passengerload(clk, clk, 1);

		-- door closing
		test_elevator_openclose(clk, clk, ZERO);

		floor_request_up(0) <= '1';
		tick(clk, clk);
		floor_request_up(0) <= '0';

		test_elevator_levelchange(clk, clk, i2lvf(1), ZERO, ZERO);
		
		test_elevator_open_load_close(clk, clk, 0);

		-- test open elevator door by pressing the floor request button for current floor
		floor_request_up(0) <= '1';
		tick(clk, clk);
		test_elevator_openclose(clk, clk, ONE);
		for i in 1 to 3 loop
			test_elevator_passengerload(clk, clk, 0);
		end loop;
		floor_request_up(0) <= '0';
		tick(clk, clk);
--		test_elevator_passengerload(clk, clk, 0);
		test_elevator_openclose(clk, clk, ZERO);

		-- test moving elevator up several levels at once
		tick(clk, clk);
		floor_request_down(FLOOR_MAX-1) <= '1';
		tick(clk, clk);
		floor_request_down(FLOOR_MAX-1) <= '0';

		-- go to second to last floor
		for i in 1 to FLOOR_MAX-2 loop
			test_elevator_levelchange(clk, clk, i2lvf(i-1), ONE, ONE);
		end loop;
		-- go to last floor
		test_elevator_levelchange(clk, clk, i2lvf(FLOOR_MAX-2), ONE, ZERO);

		test_elevator_open_load_close(clk, clk, FLOOR_MAX-1);

		tick(clk, clk);
		floor_request_up(0) <= '1';
		tick(clk, clk);
		floor_request_up(0) <= '0';

		-- go to second to first floor
		for i in FLOOR_MAX-1 downto 2 loop
			test_elevator_levelchange(clk, clk, i2lvf(i), ZERO, ONE);
		end loop;
		-- go to first floor
		test_elevator_levelchange(clk, clk, i2lvf(1), ZERO, ZERO);

		test_elevator_open_load_close(clk, clk, 0);


		-- visit every level both ways
		floor_request_down(FLOOR_MAX-1 downto 1) <= (others => '1');
		tick(clk, clk);
		floor_request_down(FLOOR_MAX-1 downto 1) <= (others => '0');

		-- go to second to last floor
		for i in 1 to FLOOR_MAX-2 loop
			test_elevator_levelchange(clk, clk, i2lvf(i-1), ONE, ZERO);
			test_elevator_open_load_close(clk, clk, i);
			-- idle state after open/close
			test_elevator_idle_single(clk, clk);
		end loop;
		-- go to last floor
		test_elevator_levelchange(clk, clk, i2lvf(FLOOR_MAX-2), ONE, ZERO);
		test_elevator_open_load_close(clk, clk, FLOOR_MAX-1);

		tick(clk, clk);
		floor_request_up(FLOOR_MAX-2 downto 0) <= (others => '1');
		tick(clk, clk);
		floor_request_up(FLOOR_MAX-2 downto 0) <= (others => '0');

		-- go to second to first floor
		for i in FLOOR_MAX-1 downto 2 loop
			test_elevator_levelchange(clk, clk, i2lvf(i), ZERO, ZERO);
			test_elevator_open_load_close(clk, clk, i-1);
			test_elevator_idle_single(clk, clk);
		end loop;
		-- go to first floor
		test_elevator_levelchange(clk, clk, i2lvf(1), ZERO, ZERO);
		test_elevator_open_load_close(clk, clk, 0);

		-- *** go to last floor and back down to first floor, stopping at every level each way
		floor_request_down(FLOOR_MAX-1 downto 1) <= (others => '1');
		tick(clk, clk);
		floor_request_down(FLOOR_MAX-1 downto 1) <= (others => '0');

		floor_request_up(FLOOR_MAX-2 downto 0) <= (others => '1');
		tick(clk, clk);
		floor_request_up(FLOOR_MAX-2 downto 0) <= (others => '0');

		for i in 1 to DELAY_LEVEL_CHANGE-1 loop
			tick(clk, clk);
			test_elevator_outputs(ONE, ZERO, i2lvf(0), ZERO, ZERO, ZERO);
    end loop;
		-- floor reached
		tick(clk, clk);
		test_elevator_outputs(ZERO, ZERO, i2lvf(1), ZERO, ZERO, ZERO);

		test_elevator_open_load_close(clk, clk, 1);
		-- idle state after open/close
		test_elevator_idle_single(clk, clk);

		-- go to second to last floor
		for i in 2 to FLOOR_MAX-2 loop
			test_elevator_levelchange(clk, clk, i2lvf(i-1), ONE, ZERO);
			test_elevator_open_load_close(clk, clk, i);
			-- idle state after open/close
			test_elevator_idle_single(clk, clk);
		end loop;
		-- go to last floor
		test_elevator_levelchange(clk, clk, i2lvf(FLOOR_MAX-2), ONE, ZERO);
		test_elevator_open_load_close(clk, clk, FLOOR_MAX-1);

		-- go to second floor
--		for i in FLOOR_MAX-1 downto 2 loop
--			test_elevator_levelchange(clk, clk, i2lvf(i), ZERO, ZERO);
--			test_elevator_open_load_close(clk, clk, i-1);
--			test_elevator_idle_single(clk, clk);
--		end loop;
--		-- go to first floor
--		test_elevator_levelchange(clk, clk, i2lvf(1), ZERO, ZERO);
--		test_elevator_open_load_close(clk, clk, 0);

		-- do nothing
		test_elevator_idle(clk, clk);
		tick_half(clk, clk);
    assert false report "elevator test 1 (basic functionality) complete" severity note;
		-- end test 1
		end if;


		-- ****************** TEST #2 door_request_open
		if (RUN_TEST2 = '1') then
		-- Test 1. Open door, hold open & let close.
		tick_half(clk, clk);
--		assert false report "elevator should be idle" severity note;
		-- elevator should be idle
		test_elevator_idle(clk, clk);

--		assert false report "elevator should begin opening door" severity note;
		door_request_open <= '1';
		tick(clk, clk);
		door_request_open <= '0';
		
		-- elevator level should now be '0', and doors should begin to open
		-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ONE, ZERO);
    end loop;

		-- door open
--		assert false report "elevator door should be open" severity note;

		test_elevator_passengerload(clk, clk, 0);

    -- 2. Open door, press & hold "open" while door closing
		door_request_open <= '1';
		-- hold door open
		test_elevator_passengerload(clk, clk, 0);
		door_request_open <= '0';

		-- door closing
		test_elevator_openclose(clk, clk, ZERO);
	

		-- Test 2. press door open button at the last cycle of closing - should start to open door
		door_request_open <= '1';
		tick(clk, clk);
		door_request_open <= '0';
		test_elevator_openclose(clk, clk, ONE);
		-- load passengers
		test_elevator_passengerload(clk, clk, 0);
		-- close door
		test_elevator_openclose(clk, clk, ZERO);

		-- Test 3. open door & hold for some time, and door_open_request should interrupt closing state
		door_request_open <= '1';
		tick(clk, clk);
		test_elevator_openclose(clk, clk, ONE);

		for i in 1 to 3 loop
			test_elevator_passengerload(clk, clk, 0);
    end loop;
		door_request_open <= '0';

		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ZERO, ONE);
    end loop;

		door_request_open <= '1';
		tick(clk, clk);
		door_request_open <= '1';

		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ONE, ZERO);
    end loop;
		test_elevator_passengerload(clk, clk, 0);
		door_request_open <= '0';

		test_elevator_openclose(clk, clk, ZERO);

		-- Test 4. open button should do nothing while in motion (GOING UP)
		tick(clk, clk);
		floor_request_down(1) <= '1';
		tick(clk, clk);
		floor_request_down(1) <= '0';

		door_request_open <= '1';
		test_elevator_levelchange(clk, clk, i2lvf(0), ONE, ZERO);
		door_request_open <= '0';

		test_elevator_openclose(clk, clk, ONE);
		test_elevator_passengerload(clk, clk, 1);
		test_elevator_openclose(clk, clk, ZERO);

		-- Test 5. open button should do nothing while in motion (GOING DOWN)
		tick(clk, clk);
		floor_request_up(0) <= '1';
		tick(clk, clk);
		floor_request_up(0) <= '0';

		door_request_open <= '1';
		test_elevator_levelchange(clk, clk, i2lvf(1), ZERO, ZERO);
		door_request_open <= '0';
		
		test_elevator_openclose(clk, clk, ONE);
		test_elevator_passengerload(clk, clk, 0);
		test_elevator_openclose(clk, clk, ZERO);

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
--		assert false report "elevator should be idle" severity note;
		-- elevator should be idle
		test_elevator_idle(clk, clk);

--		assert false report "elevator should begin opening door" severity note;
		door_sensor <= '1';
		tick(clk, clk);
		door_sensor <= '0';
		
		-- elevator level should now be '0', and doors should begin to open
				-- door opening
		for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ONE, ZERO);
    end loop;

		-- door open
--		assert false report "elevator door should be open" severity note;
		test_elevator_passengerload(clk, clk, 0);

    -- 2. Open door, press & hold "open" before door closes
		door_sensor <= '1';
		-- hold door open
		test_elevator_passengerload(clk, clk, 0);
		door_sensor <= '0';

		-- door closing
		test_elevator_openclose(clk, clk, ZERO);

-- Test 2. press door open button at the last cycle of closing - should start to open door
		door_sensor <= '1';
		tick(clk, clk);
		door_sensor <= '0';
		test_elevator_openclose(clk, clk, ONE);
		-- load passengers
		test_elevator_passengerload(clk, clk, 0);
		-- close door
		test_elevator_openclose(clk, clk, ZERO);

		-- Test 3. open door & hold for some time, and door_open_request should interrupt closing state
		door_sensor <= '1';
		tick(clk, clk);
		test_elevator_openclose(clk, clk, ONE);
		for i in 1 to 3 loop
			test_elevator_passengerload(clk, clk, 0);
    end loop;
		door_sensor <= '0';
		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ZERO, ONE);
    end loop;

		door_sensor <= '1';
		tick(clk, clk);
		door_sensor <= '1';

		for i in 1 to DELAY_DOOR_OPENCLOSE-2 loop
			tick(clk, clk);
			test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ONE, ZERO);
    end loop;
		test_elevator_passengerload(clk, clk, 0);
		door_sensor <= '0';
		test_elevator_openclose(clk, clk, ZERO);

		-- Test 4. open button should do nothing while in motion (GOING UP)
		tick(clk, clk);
		floor_request_down(1) <= '1';
		tick(clk, clk);
		floor_request_down(1) <= '0';

		door_sensor <= '1';
		test_elevator_levelchange(clk, clk, i2lvf(0), ONE, ZERO);
		door_sensor <= '0';

		test_elevator_openclose(clk, clk, One);
		test_elevator_passengerload(clk, clk, 1);
		test_elevator_openclose(clk, clk, ZERO);


		-- Test 5. open button should do nothing while in motion (GOING DOWN)
		tick(clk, clk);
		floor_request_up(0) <= '1';
		tick(clk, clk);
		floor_request_up(0) <= '0';

		door_sensor <= '1';
		test_elevator_levelchange(clk, clk, i2lvf(1), ZERO, ZERO);
		door_sensor <= '0';
		
		test_elevator_openclose(clk, clk, ONE);
		test_elevator_passengerload(clk, clk, 0);
		test_elevator_openclose(clk, clk, ZERO);



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
	
	--		assert false report "elevator should begin opening door" severity note;
			door_request_open <= '1';
			tick(clk, clk);
			door_request_open <= '0';
			door_request_close <= '1';
			
			-- door_close should have no effect on door_opening state
			-- door opening
			for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ONE, ZERO);
	    end loop;

			-- release "close door" button before gate opens
--			tick(clk, clk);
--			door_request_close <= '0';
	
			-- door open
	--		assert false report "elevator door should be open for 1 cycle" severity note;
			for i in 1 to 1 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, i2lvf(0), ONE, ZERO, ZERO);
	    end loop;
	

			-- door should be closing
			test_elevator_openclose(clk, clk, ZERO);
			door_request_close <= '0';

			-- wait a few extra cycles
			arbitrary_additional_clock_cycles(clk, clk);

    assert false report "elevator test 4 (door_request_close) complete" severity note;
		-- test 4
		end if;

				-- ****************** TEST #4 - door_request_open, door_sensor and door_request_close: test conflicting cases
		-- using door_sensor
		if (RUN_TEST5 = '1') then
			-- Test 1. Open door & attempt to close
			tick_half(clk, clk);
	
	--		assert false report "elevator should begin opening door" severity note;
			door_request_open <= '1';
			tick(clk, clk);
			door_request_open <= '0';
			door_sensor <= '1';
			door_request_close <= '1';
			
			for i in 1 to DELAY_DOOR_OPENCLOSE-1 loop
				tick(clk, clk);
				test_elevator_outputs(ZERO, ZERO, i2lvf(0), ZERO, ONE, ZERO);
	    end loop;

			-- release "close door" button before gate opens
--			tick(clk, clk);
--			door_request_close <= '0';
	
			-- door_close should have no effect with door_open or door_sensor enabled
			for i in 1 to 3 loop
				test_elevator_passengerload(clk, clk, 0);
	    end loop;
			door_sensor <= '0';
			door_request_close <= '0';

			-- door should be closing
			test_elevator_openclose(clk, clk, ZERO);


			-- wait a few extra cycles
			arbitrary_additional_clock_cycles(clk, clk);

    assert false report "elevator test 5 (door_request_open, door_sensor and door_request_close) complete" severity note;
		-- test 5
		end if;

		wait;
  end process;
end tb;
