
-- elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

-- single elevator controller for 2-levels
entity elevator_controller is
generic(
	CTR_SIZE : integer;
	DELAY_LEVEL_CHANGE: integer;
	DELAY_PASSENGER_LOADING: integer;
	DELAY_DOOR_OPENCLOSE: integer;

	--
	FLOOR_CTR_SIZE: integer;
	FLOOR_MAX: integer
);
port(
  reset_in: in std_logic;
  clk_in: in std_logic;

  floor_request_up_in: in std_logic_vector(FLOOR_MAX-1 downto 0);
  floor_request_down_in: in std_logic_vector(FLOOR_MAX-1 downto 0);

	-- request keep door open
	door_request_open_in: std_logic;
	-- request door close
	door_request_close_in: std_logic;
	-- additional step: door sensor to re-open door
	door_sensor_in: in std_logic;

  current_floor_out: out std_logic_vector(FLOOR_CTR_SIZE-1 downto 0);
  moving_direction_up_out: out std_logic;
  moving_direction_down_out: out std_logic;
  door_open_out: out std_logic;
  door_opening_out: out std_logic;
  door_closing_out: out std_logic
);

constant LEVEL_CHANGE_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(DELAY_LEVEL_CHANGE, CTR_SIZE));
constant PASSENGER_LOADING_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(DELAY_PASSENGER_LOADING, CTR_SIZE));
constant DOOR_OPENCLOSE_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(DELAY_DOOR_OPENCLOSE, CTR_SIZE));
constant ALL_ZEROES : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(0, CTR_SIZE));

constant LEVEL_REACHED_CTR_OFFSET : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(1, CTR_SIZE));

constant FLOOR_FIRST : std_logic_vector(FLOOR_CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(0, FLOOR_CTR_SIZE));
constant FLOOR_LAST : std_logic_vector(FLOOR_CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(FLOOR_MAX-1, FLOOR_CTR_SIZE));
--constant ALL_ONES_2 : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');
--constant ALL_ONES_3 : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');

end elevator_controller;

architecture behavior_1 of elevator_controller is
	type ELEVATOR_STATE is (
		S_IDLE,
		S_DOOR_OPENING, S_DOOR_OPEN, S_DOOR_CLOSING, S_DOOR_REOPEN_INTERMEDIATE,
		S_MOVING_UP, S_MOVING_DOWN, S_FLOOR_REACHED
	);

	signal e_state: ELEVATOR_STATE;

	component counter is
	generic( N: integer );
	port(
	  reset_in: in std_logic;
	  clk_in: in std_logic;
	  count_out: out std_logic_vector(N-1 downto 0)
	);
	end component;

	component bidirectional_counter_no_overflow is
	generic( N: integer );
	port(
	  reset_in: in std_logic;
	  clk_in: in std_logic;
		
		-- up or down?
		count_up_in: in std_logic;
	
	  count_out: out std_logic_vector(N-1 downto 0)
	);
	end component;

  signal current_floor: std_logic_vector(FLOOR_CTR_SIZE-1 downto 0) := (others => '0');
	signal current_floor_one_hot : std_logic_vector(FLOOR_MAX-1 downto 0) := (others => '0');
  signal moving_direction_up: std_logic := '0';
  signal moving_direction_down: std_logic := '0';
  signal door_open: std_logic := '0';
	signal floor_reached: std_logic := '0';

	signal door_opening: std_logic := '0';
	signal door_closing: std_logic := '0';

	signal any_request_up_above: std_logic := '0';
	signal any_request_up_below: std_logic := '0';
	signal any_request_down_below: std_logic := '0';
	signal any_request_down_above: std_logic := '0';

	signal traveling_up: std_logic := '0';
	signal traveling_down: std_logic := '0';

	-- internals

	-- hold input requests - users do not generally hold the button down until elevator arrives!
	signal floor_request_up: std_logic_vector(FLOOR_MAX-1 downto 0) := (others => '0');
	signal floor_request_down: std_logic_vector(FLOOR_MAX-1 downto 0) := (others => '0');

	signal current_level_floor_up: std_logic := '0';
	signal current_level_floor_down: std_logic := '0';

	-- door open/close helper
	signal clk_in_door_open_close: std_logic := '0';
	signal reset_in_door_open_close: std_logic := '0';
--	signal direction_open_close: std_logic := '1';
	signal ctr_door_open_close : std_logic_vector(CTR_SIZE-1 downto 0);

	signal door_delay_offset: std_logic_vector(CTR_SIZE-1 downto 0);
	-- to offset motion counter since it will be reset on the "floor reached" state but elevator will still be in motion
	-- e.g. when floor 3 is requested but elevator is at 0, it will "skip" past 1 and 2 but will hit the "floor reached" state every
	-- level
	signal level_reached_motion_offset : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '0');

	-- door passenger loading helper
	signal clk_in_door_passenger_loading: std_logic := '0';
	signal reset_in_door_passenger_loading: std_logic := '0';
	signal ctr_door_passenger_loading: std_logic_vector(CTR_SIZE-1 downto 0);

	-- elevator cabin motion helper
	signal clk_in_moving: std_logic := '0';
	signal reset_in_moving: std_logic := '0';
	signal ctr_moving : std_logic_vector(CTR_SIZE-1 downto 0);

	function bool_to_bit(condition_result : boolean) return std_logic is
	begin
		if (condition_result) then
			return '1';
		else
			return '0';
		end if;
	end function;
begin

	-- **** COMPONENTS
	-- counter for opening / closing the door
	CTR_CMP_DOOR_OPEN_CLOSE : counter --bidirectional_counter_no_overflow
		generic map(CTR_SIZE)
		port map(reset_in_door_open_close,
			clk_in_door_open_close,
--			direction_open_close,
			ctr_door_open_close);

	-- counter to waiting for passengers to enter/leave elevator
	CTR_CMP_PASSENGER_LOADING : counter
		generic map(CTR_SIZE)
		port map(reset_in_door_passenger_loading, clk_in_door_passenger_loading, ctr_door_passenger_loading);

	-- counter for moving the elevator
	CTR_LEVEL_TRANSITION : counter
		generic map(CTR_SIZE)
		port map(reset_in_moving, clk_in_moving, ctr_moving);
	
	-- **** PROCESSES

	TRAVEL_DIRECTION: process(current_floor,
		any_request_up_above,
		any_request_up_below,

		any_request_down_below,
		any_request_down_above,

		floor_reached) is
	begin
		-- update traveling_up and traveling_down
		if (traveling_up = '0' and traveling_down = '0') then
			if (any_request_up_above = '1' or any_request_up_below = '1') then
				traveling_up <= '1';
			elsif (any_request_down_below = '1' or any_request_down_above = '1') then
				traveling_down <= '1';
			end if;
		else
			-- keep traveling up until no requests in that direction
			if ((any_request_up_above = '0' and any_request_up_below = '0') or
					(traveling_up = '1' and any_request_up_above = '0')) then
				traveling_up <= '0';
				-- switch direction if needed
				if (any_request_down_below = '1' or any_request_down_above = '1') then
					traveling_down <= '1';
				end if;
			end if;

			-- keep traveling down until no requests in that direction
			if ((any_request_down_above = '0' and any_request_down_below = '0') or
						(traveling_down = '1' and any_request_down_below = '0')) then
				traveling_down <= '0';
				-- switch direction if needed
				if (any_request_up_above = '1' or any_request_up_below = '1') then
					traveling_up <= '1';
				end if;
			end if;
		end if;
	end process;

	ANY_UP_OR_DOWN: process(floor_request_up, floor_request_down, current_floor) is
		variable any_up_above : std_logic := '0';
    variable any_up_below : std_logic := '0';
		variable any_down_above : std_logic := '0';
    variable any_down_below : std_logic := '0';
		variable current_floor_int : integer;
	begin
		current_floor_int := to_integer(unsigned(current_floor));

		any_up_above := '0';
    any_up_below := '0';
		any_down_above := '0';
    any_down_below := '0';
		for i in 0 to FLOOR_MAX-1 loop
			-- find requests above
      any_up_above := any_up_above or (bool_to_bit(i > current_floor_int) and floor_request_up(i));
      any_up_below := any_up_below or (bool_to_bit(i > current_floor_int) and floor_request_down(i));

			-- find requests below
      any_down_above := any_down_above or (bool_to_bit(i < current_floor_int) and floor_request_up(i));
      any_down_below := any_down_below or (bool_to_bit(i < current_floor_int) and floor_request_down(i));
		end loop;

		any_request_up_above <= any_up_above;
		any_request_up_below <= any_up_below;
		any_request_down_below <= any_down_below;
		any_request_down_above <= any_down_above;
	end process;


	-- input-retention process
	INPUTS: process(reset_in, floor_request_up_in, floor_request_down_in, current_floor, door_opening, door_open,
		traveling_up, traveling_down, floor_reached) is

--		variable current_floor_one_hot_base : std_logic_vector(FLOOR_MAX-1 downto 0);
		variable up_requests_without_current_floor : std_logic_vector(FLOOR_MAX-1 downto 0);
		variable down_requests_without_current_floor : std_logic_vector(FLOOR_MAX-1 downto 0);
		variable current_floor_int : integer;
	begin
		current_floor_int := to_integer(unsigned(current_floor));

		current_floor_one_hot <= (others => '0');
		current_floor_one_hot(current_floor_int) <= '1';

		up_requests_without_current_floor := (floor_request_up or floor_request_up_in) and (not current_floor_one_hot);
		down_requests_without_current_floor := (floor_request_down or floor_request_down_in) and (not current_floor_one_hot);

		if (reset_in = '1') then
			floor_request_up <= (others => '0');
			floor_request_down <= (others => '0');
--			any_request_up_above <= '0';
--			any_request_down_below <= '0';

		-- disable floor request when door opens, but allow floor requests:
		-- 1. while door is open, to hold it open
		-- 2. while door closes, to open it again
		else
				-- mark any_request_up_above
				-- approach idea thanks to solution #2 from the folowing SO post:
				-- https://stackoverflow.com/a/20300928

				-- mark any_request_down_below
--				any_request_down_below <= '0';
--				for i in 0 to FLOOR_MAX-1 loop
--					if (i < current_floor_int and (floor_request_up_in(i) = '1' or floor_request_down_in(i) = '1')) then
--						any_request_down_below <= any_request_down_below or '1';
--					end if;
--					--any_down := any_down or (i > current_floor_int and floor_request_up_in(i) = '1');
--				end loop;
--				any_request_down_below <= any_down;

				-- clear current floor level UP request if the one-hot index is disabled

				
				if ((door_open = '1' or door_opening = '1') and
						(
							(traveling_up = '0' and traveling_down = '0' and
								(floor_request_up(current_floor_int) = '1' or floor_request_down(current_floor_int) = '1')
						 )
						or
						 (floor_request_up(current_floor_int) = '1')
						)
					 ) then
					floor_request_up <= up_requests_without_current_floor;
				else
					floor_request_up <= floor_request_up or floor_request_up_in;
				end if;

				-- clear current floor level DOWN request if the one-hot index is disabled
--				if (door_opening = '1' and floor_request_down(current_floor_int) = '1') then
				if ((door_open = '1' or door_opening = '1') and --floor_request_down(current_floor_int) = '1') then
						(
							(traveling_up = '0' and traveling_down = '0' and
								(floor_request_up(current_floor_int) = '1' or floor_request_down(current_floor_int) = '1')
						 )
						or
						 (floor_request_down(current_floor_int) = '1')
						)
					 ) then
					floor_request_down <= down_requests_without_current_floor;
				else
					floor_request_down <= floor_request_down or floor_request_down_in;
				end if;
--			floor_request_up <= up_requests_without_current_floor;
--			floor_request_down <= down_requests_without_current_floor;
			-- "up" button
--			if (current_floor = '1' and door_opening = '1') then
--				floor_request_up <= '0';
--			elsif (floor_request_up_in = '1') then
--				floor_request_up <= floor_request_up or floor_request_up_in;
----				floor_request_up <= '1';
--			end if;
--
			-- "down" button
--			if (current_floor = '0' and door_opening = '1') then
--				floor_request_down <= '0';
--			elsif (floor_request_down_in = '1') then
--				floor_request_down <= '1';
--			end if;
		end if;
	end process;


	OUTPUTS: process(clk_in, reset_in,
		current_floor, moving_direction_up, moving_direction_down, door_open, door_opening, door_closing) is
	begin
    current_floor_out <= current_floor;
    moving_direction_up_out <= moving_direction_up;
    moving_direction_down_out <= moving_direction_down;
    door_open_out <= door_open;
	  door_opening_out <= door_opening;
	  door_closing_out <= door_closing;
	end process;


	COUNTERS: process(reset_in, clk_in, door_opening, door_closing, door_open, moving_direction_up, moving_direction_down) is
	begin
		-- clocks
		clk_in_door_open_close <= clk_in and (door_opening or door_closing);
		clk_in_door_passenger_loading <= clk_in and door_open;
		clk_in_moving <= clk_in and (moving_direction_up or moving_direction_down);

		
--		direction_open_close <= not door_closing;
		-- timer resets
--		if (falling_edge(clk_in) or rising_edge(clk_in)) then
		reset_in_door_open_close <= reset_in or ((not door_opening) and (not door_closing));
		reset_in_door_passenger_loading <= reset_in or (not door_open);
		reset_in_moving <= reset_in or (not (moving_direction_up or moving_direction_down) or floor_reached);
--		end if;
	end process;
	STATE_INTERNALS: process(reset_in, e_state) is
		variable next_floor : std_logic_vector(FLOOR_CTR_SIZE-1 downto 0) := (others => '0');
	begin
		if (moving_direction_up = '1') then
			next_floor := std_logic_vector(unsigned(current_floor) + 1);
		else
			next_floor := std_logic_vector(unsigned(current_floor) - 1);
		end if;

		if (reset_in = '1') then
			moving_direction_up <= '0'; moving_direction_down <= '0';
			-- this design assumes the door is open on reset (e.g. attempt to close to get to idle state)
			door_opening <= '0'; door_closing <= '0'; door_open <= '1';
			door_delay_offset <= (others => '0');
			current_floor <= (others => '0');
			door_delay_offset <= (others => '0');
		else
			case e_state is
				when S_DOOR_OPENING =>
					door_opening <= '1'; floor_reached <= '0';
				when S_DOOR_OPEN =>
					door_opening <= '0'; door_open <= '1';
					door_delay_offset <= ( others => '0' );
				when S_DOOR_CLOSING =>
					door_open <= '0'; door_closing <= '1';
				when S_DOOR_REOPEN_INTERMEDIATE =>
					door_closing <= '0';
					door_delay_offset <= std_logic_vector(unsigned(DOOR_OPENCLOSE_DELAY) - unsigned(ctr_door_open_close));
				when S_MOVING_UP =>
					moving_direction_up <= '1'; floor_reached <= '0';
				when S_MOVING_DOWN =>
					moving_direction_down <= '1'; floor_reached <= '0';

				when S_FLOOR_REACHED =>
					floor_reached <= '1';
					current_floor <= next_floor;

					if (floor_request_up(to_integer(unsigned(next_floor))) = '1' or
							floor_request_down(to_integer(unsigned(next_floor))) = '1') then
						moving_direction_down <= '0';
						moving_direction_up <= '0';
						level_reached_motion_offset <= (others => '0');
					else
						-- continue going up
						level_reached_motion_offset <= LEVEL_REACHED_CTR_OFFSET;
					end if;
	
				--when S_IDLE =>
				when others =>
					door_delay_offset <= (others => '0');
					moving_direction_up <= '0'; moving_direction_down <= '0';
					door_opening <= '0'; door_closing <= '0'; door_open <= '0';
					floor_reached <= '0';
			end case;
		end if;
	end process;

	CURRENT_LEVEL_FLOOR_UP_OR_DOWN: process(current_floor, floor_request_up, floor_request_down) is
	begin
		current_level_floor_up <= floor_request_up(to_integer(unsigned(current_floor)));
		current_level_floor_down <= floor_request_down(to_integer(unsigned(current_floor)));
	end process;

  STATES: process(reset_in, clk_in) is
	begin
	   -- reset
		if (reset_in = '1') then
			
			-- once again, attempt to close door on reset
			e_state <= S_DOOR_CLOSING;
		-- clock
		elsif (rising_edge(clk_in)) then
			case e_state is
				when S_IDLE =>
					-- open door when on selected floor
					if (current_level_floor_up = '1' or
							current_level_floor_down = '1') then
--					if ((floor_request_up = '1' and current_floor = '1') or
--						(floor_request_down = '1' and current_floor = '0')) then
						e_state <= S_DOOR_OPENING;
					-- move to selected floor
					elsif (any_request_up_above = '1' or any_request_up_below = '1') then
						e_state <= S_MOVING_UP;
					elsif (any_request_down_below = '1' or any_request_down_above = '1') then
						e_state <= S_MOVING_DOWN;

					-- technically ... the sensor should not be triggering when doors are closed!
					elsif (door_request_open_in = '1' or door_sensor_in = '1') then
						e_state <= S_DOOR_OPENING;
					end if;

				when S_DOOR_OPENING =>
					if (ctr_door_open_close = std_logic_vector(unsigned(DOOR_OPENCLOSE_DELAY) - unsigned(door_delay_offset))) then
						e_state <= S_DOOR_OPEN;
					end if;

				when S_DOOR_OPEN =>
						if (door_request_open_in = '1' or door_sensor_in = '1' or
								(traveling_up = '0' and traveling_down = '0' and (current_level_floor_up = '1' or current_level_floor_down = '1')) or
								(traveling_up = '1' and current_level_floor_up = '1') or
								(traveling_down = '1' and current_level_floor_down = '1')) then
							-- remain open
						elsif ((door_request_close_in = '1') or (ctr_door_passenger_loading >= PASSENGER_LOADING_DELAY)) then
							e_state <= S_DOOR_CLOSING;
						end if;

				when S_DOOR_CLOSING =>
					-- process "open door" button press
					if (door_request_open_in = '1' or door_sensor_in = '1' or
								(traveling_up = '1' and current_level_floor_up = '1') or
								(traveling_down = '1' and current_level_floor_down = '1')) then
						e_state <= S_DOOR_REOPEN_INTERMEDIATE;

					elsif (ctr_door_open_close = DOOR_OPENCLOSE_DELAY) then
						e_state <= S_IDLE;
					end if;

				when S_DOOR_REOPEN_INTERMEDIATE =>
					e_state <= S_DOOR_OPENING;

				when S_MOVING_UP =>
					-- floor reached
					if (ctr_moving = std_logic_vector(unsigned(LEVEL_CHANGE_DELAY) - unsigned(level_reached_motion_offset))) then
						e_state <= S_FLOOR_REACHED;
					end if;

				when S_MOVING_DOWN =>
					-- floor reached
					if (ctr_moving = std_logic_vector(unsigned(LEVEL_CHANGE_DELAY) - unsigned(level_reached_motion_offset))) then
						e_state <= S_FLOOR_REACHED;
					end if;

				when S_FLOOR_REACHED =>
					if ((traveling_up = '1' and current_level_floor_up = '1') or
							(traveling_down = '1' and current_level_floor_down = '1') or
								(traveling_down = '0' and traveling_up = '0' and
								(current_level_floor_up = '1' or current_level_floor_down = '1'))) then
						e_state <= S_DOOR_OPENING;
					else
					-- continue moving
						if (moving_direction_up = '1') then
							e_state <= S_MOVING_UP;
						else
							e_state <= S_MOVING_DOWN;
						end if;
					end if;
			end case;

		end if;
  end process;
end behavior_1;
