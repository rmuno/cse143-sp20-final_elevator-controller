
-- elevator
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

-- single elevator controller for 2-levels
entity elevator_controller is
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

constant LEVEL_CHANGE_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(8, CTR_SIZE));
constant PASSENGER_LOADING_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(5, CTR_SIZE));
constant DOOR_OPENCLOSE_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(3, CTR_SIZE));
--constant ALL_ONES_2 : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');
--constant ALL_ONES_3 : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');

end elevator_controller;

architecture behavior_1 of elevator_controller is
	component counter is
	generic( N: integer );
	port(
	  reset_in: in std_logic;
	  clk_in: in std_logic;
	  count_out: out std_logic_vector(N-1 downto 0)
	);
	end component;

--	signal clk_in_door_open_close : std_logic := '0';
  
  signal current_floor: std_logic := '0';
  signal moving_direction_up: std_logic := '0';
  signal moving_direction_down: std_logic := '0';
  signal door_open: std_logic := '0';

	signal door_opening: std_logic := '0';
	signal door_closing: std_logic := '0';


	-- internals
	signal moving: std_logic := '0';--(not door_open) and (moving_up or moving_down);
	signal floor_request: std_logic := '0';--request_up or request_down;
	signal door_process: std_logic := '0';--door_opening or door_closing or door_open;
	signal serve_request: std_logic := '0';--(not moving) & (not door_process) & floor_request;

	signal should_move_up: std_logic := '0';
	signal should_move_down: std_logic := '0';

	-- hold input requests - users do not generally hold the button down until elevator arrives!
	signal floor_request_up: std_logic := '0';
	signal floor_request_down: std_logic := '0';

	-- door open/close helper
	signal clk_in_door_open_close: std_logic := '0';
	signal reset_in_door_open_close: std_logic := '0';
	signal ctr_door_open_close : std_logic_vector(CTR_SIZE-1 downto 0);

	-- door passenger loading helper
	signal clk_in_door_passenger_loading: std_logic := '0';
	signal reset_in_door_passenger_loading: std_logic := '0';
	signal ctr_door_passenger_loading: std_logic_vector(CTR_SIZE-1 downto 0);

	-- elevator cabin motion helper
	signal clk_in_moving: std_logic := '0';
	signal reset_in_moving: std_logic := '0';
	signal ctr_moving : std_logic_vector(CTR_SIZE-1 downto 0);

begin
	-- counter for opening / closing the door
	CTR_CMP_DOOR_OPEN_CLOSE : counter
		generic map(CTR_SIZE)
		port map(reset_in_door_open_close, clk_in_door_open_close, ctr_door_open_close);

	-- counter to waiting for passengers to enter/leave elevator
	CTR_CMP_PASSENGER_LOADING : counter
		generic map(CTR_SIZE)
		port map(reset_in_door_passenger_loading, clk_in_door_passenger_loading, ctr_door_passenger_loading);

	-- counter for moving the elevator
	CTR_LEVEL_TRANSITION : counter
		generic map(CTR_SIZE)
		port map(reset_in_moving, clk_in_moving, ctr_moving);

	-- input-retention process
	INPUTS: process(reset_in, floor_request_up_in, floor_request_down_in, current_floor, door_opening) is
	begin
		if (reset_in = '1') then
			floor_request_up <= '0';
			floor_request_down <= '0';

		-- disable floor request when door opens, but allow floor requests:
		-- 1. while door is open, to hold it open
		-- 2. while door closes, to open it again
		else

			-- "up" button
			if (current_floor = '1' and door_opening = '1') then
				floor_request_up <= '0';
			elsif (floor_request_up_in = '1') then
				floor_request_up <= '1';
			end if;

			-- "down" button
			if (current_floor = '0' and door_opening = '1') then
				floor_request_down <= '0';
			elsif (floor_request_down_in = '1') then
				floor_request_down <= '1';
			end if;
		end if;
	end process;


	OUTPUTS: process(clk_in, reset_in,
		current_floor, moving_direction_up, moving_direction_down, door_open, door_opening) is
	begin
		-- assign output values to internals
    current_floor_out <= current_floor;
    moving_direction_up_out <= moving_direction_up;
    moving_direction_down_out <= moving_direction_down;
    door_open_out <= door_open;
--		end if;
	end process;


	MOTION: process(floor_request_up, floor_request_down, current_floor) is
	begin
		-- motion
		should_move_up <= (floor_request_up) and (not current_floor);
		should_move_down <= (floor_request_down) and (current_floor);
	end process;

	COUNTERS: process(reset_in, clk_in, door_opening, door_closing, door_open, moving) is
	begin
		-- clocks
		clk_in_door_open_close <= clk_in and (door_opening or door_closing);
		clk_in_door_passenger_loading <= clk_in and door_open;
		clk_in_moving <= clk_in and moving;

		-- timer resets
		reset_in_door_open_close <= reset_in or ((not door_opening) and (not door_closing));
		reset_in_door_passenger_loading <= reset_in or (not door_open);
		reset_in_moving <= reset_in or (not moving);
	end process;

	INTERNALS: process(reset_in, clk_in,
		door_open, door_opening, door_closing,
		floor_request_up, floor_request_down,
		moving_direction_up, moving_direction_down) is
	begin
		moving <= (not door_open) and (moving_direction_up or moving_direction_down);
		floor_request <= floor_request_up or floor_request_down;
		door_process <= door_opening or door_closing or door_open;
		serve_request <= (not moving) and (not door_process) and floor_request;
	end process;

  STATES: process(reset_in, clk_in) is
	begin
	   -- reset
		if (reset_in = '1') then
			current_floor <= '0';
		  moving_direction_up <= '0';
		  moving_direction_down <= '0';
			door_open <= '0';
			door_opening <= '0';
			door_closing <= '0';

		-- clock
		elsif (rising_edge(clk_in)) then
--			assert false report "should_move_up " & std_logic'image(should_move_up) severity note;
			if (serve_request = '1') then
				-- attempt to move up, or open doors
				if (floor_request_up = '1') then
					moving_direction_up <= should_move_up;
					door_opening <= not should_move_up;
				else
				-- attempt to move down, or open doors
					moving_direction_down <= should_move_down;
					door_opening <= not should_move_down;
				end if;
			elsif (moving = '1') then
				-- disable motion once desired floor is reached
				if (ctr_moving = LEVEL_CHANGE_DELAY) then
					-- floor 1 reached
					if (moving_direction_up = '1') then
						moving_direction_up <= '0';
						current_floor <= '1';
					-- floor 0 reached
					elsif (moving_direction_down = '1') then
						moving_direction_down <= '0';
						current_floor <= '0';
					end if;

					door_opening <= '1';
				end if;

			elsif (door_process = '1') then
				-- open doors
				if ((door_opening = '1') and (ctr_door_open_close = DOOR_OPENCLOSE_DELAY)) then
					door_opening <= '0';
					door_open <= '1';
				-- door open - load passengers
				elsif ((door_open = '1') and (ctr_door_passenger_loading = PASSENGER_LOADING_DELAY)) then
					door_open <= '0';
					door_closing <= '1';
				-- door closing
				elsif ((door_closing = '1') and (ctr_door_open_close = DOOR_OPENCLOSE_DELAY)) then
					door_closing <= '0';
				end if;
			end if;
		-- rising_edge(clk)
		end if;
  end process;
end behavior_1;
