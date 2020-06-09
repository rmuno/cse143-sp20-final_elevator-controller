
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
	DELAY_DOOR_OPENCLOSE: integer
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

constant LEVEL_CHANGE_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(DELAY_LEVEL_CHANGE, CTR_SIZE));
constant PASSENGER_LOADING_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(DELAY_PASSENGER_LOADING, CTR_SIZE));
constant DOOR_OPENCLOSE_DELAY : std_logic_vector(CTR_SIZE-1 downto 0) := std_logic_vector(to_unsigned(DELAY_DOOR_OPENCLOSE, CTR_SIZE));
--constant ALL_ONES_2 : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');
--constant ALL_ONES_3 : std_logic_vector(CTR_SIZE-1 downto 0) := (others => '1');

end elevator_controller;

architecture behavior_1 of elevator_controller is
	type ELEVATOR_STATE is (
		S_IDLE,
		S_DOOR_OPENING, S_DOOR_OPEN, S_DOOR_CLOSING,
		S_MOVING_UP, S_MOVING_DOWN
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

  signal current_floor: std_logic := '0';
  signal moving_direction_up: std_logic := '0';
  signal moving_direction_down: std_logic := '0';
  signal door_open: std_logic := '0';

	signal door_opening: std_logic := '0';
	signal door_closing: std_logic := '0';


	-- internals
	signal moving: std_logic := '0';

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
    current_floor_out <= current_floor;
    moving_direction_up_out <= moving_direction_up;
    moving_direction_down_out <= moving_direction_down;
    door_open_out <= door_open;
	end process;


	COUNTERS: process(reset_in, clk_in, door_opening, door_closing, door_open, moving_direction_up, moving_direction_down) is
	begin
		-- clocks
		clk_in_door_open_close <= clk_in and (door_opening or door_closing);
		clk_in_door_passenger_loading <= clk_in and door_open;
		clk_in_moving <= clk_in and (moving_direction_up or moving_direction_down);

		-- timer resets
--		if (falling_edge(clk_in) or rising_edge(clk_in)) then
		reset_in_door_open_close <= reset_in or ((not door_opening) and (not door_closing));
		reset_in_door_passenger_loading <= reset_in or (not door_open);
		reset_in_moving <= reset_in or (not (moving_direction_up or moving_direction_down));
--		end if;
	end process;

	INTERNALS: process(reset_in, e_state) is
	begin
		if (reset_in = '1') then
			moving <= '0'; moving_direction_up <= '0'; moving_direction_down <= '0';
			-- this design assumes the door is open on reset (e.g. attempt to close to get to idle state)
			door_opening <= '0'; door_closing <= '0'; door_open <= '1';
		else
			case e_state is
				when S_DOOR_OPENING =>
					moving_direction_up <= '0'; moving_direction_down <= '0'; moving <= '0';
					door_opening <= '1';
				when S_DOOR_OPEN =>
					door_opening <= '0'; door_open <= '1';
				when S_DOOR_CLOSING =>
					door_open <= '0'; door_closing <= '1';
				when S_MOVING_UP =>
					moving_direction_up <= '1'; moving <= '1';
				when S_MOVING_DOWN =>
					moving_direction_down <= '1'; moving <= '1';
	
				--when S_IDLE =>
				when others =>
					moving <= '0'; moving_direction_up <= '0'; moving_direction_down <= '0';
					door_opening <= '0'; door_closing <= '0'; door_open <= '0';
			end case;
		end if;
	end process;

  STATES: process(reset_in, clk_in) is
	begin
	   -- reset
		if (reset_in = '1') then
			current_floor <= '0';
			
			-- once again, attempt to close door on reset
			e_state <= S_DOOR_CLOSING;
		-- clock
		elsif (rising_edge(clk_in)) then
			case e_state is
				when S_IDLE =>
					-- open door when on selected floor
					if ((floor_request_up = '1' and current_floor = '1') or
						(floor_request_down = '1' and current_floor = '0')) then
						e_state <= S_DOOR_OPENING;
					-- move to selected floor
					elsif (floor_request_up = '1') then
						e_state <= S_MOVING_UP;
					elsif (floor_request_down = '1') then
						e_state <= S_MOVING_DOWN;
					end if;

				when S_DOOR_OPENING =>
					if (ctr_door_open_close = DOOR_OPENCLOSE_DELAY) then
						e_state <= S_DOOR_OPEN;
					end if;

				when S_DOOR_OPEN =>
					if (ctr_door_passenger_loading = PASSENGER_LOADING_DELAY) then
						e_state <= S_DOOR_CLOSING;
					end if;

				when S_DOOR_CLOSING =>
--					if (floor_request_down = '1') then
--					elsif (ctr_door_open_close = DOOR_OPENCLOSE_DELAY) then
					if (ctr_door_open_close = DOOR_OPENCLOSE_DELAY) then
						e_state <= S_IDLE;
					end if;

				when S_MOVING_UP =>
					-- floor reached
					if (ctr_moving = LEVEL_CHANGE_DELAY) then
						current_floor <= '1';
						e_state <= S_DOOR_OPENING;
					end if;

--				when S_FLOOR_REACHED =>

				when S_MOVING_DOWN =>
					-- floor reached
					if (ctr_moving = LEVEL_CHANGE_DELAY) then
						current_floor <= '0';
						e_state <= S_DOOR_OPENING;
					end if;
			end case;

		end if;
  end process;
end behavior_1;
