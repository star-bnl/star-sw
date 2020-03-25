library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library UNISIM;
use UNISIM.VComponents.all;


--================================================================
--========== Version 1 (Nov 2019) ================================
--================================================================
-- Changes: 
--    gain increased from 4.6 (10 bits) to 4.8 (12 bits) 
--    added PARAM(8 bits) instead of PEAK_IGNORE(1 bit)
--
--
--================================================================

entity stage_0 is
generic (
	VERSION		: std_logic_vector(3 downto 0) := x"0"
) ;
port ( 
	RESET	 	: in std_logic ;
	
	CLK_80		: in std_logic ;	-- 80 MHz ADC clock
	CLK_FASTEST	: in std_logic ;	-- a multiple of 80!

	STROBE_IN	: in std_logic ;	-- start of train of 8 data

	PARAM		: in std_logic_vector(7 downto 0) ;	-- params

	ADC_IN		: in std_logic_vector(11 downto 0) := (others => '0') ;
	
	ADC_OUT		: out std_logic_vector(18 downto 0) ;	-- at the "fastest clock"!

	-- constants from registers
	PEDESTAL	: in std_logic_vector(14 downto 0) := (others => '0');	-- 12*3=15
	GAIN		: in std_logic_vector(11 downto 0) := (others => '0')	-- 4.8 = 12

);
end stage_0;

architecture Beh of stage_0 is


type state_t is (S_START, S_0, S_1, S_2, S_3, S_4, S_5, S_6, S_7) ;

signal state	: state_t := S_START ;

signal i_adc	: integer range 0 to 4095 ;
signal i_ped	: integer range 0 to 32767;
signal f_sum	: integer range -32767 to 32767 ;
signal sum	: integer range 0 to 32767 ;
signal last 	: integer range 0 to 4095 ;
signal peak	: std_logic_vector(1 downto 0) := B"00" ;

signal s_sum	: std_logic_vector(14 downto 0) := (others => '0') ;
signal p_out	: std_logic_vector(26 downto 0) ;



begin


mult_i : entity work.dsp48_mult
port map (
	CLK => CLK_FASTEST,
	A => s_sum,
	B => GAIN,
	P => p_out
) ;

-- strip lower 6 bits to get rid of the gain decimal points, leaves 19 bits
ADC_OUT <= p_out(26 downto 8) ;

-- turn to vector
s_sum <= std_logic_vector(to_unsigned(f_sum,15)) when (f_sum > 0) else (others => '0') ;


i_adc <= to_integer(unsigned(ADC_IN)) ;
i_ped <= to_integer(unsigned(PEDESTAL)) ;


process(RESET,CLK_80)
begin
	if(rising_edge(CLK_80)) then
		if(RESET='1') then
			state <= S_START ;
			f_sum <= 0 ;
			sum <= 0 ;
			peak <= (others => '0') ;
			last <= 0 ;
		else
			case state is
			when S_START =>
				if(STROBE_IN='1') then
					state <= S_0 ;
				end if ;
			when S_0 =>
				sum <= i_adc ;
				peak <= (others => '0') ;

				state <= S_1 ;
			when S_1 =>
				sum <= sum + i_adc ;
				state <= S_2 ;

			when S_2 =>
				sum <= sum + i_adc ;
				last <= i_adc ;

				state <= S_3 ;
			when S_3 =>			-- peak expected here!
				sum <= sum + i_adc ;
				last <= i_adc ;

				if(i_adc > last) then
					peak(0) <= '1' ;
				end if ;

				state <= S_4 ;
			when S_4 =>
				sum <= sum + i_adc ;

				if(i_adc < last) then
					peak(1) <= '1' ;
				end if ;

				state <= S_5 ;
			when S_5 =>
				sum <= sum + i_adc ;
				state <= S_6 ;
			when S_6 =>
				sum <= sum + i_adc ;
				state <= S_7 ;
			when S_7 =>
				if(PARAM(0)='1' or peak=B"11") then
					f_sum <= (sum + i_adc)-i_ped ;
				else	
					f_sum <= 0 ;
				end if ;

				state <= S_0 ;
				
			end case ;
		end if ;
	end if ;
end process ;

end Beh;
