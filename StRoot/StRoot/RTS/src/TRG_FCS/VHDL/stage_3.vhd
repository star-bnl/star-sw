library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work ;
use work.lib.all ;


entity stage_3 is
generic (
	VERSION		: std_logic_vector(3 downto 0) := x"0"
) ;
port (
	RESET	: in std_logic ;
	CLK	: in std_logic ;
	
	STROBE_IN	: in std_logic ;
	DTA_IN		: in fpre_t(0 downto 0) ;	-- bits, not integers...

	STROBE_OUT	: out std_logic ;
	DSM_OUT		: out std_logic_vector(11 downto 0) 

);
end entity stage_3 ;

architecture beh of stage_3 is


signal em	: std_logic_vector(2 downto 1) ;
signal gam	: std_logic_vector(2 downto 1) ;
signal ele	: std_logic_vector(2 downto 1) ;
signal had	: std_logic_vector(2 downto 1) ;

begin


--*************************************************************************
--***************** VERSION 0 (2019) **************************************
--*************************************************************************
assert (VERSION = x"0") report "UNKNOWN STAGE_3 VERSION: " & integer'image(to_integer(unsigned(VERSION))) severity failure ;

--=========================================================================
--==================== Pipeline Stage/Clock 1 =============================
--=========================================================================


em(1) <= '0' when (DTA_IN(0)(0)(2 downto 0)=B"000") else '1' ;
em(2) <= '0' when (DTA_IN(0)(0)(5 downto 3)=B"000") else '1' ;

gam(1) <= '0' when (DTA_IN(0)(1)(2 downto 0)=B"000") else '1' ;
gam(2) <= '0' when (DTA_IN(0)(1)(5 downto 3)=B"000") else '1' ;

ele(1) <= '0' when (DTA_IN(0)(2)(2 downto 0)=B"000") else '1' ;
ele(2) <= '0' when (DTA_IN(0)(2)(5 downto 3)=B"000") else '1' ;

had(1) <= '0' when (DTA_IN(0)(3)(2 downto 0)=B"000") else '1' ;
had(2) <= '0' when (DTA_IN(0)(3)(5 downto 3)=B"000") else '1' ;


-- in FY19 I only need 1 clock cycle...
process(CLK)
begin
	if(rising_edge(CLK)) then
		STROBE_OUT <= STROBE_IN ;	

		DSM_OUT(0) <= em(1) ;
		DSM_OUT(1) <= em(2) ;

		DSM_OUT(2) <= gam(1) ;
		DSM_OUT(3) <= gam(2) ;

		DSM_OUT(4) <= ele(1) ;
		DSM_OUT(5) <= ele(2) ;

		DSM_OUT(6) <= had(1) ;
		DSM_OUT(7) <= had(2) ;

		DSM_OUT(8) <= DTA_IN(0)(4)(0) ;		-- JP1
		DSM_OUT(9) <= DTA_IN(0)(4)(1) ;		-- JP2

		DSM_OUT(10) <= '0' ;
		DSM_OUT(11) <= DTA_IN(0)(7)(7) ;	-- fPRE hit
	end if ;
end process ;


--============================================================================
--=============== And final output ===========================================
--============================================================================

-- already done in FY19

end architecture beh ;

