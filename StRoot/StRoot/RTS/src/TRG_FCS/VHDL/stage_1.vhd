library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library UNISIM;
use UNISIM.VComponents.all;

library WORK;
use WORK.lib.ALL ;


--==============================================================
--============ Version 1 (Nov 2019) ============================
--==============================================================
--
-- Changes
--      added generic parameter PARAM (8 bits) instead of IS_FPRE
--      output is now 8 beats of 12 bits instead of 8x8
--
--===============================================================

entity stage_1 is
generic (
	VERSION	: std_logic_vector(3 downto 0) := x"1"
) ;
port ( 
	RESET	: in std_logic ;

	-- can be a fast clock e.g. 320 MHz
	CLK		: in std_logic ;

	-- 31 values of 19 bit ADCs
	VAL_IN		: in vec19_t(31 downto 0) := (others => (others => '0')) ;

	-- 1 bit output if I want self triggering
	SELF_TRIGGER	: out std_logic  ;

	-- 8x8 bits which go to Stage 2
	RES_OUT		: out vec12_t(7 downto 0) ;

	-- algorithm parameters...
	PARAM		: in std_logic_vector(7 downto 0) ;
	THR		: in std_logic_vector(15 downto 0) := (others => '0') 

);
end stage_1;

architecture Beh of stage_1 is

type sum_t	is array(7 downto 0) of integer range 0 to 2097151;	-- 21 bits
type val_t	is array(31 downto 0) of integer range 0 to 524287;	-- 19 bits


signal t	: std_logic_vector(31 downto 0) := (others => '0') ;
signal i_thr	: integer range 0 to 65535 ;

signal sum	: sum_t := (others => 0) ;
signal i_val	: val_t := (others => 0) ;

signal ecal_out	: vec8_t(7 downto 0) ;
signal fpre_out : vec8_t(7 downto 0) ;

signal self_trg	: std_logic ;

begin


i_thr <= to_integer(unsigned(THR)) ;	-- to integer


i_val_g: for i in 0 to 31 generate
	i_val(i) <= to_integer(unsigned(VAL_IN(i))) ;
end generate ;

------------------------------------------------
-- local HT-like self-trigger ------------------
------------------------------------------------

--============== Pipeline 1 =====================================
i_thr_g: for i in 0 to 31 generate

process(CLK,i_val(i),i_thr)
begin
	if(rising_edge(CLK)) then
		if(i_val(i) > i_thr) then
			t(i) <= '1' ;
		else
			t(i) <= '0' ;
		end if ;
	end if ;
end process ;

end generate ;

-------------------------------------------------
--  Self-trigger is "any above threshold" -------
-------------------------------------------------
self_trg <= or_reduce(t) ;

--============ Pipeline 2 ==========================================
process(CLK)
begin
	if(rising_edge(CLK)) then
		SELF_TRIGGER <= self_trg ;
	end if ;
end process ;


--=========================================================================
--================ FPRE Algo ==============================================
--=========================================================================
------------ Pipeline 2 ---------------------------------------------------
process(CLK)
begin
        if(rising_edge(CLK)) then
                --============ according to Akio's code =============================
                fpre_out(0)(0) <= t(0) or t(1) or t(2) or t(3) or t(4) ;
                fpre_out(0)(1) <= t(2) or t(3) or t(4) or t(5) or t(6) ;
                fpre_out(0)(2) <= t(4) or t(5) or t(6) or t(7) or t(8) ;

                fpre_out(0)(7 downto 3) <= B"0000_0" ;  -- other bits are 0

                --========== Tonko's addition for FY19 ==============================
                fpre_out(1) <= t(7 downto 0) ;
                fpre_out(2) <= t(15 downto 8) ;
                fpre_out(3) <= t(23 downto 16) ;
                fpre_out(4) <= t(31 downto 24) ;
                fpre_out(5) <= x"00" ;
                fpre_out(6) <= x"00" ;
                fpre_out(7) <= self_trg & B"000_0000" ;
        end if ;
end process ;


--=========================================================================
--================== H/E CAL Algo =========================================
--=========================================================================
----------------------------------------------------------
-- Partial sums (pipeline 1 ) ----------------------------
----------------------------------------------------------
process(CLK)
begin
	if(rising_edge(CLK)) then
		sum(0) <= i_val(0) + i_val(1) + i_val(4) + i_val(5) ;
		sum(1) <= i_val(2) + i_val(3) + i_val(6) + i_val(7) ;
		sum(2) <= i_val(8) + i_val(9) + i_val(12) + i_val(13) ;
		sum(3) <= i_val(10) + i_val(11) + i_val(14) + i_val(15) ;
		sum(4) <= i_val(16) + i_val(17) + i_val(20) + i_val(21) ;
		sum(5) <= i_val(18) + i_val(19) + i_val(22) + i_val(23) ;
		sum(6) <= i_val(24) + i_val(25) + i_val(28) + i_val(29) ;
		sum(7) <= i_val(26) + i_val(27) + i_val(30) + i_val(31) ;
	end if ;
end process ;

----------------------------------------------
-- 8 outputs to Stage 2 (pipeline 2) ---------
----------------------------------------------
s2_g: for i in 0 to 7 generate

process(CLK,sum(i))
begin
	if(rising_edge(CLK)) then
		if(sum(i) > 131071) then	-- clip to maximum
			ecal_out(i) <= x"FF" ;
		else 
			ecal_out(i) <= std_logic_vector(to_unsigned(sum(i),21)(14 downto 7)) ;
		end if ;
	end if ;
end process ;

end generate ; 
----------------------------------------------------------



--============================================================================
--================== Final output mux ========================================
--============================================================================
out_g: for i in 0 to 7 generate
	RES_OUT(i) <= B"0000" & ecal_out(i) when (PARAM(0)='0') else B"0000" & fpre_out(i) ;
end generate ;


end Beh;
