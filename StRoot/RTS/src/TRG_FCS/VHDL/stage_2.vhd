library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work ;
use work.lib.all ;


entity stage_2 is
generic (
	VERSION		: std_logic_vector(3 downto 0) := x"0"
) ;
port (
	RESET	: in std_logic ;
	CLK	: in std_logic ;
	
	STROBE_IN	: in std_logic ;
	STROBE_OUT	: out std_logic ;

	ECAL	: in cal_t(1 downto 0) ;
	HCAL	: in cal_t(0 downto 0) ;
	FPRE	: in fpre_t(0 downto 0) ;	-- bitpattern 

	REGS	: in int16_t(7 downto 0) ;	

	OUTPUT	: out vec8_t(7 downto 0)	-- bitpattern

);
end entity stage_2 ;

architecture beh of stage_2 is

constant map_dep : ecal_map_t := make_dep_map('0') ;
constant map_adr : ecal_map_t := make_adr_map('0') ;


type esum_t is array(0 to 2, 0 to 2) of integer range 0 to 2**(8+2)-1 ;
type mult_t is array(0 to 2, 0 to 2) of integer range 0 to 2**(8+2+8)-1 ;
type sum_t is array(0 to 2, 0 to 2) of integer range 0 to 2**(8+2+1)-1 ;

signal hsum	: integer range 0 to 2**(8+2)-1 ;
signal hsum_2	: integer range 0 to 2**(8+2+8)-1 ;

signal esum	: esum_t ;

signal sum	: sum_t ;

signal etot	: integer range 0 to 2**(8+2+2)-1 ;
signal jet	: integer range 0 to 2**(8+2+2+1)-1 ;

signal is_em	: mult_t ;
signal is_had	: mult_t ;

type res_t is array(1 to 2) of std_logic_vector(2 downto 0) ;
signal em	: res_t ;
signal gam	: res_t ;
signal ele	: res_t ;
signal had	: res_t ;
signal jp	: std_logic_vector(2 downto 1) ;

signal out_dta	: vec8_t(7 downto 0) := (others => x"00") ;

signal fpre_1	: fpre_t(0 downto 0) ;
signal fpre_2	: fpre_t(0 downto 0) ;
signal fpre_3	: fpre_t(0 downto 0) ;

signal strobe	: std_logic_vector(3 downto 0) ;


begin


--*************************************************************************
--************* VERSION 0 (2019) ******************************************
--*************************************************************************
assert (VERSION = x"0") report "UNKNOWN STAGE_2 VERSION: " & integer'image(to_integer(unsigned(VERSION))) severity failure ;

--=========================================================================
--==================== Pipeline Stage/Clock 1 =============================
--=========================================================================

--------- keep moving strobe_in through the pipeline stages ---------------
process(CLK)
begin
	if(rising_edge(CLK)) then
		strobe(1) <= STROBE_IN ;	
	end if ;
end process ;

--------- generate ECAL sums & ----------------------------
process(RESET,CLK)
begin
	if(RESET='1') then
		hsum <= 0 ;
	elsif(rising_edge(CLK)) then
		-- hsum is 8+2 bits
		hsum <= HCAL(0)(0) + HCAL(0)(2) + HCAL(0)(4) + HCAL(0)(6) ;

--		report "HSUM " & integer'image(hsum) ;

	end if ;
end process ;

--------- generate HCAL sums  ----------------------------
process(RESET,CLK)
begin 
	if(RESET='1') then
		esum <= (others => (others => 0)) ;
	elsif(rising_edge(CLK)) then
		for r in 0 to 2 loop
			for c in 0 to 2 loop
				-- esum is 8+2 bits
				esum(r,c) <= 	ECAL(map_dep(r,c))(map_adr(r,c)) + 
						ECAL(map_dep(r,c+1))(map_adr(r,c+1)) + 
						ECAL(map_dep(r+1,c))(map_adr(r+1,c)) + 
						ECAL(map_dep(r+1,c+1))(map_adr(r+1,c+1)) ;

			end loop ;
		end loop ;
	end if ;
end process ;


------------------- move FPRE 1 tick -----------------------------------------------------
process(CLK) 
begin
	if(rising_edge(CLK)) then
		fpre_1 <= FPRE ;
	end if ;
end process ;


--============== Report maps, to make sure all is well
process
begin
	for r in 0 to 3 loop
		for c in 0 to 3 loop
			report "XMAPS: " & integer'image(r) & ":" & integer'image(c) & " --- " & integer'image(map_dep(r,c)) & "-" & integer'image(map_adr(r,c)) ;
		end loop ;
	end loop ;

	wait ;
end process ;
	

--=============================================================================
--====================== Pipeline Stage/Clock 2 ===============================
--=============================================================================

--------------------- move strobe again ---------------------------------------
process(CLK)
begin	
	if(rising_edge(CLK)) then
		strobe(2) <= strobe(1) ;
	end if ;
end process ;


------------- generate etot & jet ---------------------------------------------------------
process(RESET, CLK,esum,hsum)
begin
	if(RESET='1') then
		etot <= 0 ;
		jet <= 0 ;
	elsif(rising_edge(CLK)) then
		-- etot is (8+2+2) bits
		etot <= esum(0,0) + esum(0,2) + esum(2,0) + esum(2,2) ;

		-- jet is (8+2+2+1) bits
		jet <= hsum + esum(0,0) + esum(0,2) + esum(2,0) + esum(2,2) ;

--		report "ETOT/JET " & integer'image(etot) & " --- " & integer'image(jet) ;

	end if ;
end process ;

-------------- generate total sums ---------------------------------------------------------
process(RESET,CLK)
begin 
	if(RESET='1') then
		sum <= (others => (others => 0)) ;
	elsif(rising_edge(CLK)) then
		for r in 0 to 2 loop
			for c in 0 to 2 loop
				-- sum is (8+2+1) bits
				sum(r,c) <= esum(r,c) + hsum ;	-- this looks odd.. do I really need to add this?
			end loop ;
		end loop ;

	end if ;
end process ;


-------------- move hsum 1 tick to align to stage 3 --------------------------------
-------------- and muliply (shift) by 128 to bring it in range of esum*RATIO -------
process(CLK)
begin
	if(rising_edge(CLK)) then
		-- hsum_2 is (8+2+8) bits
		hsum_2 <= hsum * 128 ;
	end if ;
end process ;

-------------- move fpre another tick ----------------------------------------------
process(CLK)
begin
	if(rising_edge(CLK)) then
		fpre_2 <= fpre_1 ;
	end if ;
end process ;

-------------- generate multiplier primitives --------------------------------------

process(RESET,CLK)
begin
	if(RESET='1') then
		is_em <= (others => (others => 0)) ;
		is_had <= (others => (others => 0)) ;
	elsif(rising_edge(CLK)) then
		for r in 0 to 2 loop
			for c in 0 to 2 loop
				-- is_em is (8+2+8) 
				is_em(r,c) <= esum(r,c) * regs(EM_HERATIO_THR) ;

				is_had(r,c) <= esum(r,c) * regs(HAD_HERATIO_THR) ;
			end loop ;
		end loop ;
	end if ;
end process ;

--====================================================================================
--======================== Pipeline Stage/Clock 3 ====================================
--====================================================================================

--------------------- move strobe again ---------------------------------------
process(CLK)
begin	
	if(rising_edge(CLK)) then
		strobe(3) <= strobe(2) ;
	end if ;
end process ;
				
------------ where I use sum, hsum and multiplier primitives -------------------------

process(RESET,CLK, hsum_2, sum, is_em, is_had, fpre_2, regs)
begin
	if(RESET='1') then
		em <= (others => (others => '0')) ;
		gam <= (others => (others => '0')) ;
		ele <= (others => (others => '0')) ;
		had <= (others => (others => '0')) ;
	elsif(rising_edge(CLK)) then
		em <= (others => (others => '0')) ;
		gam <= (others => (others => '0')) ;
		ele <= (others => (others => '0')) ;
		had <= (others => (others => '0')) ;

		for r in 0 to 2 loop
			for c in 0 to 2 loop
				report "HSUM_2 " & integer'image(hsum_2) & " -- " & integer'image(is_em(r,c)) ;

				if(hsum_2 < is_em(r,c)) then
					report "IS_EM " & integer'image(r) & integer'image(c) ;

					if(sum(r,c) > regs(EMTHR1)) then
						em(1)(r) <= '1' ;

						if(fpre_2(0)(0)(r) = '0') then
							gam(1)(r) <= '1' ;
						else
							ele(1)(r) <= '1' ;
						end if ;
					end if ;

					if(sum(r,c) > regs(EMTHR2)) then
						em(2)(r) <= '1' ;
					
						if(fpre_2(0)(0)(r) = '0') then
							gam(2)(r) <= '1' ;
						else
							ele(2)(r) <= '1' ;
						end if ;
					end if ;
				end if ;


				if(hsum_2 > is_had(r,c)) then
					if(sum(r,c) > regs(HADTHR1)) then
						had(1)(r) <= '1' ;
					end if ;

					if(sum(r,c) > regs(HADTHR2)) then
						had(2)(r) <= '1' ;
					end if ;
				end if ;
			end loop ;
		end loop ;
	end if ;
end process ;


------------- generate JP bits ----------------------------------------------------

process(RESET,CLK,etot,jet,regs)
begin
	if(RESET='1') then
		jp <= (others => '0') ;
	elsif(rising_edge(CLK)) then
		jp <= (others => '0') ;

		if(etot > regs(JETTHR1)) then 
			jp(1) <= '1' ;
		end if ;

		if(jet > regs(JETTHR2)) then 
			jp(2) <= '1' ;
		end if ;

--		report "JP " & std_logic'image(jp(2)) & std_logic'image(jp(1)) ;
	end if ;
end process ;


------------- Tonko: move fpre's last byte where I hold the "something in FPRE" bits
process(CLK)
begin
	if(rising_edge(CLK)) then
		fpre_3 <= fpre_2 ;
	end if ;
end process ;

--============================================================================
--=============== And final output ===========================================
--============================================================================

--------- Direct (non-clocked) output because the registers will be re-clocked outside ----


out_dta(0) <= B"10" & em(2) & em(1) ;
out_dta(1) <= B"00" & gam(2) & gam(1) ;
out_dta(2) <= B"00" & ele(2) & ele(1) ;
out_dta(3) <= B"00" & had(2) & had(1) ;
out_dta(4) <= B"0000_00" & jp(2) & jp(1) ;
out_dta(5) <= fpre_3(0)(0) ;
out_dta(6) <= fpre_3(0)(1) ;
out_dta(7) <= fpre_3(0)(7) ;	-- Tonko: copy over the last byte

STROBE_OUT <= strobe(3) ;

OUTPUT <= out_dta ;


process(CLK, out_dta)
begin
	if(rising_edge(CLK)) then
		f_g: for i in 0 to 7 loop
			report "OUTPUT " & integer'image(i) & " --- " & std_logic'image(strobe(3)) & " --- " & integer'image(to_integer(unsigned(out_dta(i)))) ;
		end loop ;
	end if ;
end process ;


end architecture beh ;

