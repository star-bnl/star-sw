	SUBROUTINE MHEADRD()
	common/HDRNFO/ SUB_ID,NBIN_COLL,NWE,NWW,NJETS,NREJ
	integer        SUB_ID,NBIN_COLL,NWE,NWW,NJETS,NREJ
	integer ISTAT
	data ISTAT/0/

* transporting the characteristic event
* info from the Zebra banks into the struct:

	STRUCTURE MPAR {int Version, int SUBPR, int NBIN, int NWE, int NWW, int NJETS, int NREJ }

	USE /EVNT/PASS/MPAR stat=ISTAT

	if(ISTAT.GE.0) then
	   write(*,*) '*******************  + ZERO ISTAT ***********', ISTAT
	   SUB_ID    = MPAR_SUBPR
	   NBIN_COLL = MPAR_NBIN
	   NWE       = MPAR_NWE
	   NWW       = MPAR_NWW
	   NJETS     = MPAR_NJETS
	   NREJ      = MPAR_NREJ
	   write(*,*) '####################################################################'
	   write(*,*) MPAR_SBPR
	   write(*,*) '####################################################################'

	else
	   write(*,*) '*******************  - ZERO ISTAT ***********'
	   SUB_ID    = 0
	   NBIN_COLL = 0
	   NWE       = 0
	   NWW       = 0
	   NJETS     = 0
	   NREJ      = 0
	endif

	return
	end
