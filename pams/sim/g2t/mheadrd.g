	SUBROUTINE MHEADRD()
	common/HDRNFO/ SUB_ID,NBIN_COLL,NWE,NWW,NJETS
	integer SUB_ID,NBIN_COLL,NWE,NWW,NJETS,ISTAT
	data ISTAT/0/

* transporting the characteristic event
* info from the Zebra banks into the struct:

	STRUCTURE MPAR {int Version, int SUBPR, int NBIN, int NWE, int NWW, int NJETS}

	USE /EVNT/PASS/MPAR stat=ISTAT

	IF(ISTAT.GE.0) THEN
*	   write(*,*) '*******************  + ZERO ISTAT ***********'
	   SUB_ID    = MPAR_SUBPR
	   NBIN_COLL = MPAR_NBIN
	   NWE       = MPAR_NWE
	   NWW       = MPAR_NWW
	   NJETS     = MPAR_NJETS
	ELSE
*	   write(*,*) '*******************  - ZERO ISTAT ***********'
	   SUB_ID    = 0
	   NBIN_COLL = 0
	   NWE       = 0
	   NWW       = 0
	   NJETS     = 0
	ENDIF

	RETURN
	END
