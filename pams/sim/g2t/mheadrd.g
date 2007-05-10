	SUBROUTINE MHEADRD()
	IMPLICIT NONE
*
* $Id: mheadrd.g,v 1.6 2007/05/10 19:13:52 potekhin Exp $
*
* $Log: mheadrd.g,v $
* Revision 1.6  2007/05/10 19:13:52  potekhin
* Added mstu72-73 and mstp111 to the list of forwarded data,
* as per the Spin PWG request; removed extra prints; handle
* error codes for the "main" event header and for pythia
* separately
*
* Revision 1.5  2004/08/05 16:37:23  potekhin
* New format for the common block due to Pythia work
*
*
	common/HDRNFO/ EVSTAT,SUB_ID,NBIN_COLL,NWE,NWW,NJETS,NREJ
	integer        EVSTAT,SUB_ID,NBIN_COLL,NWE,NWW,NJETS,NREJ

	common/PYTNFO/ PYSTAT,PYT_ID,MANDS,MANDT,MANDU,HARDP,COSTH,BJOR1,BJOR2,MSTU72,MSTU73,MSTP111
	integer        PYSTAT,PYT_ID,MSTU72,MSTU73,MSTP111
	real           MANDS,MANDT,MANDU,HARDP,COSTH,BJOR1,BJOR2

	integer ISTAT,JSTAT
	data    ISTAT/0/,JSTAT/0/

* transporting the characteristic event info from the Zebra banks into the struct:
	STRUCTURE MPAR {int Version, int SUBPR, int NBIN, int NWE, int NWW, int NJETS, int NREJ }
	STRUCTURE PYTH {int Version, int SUBPR,
                        real MANDS,  real MANDT,  real MANDU,
                        real HARDP,  real COSTH,
                        real BJOR1,  real BJOR2,
                        real MSTU72, real MSTU73, real MSTP111}


	USE /EVNT/PASS/MPAR stat=ISTAT

	if(ISTAT.GE.0) then
	   write(*,*) 'EVENT HEADER FOUND'

	   SUB_ID    = MPAR_SUBPR
	   NBIN_COLL = MPAR_NBIN
	   NWE       = MPAR_NWE
	   NWW       = MPAR_NWW
	   NJETS     = MPAR_NJETS
	   NREJ      = MPAR_NREJ
	endif

	EVSTAT=ISTAT

* Useful but verbose diagnostic
*	else
*	   write(*,*) 'EVENT HEADER NOT FOUND'


	USE /EVNT/PASS/PYTH stat=JSTAT

	if(JSTAT.GE.0) then
	   write(*,*) 'PYTHIA HEADER FOUND'

	   PYT_ID = PYTH_SUBPR
	   MANDS  = PYTH_MANDS
	   MANDT  = PYTH_MANDT
	   MANDU  = PYTH_MANDU
	   HARDP  = PYTH_HARDP
	   COSTH  = PYTH_COSTH
	   BJOR1  = PYTH_BJOR1
	   BJOR2  = PYTH_BJOR2
	   MSTU72 = PYTH_MSTU72
	   MSTU73 = PYTH_MSTU73
	   MSTP111= PYTH_MSTP111
	endif

	PYSTAT=JSTAT

* Useful but verbose diagnostic
*	else
*	   write(*,*) 'PYTHIA HEADER NOT FOUND'

	return
	end
