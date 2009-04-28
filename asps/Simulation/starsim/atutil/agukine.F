*
* $Id: agukine.F,v 1.2 2009/04/28 22:48:01 perev Exp $
* $Name:  $
* $Log: agukine.F,v $
* Revision 1.2  2009/04/28 22:48:01  perev
* More comments
*
* Revision 1.1  2009/04/09 22:48:55  perev
* Filter before Geant tracking
*
      	subroutine agukine
#include "geant321/gcflag.inc"
	integer filt_act
   	ieotri = filt_act(4,0,0) !// RejectGT (GeantTracking)
	end
