*###[ ffxa0:
	subroutine ffxa0(ca0,xm,ier)
***#[*comment:***********************************************************
*									*
*	calculates the one-point function (see 't Hooft and		*
*	Veltman) for real mass						*
*									*
*	Input:	xm	(real) mass2,					*
*									*
*	Output:	ca0	(complex) A0, the one-point function,		*
*		ier	0 (ok)						*
*									*
*	Calls:	log.							*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE COMPLEX ca0
	DOUBLE PRECISION xm
*
*	local variables
*
	DOUBLE PRECISION xmu,xlogm
*
*	common blocks etc
*
	include 'ff.h'
*  #] declarations:
*  #[ "calculations":
	if ( mudim2 .ne. 0 ) then
	    xmu = xm/mudim2
	else
	    xmu = xm
	endif
	if ( xmu .gt. xalogm ) then
	    xlogm = log(xmu)
	else
	    xlogm = 0
	    if ( xmu .ne. 0 ) call fferr(2,ier)
	endif
	ca0 = -(xm*(xlogm - 1 - divergence))
*  #] "calculations":
*###] ffxa0:
	end
