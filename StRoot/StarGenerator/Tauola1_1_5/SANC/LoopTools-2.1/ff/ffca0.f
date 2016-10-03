*###[ ffca0:
	subroutine ffca0(ca0,cm,ier)
***#[*comment:***********************************************************
*									*
*	calculates the one-point function (see 't Hooft and		*
*	Veltman) for complex mass					*
*									*
*	Input:	cm	(complex) mass2, re>0, im<0.			*
*									*
*	Output:	ca0	(complex) A0, the one-point function,		*
*		ier	0 (OK)						*
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
	DOUBLE COMPLEX ca0,cm
*
*	local variables
*
	DOUBLE COMPLEX cmu,clogm,c
	DOUBLE PRECISION absc,xm
*
*	common blocks etc
*
	include 'ff.h'
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*  #] declarations:
*  #[ the real case:
*	
*	adapted to log-and-pole scheme 25-mar-1992
*	
	if ( DIMAG(cm) .eq. 0 .or. nschem .lt. 7 ) then
	    xm = DBLE(cm)
	    call ffxa0(ca0,xm,ier)
	    return
	endif
*  #] the real case:
*  #[ "calculations":
	if ( mudim2 .ne. 0 ) then
	    cmu = cm/DBLE(mudim2)
	else
	    cmu = cm
	endif
	if ( absc(cmu) .gt. xclogm ) then
	    clogm = log(cmu)
	else
	    clogm = 0
	    if ( cmu .ne. czero ) call fferr(1,ier)
	endif
	ca0 = - cm * ( clogm - 1 - DBLE(divergence) )
*  #] "calculations":
*###] ffca0:
	end
