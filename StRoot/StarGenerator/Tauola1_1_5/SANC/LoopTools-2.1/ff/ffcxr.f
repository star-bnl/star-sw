*--#[ log:
*	$Id: ffcxr.f,v 1.1 2016/09/23 18:39:42 jwebb Exp $
*	$Log: ffcxr.f,v $
*	Revision 1.1  2016/09/23 18:39:42  jwebb
*	Initial commit of Tauola
*	
c Revision 1.2  1995/11/10  19:04:24  gj
c Added nicer logging header...
c
*--#] log: 


*###[ ffcxr:
	subroutine ffcxr(crr,ipi12,y,y1,z,z1,dyz,ld2yzz,d2yzz,zz,zz1,
     +		ldy2z,dy2z,ieps,ier)
***#[*comment:***********************************************************
*									*
*	calculates R as defined in appendix b:				*
*									*
*		   /1    log(x-z+i*eps) - log(y-z+i*eps)		*
*	r(y,z)  =  \ dx  -----------------------------------		*
*		   /0		      x-y				*
*									*
*	    = li2(y/(y-z)+i*eps') - li2((y-1)/(y-z)+i*eps')		*
*									*
*	y,z are real, ieps integer denoting the sign of i*eps.		*
*	factors pi^2/12 are passed in the integer ipi12.		*
*									*
*	Input:	y	(real)						*
*		y1	(real)		1-y				*
*		z	(real)						*
*		z1	(real)		1-z				*
*		dyz	(real)		y-z				*
*									*
*		ld2yzz	(logical)	if .TRUE. also defined are:	*
*		d2yzz	(real)		2*y - z^+ - z^-			*
*		zz	(real)		the other z-root		*
*		zz1	(real)		1 - zz				*
*									*
*		ieps	(integer)	if +/-1 denotes sign imaginary	*
*					part of	argument logs		*
*		ieps	(integer)	if +/-2 denotes sign imaginary	*
*					part of	argument dilogs		*
*									*
*	Output	crr	(complex)	R modulo factors pi^2/12	*
*		ipi12	(integer)	these factors			*
*		ier	(intger)	0=ok, 1=num prob, 2=error	*
*									*
*	Calls:	ffxli2,(test: ffzxdl),dfflo1,zxfflg			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ipi12,ieps,ier
	logical ld2yzz,ldy2z
	DOUBLE PRECISION y,y1,z,z1,dyz,d2yzz,zz,zz1,dy2z(3)
	DOUBLE COMPLEX crr(7)
*
*	local variables
*
        integer i,iclas1,iclas2
	DOUBLE PRECISION fact,xx1,xx2,xx1p,xx2p,arg2,arg3,
     +		xli1,xli2,xli3,xlo1,xlo2,xlo3,xhill,xlog1,
     +		xlog2p,xx1n,d2,d21,d2n,d21n1,term,tot,xtroep,xli4,
     +		xlo4,som,xmax
	DOUBLE COMPLEX clog1p,clog2p
	DOUBLE PRECISION dfflo1
	DOUBLE COMPLEX zxfflg
	external dfflo1,zxfflg
*
*	common blocks
*
	include 'ff.h'
*  #] declarations:
*  #[ groundwork:
*
*	get the arguments
*
	if ( dyz .eq. 0 ) return
	fact = 1/dyz
	xx1 = y * fact
	xx2 = - y1 * fact
*
*  #] groundwork:
*  #[ which area?:
*
*	determine the area:	1 = [-1+xloss,1/2]
*				2 = (1/2,2-xloss]
*				3 = [2+xloss,->) U (<-,-1-xloss]
*				4 = [-1-xloss,-1+xloss]
*				5 = [2-xloss,2+xloss]
*
	if ( xx1 .lt. -1-xloss/2 ) then
	    iclas1 = 3
	    xx1p = 1/xx1
	elseif( xx1 .lt. -1+xloss/2 ) then
	    if ( ld2yzz ) then
		iclas1 = 4
	    else
		iclas1 = 1
	    endif
	    xx1p = xx1
	elseif( xx1 .le. .5D0 ) then
	    iclas1 = 1
	    xx1p = xx1
	elseif ( xx1 .lt. 2-xloss ) then
	    iclas1 = 2
	    xx1p = -z*fact
	elseif ( ldy2z .and. xx1 .lt. 2+xloss ) then
	    iclas1 = 5
	    xx1p = dy2z(1)*fact
	else
	    iclas1 = 3
	    xx1p = 1/xx1
	endif
	if ( xx2 .lt. -1-xloss/2 ) then
	    iclas2 = 3
	    xx2p = 1/xx2
	elseif( xx2 .lt. -1+xloss/2 ) then
	    if ( ld2yzz ) then
		iclas2 = 4
	    else
		iclas2 = 1
	    endif
	    xx2p = xx2
	elseif ( xx2 .le. .5D0 ) then
	    iclas2 = 1
	    xx2p = xx2
	elseif ( xx2 .lt. 2-xloss ) then
	    iclas2 = 2
	    xx2p = z1*fact
	elseif ( ldy2z .and. xx2 .lt. 2+xloss ) then
	    iclas2 = 5
	    xx2p = -dy2z(3)*fact
	else
	    iclas2 = 3
	    xx2p = 1/xx2
	endif
*
*	throw together if they are close
*
	if ( iclas1 .ne. iclas2 .and. abs(xx1-xx2) .lt. 2*xloss )
     +		then
*	    we don't want trouble with iclasn = 4,5
	    if ( iclas1 .eq. 4 ) then
		iclas1 = 1
	    elseif ( iclas1 .eq. 5 ) then
		iclas1 = 3
		xx1p = 1/xx1
	    endif
	    if ( iclas2 .eq. 4 ) then
		iclas2 = 1
	    elseif ( iclas2 .eq. 5 ) then
		iclas2 = 3
		xx2p = 1/xx2
	    endif
	    if ( iclas1 .eq. iclas2 ) goto 5
*	    go on
	    if ( iclas1 .le. iclas2 ) then
		iclas2 = iclas1
		if ( iclas1 .eq. 1 ) then
		    xx2p = xx2
		else
		    xx2p = z1*fact
		endif
	    else
		iclas1 = iclas2
		if ( iclas1 .eq. 1 ) then
		    xx1p = xx1
		else
		    xx1p = -z*fact
		endif
	    endif
	endif
*  #] which area?:
*  #[ calculations:
    5	if ( iclas1 .eq. iclas2 .and.
     +		abs(xx1p-xx2p) .lt. 2*xloss*max(abs(xx1p),abs(xx2p))
     +		.and. iclas1 .ne. 5 ) then
*		      |----->temporary!
*	    Close together:
* -#[	    handle dilog's:
	    if ( abs(xx2p) .gt. xloss ) then
*--#[		Hill identity:
*
*		Use the Hill identity to get rid of the cancellations.
*
*
*	    first get the arguments:
*
		if ( iclas1 .eq. 1 .or. iclas1 .eq. 4 ) then
		    d2 = 1/y
		    arg2 = 1/z1
		    arg3 = arg2/xx1p
		elseif ( iclas1 .eq. 2 ) then
		    d2 = 1/z
		    arg2 = 1/y1
		    arg3 = arg2/xx1p
		elseif ( iclas1 .eq. 3 ) then
		    d2 = 1/y1
		    arg3 = 1/z1
		    arg2 = arg3*xx1p
		endif
		call ffxli2(xli1,xlo1,d2,ier)
		call ffxli2(xli2,xlo2,arg2,ier)
		call ffxli2(xli3,xlo3,arg3,ier)
		if ( abs(xx2p) .lt. xloss ) then
		    xlog2p = dfflo1(xx2p,ier)
		else
		    xlog2p = DBLE(zxfflg(1-xx2p,0,1D0,ier))
		endif
		xhill = xlo1*xlog2p
*--#]		Hill identity:
	    else
*--#[		Taylor expansion:
*
*		if the points are close to zero do a Taylor
*		expansion of the first and last dilogarithm
*
*			Li2(xx1p) - Li2(xx2p)
*			  = sum xx1p^i ( 1-(1-d2)^i ) /i^2
*
*		with d2 = 1-xx2p/xx1p = ...
*
		if ( iclas1 .eq. 1 .or. iclas1 .eq. 4 ) then
		    d2 = 1/y
		elseif ( iclas1 .eq. 2 ) then
		    d2 = 1/z
		elseif ( iclas1 .eq. 3 ) then
		    d2 = 1/y1
		endif
*		flag to the print section that we did a Taylor expansion
		d21 = 1-d2
		d21n1 = 1
		xx1n = xx1p
		d2n = d2
		tot = xx1p*d2
*		check for possible underflow on the next line
		if ( abs(xx1p) .lt. xalog2 ) goto 51
		do 50 i=2,20
		    xx1n = xx1n*xx1p
		    d21n1 = d21n1*d21
		    d2n = d2n + d2*d21n1
		    term = xx1n*d2n*xn2inv(i)
		    tot = tot + term
		    if ( abs(term) .le. precx*abs(tot) ) goto 51
   50		continue
   51		continue
		xli1 = tot
		xli2 = 0
		xli3 = 0
		xhill = 0
*		for the eta+transformation section we also need
		if ( iclas1 .ne. 1 ) then
		    if ( abs(d2) .lt. xloss ) then
			xlo1 = dfflo1(d2,ier)
		    else
			xlo1 = DBLE(zxfflg(d21,0,1D0,ier))
		    endif
		endif
		if ( iclas1 .eq. 2 ) xlo2 = dfflo1(1/y1,ier)
*--#]		Taylor expansion:
	    endif
*
* -#]	    handle dilog's:
* -#[	    handle transformation terms:
	    if ( iclas1 .eq. 1 .or. iclas1 .eq. 4 ) then
*
*		no transformation was made.
*
*		crr(5) = 0
*		crr(6) = 0
	    elseif ( iclas1 .eq. 2 ) then
*
*		we tranformed to 1-x for both dilogs
*
		if ( abs(xx1p) .lt. xloss ) then
		    xlog1 = dfflo1(xx1p,ier)
		else
		    xlog1 = DBLE(zxfflg(xx1,0,1D0,ier))
		endif
		crr(5) = xlo1*xlog1
		clog2p = zxfflg(xx2p,ieps,-y1,ier)
		crr(6) = -DBLE(xlo2)*clog2p
	    elseif ( iclas1 .eq. 3 ) then
*
*		we transformed to 1/x for both dilogs
*
		clog2p = zxfflg(-xx2p,-ieps,-y1,ier)
		crr(5) = DBLE(xlo1)*(clog2p - DBLE(xlo1)/2)
	    endif
* -#]	    handle transformation terms:
* -#[	    add up and print out:
	    if ( iclas1 .eq. 1 .or. iclas1 .eq. 4 ) then
		crr(1) = xli1
		crr(2) = xli2
		crr(3) = - xli3
		crr(4) = xhill
	    else
		crr(1) = - xli1
		crr(2) = - xli2
		crr(3) = xli3
		crr(4) = - xhill
	    endif
* -#]	    add up and print out:
	else
*	    Normal case:
* -#[	    handle dilogs:
*
*	    the dilogs will not come close together so just go on
*	    only the special case xx1p ~ -1 needs special attention
*	    - and the special case xx1 ~ 2 also needs special attention
*
	    if ( iclas1 .eq. 4 ) then
		d2 = d2yzz + zz
		xmax = abs(d2yzz)
		if ( abs(d2) .lt. xloss*xmax ) then
		    som = y + dyz
		    if ( abs(y).lt.xmax ) then
			d2 = som
			xmax = abs(y)
		    endif
		endif
		d2 = d2/dyz
		fact = 1/(2-d2)
		call ffxli2(xli1,xlo1,d2*fact,ier)
		call ffxli2(xli3,xlo3,-d2*fact,ier)
		call ffxli2(xli4,xlo4,d2,ier)
	    elseif ( iclas1 .eq. 5 ) then
		call ffxl22(xli1,xx1p,ier)
		ipi12 = ipi12 + 3
	    else
		call ffxli2(xli1,xlo1,xx1p,ier)
	    endif
	    if ( iclas2 .eq. 4 ) then
		if ( iclas1 .eq. 4 ) call fferr(26,ier)
		d2 = d2yzz - zz1
		xmax = abs(d2yzz)
		if ( abs(d2) .lt. xloss*xmax ) then
		    som = dyz - y1
		    if ( abs(y1).lt.xmax ) then
			d2 = som
			xmax = abs(y1)
		    endif
		endif
		d2 = d2/dyz
		fact = 1/(2-d2)
		call ffxli2(xli2,xlo2,d2*fact,ier)
		call ffxli2(xli3,xlo3,-d2*fact,ier)
		call ffxli2(xli4,xlo4,d2,ier)
	    elseif ( iclas2 .eq. 5 ) then
		call ffxl22(xli2,xx2p,ier)
		ipi12 = ipi12 - 3
	    else
		call ffxli2(xli2,xlo2,xx2p,ier)
	    endif
* -#]	    handle dilogs:
* -#[	    handle transformation terms xx1:
*
*	    transformation of c1
*
	    if ( iclas1 .eq. 1 ) then
		crr(1) = xli1
	    elseif( iclas1 .eq. 2 ) then
		crr(1) = -xli1
		ipi12 = ipi12 + 2
		clog1p = zxfflg(xx1p,ieps,y,ier)
		crr(3) = - DBLE(xlo1)*clog1p
	    elseif ( iclas1 .eq. 3 ) then
		crr(1) = -xli1
		ipi12 = ipi12 - 2
		clog1p = zxfflg(-xx1p,-ieps,y,ier)
		crr(3) = - clog1p**2/2
	    elseif ( iclas1 .eq. 4 ) then
		crr(1) = xli1
*		Note that this sum does not cause problems as d2<<1
		crr(3) = DBLE(-xli3-xli4) + DBLE(xlo4)*
     +			zxfflg(fact,0,0D0,ier)
		ipi12 = ipi12 - 1
	    elseif ( iclas1 .eq. 5 ) then
		crr(1) = xli1
*		supply an imaginary part
		clog1p = zxfflg(-1/xx1,-ieps,y,ier)
		xtroep = -DIMAG(clog1p)*DBLE(clog1p)
		crr(3) = DCMPLX(0D0,xtroep)
	    else
		call fferr(26,ier)
	    endif
* -#]	    handle transformation terms xx1:
* -#[	    handle transformation terms xx2:
*
*	    transformation of c2
*
	    if ( iclas2 .eq. 1 ) then
		crr(2) = -xli2
	    elseif( iclas2 .eq. 2 ) then
		crr(2) = +xli2
		ipi12 = ipi12 - 2
		clog2p = zxfflg(xx2p,ieps,-y1,ier)
		crr(4) = + DBLE(xlo2)*clog2p
	    elseif ( iclas2 .eq. 3 ) then
		crr(2) = +xli2
		ipi12 = ipi12 + 2
		clog2p = zxfflg(-xx2p,-ieps,-y1,ier)
		crr(4) = clog2p**2/2
	    elseif ( iclas2 .eq. 4 ) then
		crr(2) = -xli2
*		Note that this sum does not cause problems as d2<<1
		crr(4) = DBLE(xli3+xli4) - DBLE(xlo4)*
     +			zxfflg(fact,0,0D0,ier)
		ipi12 = ipi12 + 1
	    elseif ( iclas2 .eq. 5 ) then
		crr(2) = -xli2
*		supply an imaginary part
		clog2p = zxfflg(-1/xx2,-ieps,-y1,ier)
		xtroep = DIMAG(clog2p)*DBLE(clog2p)
		crr(4) = DCMPLX(0D0,xtroep)
	    else
		call fferr(28,ier)
	    endif
* -#]	    handle transformation terms xx2:
	endif
*  #] calculations:
*###] ffcxr:
	end
