*###[ ffxb1:
	subroutine ffxb1(cb1,cb0,ca0i,xp,xm1,xm2,piDpj,ier)
***#[*comment:***********************************************************
*									*
*	Calculate   1	/	   d^n Q Q(mu)				*
*		 ------	| ------------------------ = B1*p(mu)		*
*		 i pi^2	/ (Q^2-m1^2)((Q+p)^2-m2^2)			*
*									*
*	Input:	cb0	   complex	scalar twopoint function	*
*		ca0i(2)	   complex	scalar onepoint function with	*
*						m1,m2			*
*		xp	   real		p.p in B&D metric		*
*		xm1,2	   real		m_1^2,m_2^2			*
*		piDpj(3,3) real		dotproducts between s1,s2,p	*
*		ier	   integer	digits lost so far		*
*	Output:	cb1	   complex	B1				*
*		ier	   integer	digits lost			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE PRECISION xp,xm1,xm2,piDpj(3,3)
	DOUBLE COMPLEX cb1,cb0,ca0i(2)
*
*	local variables
*
	DOUBLE PRECISION dm1m2
*
*	common blocks
*
	include 'ff.h'
*
*  #] declarations:
*  #[ get differences:
	dm1m2 = xm1 - xm2
*  #] get differences:
*  #[ call ffxb1a:
	call ffxb1a(cb1,cb0,ca0i,xp,xm1,xm2,dm1m2,piDpj,ier)
*  #] call ffxb1a:
*###] ffxb1:
	end


*###[ ffxb1a:
	subroutine ffxb1a(cb1,cb0,ca0i,xp,xm1,xm2,dm1m2,piDpj,ier)
***#[*comment:***********************************************************
*									*
*	Calculate   1	/	   d^n Q Q(mu)				*
*		 ------	| ------------------------ = B1*p(mu)		*
*		 i pi^2	/ (Q^2-m1^2)((Q+p)^2-m2^2)			*
*									*
*	Input:	cb0	   complex	scalar twopoint function	*
*		ca0i(2)	   complex	scalar onepoint function with	*
*						m1,m2			*
*		xp	   real		p.p in B&D metric		*
*		xm1,2	   real		m_1^2,m_2^2			*
*		piDpj(3,3) real		dotproducts between s1,s2,p	*
*		ier	   integer	digits lost so far		*
*	Output:	cb1	   complex	B1				*
*		ier	   integer	digits lost			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE PRECISION xp,xm1,xm2,dm1m2,piDpj(3,3)
	DOUBLE COMPLEX cb1,cb0,ca0i(2)
*
*	local variables
*
	logical lneg
	DOUBLE PRECISION xmax,absc,s,s1,h,slam,bnd101,bnd105,bnd110,
     +		xma,xmb,x,ax,xlogm,small,dmbma,xprec,xlam,ts2Dp,
     +		xmxp,xlo3,dfflo3
	DOUBLE COMPLEX cs(5),cc,csom
	DOUBLE PRECISION ffbnd,dfflo1
	external ffbnd,dfflo1,dfflo3
	save xprec,bnd101,bnd105,bnd110
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(cc) = abs(DBLE(cc)) + abs(DIMAG(cc))
*
*	data
*
	data xprec /0D0/
*
*  #] declarations:
*  #[ p^2 != 0:
	if ( xp .ne. 0 ) then
* 	#[ normal case:
	    if ( dm1m2 .ne. 0 ) then
		cs(1) = -ca0i(2)
		cs(2) = +ca0i(1)
	    else
		cs(1) = 0
		cs(2) = 0
	    endif
	    cs(3) = +DBLE(2*piDpj(1,3))*cb0
	    cb1 = cs(1) + cs(2) + cs(3)
	    xmax = max(absc(cs(2)),absc(cs(3)))
	    if ( absc(cb1) .ge. xloss*xmax ) goto 110
* 	#] normal case:
* 	#[ almost equal masses:
	    if ( abs(dm1m2) .le. xloss*xm1 ) then
		cs(2) = DBLE(dm1m2/xm1)*cs(2)
		cs(1) = -xm2*dfflo1(-dm1m2/xm2,ier)
		cb1 = cs(1) + cs(2) + cs(3)
		xmax = max(absc(cs(2)),absc(cs(3)))
		if ( absc(cb1) .ge. xloss*xmax ) goto 110
*		for the perfectionist (not me (today)):
*		if d0=0 and mu~m1(~m2), then the terms of order
*		(m1^2-m2^2) also cancel.  To patch this I need d0 and mu
	    endif
* 	#] almost equal masses:
* 	#[ p2 -> 0:
	    if ( xloss**2*max(xm1,xm2) .gt. abs(xp) ) then
		if ( xm2.gt.xm1 ) then
		    xma = xm1
		    xmb = xm2
		    ts2Dp = +2*piDpj(2,3)
		    lneg = .FALSE.
		else
		    xma = xm2
		    xmb = xm1
		    ts2Dp = -2*piDpj(1,3)
		    lneg = .TRUE.
		endif
	    else
		goto 100
	    endif
*
*	    We found a situation in which p2 is much smaller than
*	    the masses.
*
	    dmbma = abs(dm1m2)
	    if ( xma.eq.0 ) then
		xlogm = 1
	    elseif ( dmbma .gt. xloss*xmb ) then
		xlogm = log(xmb/xma)
	    else
		xlogm = dfflo1(-dmbma/xma,ier)
	    endif
	    xlam =  (dmbma-xp)**2 - 4*xma*xp
	    if ( xlam.gt.0 ) then
*		#[ real roots:
		slam = sqrt(xlam)
		small = xp*(-2*(xma+xmb) + xp)/(slam+dmbma)
		h = slam+2*piDpj(1,2)
		cs(1) = xlogm*xma*(4*xmb*(small-xp) + (small-xp)**2)/(2*
     +			(slam+dmbma)*h)
		if ( xprec.ne.precx ) then
		    xprec = precx
		    bnd101 = ffbnd(2,1,xinfac)
		    bnd105 = ffbnd(2,5,xinfac)
		    bnd110 = ffbnd(2,10,xinfac)
		endif
		x = xp/slam
		ax = abs(x)
		if ( ax.gt.bnd110 ) then
		    s = x*(xinfac(12) + x*(xinfac(13) + x*(xinfac(14) +
     +			x*(xinfac(15) + x*xinfac(16) ))))
		else
		    s = 0
		endif
		if ( ax.gt.bnd105 ) then
		    s = x*(xinfac(7) + x*(xinfac(8) + x*(xinfac(9) +
     +			x*(xinfac(10) + x*(xinfac(11) + s )))))
		endif
		if ( ax.gt.bnd101) then
		    s = x*(xinfac(3) + x*(xinfac(4) + x*(xinfac(5) +
     +			x*(xinfac(6) + s ))))
		endif
		s = x**2*(.5D0 + s)
		h = ts2Dp + slam
		s1 = 2*xp/h*(s + x)
		h = -4*xp**2*xmb/(slam*h**2) - s + s1
		if ( abs(h) .lt. .1 ) then
		    cs(2) = dmbma*slam/xp*dfflo1(h,ier)
		else
		    goto 100
		endif
		if ( lneg ) then
		    cs(1) = -cs(1)
		    cs(2) = -cs(2)
		endif
		cs(3) = -DBLE(xp)*cb0
		cb1 = cs(1) + cs(2) + cs(3)
		xmax = max(absc(cs(2)),absc(cs(3)))
		if ( absc(cb1) .gt. xloss*xmax) goto 110
*
*		this still occurs in the case xp << dmamb << xma,
*		with a cancellation of order dmamb/xma between cs1 and
*		cs2; as the standard model does not contain these kind
*		of doublets I leave this as an exercise for the
*		reader...
*
*		#] real roots:
	    else
*		#[ imaginary roots:
*		#] imaginary roots:
	    endif
* 	#] p2 -> 0:
* 	#[ give up:
*
*	    give up...
*
  100	    continue
  110	    continue
* 	#] give up:
	    cb1 = cb1*(1/DBLE(2*xp))
*  #] p^2 != 0:
*  #[ p^2=0, m1 != m2:
	elseif ( dm1m2 .ne. 0 ) then
	    cs(1) = +DBLE(xm2/(2*dm1m2**2))*(ca0i(2)+DBLE(xm2)/2)
	    cs(2) = -DBLE(xm1/(2*dm1m2**2))*(ca0i(1)+DBLE(xm1)/2)
	    cs(3) = +ca0i(2)*(1/DBLE(dm1m2))
	    cb1 = cs(1) + cs(2) + cs(3)
	    xmax = max(absc(cs(1)),absc(cs(2)),absc(cs(3)))
	    if ( absc(cb1).ge.xloss**2*xmax ) goto 120
*
*	    m1 ~ m2, see b21.frm
*
	    if ( abs(dm1m2).lt.xloss*xm1 ) then
		xlogm = dfflo1(dm1m2/xm1,ier)
	    else
		xlogm = log(xm2/xm1)
	    endif
	    cs(1) = -(xm1/dm1m2)/2
	    cs(2) = -xlogm/2*(xm1/dm1m2)**2
	    cs(3) = +1/DBLE(4) - ca0i(1)*DBLE(1/(2*xm1))
	    cs(4) = xlogm/2
	    csom = cs(1) + cs(2) + cs(3) + cs(4)
	    xmxp = max(absc(cs(2)),absc(cs(3)),absc(cs(4)))
	    if ( xmxp.lt.xmax ) then
		xmax = xmxp
		cb1 = csom
		if ( absc(cb1).gt.xloss**2*xmax ) goto 120
	    endif
*
*	    better
*
	    xlo3 = dfflo3(dm1m2/xm1,ier)
	    cs(1) = -(dm1m2/xm1)**2/4
	    cs(2) = -(dm1m2/xm1)/2
	    cs(3) = -xlo3/(dm1m2/xm1)**2/2
	    cs(4) = xlo3/2
	    cs(5) = 1/DBLE(2) - ca0i(1)*DBLE(1/(2*xm1))
	    csom = cs(1) + cs(2) + cs(3) + cs(4) + cs(5)
	    xmxp = max(absc(cs(2)),absc(cs(3)),absc(cs(4)),absc(cs(5)))
	    if ( xmxp.lt.xmax ) then
		xmax = xmxp
		cb1 = csom
		if ( absc(cb1).gt.xloss**2*xmax ) goto 120
	    endif
*
*	    give up
*
  120	    continue
*  #] p^2=0, m1 != m2:
*  #[ p^2=0, m1 == m2:
	else
	    cb1 = -cb0/2
	endif
*  #] p^2=0, m1 == m2:
*###] ffxb1a:
	end
