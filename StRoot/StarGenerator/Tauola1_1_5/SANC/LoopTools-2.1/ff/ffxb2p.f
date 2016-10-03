*###[ ffxb2p:
	subroutine ffxb2p(cb2i,cb1,cb0,ca0i,xp,xm1,xm2,piDpj,ier)
***#[*comment:***********************************************************
*									*
*	Compute the PV B2, the coefficients of p(mu)p(nu) and g(mu,nu)	*
*	of 1/(ipi^2)\int d^nQ Q(mu)Q(nu)/(Q^2-m_1^2)/((Q+p)^2-m_2^2)	*
*	originally based on aaxbx by Andre Aeppli.			*
*									*
*	Input:	cb1	   complex	vector two point function	*
*		cb0	   complex	scalar two point function	*
*		ca0i(2)	   complex	scalar onepoint function with	*
*						m1,m2			*
*		xp	   real		p.p in B&D metric		*
*		xm1,2	   real		m_1^2,m_2^2			*
*		piDpj(3,3) real		dotproducts between s1,s2,p	*
*		ier	   integer	digits lost so far		*
*									*
*	Output:	cb2i(2)	   complex	B21,B22: coeffs of p*p, g in B2	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE PRECISION xp,xm1,xm2,piDpj(3,3)
	DOUBLE COMPLEX cb2i(2),cb1,cb0,ca0i(2)
*
*	local variables
*
	DOUBLE PRECISION dm1m2
*
*  #] declarations:
*  #[ work:
*
	dm1m2= xm1 - xm2
	call ffxb2q(cb2i,cb1,cb0,ca0i,xp,xm1,xm2,dm1m2,piDpj,ier)
*
*  #] work:
*###] ffxb2p:
	end


*###[ ffxb2q:
	subroutine ffxb2q(cb2i,cb1,cb0,ca0i,xp,xm1,xm2,dm1m2,piDpj,ier)
***#[*comment:***********************************************************
*									*
*	Compute the PV B2, the coefficients of p(mu)p(nu) and g(mu,nu)	*
*	of 1/(ipi^2)\int d^nQ Q(mu)Q(nu)/(Q^2-m_1^2)/((Q+p)^2-m_2^2)	*
*	originally based on aaxbx by Andre Aeppli.			*
*									*
*	Input:	cb1	   complex	vector two point function	*
*		cb0	   complex	scalar two point function	*
*		ca0i(2)	   complex	scalar onepoint function with	*
*						m1,m2			*
*		xp	   real		p.p in B&D metric		*
*		xm1,2	   real		m_1^2,m_2^2			*
*		piDpj(3,3) real		dotproducts between s1,s2,p	*
*		ier	   integer	digits lost so far		*
*									*
*	Output:	cb2i(2)	   complex	B21,B22: coeffs of p*p, g in B2	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE PRECISION xp,xm1,xm2,dm1m2,piDpj(3,3)
	DOUBLE COMPLEX cb2i(2),cb1,cb0,ca0i(2)
*
*	local variables
*
	integer i,ier0,ier1
	logical llogmm
	DOUBLE PRECISION xmax,absc,xlam,slam,bet,xmxp,dfflo3,xlo3,
     +		xmxsav,xnoe,xnoe2,xlogmm,dfflo1
	DOUBLE COMPLEX cs(16),cc,csom,clo3,zfflo3
	external dfflo1,dfflo3,zfflo3
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(cc) = abs(DBLE(cc)) + abs(DIMAG(cc))
*
*  #] declarations:
*  #[ normal case:
	ier0 = ier
	ier1 = ier
*
*	with thanks to Andre Aeppli, off whom I stole the original
*
	if ( xp .ne. 0) then
	    cs(1) = ca0i(2)
	    cs(2) = DBLE(xm1)*cb0
	    cs(3) = DBLE(2*piDpj(1,3))*cb1
	    cs(4) = (xm1+xm2)/2
	    cs(5) = -xp/6
	    cb2i(1) = cs(1) - cs(2) + 2*cs(3) - cs(4) - cs(5)
	    cb2i(2) = cs(1) + 2*cs(2) - cs(3) + 2*cs(4) + 2*cs(5)
	    xmax = max(absc(cs(2)),absc(cs(3)),absc(cs(4)),absc(cs(5)))
	    xmxsav = xmax
	    if ( absc(cb2i(1)) .ge. xloss*xmax ) goto 100
*  #] normal case:
*  #[   improve: m1=m2:
*
*	    a relatively simple case: dm1m2 = 0 (bi0.frm)
*
	    if ( dm1m2.eq.0 .and. xm1.ne.0 ) then
		if ( xp.lt.0 ) then
		    slam = sqrt(xp**2-4*xm1*xp)
		    xlo3 = dfflo3((xp-slam)/(2*xm1),ier)
		    cs(1) = xp*(-1/DBLE(3) + slam/(4*xm1))
		    cs(2) = xp**2*(-slam/(4*xm1**2) - 3/(4*xm1))
		    cs(3) = xp**3/(4*xm1**2)
		    cs(4) = DBLE(xp/xm1)*ca0i(1)
		    cs(5) = xlo3/xp*(-xm1*slam)
		    cs(6) = xlo3*slam
		else
		    slam = isgnal*sqrt(-xp**2+4*xm1*xp)
		    clo3 = zfflo3(DCMPLX(DBLE(xp/(2*xm1)),
     +			DBLE(-slam/(2*xm1))),ier)
		    cs(1) = DBLE(xp)*DCMPLX(-1/DBLE(3),
     +			DBLE(slam/(4*xm1)))
		    cs(2) = DBLE(xp**2)*DCMPLX(DBLE(-3/(4*xm1)),
     +			DBLE(-slam/(4*xm1**2)))
		    cs(3) = DBLE(xp**3/(4*xm1**2))
		    cs(4) = DBLE(xp/xm1)*ca0i(1)
		    cs(5) = clo3*DCMPLX(DBLE(0),DBLE(-xm1*slam/xp))
		    cs(6) = clo3*DCMPLX(DBLE(0),DBLE(slam))
		endif
		csom = cs(1) + cs(2) + cs(3) + cs(4) + cs(5) + cs(6)
		xmxp = max(absc(cs(2)),absc(cs(3)),absc(cs(4)),
     +			absc(cs(5)),absc(cs(6)))
*		
*		get rid of noise in the imaginary part
*	
		if ( xloss*abs(DIMAG(csom)).lt.precc*abs(DBLE(csom)) ) 
     +			csom = DCMPLX(DBLE(csom),DBLE(0))
		if ( xmxp.lt.xmax ) then
		    cb2i(1) = csom
		    xmax = xmxp
		endif
		if ( absc(cb2i(1)).ge.xloss**2*xmax ) goto 100
	    endif
*  #]   improve: m1=m2:
*  #[   improve: |xp| < xm1 < xm2:
*
*	    try again (see bi.frm)
*
	    xlam =  4*(piDpj(1,3)**2 - xm1*xp)
	    if ( xm1.eq.0 .or. xm2.eq.0 ) then
	    	xlogmm = 0
	    elseif ( abs(dm1m2).lt.xloss*xm1 ) then
	    	xlogmm = dfflo1(dm1m2/xm1,ier)
	    else
	    	xlogmm = log(xm2/xm1)
	    endif
	    if ( xlam.gt.0 .and. abs(xp).lt.xloss*xm2 .and.
     +			xm1.lt.xm2 ) then
		slam = sqrt(xlam)
		bet = 4*xm1*xp/(2*piDpj(1,3)+slam)
		cs(1) = DBLE(xp/xm2)*ca0i(2)
		cs(2) = xlogmm*bet*(-2*xm1**2*xm2 - 2*xm1**3)
     +		/((-dm1m2+slam)*(2*piDpj(1,2)+slam)*(2*piDpj(1,3)+slam))
		cs(3) = xlogmm*(-4*xp*xm1**3)
     +		/((-dm1m2+slam)*(2*piDpj(1,2)+slam)*(2*piDpj(1,3)+slam))
		xnoe = 1/(2*piDpj(2,3)+slam)
		xnoe2 = xnoe**2
		cs(4) = xnoe2*xm1*bet*(xp-4*xm2)
		cs(5) = xnoe2*xm1*2*xp*xm2
		cs(6) = xnoe2*xm1**2*bet
		cs(7) = xnoe2*xm1**2*4*xp
		cs(8) = xnoe2*bet*(xp*xm2+3*xm2**2)
		cs(9) = xnoe2*(-6*xp*xm2**2)
		cs(10)= xp*(7/6.d0 - 2*xm1*slam*xnoe2 +
     +			4*xm2*slam*xnoe2 - 2*slam*xnoe)
		cs(11)= xp**2*( -2*slam*xnoe2 )
		xlo3 = dfflo3(2*xp*xnoe,ier)
		cs(12) = xlo3*dm1m2**2*slam/xp**2
		cs(13) = xlo3*(xm1 - 2*xm2)*slam/xp
		cs(14) = xlo3*slam
		csom = 0
		xmxp = 0
		do 50 i=1,14
		    csom = csom + cs(i)
		    xmxp = max(xmxp,absc(cs(i)))
   50		continue
		if ( xmxp.lt.xmax ) then
		    cb2i(1) = csom
		    xmax = xmxp
		endif
		if ( absc(cb2i(1)).ge.xloss**2*xmax ) goto 100
	    endif
*  #]   improve: |xp| < xm1 < xm2:
*  #[   improve: |xp| < xm2 < xm1:
	    if ( xlam.gt.0 .and. abs(xp).lt.xloss*xm1 .and.
     +			xm2.lt.xm1 ) then
		slam = sqrt(xlam)
		bet = 4*xm2*xp/(-2*piDpj(2,3)+slam)
		xnoe = 1/(-2*piDpj(1,3)+slam)
		xnoe2 = xnoe**2
		cs(1) = DBLE(xp/xm1)*ca0i(1)
		cs(2) = -xlogmm*bet*(12*xp*xm1*xm2+6*xp*xm2**2-
     +		6*xp**2*xm2-2*xm1*xm2**2-2*xm2**3)
     +		/((dm1m2+slam)*(2*piDpj(1,2)+slam)*(-2*piDpj(2,3)+slam))
		cs(3) = -xlogmm*(-24*xp*xm1**2*xm2-4*xp*xm2**3+36*
     +		xp**2*xm1*xm2+12*xp**2*xm2**2-12*xp**3*xm2)
     +		/((dm1m2+slam)*(2*piDpj(1,2)+slam)*(-2*piDpj(2,3)+slam))
		cs(4) = xnoe2*xm2*bet*(xp-4*xm1)
		cs(5) = xnoe2*xm2*(-10*xp*xm1)
		cs(6) = xnoe2*xm2**2*bet
		cs(7) = xnoe2*xm2**2*4*xp
		cs(8) = xnoe2*bet*(xp*xm1+3*xm1**2)
		cs(9) = xnoe2*6*xp*xm1**2
		cs(10)= xp*(7/6.d0 - 2*xm1*slam*xnoe2 +
     +			4*xm2*slam*xnoe2 - 2*slam*xnoe)
		cs(11)= xp**2*( -2*slam*xnoe2 )
		xlo3 = dfflo3(2*xp*xnoe,ier)
		cs(12) = xlo3*dm1m2**2*slam/xp**2
		cs(13) = xlo3*(xm1 - 2*xm2)*slam/xp
		cs(14) = xlo3*slam
		csom = 0
		xmxp = 0
		do 60 i=1,14
		    csom = csom + cs(i)
		    xmxp = max(xmxp,absc(cs(i)))
   60		continue
		if ( xmxp.lt.xmax ) then
		    cb2i(1) = csom
		    xmax = xmxp
		endif
		if ( absc(cb2i(1)).ge.xloss**2*xmax ) goto 100
	    endif
*  #]   improve: |xp| < xm2 < xm1:
*  #[   wrap up:
  100	    continue
	    xmax = xmxsav
	    cb2i(1) = DBLE(1/(3*xp)) * cb2i(1)
	    cb2i(2) = DBLE(1/6.d0)   * cb2i(2)
*  #]   wrap up:
*  #[ xp=0, m1!=m2:
	elseif (dm1m2 .ne. 0) then
*		#[ B21:
		llogmm = .FALSE.
*
*		B21 (see thesis, b21.frm)
*
		cs(1) = DBLE(xm1**2/3/dm1m2**3)*ca0i(1)
		cs(2) = DBLE((-xm1**2 + xm1*xm2 - xm2**2/3)/dm1m2**3)*
     +			ca0i(2)
		cs(3) = (5*xm1**3/18 - xm1*xm2**2/2 + 2*xm2**3/9)
     +			/dm1m2**3
		cb2i(1) = cs(1)+cs(2)+cs(3)
		xmax = max(absc(cs(2)),absc(cs(3)))
		if ( absc(cb2i(1)).gt.xloss**2*xmax ) goto 160
*
*		ma ~ mb
*
		if ( abs(dm1m2).lt.xloss*xm1 ) then
		    xlogmm = dfflo1(dm1m2/xm1,ier)
		else
		    xlogmm = log(xm2/xm1)
		endif
		llogmm = .TRUE.
		cs(1) = (xm1/dm1m2)/6
		cs(2) = (xm1/dm1m2)**2/3
		cs(3) = (xm1/dm1m2)**3*xlogmm/3
		cs(4) = -2/DBLE(9) + ca0i(1)*DBLE(1/(3*xm1))
		cs(5) = -xlogmm/3
		csom = cs(1)+cs(2)+cs(3)+cs(4)+cs(5)
		xmxp = max(absc(cs(2)),absc(cs(3)),absc(cs(4)),
     +			absc(cs(5)))
		if ( xmxp.lt.xmax ) then
		    xmax = xmxp
		    cb2i(1) = csom
		    if ( absc(cb2i(1)).gt.xloss**2*xmax ) goto 160
		endif
*
*		and last try
*
		xlo3 = dfflo3(dm1m2/xm1,ier)
		cs(1) = (dm1m2/xm1)**2/6
		cs(2) = (dm1m2/xm1)/3
		cs(3) = xlo3/(3*(dm1m2/xm1)**3)
*same		cs(4) = -2/DBLE(9) + ca0i(1)*DBLE(1/(3*xm1))
		cs(5) = -xlo3/3
		csom = cs(1)+cs(2)+cs(3)+cs(4)+cs(5)
		xmxp = max(absc(cs(2)),absc(cs(3)),absc(cs(4)),
     +			absc(cs(5)))
		if ( xmxp.lt.xmax ) then
		    xmax = xmxp
		    cb2i(1) = csom
		    if ( absc(cb2i(1)).gt.xloss**2*xmax ) goto 160
		endif
*
*		give up
*
  160		continue
*		#] B21:
*		#[ B22:
*
*		B22
*
		cs(1) = +DBLE(xm1/(4*dm1m2))*ca0i(1)
		cs(2) = -DBLE(xm2/(4*dm1m2))*ca0i(2)
		cs(3) = (xm1+xm2)/8
		cb2i(2) = cs(1) + cs(2) + cs(3)
		xmax = max(absc(cs(2)),absc(cs(3)))
		if ( absc(cb2i(2)).gt.xloss*xmax ) goto 210
*
*		second try, close together
*
		if ( .not.llogmm ) then
		    if ( abs(dm1m2).lt.xloss*xm1 ) then
		    	xlogmm = dfflo1(dm1m2/xm1,ier)
		    else
		    	xlogmm = log(xm2/xm1)
		    endif
		endif
		cs(1) = dm1m2*( -1/DBLE(8) - ca0i(1)*DBLE(1/(4*xm1)) )
		cs(2) = dm1m2*xlogmm/4
		cs(3) = xm1*(xm1/dm1m2)/4*xlogmm
		cs(4) = xm1*( 1/DBLE(4) + ca0i(1)*DBLE(1/(2*xm1)) )
		cs(5) = -xm1*xlogmm/2
		csom = cs(1) + cs(2) + cs(3) + cs(4) + cs(5)
		xmxp = max(absc(cs(2)),absc(cs(3)),absc(cs(4)),
     +			absc(cs(5))) 
		if ( xmxp.lt.xmax ) then
		    xmax = xmxp
		    cb2i(2) = csom
		endif
		if ( absc(cb2i(2)).gt.xloss*xmax ) goto 210
*
*		give up
*
  210		continue
*		#] B22:
*  #] xp=0, m1!=m2:
*  #[ xp=0, m1==m2:
	else
*
*	    taken over from ffxb2a, which in turns stem from my thesis GJ
*
	    cb2i(1) = cb0/3
	    cb2i(2) = DBLE(xm1/2)*(cb0 + 1)
	endif
*  #] xp=0, m1==m2:
*  #[ finish up:
	ier = max(ier0,ier1)
*  #] finish up:
*###] ffxb2q:
	end
