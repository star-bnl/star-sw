*###[ ffdcc0:
	subroutine ffdcc0(cs3,ipi12,isoort,clogi,ilogi,xpi,piDpj,
     +		xqi,qiDqj,sdel2,del2s,etalam,etami,delpsi,alph,
     +		ddel2s,ldel2s,npoin,ier)
***#[*comment:***********************************************************
*									*
*	Calculates the difference of two threepoint functions		*
*	C(3,...a) - C(4,...b)						*
*									*
*	Input:	xpi(6,3:4)    (complex)	transformed mi,pi squared in Ci	*
*		piDpj(6,6,3:4)(complex)	pi(i).pi(j)			*
*		xqi(10,10)    (complex)	transformed mi,pi squared in D	*
*		qiDqj(10,10)  (complex)	qi(i).qi(j)			*
*		sdel2	      (complex)	sqrt(delta_{p_1 p_2}^{p_1 p_2})	*
*		del2s(3,3:4)  (complex)	delta_{p_i s_i}^{p_i s_i}	*
*		etalam(3:4)   (complex)	delta_{s_1 s_2 s_3}^{s_1 s_2 s_3}
*					  /delta_{p_1 p_2}^{p_1 p_2}	*
*		etami(6,3:4)  (complex)	m_i^2 - etalam			*
*		ddel2s(2:3)   (complex)	del2s(i,3) - del2s(i,4)		*
*		alph(3)	      (complex)	alph(1)=alpha, alph(3)=1-alpha	*
*									*
*	Output: cs3	 (complex)(160)	C0(3)-C0(4), not yet summed.	*
*		ipi12	 (integer)(6)	factors pi^2/12, not yet summed	*
*		slam	 (complex)	lambda(p1,p2,p3).		*
*		isoort	 (integer)(16)	indication of he method used	*
*		clogi	 (complex)(6)	log(-dyz(2,1,i)/dyz(2,2,i))	*
*		ilogi	 (integer)(6)	factors i*pi in this		*
*		ier	 (integer)	0=ok, 1=inaccurate, 2=error	*
*									*
*	Calls:	...							*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer ipi12(16),isoort(16),ilogi(6),npoin,ier
	logical ldel2s
	DOUBLE COMPLEX cs3(160),clogi(6)
	DOUBLE COMPLEX xqi(10),qiDqj(10,10),
     +		xpi(6,3:4),piDpj(6,6,3:4),
     +		sdel2,del2s(3,3:4),etalam(3:4),etami(6,3:4),alph(3),
     +		ddel2s(2:3),delpsi(3,3:4)
*
*	local variables:
*
	integer i,j,k,ip,ii,ifirst,ieri(8)
	DOUBLE COMPLEX c,cc
	DOUBLE COMPLEX sdel2i(3,3:4),s(5),som,zfflo1,
     +		y(4,3:4,3),z(4,3:4,3),dyz(2,2,3:4,3),d2yzz(3:4,3),
     +		dyzzy(4,3),dsdel2,dyyzz(2,3)
	DOUBLE PRECISION smax,absc,xmax
	DOUBLE COMPLEX zfflog
	external zfflo1,zfflog
*
*	common blocks:
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*  #] declarations:
*  #[   get y,z-roots:
	do 20 k=3,4
	do 10 i=1,3
*
*	get roots (y,z)
*
	    ip = i+3
	    sdel2i(i,k) = sqrt(-del2s(i,k))
*	    then handle the special case Si = 0
	    if ( xpi(ip,k) .eq. 0 ) then
		if ( i .eq. 1 .and. alph(3) .eq. 0 .or.
     +		     i .eq. 3 .and. alph(1) .eq. 0 ) then
		    isoort(2*i-1+8*(k-3)) = 0
		    isoort(2*i+8*(k-3)) = 0
		    goto 10
		endif
	    endif
	    call ffccyz(y(1,k,i),z(1,k,i),dyz(1,1,k,i),d2yzz(k,i),i,
     +		sdel2,sdel2i(i,k),etalam(k),etami(1,k),delpsi(i,k),
     +		xpi(1,k),piDpj(1,1,k),isoort(2*i-1+8*(k-3)),ier)
   10	continue
   20	continue
*  #]   get y,z-roots:
*  #[   get differences:
*
*	the only important differences are y4z3-z3y4 and (1-y4)(1-z3)-
*	(1-y3)(1-z4).  Note that the errors work in parallel.
*
	do 199 i=1,8
	    ieri(i) = 0
  199	continue
	if ( isoort(1) .eq. isoort(9) ) then
*  #[	    vertices (1):
	    som = qiDqj(7,2)/sdel2
*
*	    flag if we have a cancellation
*
	    if ( absc(som) .lt. xloss ) then
		    isoort(1) = isoort(1) - 10
		    isoort(9) = isoort(9) - 10
	    endif
	    do 201 k=1,4
		dyzzy(k,1) = som*z(k,3,1)
		if ( k .gt. 2 ) dyzzy(k,1) = -dyzzy(k,1)
  201	    continue
	    dyyzz(1,1) = som
	    dyyzz(2,1) = som
*  #]	    vertices (1):
	endif
	if ( isoort(3) .eq. isoort(11) ) then
*  #[       vertices (2):
	    ifirst = 0
	    do 22 j=1,2
	    do 21 k=1,2
		ii = 2*(j-1) + k
		dyzzy(ii,2) = y(2*j,4,2)*z(ii,3,2)-y(2*j,3,2)*z(ii,4,2)
		xmax = absc(y(2*j,4,2)*z(ii,3,2))
		if ( absc(dyzzy(ii,2)) .ge. xmax ) goto 21
		isoort(3) = isoort(3) - 10
		isoort(11) = isoort(11) - 10
		if ( ifirst .eq. 0 ) then
		    if ( ddel2s(2) .eq. 0 ) then
			dsdel2 = 0
		    else
			dsdel2 = ddel2s(2)/(sdel2i(2,3)+sdel2i(2,4))
		    endif
		endif
		if ( ifirst .le. 1 ) then
		    if ( j .eq. 1 ) then
			s(1) = xqi(6)*qiDqj(7,4)*qiDqj(5,4)/sdel2
			s(2) = -qiDqj(7,4)*sdel2i(2,3)
			s(3) = +qiDqj(6,4)*dsdel2
		    else
			s(1) = xqi(6)*qiDqj(7,2)*qiDqj(5,2)/sdel2
			s(2) = -qiDqj(7,2)*sdel2i(2,3)
			s(3) = +qiDqj(6,2)*dsdel2
		    endif
		endif
		if ( ifirst .le. 0 ) then
		    ifirst = 2
		    s(4) = -qiDqj(5,10)*qiDqj(7,4)*sdel2i(2,3)/sdel2
		    s(5) = delpsi(2,3)*dsdel2/sdel2
		endif
		if ( k .eq. 1 ) then
		    som = s(1) + s(2) + s(3) + s(4) + s(5)
		else
		    som = s(1) - s(2) - s(3) - s(4) - s(5)
		endif
		smax = max(absc(s(1)),absc(s(2)),absc(s(3)),absc(s(4)),
     +			absc(s(5)))/DBLE(xqi(6))**2
		if ( smax .lt. xmax ) then
		    dyzzy(ii,2) = som*(1/DBLE(xqi(6))**2)
		    xmax = smax
		endif
   21	    continue
*
*	    get dyyzz
*
	    if ( ldel2s ) then
		dyyzz(j,2) = dyz(2,j,4,2) - dyz(2,j,3,2)
		xmax = absc(dyz(2,j,4,2))
		if ( absc(dyyzz(j,2)) .ge. xloss*xmax ) goto 22
 1002		format(a,i1,a,2g22.14,g12.4)
		print *,'ffdcc0: under construction!'
*
*		(could be copied from real case)
*
	    endif
*
*	    bookkeeping
*
	    ifirst = ifirst - 1
   22	    continue
*  #]       vertices (2):
	endif
	if ( isoort(5) .eq. isoort(13) ) then
*  #[       vertices (3):
	    ifirst = 0
	    do 26 j=1,2
	    do 25 k=1,2
		ii = 2*(j-1) + k
		dyzzy(ii,3) = y(2*j,4,3)*z(ii,3,3)-y(2*j,3,3)*z(ii,4,3)
		xmax = absc(y(2*j,4,3)*z(ii,3,3))
		if ( absc(dyzzy(ii,3)) .ge. xmax ) goto 25
		isoort(5) = isoort(5) - 10
		isoort(13) = isoort(13) - 10
		if ( ifirst .eq. 0 ) then
		    if ( ddel2s(2) .eq. 0 ) then
			dsdel2 = 0
		    else
			dsdel2 = ddel2s(3)/(sdel2i(3,3)+sdel2i(3,4))
		    endif
		endif
		if ( ifirst .le. 1 ) then
		    if ( j .eq. 1 ) then
			s(1) = xqi(8)*qiDqj(7,1)*qiDqj(5,1)/sdel2
			s(2) = +qiDqj(7,1)*sdel2i(3,3)
			s(3) = +qiDqj(9,1)*dsdel2
		    else
			s(1) = xqi(8)*qiDqj(7,4)*qiDqj(5,4)/sdel2
			s(2) = +qiDqj(7,4)*sdel2i(3,3)
			s(3) = +qiDqj(9,4)*dsdel2
		    endif
		endif
		if ( ifirst .le. 0 ) then
		    ifirst = 2
		    s(4) = -qiDqj(5,9)*qiDqj(7,1)*sdel2i(3,3)/sdel2
		    s(5) = delpsi(3,3)*dsdel2/sdel2
		endif
		if ( k .eq. 1 ) then
		    som = s(1) + s(2) + s(3) + s(4) + s(5)
		else
		    som = s(1) - s(2) - s(3) - s(4) - s(5)
		endif
		smax = max(absc(s(1)),absc(s(2)),absc(s(3)),absc(s(4)),
     +			absc(s(5)))/DBLE(xqi(8))**2
		if ( smax .lt. xmax ) then
		    dyzzy(ii,3) = som*(1/DBLE(xqi(8))**2)
		    xmax = smax
		endif
   25	    continue
*
*	    get dyyzz
*
	    if ( ldel2s ) then
		dyyzz(j,3) = dyz(2,j,4,3) - dyz(2,j,3,3)
		xmax = absc(dyz(2,j,4,3))
		if ( absc(dyyzz(j,3)) .ge. xloss*xmax ) goto 24
		print *,'ffdcc0: under construction!'
*
*		(could be copied from real case)
*
	    endif
*
*	    bookkeeping
*
   24	    continue
	    ifirst = ifirst - 1
   26	    continue
*  #]       vertices (3):
	endif
	ier = ier + max(ieri(1),ieri(2),ieri(3),ieri(4),ieri(5),ieri(6),
     +							ieri(7),ieri(8))
*  #]   get differences:
*  #[ logarithms for 4point function:
	if ( npoin .eq. 4 ) then
	do 96 k = 3,4
	do 95 i = 1,3
	    ii = i+3*(k-3)
	    if ( ilogi(ii) .ne. -999 ) goto 95
	    if ( isoort(2*i+8*(k-3)) .ne. 0 ) then
*		maybe add sophisticated factors i*pi later
		c = -dyz(2,1,i,k)/dyz(2,2,i,k)
		cc = c-1
		if ( absc(cc) .lt. xloss ) then
		    s(1) = d2yzz(i,k)/dyz(2,2,i,k)
		    clogi(ii) = zfflo1(s(1),ier)
		    ilogi(ii) = 0
		elseif ( DBLE(c) .gt. 0 ) then
		    clogi(ii) = zfflog(c,0,czero,ier)
		    ilogi(ii) = 0
		else
		    cc = c+1
		    if ( absc(cc) .lt. xloss ) then
			s(1) = -2*sdel2i(i,k)/dyz(2,2,i,k)/
     +				DBLE(xpi(i+3,k))
			clogi(ii) = zfflo1(s(1),ier)
		    else
			s(1) = 0
			clogi(ii) = zfflog(-c,0,czero,ier)
		    endif
		    if ( DIMAG(c) .lt. -precc*absc(c) .or. DIMAG(s(1))
     +				.lt. -precc*absc(s(1)) ) then
			ilogi(ii) = -1
		    elseif ( DIMAG(c) .gt. precc*absc(c) .or.
     +				DIMAG(s(1)) .gt. precc*absc(s(1)) ) then
			ilogi(ii) = +1
		    elseif ( DBLE(dyz(2,2,i,k)) .eq. 0 ) then
			ilogi(ii) = -nint(sign(1D0,DBLE(xpi(i+3,k))))
			ier = ier + 50
			print *,'doubtful imaginary part ',ilogi(ii)
		    else
			call fferr(78,ier)
			print *,'c = ',c
		    endif
		endif
	    endif
   95	continue
   96	continue
	endif
*  #] logarithms for 4point function:
*  #[ integrals:
	do 100 i=1,3
	    j = 2*i-1
	    if ( isoort(j) .eq. 0 ) then
		if ( isoort(j+8) .ne. 0 ) then
		    call ffcs3(cs3(20*i+61),ipi12(j+8),y(1,4,i),
     +			z(1,4,i),dyz(1,1,4,i),d2yzz(4,i),
     +			xpi(1,4),piDpj(1,1,4),i,6,isoort(j+8),ier)
		endif
	    elseif ( isoort(j+8) .eq. 0 ) then
		call ffcs3(cs3(20*i-19),ipi12(j),y(1,3,i),
     +		    z(1,3,i),dyz(1,1,3,i),d2yzz(3,i),
     +		    xpi(1,3),piDpj(1,1,3),i,6,isoort(j),ier)
	    else
		call ffdcs(cs3(20*i-19),ipi12(j),y(1,3,i),z(1,3,i),
     +		    dyz(1,1,3,i),d2yzz(3,i),dyzzy(1,i),dyyzz(1,i),
     +		    xpi,piDpj,i,6,isoort(j),ier)
	    endif
  100	    continue
*  #] integrals:
*###] ffdcc0:
	end
