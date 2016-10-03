*--#[ log:
*	$Id: ffdcxs.f,v 1.1 2016/09/23 18:39:42 jwebb Exp $
*	$Log: ffdcxs.f,v $
*	Revision 1.1  2016/09/23 18:39:42  jwebb
*	Initial commit of Tauola
*	
c Revision 1.7  1996/03/22  08:13:30  gj
c Fixed bug in bugfix of ffdcxs.f
c
c Revision 1.6  1996/03/14  15:53:13  gj
c Fixed bug in ffcb0: cp in C, cma=cmb=0 was computed incorrectly.
c
c Revision 1.5  1996/03/13  15:43:36  gj
c Fixed bug, when ieps unknown already some things were computed and not zero'd.
c Now I first check ieps, and then compute.
c
c Revision 1.4  1995/12/08  10:38:16  gj
c Fixed too long line
c
*--#] log:


*###[ ffdcxs:
	subroutine ffdcxs(cs3,ipi12,y,z,dyz,d2yzz,dy2z,dyzzy,xpi,piDpj,
     +						ii,ns,isoort,ier)
***#[*comment:***********************************************************
*									*
*	calculates the the difference of two S's with y(3,4),z(3,4) and *
*	y(4)z(3)-y(3)z(4) given.  Note the difference with ffdcxs4, in	*
*	which the y's are the same and only the z's different.  Here	*
*	both can be different.	Also we skip an intermediate level.	*
*	Note also that this routine is much less conservative than	*
*	ffcxs3 in its expectations of the order of the roots: it knows	*
*	that it is (z-,z+,1-z-,1-z+)!					*
*									*
*	input:	y(4,3:4)	(real)	y,1-y in S with s3,s4		*
*		z(4,3:4)	(real)	z,1-z in S with s3,s4		*
*		dyz(2,2,3:4)	(real)	y - z				*
*		d2yzz(3:4)	(real)	2*y - z+ - z-			*
*		dy2z(4,3:4)	(real)	y - 2*z				*
*		dyzzy(4)	(real)	y(i,4)*z(i,4)-y(i,3)*z(i,4)	*
*		xpi(6,3:4)	(real)	usual				*
*		piDpj(6,3:4)	(real)	usual				*
*		cs3(40)	(complex)	assumed zero.			*
*									*
*	output: cs3(40)	(complex)	mod factors pi^2/12, in array	*
*		ipi12(6)(integer)	these factors			*
*		isoort(6)(integer)	returns kind of action taken	*
*		ier	(integer)	0=ok 1=inaccurate 2=error	*
*									*
*	calls:	ffcrr,ffcxr,real/dble,DCMPLX,log,ffadd1,ffadd2,ffadd3	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE COMPLEX cs3(100)
	DOUBLE PRECISION y(4,3:4),z(4,3:4),dyz(2,2,3:4),d2yzz(3:4),
     +		dy2z(4,3:4),dyzzy(4),xpi(6,3:4),piDpj(6,6,3:4)
	integer ipi12(10),ii,ns,isoort(10),ier
*
*	local variables
*
	integer i,j,k,l,m,iepsi(4),iepsj(2,2)
	logical normal
	DOUBLE PRECISION yy,zz,yy1,zz1,dyyzz,hulp3,hulp4,x00(3)
	save iepsi
*
*	common blocks
*
	include 'ff.h'
*
*	data
*
	data iepsi /-2,+2,+2,-2/
*
*	check constants
*  #] declarations:
*  #[ normal case:
	normal = .FALSE.
   10	continue
	if ( normal .or. isoort(1) .ne. isoort(9) .or. isoort(1) .lt.
     +		10 ) then
	    call ffcxs3(cs3( 1),ipi12(1),y(1,3),z(1,3),dyz(1,1,3),
     +		d2yzz(3),dy2z(1,3),xpi(1,3),piDpj(1,1,3),ii,6,
     +		isoort(1),ier)
	    call ffcxs3(cs3(81),ipi12(9),y(1,4),z(1,4),dyz(1,1,4),
     +		d2yzz(4),dy2z(1,4),xpi(1,4),piDpj(1,1,4),ii,6,
     +		isoort(9),ier)
	    return
	endif
*  #] normal case:
*  #[ rotate R's:
	if ( abs(y(2,3)) .lt. 1/xloss ) then
	    do 102 i=1,2
	    do 101 j=1,2
*		iepsi() = /-2,+2,+2,-2/
*		BUT I AM NOT YET SURE OF THE SIGNS (29/6/89)
		k = 2*(i-1)+j
		if ( y(2*i,3) .gt. 0 ) then
		    iepsj(j,i) = iepsi(k)
		else
		    iepsj(j,i) = -iepsi(k)
		endif
		if ( y(2*i,3) .gt. 0 .neqv. y(2*i,4) .gt. 0 ) then
*		    I have no clue to the ieps, take normal route
*		    iepsj(j,i) = 0
		    normal = .TRUE.
		    goto 10
		endif
  101	    continue
  102	    continue
*	    loop over y,z , 1-y,1-z
	    do 120 i=1,2
*	    loop over z+ , z-
	    do 110 j=1,2
		if ( j .eq. 2 ) then
*		    do not calculate if not there (isoort=0, one root)
*			(this is probably not needed as this case should
*			have been dealt with in ffdxc0)
		    if ( isoort(9) .eq. 0 ) goto 110
*		    or if not needed (isoort=2, two equal roots)
		    if ( mod(isoort(9),10) .eq. 2 ) then
*			we use that l still contains the correct value
			do 105 m=1,7
			    cs3(10*(l-1)+m) = 2*DBLE(cs3(10*(l-1)+m))
  105			continue
			ipi12(l) = 2*ipi12(l)
			goto 110
		    endif
		endif
		k = 2*(i-1)+j
		l = 8*(i-1)+j
		if ( dyzzy(k) .ne. 0 ) then
*		    minus sign wrong in thesis (2.78)
		    hulp3 = -dyz(2,j,3)/dyzzy(k)
		    hulp4 = +dyz(2,j,4)/dyzzy(k)
		    yy = y(2*i,3)*hulp4
		    yy1 = y(2*i,4)*hulp3
		    zz = z(k,3)*hulp4
		    zz1 = z(k,4)*hulp3
		    dyyzz = dyz(2,j,3)*hulp4
		    if ( i .eq. 2 ) then
			yy = -yy
			yy1 = -yy1
			zz = -zz
			zz1 = -zz1
		    endif
		    call ffcxr(cs3(10*l-9),ipi12(l),yy,yy1,zz,zz1,dyyzz,
     +			.FALSE.,0D0,0D0,0D0,.FALSE.,x00,iepsj(j,i),ier)
		endif
  110	    continue
  120	    continue
	    goto 800
	endif
*  #] rotate R's:
*  #[ other cases (not ready):
	call ffcxs3(cs3( 1),ipi12(1),y(1,3),z(1,3),dyz(1,1,3),
     +		d2yzz(3),dy2z(1,3),xpi(1,3),piDpj(1,1,3),ii,ns,
     +		isoort(1),ier)
	call ffcxs3(cs3(81),ipi12(9),y(1,4),z(1,4),dyz(1,1,4),
     +		d2yzz(4),dy2z(1,4),xpi(1,4),piDpj(1,1,4),ii,ns,
     +		isoort(9),ier)
	return
*  #] other cases (not ready):
  800	continue
*###] ffdcxs:
	end


*###[ ffdcs:
	subroutine ffdcs(cs3,ipi12,cy,cz,cdyz,cd2yzz,cdyzzy,cdyyzz,
     +		cpi,cpiDpj,ii,ns,isoort,ier)
***#[*comment:***********************************************************
*									*
*	calculates the the difference of two S's with cy(3,4),cz(3,4),	*
*	cy(4)cz(3)-cy(3)cz(4) given.  Note the difference with ffdcs4,	*
*	in which the cy's are the same and only the cz's different.	*
*	Here both can be different.	Also we skip an intermediat	*
*	level.								*
*									*
*	input:	cy(4,3:4)    (complex)	cy,1-cy in S with s3,s4		*
*		cz(4,3:4)    (complex)	cz,1-cz in S with s3,s4		*
*		cdyz(2,2,3:4)(complex)	cy - cz				*
*		cd2yzz(3:4)  (complex)	2*cy - cz+ - cz-		*
*		cdyzzy(4)    (complex)	cy(i,4)*cz(i,4)-cy(i,3)*cz(i,4)	*
*		cdyyzz(2)    (complex)	cy(i,4)-cz(i,4)-cy(i,3)+cz(i,4)	*
*		cpi(6,3:4)   (complex)	usual				*
*		cpiDpj(6,3:4)(complex)	usual				*
*		cs3(40)	     (complex)	assumed zero.			*
*									*
*	output: cs3(40)	     (complex)	mod factors pi^2/12, in array	*
*		ipi12(6)     (integer)	these factors			*
*		isoort(6)    (integer)	returns kind of action taken	*
*		ier	     (integer)	number of digits lost		*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE COMPLEX cs3(100)
	DOUBLE COMPLEX cy(4,3:4),cz(4,3:4),cdyz(2,2,3:4),cd2yzz(3:4),
     +		cdyzzy(4),cdyyzz(2),cpi(6,3:4),cpiDpj(6,6,3:4)
	integer ipi12(10),ii,ns,isoort(10),ier
*
*	local variables
*
	integer i,j,k,l,m,n,ieps,ni(4,3:4),ntot(3:4),
     +		n1a,nffeta,nffet1,ip
	DOUBLE COMPLEX c,cc,clogy,zfflog,
     +		zfflo1,cmip,yy,zz,yy1,zz1,dyyzz,hulp3,hulp4
	DOUBLE PRECISION absc
	external nffeta,nffet1,zfflo1,zfflog
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) +abs(DIMAG(c))
*
*	check constants
*  #] declarations:
*  #[ normal case:
	if ( mod(isoort(1),5).ne.mod(isoort(9),5) .or. isoort(1).gt.-5
     +								) then
	    call ffcs3(cs3( 1),ipi12(1),cy(1,3),cz(1,3),cdyz(1,1,3),
     +		cd2yzz(3),cpi(1,3),cpiDpj(1,1,3),ii,6,isoort(1),ier)
	    call ffcs3(cs3(81),ipi12(9),cy(1,4),cz(1,4),cdyz(1,1,4),
     +		cd2yzz(4),cpi(1,4),cpiDpj(1,1,4),ii,6,isoort(9),ier)
	    return
	endif
*  #] normal case:
*  #[ rotate R's:
	if ( absc(cy(2,3)) .lt. 1/xloss .or. isoort(1) .le. -100 ) then
*
*	    loop over cy,cz , 1-cy,1-cz
	    do 190 i=1,2

	    if ( isoort(1).le.-100 .and. i.eq.2 ) then
*
*		special case del2s=0, a limit has been taken
*
		if ( ii .eq. 2  ) then
*
*		    we took the wrong sign for the dilogs...
*
		    do 110 j=1,20
			cs3(j) = -cs3(j)
  110		    continue
		    ipi12(1) = -ipi12(1)
		    ipi12(2) = -ipi12(2)
		endif
*
*		now the remaining logs.  take care to get the ieps
*		correct!
*
		if ( i.eq.1 .eqv. DBLE(cy(2*i,3)).gt.0 ) then
		    ieps = -3
		else
		    ieps = +3
		endif
		call ffclg2(cs3(81),cdyz(2,1,3),cdyz(2,1,4),
     +			cdyyzz(1),ieps,ier)
		if ( ii .eq. 2 ) then
*		    we have the wrong sign
		    do 120 j=81,83
			cs3(j) = -cs3(j)
  120		    continue
		    ipi12(9) = -ipi12(9)
		endif
		if ( mod(isoort(1),5).eq.0 .and. mod(isoort(9),5).eq.0
     +								) then
		    do 130 j=81,83
			cs3(j) = 2*DBLE(cs3(j))
  130		    continue
		    ipi12(9) = 2*ipi12(9)
		else
		    print *,'ffdcs: error: not yet tested'
		    call ffclg2(cs3(91),cdyz(2,2,3),cdyz(2,2,4),
     +			cdyyzz(2),-ieps,ier)
		    if ( ii .eq. 2 ) then
*			we have the wrong sign
			do 140 j=91,93
			    cs3(j) = -cs3(j)
  140			continue
			ipi12(10) = -ipi12(10)
		    endif
		endif
		goto 190
	    endif
*
*	    loop over cz- , cz+
	    do 180 j=1,2
		if ( j .eq. 2 ) then
		    if ( isoort(9) .eq. 0 .or. isoort(1) .eq. 0 ) then
*
*			(this is not correct as this case should
*			have been dealt with in ffdxc0,ffdcc0)
*
			call fferr(79,ier)
			goto 180
		    elseif ( mod(isoort(9),5) .eq. 0 .and.
     +			     mod(isoort(1),5) .eq. 0 ) then
*
*			or if not needed (isoort=-10, two conjugate roots)
*
*			we use that l still contains the correct value
			do 150 m=1,9
			    cs3(10*(l-1)+m) = 2*DBLE(cs3(10*(l-1)+m))
  150			continue
			ipi12(l) = 2*ipi12(l)
			goto 180
		    elseif ( mod(isoort(9),10) .eq. 2 ) then
*			we use that l still contains the correct value
			do 160 m=1,9
			    cs3(10*(l-1)+m) = 2*cs3(10*(l-1)+m)
  160			continue
			ipi12(l) = 2*ipi12(l)
			goto 180
		    endif
		endif
		k = 2*(i-1)+j
		l = 8*(i-1)+j
		if ( cdyzzy(k) .ne. 0 ) then
		    hulp3 = -cdyz(2,j,3)/cdyzzy(k)
		    hulp4 = cdyz(2,j,4)/cdyzzy(k)
		    yy = cy(2*i,3)*hulp4
		    yy1 = cy(2*i,4)*hulp3
		    zz = cz(k,3)*hulp4
		    zz1 = cz(k,4)*hulp3
		    dyyzz = cdyz(2,j,3)*hulp4
		    if ( i .eq. 2 ) then
			yy = -yy
			yy1 = -yy1
			zz = -zz
			zz1 = -zz1
		    endif
*
*		    ieps = 3 means: dear ffcrr, do not use eta terms,
*		    they are calculated here.  The sign gives the sign
*		    of the imag. part of the argument of the dilog, not
*		    y-z.
*
		    if ( i.eq.1 .eqv. j.eq.1 .eqv. DBLE(cy(2*i,3)).gt.0
     +								) then
			ieps = -3
		    else
			ieps = +3
		    endif
		    call ffcrr(cs3(10*l-9),ipi12(l),yy,yy1,zz,zz1,dyyzz,
     +			.FALSE.,czero,czero,czero,isoort(j),ieps,ier)
*
*		    eta terms of the R's (eta(.)*log(c1)-eta(.)*log(c2))
*
		    do 170 m=3,4
*			no eta terms in the real case
			if ( DIMAG(cz(k,m)) .eq. 0 .and.
     +			     DIMAG(cdyz(2,j,m)) .eq. 0 ) then
			    ni(k,m) = 0
			elseif ( i .eq. 1 ) then
			    ni(k,m) = nffeta(-cz(k,m),1/cdyz(2,j,m),ier)
			else
			    ni(k,m) = nffeta(cz(k,m),1/cdyz(2,j,m),ier)
			endif
  170		    continue
		    if ( ni(k,3) .ne. 0 .or. ni(k,4) .ne. 0 ) then
			if ( ni(k,3) .ne. ni(k,4) ) then
			    do 175 m=3,4
				c = cy(2*i,m)/cdyz(2,j,m)
				if ( i .eq. 2 ) c = -c
				cc = c-1
				if ( absc(cc) .lt. xloss ) then
				    c = cz(k,m)/cdyz(2,j,m)
				    clogy = zfflo1(c,ier)
				else
				    clogy = zfflog(c,0,czero,ier)
				endif
				n = 10*l + (m-3) - 2
				if ( m .eq. 3 ) then
				   cs3(n) = + ni(k,m)*c2ipi*clogy
				else
				   cs3(n) = - ni(k,m)*c2ipi*clogy
				endif
 175			    continue
			else
			    if ( i .eq. 1 ) then
				n1a = nffeta(cy(k,3)/cdyz(2,j,3),
     +					cdyz(2,j,4)/cy(k,4),ier)
			    else
				n1a = nffeta(-cy(k,3)/cdyz(2,j,3),
     +					-cdyz(2,j,4)/cy(k,4),ier)
			    endif
			    if ( n1a .ne. 0 ) then
				call fferr(80,ier)
			    endif
			    c =cy(k,3)*cdyz(2,j,4)/(cdyz(2,j,3)*cy(k,4))
			    cc = c-1
			    if ( absc(cc) .lt. xloss ) then
				c = -cdyzzy(k)/(cdyz(2,j,3)*cy(k,4))
				clogy = zfflo1(c,ier)
			    else
				clogy = zfflog(c,0,czero,ier)
			    endif
			    n = 10*l - 2
			    if ( i .eq. 1 ) then
				cs3(n) = +ni(k,3)*c2ipi*clogy
			    else
				cs3(n) = -ni(k,3)*c2ipi*clogy
			    endif
			endif
		    endif
		endif
  180	    continue
  190	    continue
	    goto 700
	endif
*  #] rotate R's:
*  #[ other cases (not ready):
	call ffcs3(cs3( 1),ipi12(1),cy(1,3),cz(1,3),cdyz(1,1,3),
     +		cd2yzz(3),cpi(1,3),cpiDpj(1,1,3),ii,ns,isoort(1),ier)
	call ffcs3(cs3(81),ipi12(9),cy(1,4),cz(1,4),cdyz(1,1,4),
     +		cd2yzz(4),cpi(1,4),cpiDpj(1,1,4),ii,ns,isoort(9),ier)
	return
*  #] other cases (not ready):
*  #[ get eta's:
  700	continue
	ip = ii+3
	do 740 k=3,4
	l = 8*(k-3) + 1
	if ( DIMAG(cpi(ip,k)) .eq. 0 ) then
*
*	complex because of a complex root in y or z
*
	if ( (mod(isoort(l),10).eq.-1 .or. mod(isoort(l),10).eq.-3)
     +		.and. isoort(l+1) .ne. 0 ) then
*
*	    isoort = -1:    y is complex, possibly z as well
*	    isoort = -3:    y,z complex, but (y-z-)(y-z+) real
*	    isoort = 0:     y is complex, one z root only
*	    isoort = -10:   y is real, z is complex
*	    isoort = -5,-6: y,z both real
*
	    cmip = DCMPLX(0D0,-DBLE(cpi(ip,k)))
	    if ( DIMAG(cz(1,k)) .eq. 0 ) then
		ni(1,k) = 0
	    else
		ni(1,k) = nffet1(-cz(1,k),-cz(2,k),cmip,ier)
		i = nffet1(cz(3,k),cz(4,k),cmip,ier)
		if ( i .ne. ni(1,k) ) call fferr(53,ier)
	    endif
	    ni(2,k) = 0
	    if ( DBLE(cd2yzz(k)).eq.0 .and. ( DIMAG(cz(1,k)).eq.0 .and.
     +		 DIMAG(cz(2,k)).eq.0 .or. DBLE(cdyz(2,1,k)).eq.0 .and.
     +		 DBLE(cdyz(2,2,k)) .eq. 0 ) ) then
*		follow the i*epsilon prescription as (y-z-)(y-z+) real
		if ( DBLE(cpi(ip,k)) .lt. 0 ) then
		    ni(3,k) = -1
		else
		    ni(3,k) = 0
		endif
		ni(4,k) = -nffet1(cdyz(2,1,k),cdyz(2,2,k),cmip,ier)
	    else
		if ( DBLE(cpi(ip,k)) .lt. 0 .and. DIMAG(cdyz(2,1,k)*
     +			cdyz(2,2,k)) .lt. 0 ) then
		    ni(3,k) = -1
		else
		    ni(3,k) = 0
		endif
		ni(4,k) = -nffeta(cdyz(2,1,k),cdyz(2,2,k),ier)
	    endif
	elseif ( (mod(isoort(l),10).eq.-1 .or. mod(isoort(l),10).eq.-3)
     +		.and. isoort(l+1).eq.0 ) then
	    ni(1,k) = 0
	    if ( DIMAG(cz(1,k)) .ne. 0 ) then
		ni(2,k) = nffet1(-cpiDpj(ii,ip,k),-cz(1,k),DCMPLX(DBLE(0
     +			),DBLE(-1)),ier)
	    else
		ni(2,k) = nffet1(-cpiDpj(ii,ip,k),DCMPLX(DBLE(0),
     +			DBLE(1)),DCMPLX(DBLE(0),DBLE(-1)),ier)
	    endif
	    ni(3,k) = 0
	    ni(4,k) = -nffeta(-cpiDpj(ii,ip,k),cdyz(2,1,k),ier)
	else
	    if ( mod(isoort(l),5).ne.0 .and. mod(isoort(l),5).ne.-1
     +			.and. mod(isoort(l),5).ne.-3 ) then
		call fferr(81,ier)
		print *,'isoort(',l,') = ',isoort(l)
	    endif
	    ni(1,k) = 0
	    ni(2,k) = 0
	    ni(3,k) = 0
	    ni(4,k) = 0
	endif
	else
	    print *,'ffdcs: error: cpi complex should not occur'
	    stop
	endif
  740	continue
*  #] get eta's:
*  #[ add eta's:
	do 750 k=3,4
	    ntot(k) = ni(1,k)+ni(2,k)+ni(3,k)+ni(4,k)
  750	continue
	do 760 k=3,4
	    if ( ntot(k) .ne. 0 ) call ffclgy(cs3(20+80*(k-3)),
     +		ipi12(2+8*(k-3)),ni(1,k),cy(1,k),cz(1,k),cd2yzz(k),ier)
  760	continue
*  #] add eta's:
*###] ffdcs: 
	end


*###[ ffclg2:
	subroutine ffclg2(cs3,cdyz3,cdyz4,cdyyzz,ieps,ier)
***#[*comment:***********************************************************
*									*
*	Calculate the finite part of the divergent dilogs in case	*
*	del2s=0.  These are given by					*
*									*
*		log^2(-cdyz3)/2 - log^2(-cdyz4)/2			*
*									*
*	Note that often we only need the imaginary part, which may be	*
*	very unstable even if the total is not.				*
*									*
*									*
*	Input:	cy3,cz3,cdyz3	(complex)	y,z,diff in C with s3	*
*		cy4,cz4,cdyz4	(complex)	y,z,diff in C with s4	*
*		cdyyzz		(complex)	y4 - z4 - y3 + z3	*
*		isort3,4	(integer)				*
*									*
*	Output	cs3(4)		(complex)	output			*
*		ipi12		(integer)	terms pi^2/12		*
*		ier		(integer)	error flag		*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE COMPLEX cs3(3),cdyz3,cdyz4,cdyyzz
	integer ieps,ier
*
*	local variables
*
	integer n1,nffeta,nffet1,ipi3,ipi4
	DOUBLE COMPLEX c,cc,clog3,clog4,clog1,zfflo1,cipi
	DOUBLE PRECISION absc
	external nffeta,nffet1,zfflo1
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*  #] declarations:
*  #[ calculations:
	cipi = DCMPLX(0D0,pi)
	if ( DBLE(cdyz3) .lt. 0 ) then
	    clog3 = log(-cdyz3)
	    ipi3 = 0
	else
	    clog3 = log(cdyz3)
	    if ( DIMAG(cdyz3) .gt. 0 ) then
		ipi3 = -1
	    elseif ( DIMAG(cdyz3) .lt. 0 ) then
		ipi3 = +1
	    else
		ipi3 = sign(1,-ieps)
	    endif
	endif
	if ( DBLE(cdyz4) .lt. 0 ) then
	    clog4 = log(-cdyz4)
	    ipi4 = 0
	else
	    clog4 = log(cdyz4)
	    if ( DIMAG(cdyz4) .gt. 0 ) then
		ipi4 = -1
	    elseif ( DIMAG(cdyz4) .lt. 0 ) then
		ipi4 = +1
	    else
		ipi4 = sign(1,-ieps)
	    endif
	endif
	cc = clog3-clog4
	if ( absc(cc) .ge. xloss*absc(clog3) ) then
	    cs3(1) = -(clog3+ipi3*cipi)**2/2
	    cs3(2) = +(clog4+ipi4*cipi)**2/2
	else
	    c = cdyyzz/cdyz4
	    clog1 = zfflo1(c,ier)
*
*	    notice that zfflog return log(a-ieps) (for compatibility
*	    with the dilog)		   ^
*
	    if ( DIMAG(cdyz3) .eq. 0 ) then
		n1 = nffet1(DCMPLX(DBLE(0),DBLE(-ieps)),-1/cdyz4,-c,
     +			ier)
	    elseif ( DIMAG(cdyz3) .eq. 0 ) then
		n1 = nffet1(-cdyz3,DCMPLX(DBLE(0),DBLE(ieps)),-c,ier)
	    else
		n1 = nffeta(-cdyz3,-1/cdyz4,ier)
	    endif
	    if ( n1 .ne. 0 ) then
		clog1 = clog1 - n1*c2ipi
	    endif
	    cs3(1) = -clog3*clog1/2
	    cs3(2) = -clog4*clog1/2
	    cs3(3) = -(ipi3+ipi4)*cipi*clog1/2
*	    we could split off a factor 2*pi^2 if needed
	endif
*	ATTENTION: now (23-jul-1989) ffdcs assumes that only *3* cs are
*	set. Change ffdcs as well if this is no longer true!
*  #] calculations:
*###] ffclg2: 
	end
