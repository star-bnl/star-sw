*	$Id: ffcc0.f,v 1.1 2016/09/23 18:39:42 jwebb Exp $
*###[ ffcc0:
	subroutine ffcc0(cc0,cpi,ier)
***#[*comment:***********************************************************
*									*
*	Calculates the threepoint function closely following		*
*	recipe in 't Hooft & Veltman, NP B(183) 1979.			*
*	B&D metric is used throughout!					*
*									*
*	    p2	| |							*
*		v |							*
*		 / \							*
*	      m2/   \m3 						*
*	p1     /     \	p3						*
*	->    /  m1   \ <-						*
*	------------------------					*
*									*
*		1   /			     1				*
*	    = ----- \d^4Q----------------------------------------	*
*	      ipi^2 /	 [Q^2-m1^2][(Q+p1)^2-m2^2][(Q-p3)^2-m3^2]	*
*									*
*	If the function is infra-red divergent (p1=m2,p3=m3,m1=0 or	*
*	cyclic) the function is calculated with a user-supplied cutoff	*
*	delta in the common block /ffcut/.				*
*									*
*	the parameter nschem in the common block /fflags/ determines	*
*	which recipe is followed, see ffinit.f				*
*									*
*	Input:	cpi(6)	(complex)   m1^2,m2^3,p1^2,p2^2,p3^2		*
*				    of divergences, but C0 has none)	*
*	/ffcut/ delta	(real)      IR cutoff				*
*	/fflags/..nschem(integer)   6: full complex, 0: real, else:	*
*					some or all logs		*
*	/fflags/..nwidth(integer)   when |p^2-Re(m^2)| < nwidth|Im(m^2)	*
*				    use complex mass			*
*		ier	(integer)   number of digits lost so far	*
*	Output: cc0	(complex)   C0, the threepoint function 	*
*		ier	(integer)   number of digits lost more than (at	*
*				    most) xloss^5			*
*	 Calls: ffcc0p,ffcb0p						*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE COMPLEX cc0,cpi(6)
*
*	local variables:
*
	integer i,j,ier0,init
	DOUBLE COMPLEX c,cc0r,cc0p,cc00
	DOUBLE COMPLEX cdpipj(6,6)
	DOUBLE PRECISION xmax,xpi(6),sprecx,dm
	save init
*
*	common blocks:
*
	include 'ff.h'
*
*	data
*
	data init/0/
*
*  #] declarations:
*  #[ the real case:
*
*	take a faster route if all masses are real or nschem < 3
*
	if ( nschem .ge. 3 ) then
	    do 10 i = 1,6
		if ( DIMAG(cpi(i)) .ne. 0 ) goto 30
   10	    continue
	elseif ( init .eq. 0 ) then
	    init = 1
	    print *,'ffcc0: disregarding complex masses, nschem= ',
     +			nschem
	endif
	do 20 i = 1,6
	    xpi(i) = DBLE(cpi(i))
   20	continue
	sprecx = precx
	precx = precc
	call ffxc0(cc0,xpi,ier)
	precx = sprecx
	if ( ldot ) call ffcod3(cpi)
	return
   30	continue
*
*  #] the real case:
*  #[ check input:
*
	idsub = 0
*
*  #] check input:
*  #[ convert input:
	    do 70 i=1,6
		cdpipj(i,i) = 0
		do 60 j = 1,6
		    cdpipj(j,i) = cpi(j) - cpi(i)
   60		continue
   70	    continue
*  #] convert input:
*  #[ call ffcc0a:
	call ffcc0a(cc0,cpi,cdpipj,ier)
*  #] call ffcc0a:
*###] ffcc0:
	end
*###[ ffcc0r:
	subroutine ffcc0r(cc0,cpi,ier)
***#[*comment:***********************************************************
*									*
*	Tries all 2 permutations of the 3pointfunction			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
	integer ier
	DOUBLE COMPLEX cc0,cc0p,cpi(6),cqi(6)
	integer inew(6,2),irota,ier1,i,j,ialsav
	save inew
	include 'ff.h'
	data inew /1,2,3,4,5,6,
     +		   1,3,2,6,5,4/
*  #] declarations:
*  #[ calculations:
	cc0 = 0
	ier = 999
	ialsav = isgnal
	do 30 j = -1,1,2
	    do 20 irota=1,2
		do 10 i=1,6
		    cqi(inew(i,irota)) = cpi(i)
   10		continue
		print '(a,i1,a,i2)','---#[ rotation ',irota,': isgnal ',
     +			isgnal
		ier1 = 0
		ner = 0
		id = id + 1
		isgnal = ialsav
		call ffcc0(cc0p,cqi,ier1)
		ier1 = ier1 + ner
		print '(a,i1,a,i2)','---#] rotation ',irota,': isgnal ',
     +			isgnal
		print '(a,2g28.16,i3)','c0 = ',cc0p,ier1
		if ( ier1 .lt. ier ) then
		    cc0 = cc0p
		    ier = ier1
		endif
   20	    continue
	    ialsav = -ialsav
   30	continue
*  #] calculations:
*###] ffcc0r:
	end
*###[ ffcc0a:
	subroutine ffcc0a(cc0,cpi,cdpipj,ier)
***#[*comment:***********************************************************
*									*
*	see ffcc0							*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE COMPLEX cc0,cpi(6),cdpipj(6,6)
*
*	local variables:
*
	integer i,j,irota,inew(6,6),i1,i2,i3,initlo,ithres(3),ifound
	logical ljust
*	DOUBLE COMPLEX cs,cs1,cs2
	DOUBLE COMPLEX c,cqi(6),cqiqj(6,6),cqiDqj(6,6)
	DOUBLE PRECISION xqi(6),dqiqj(6,6),qiDqj(6,6),sprec
	save initlo
*
*	common blocks:
*
	include 'ff.h'
*
*	memory
*
	integer iermem(memory),ialmem(memory),nscmem(memory),memind,
     +		ierini
	DOUBLE COMPLEX cpimem(6,memory)
	DOUBLE COMPLEX cc0mem(memory)
	DOUBLE PRECISION dl2mem(memory)
	save memind,iermem,ialmem,cpimem,cc0mem
	data memind /0/
*
*	data
*
	data inew /1,2,3,4,5,6,
     +		   2,3,1,5,6,4,
     +		   3,1,2,6,4,5,
     +		   1,3,2,6,5,4,
     +		   3,2,1,5,4,6,
     +		   2,1,3,4,6,5/
	data initlo /0/
*
*  #] declarations:
*  #[ initialisations:
	if ( lmem .and. memind .eq. 0 ) then
	    do 2 i=1,memory
		do 1 j=1,6
		    cpimem(j,i) = 0
    1		continue
		ialmem(i) = 0
		nscmem(i) = -1
    2	    continue
	endif
	idsub = 0
	ljust = .FALSE.
*  #] initialisations:
*  #[ handel special cases:
	if ( DIMAG(cpi(1)).eq.0 .and. DIMAG(cpi(2)).eq.0 .and.
     +		DIMAG(cpi(3)).eq.0 ) then
	    do 4 i=1,6
		xqi(i) = DBLE(cpi(i))
		do 3 j=1,6
		    dqiqj(j,i) = DBLE(cdpipj(j,i))
    3		continue
    4	    continue
	    sprec = precx
	    precx = precc
	    call ffxc0a(cc0,xqi,dqiqj,ier)
	    precx = sprec
	    if ( ldot ) call ffcod3(cpi)
	    return
	endif
*	goto 5
*	No special cases for the moment...
**
*	The infrared divergent diagrams cannot be complex
**
*	The general case cannot handle cpi=0, pj=pk. These are simple
*	though.
**
*	if ( cpi(4) .eq. 0 .and. cdpipj(5,6) .eq. 0 .and. cdpipj(1,2)
*     +		.ne. 0 ) then
*	    call ffcb0p(cs1,-cpi(5),cpi(1),cpi(3),cdpipj(1,6),
*     +					cdpipj(3,5),cdpipj(1,3),ier)
*	    call ffcb0p(cs2,-cpi(5),cpi(2),cpi(3),cdpipj(2,5),
*     +					cdpipj(3,5),cdpipj(2,3),ier)
*	    cs = cs1 - cs2
*	    cc0 = cs/cdpipj(1,2)
*	elseif ( cpi(6) .eq. 0 .and. cdpipj(4,5) .eq. 0 .and.
*     +		cdpipj(3,1) .ne. 0 ) then
*	    call ffcb0p(cs1,-cpi(4),cpi(3),cpi(2),cdpipj(3,5),
*     +					cdpipj(2,4),cdpipj(3,2),ier)
*	    call ffcb0p(cs2,-cpi(4),cpi(1),cpi(2),cdpipj(1,4),
*     +					cdpipj(2,4),cdpipj(1,2),ier)
*	    cs = cs1 - cs2
*	    cc0 = cs/cdpipj(3,1)
*	elseif ( cpi(5) .eq. 0 .and. cdpipj(6,4) .eq. 0 .and.
*     +		cdpipj(2,3) .ne. 0 ) then
*	    call ffcb0p(cs1,-cpi(6),cpi(2),cpi(1),cdpipj(2,4),
*     +					cdpipj(1,6),cdpipj(2,1),ier)
*	    call ffcb0p(cs2,-cpi(6),cpi(3),cpi(1),cdpipj(3,6),
*     +					cdpipj(1,6),cdpipj(3,1),ier)
*	    cs = cs1 - cs2
*	    cc0 = cs/cdpipj(2,3)
*	else
*	    goto 5
*	endif
**
*	common piece - excuse my style
**
*	print *,'ffcc0: WARNING: this algorithm has not yet been tested'
*	if ( absc(cs) .lt. xloss*absc(cs1) )
*     +		call ffwarn(26,ier,absc(cs),absc(cs1))
**
*	return
*    5	continue
*  #] handel special cases:
*  #[ rotate to alpha in (0,1):
	call ffcrt3(irota,cqi,cqiqj,cpi,cdpipj,6,2,ier)
*  #] rotate to alpha in (0,1):
*  #[ look in memory:
	ierini = ier+ner
	if ( lmem ) then
	    do 70 i=1,memory
		do 60 j=1,6
		    if ( cqi(j) .ne. cpimem(j,i) ) goto 70
   60		continue
		if ( ialmem(i) .ne. isgnal .or.
     +		     nscmem(i) .ne. nschem ) goto 70
*		we found an already calculated masscombination ..
*		(maybe check differences as well)
		cc0 = cc0mem(i)
		ier = ier+iermem(i)
		if ( ldot ) then
		    fodel2 = dl2mem(i)
		    fdel2 = fodel2
*		    we forgot to recalculate the stored quantities
		    ljust = .TRUE.
		    goto 71
		endif
		return
   70	    continue
	endif
   71	continue
*  #] look in memory:
*  #[ dot products:
	call ffcot3(cqiDqj,cqi,cqiqj,6,ier)
*
*	save dotproducts for tensor functions if requested
*
	if ( ldot ) then
	    do 75 i=1,6
		do 74 j=1,6
		    cfpij3(j,i) = cqiDqj(inew(i,irota),inew(j,irota))
   74		continue
   75	    continue
	    if ( irota .gt. 3 ) then
*
*		the signs of the s's have been changed
*
		do 77 i=1,3
		    do 76 j=4,6
			cfpij3(j,i) = -cfpij3(j,i)
			cfpij3(i,j) = -cfpij3(i,j)
   76		    continue
   77		continue
	    endif
*
*	    also give the real dotproducts as reals
*
	    do 79 i=4,6
		do 78 j=4,6
		    fpij3(j,i) = DBLE(cfpij3(j,i))
   78		continue
   79	    continue
	endif
	if ( ljust ) return
*  #] dot products:
*  #[ handle poles-only approach:
	sprec = precx
	precx = precc
	if (  nschem.le.6 ) then
	    if ( initlo.eq.0 ) then
		initlo = 1
		if ( nschem.eq.1 .or. nschem.eq.2 ) then
		    print *,'ffcc0a: disregarding all complex masses'
		elseif ( nschem.eq.3 ) then
		    print *,'ffcc0a: undefined nschem=3'
		elseif ( nschem.eq.4 ) then
		    print *,'ffcc0a: using the scheme in which ',
     +		       'complex masses are used everywhere when ',
     +		       'there is a divergent log'
		elseif ( nschem.eq.5 ) then
		    print *,'ffcc0a: using the scheme in which ',
     +		       'complex masses are used everywhere when ',
     +		       'there is a divergent or almost divergent log'
		elseif ( nschem.eq.6 ) then
		    print *,'ffcc0a: using the scheme in which ',
     +		       'complex masses are used everywhere when ',
     +		       'there is a singular log'
		elseif ( nschem.eq.7 ) then
		    print *,'ffcc0a: using complex masses'
		endif
		if ( nschem.ge.3 ) then
		    print *,'ffcc0a: switching to complex when ',
     +			'|p^2-Re(m^2)| < ',nwidth,'*|Im(m^2)|'
		endif
	    endif
	    do 9 i=1,6
		xqi(i) = DBLE(cqi(i))
		do 8 j=1,6
		    dqiqj(j,i) = DBLE(cqiqj(j,i))
		    qiDqj(j,i) = DBLE(cqiDqj(j,i))
    8		continue
    9	    continue
	    i1 = 0
	    ithres(1) = 0
	    ithres(2) = 0
	    ithres(3) = 0
	    if ( nschem.le.2 ) goto 21
*
	    do 10 i1=1,3
*
*		search for a combination of 2 almost on-shell particles
*		and a light one
*
		i2 = mod(i1,3)+1
		i3 = mod(i2,3)+1
		call ffbglg(ifound,cqi,cqiqj,cqiDqj,6,i1,i2,i3,i1+3,
     +			i3+3)
		if ( ifound .ne. 0 ) goto 11
   10	    continue
	    i1 = 0
   11	    continue
	    if ( nschem.ge.4 .and. i1.ne.0 ) goto 30
	    if ( nschem.le.3 ) goto 21
*
	    do 20 i=1,3
		i2 = mod(i,3)+1
		call ffthre(ithres(i),cqi,cqiqj,6,i,i2,i+3)
   20	    continue
*
	    if ( nschem.eq.5 .and. (ithres(1).eq.2 .or.
     +		ithres(2).eq.2 .or. ithres(3).eq.2) ) goto 30
	    if ( nschem.eq.6 .and. (ithres(1).eq.1 .or.
     +		ithres(2).eq.1 .or. ithres(3).eq.1) ) goto 30
*
   21	    continue
*
*	    The infrared divergent diagrams are calculated in ffxc0i:
*
	    if ( dqiqj(2,4).eq.0 .and. dqiqj(3,6).eq.0 .and. xqi(1).eq.0
     +	    .or. dqiqj(3,5).eq.0 .and. dqiqj(1,4).eq.0 .and. xqi(2).eq.0
     +	    .or. dqiqj(1,6).eq.0 .and. dqiqj(2,5).eq.0 .and. xqi(3).eq.0
     +			) then
		call ffxc0i(cc0,xqi,dqiqj,ier)
	    else
		call ffxc0b(cc0,xqi,dqiqj,qiDqj,ier)
	    endif
*	    the dotproducts are already set, but I forgot this
	    if ( ldot ) fodel2 = fdel2
	    goto 31
*
*	    the complex case
*
   30	    continue
	    precx = sprec
	    call ffcc0b(cc0,cqi,cqiqj,cqiDqj,ier)
   31	    continue
*
*  #] handle poles-only approach:
*  #[ call ffcc0b:
	else
	    precx = sprec
	    call ffcc0b(cc0,cqi,cqiqj,cqiDqj,ier)
	endif
*  #] call ffcc0b:
*  #[ add to memory:
	if ( lmem ) then
	    memind = memind + 1
	    if ( memind .gt. memory ) memind = 1
	    do 200 j=1,6
		cpimem(j,memind) = cqi(j)
  200	    continue
	    cc0mem(memind) = cc0
	    iermem(memind) = ier+ner-ierini
	    ialmem(memind) = isgnal
	    nscmem(memind) = nschem
	    dl2mem(memind) = fodel2
	endif
*  #] add to memory:
*###] ffcc0a:
	end
*###[ ffcc0b:
	subroutine ffcc0b(cc0,cqi,cqiqj,cqiDqj,ier)
***#[*comment:***********************************************************
*									*
*	see ffcc0							*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
	integer nerr
	parameter (nerr=6)
*
*	arguments
*
	DOUBLE COMPLEX cc0,cqi(6),cqiqj(6,6),cqiDqj(6,6)
	integer ier
*
*	local variables:
*
	integer isoort(8),ipi12(8),i,j,k,ipi12t,ilogi(3),ier0,ieri(nerr)
	DOUBLE COMPLEX cs3(80),cs,cs1,cs2,cslam,c,cel2,cel3,cel2s(3),
     +		cel3mi(3),clogi(3),calph(3),cblph(3),cetalm,cetami(6),
     +		clamp,ceta,csdel2,celpsi(3)
	DOUBLE PRECISION xmax,absc,del2,qiDqj(6,6)
*
*	common blocks:
*
	include 'ff.h'
*
*	statement function:
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*  #] declarations:
*  #[ calculations:
*
*	some determinants
*
	do 98 i = 1,nerr
	    ieri(i) = 0
   98	continue
	do 104 i=4,6
	    do 103 j=4,6
		qiDqj(j,i) = DBLE(cqiDqj(j,i))
  103	    continue
  104	continue
	call ffdel2(del2,qiDqj,6,4,5,6,1,ier)
	fodel2 = del2
	fdel2 = fodel2
	cel2 = DCMPLX(DBLE(del2))
	call ffcel3(cel3,cqi,cqiDqj,6,ier)
	if ( DIMAG(cel3).ne.0 .and. 
     +		abs(DIMAG(cel3)).lt.precc*abs(DBLE(cel3)) ) then
	    cel3 = DBLE(cel3)
	endif
	call ffcl3m(cel3mi,.TRUE.,cel3,cel2,cqi,cqiqj,cqiDqj,6, 4,5,6,
     +							1,3)
	do 105 i=1,3
	    j = i+1
	    if ( j .eq. 4 ) j = 1
	    call ffcel2(cel2s(i),cqiDqj,6,i+3,i,j,1,ieri(i))
	    k = i-1
	    if ( k .eq. 0 ) k = 3
	    call ffcl2p(celpsi(i),cqi,cqiqj,cqiDqj,i+3,j+3,k+3,i,j,k,6)
  105	continue
	cetalm = cel3*DBLE(1/del2)
	do 108 i=1,3
	    cetami(i) = cel3mi(i)*DBLE(1/del2)
  108	continue
	csdel2 = isgnal*DBLE(sqrt(-del2))
	ier0 = 0
	do 99 i=1,nerr
	    ier0 = max(ier0,ieri(i))
   99	continue
	ier = ier + ier0
*
*	initialize cs3:
*
	do 80 i=1,80
	    cs3(i) = 0
   80	continue
	do 90 i=1,8
	    ipi12(i) = 0
   90	continue
*
*	get alpha,1-alpha
*
	call ffcoot(cblph(1),calph(1),cqi(5),-cqiDqj(5,6),cqi(6),csdel2,
     +								ier)
	call ffcoot(calph(3),cblph(3),cqi(5),-cqiDqj(5,4),cqi(4),csdel2,
     +								ier)
	cs1 = cblph(1) - chalf
	cs2 = calph(1) - chalf
	if ( l4also .and. ( DBLE(calph(1)) .gt. 1 .or. DBLE(calph(1))
     +		.lt. 0 ) .and. absc(cs1) .lt. absc(cs2) ) then
	    calph(1) = cblph(1)
	    calph(3) = cblph(3)
	    csdel2 = -csdel2
	    isgnal = -isgnal
	endif
	cslam = 2*csdel2
*
*	and the calculations
*
	call ffcc0p(cs3,ipi12,isoort,clogi,ilogi,cqi,cqiqj,cqiDqj,
     +	    csdel2,cel2s,cetalm,cetami,celpsi,calph,3,ier)
*
*	sum'em up:
*
	cs = 0
	xmax = 0
	do 110 i=1,80
	    cs = cs + cs3(i)
	    xmax = max(xmax,absc(cs))
  110	continue
	ipi12t = ipi12(1)
	do 120 i=2,8
	    ipi12t = ipi12t + ipi12(i)
  120	continue
	cs = cs + ipi12t*DBLE(pi12)
*
*	check for imaginary part zero (this may have to be dropped)
*
	if ( abs(DIMAG(cs)) .lt. precc*abs(DBLE(cs)) )
     +	    cs = DCMPLX(DBLE(cs))
	cc0 = - cs/cslam
*  #] calculations:
*###] ffcc0b:
	end
*###[ ffcrt3:
	subroutine ffcrt3(irota,cqi,cdqiqj,cpi,cdpipj,ns,iflag,ier)
***#[*comment:***********************************************************
*									*
*	rotates the arrays cpi, cdpipj into cqi,cdqiqj so that		*
*	cpi(6),cpi(4) suffer the strongest outside cancellations and	*
*	cpi(6) > cpi(4) if iflag = 1, so that cpi(5) largest and cpi(5)	*
*	and cpi(6) suffer cancellations if iflag = 2.			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer irota,ns,iflag,ier
	DOUBLE COMPLEX cpi(ns),cdpipj(ns,ns),cqi(ns),cdqiqj(ns,ns)
*
*	local variables
*
	DOUBLE PRECISION a1,a2,a3,xpimax,absc
	DOUBLE COMPLEX c
	integer i,j,inew(6,6),ier0
	save inew
*
*	common blocks
*
	include 'ff.h'
*
*	data
*
	data inew /1,2,3,4,5,6,
     +		   2,3,1,5,6,4,
     +		   3,1,2,6,4,5,
     +		   1,3,2,6,5,4,
     +		   3,2,1,5,4,6,
     +		   2,1,3,4,6,5/
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*  #] declarations:
*  #[ get largest cancellation:
	if ( iflag .eq. 1 ) then
	    a1 = absc(cdpipj(6,4))/max(absc(cpi(6)+cpi(4)),xclogm)
	    a2 = absc(cdpipj(5,4))/max(absc(cpi(5)+cpi(4)),xclogm)
	    a3 = absc(cdpipj(5,6))/max(absc(cpi(6)+cpi(5)),xclogm)
	    if ( a1 .le. a2 .and. a1 .le. a3 ) then
		if ( absc(cpi(6)) .lt. absc(cpi(4)) ) then
		    irota = 4
		else
		    irota = 1
		endif
	    elseif ( a2 .le. a3 ) then
		if ( absc(cpi(4)) .lt. absc(cpi(5)) ) then
		    irota = 6
		else
		    irota = 3
		endif
	    else
		if ( absc(cpi(5)) .lt. absc(cpi(6)) ) then
		    irota = 5
		else
		    irota = 2
		endif
	    endif
	elseif ( iflag .eq. 2 ) then
	    xpimax = max(DBLE(cpi(4)),DBLE(cpi(5)),DBLE(cpi(6)))
	    if ( xpimax .eq. 0 ) then
		if ( DBLE(cpi(5)) .ne. 0 ) then
		    irota = 1
		elseif ( DBLE(cpi(4)) .ne. 0 ) then
		    irota = 2
		elseif ( DBLE(cpi(6)) .ne. 0 ) then
		    irota = 3
		else
		    call fferr(40,ier)
		    return
		endif
	    elseif ( DBLE(cpi(5)) .eq. xpimax ) then
		if ( DBLE(cpi(4)) .le. DBLE(cpi(6)) ) then
		    irota = 1
		else
		    irota = 4
		endif
	    elseif ( DBLE(cpi(4)) .eq. xpimax ) then
		if ( DBLE(cpi(5)) .ge. DBLE(cpi(6)) ) then
		    irota = 2
		else
		    irota = 5
		endif
	    else
		if ( DBLE(cpi(4)) .ge. DBLE(cpi(6)) ) then
		    irota = 3
		else
		    irota = 6
		endif
	    endif
	else
	    call fferr(35,ier)
	endif
*  #] get largest cancellation:
*  #[ rotate:
	do 20 i=1,6
	    cqi(inew(i,irota)) = cpi(i)
	    do 10 j=1,6
		cdqiqj(inew(i,irota),inew(j,irota)) = cdpipj(i,j)
   10	    continue
   20	continue
*  #] rotate:
*###] ffcrt3:
	end
*###[ ffcot3:
	subroutine ffcot3(cpiDpj,cpi,cdpipj,ns,ier)
***#[*comment:***********************************************************
*									*
*	calculate the dotproducts pi.pj with				*
*									*
*		pi = si		i1=1,3					*
*		pi = p(i-3)	i1=4,6					*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ns,ier
	DOUBLE COMPLEX cpi(ns),cdpipj(ns,ns),cpiDpj(ns,ns)
*
*	locals
*
	integer is1,is2,is3,ip1,ip2,ip3,i,ier0,ier1
	DOUBLE COMPLEX check,c
	DOUBLE PRECISION absc
*
*	rest
*
	include 'ff.h'
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*  #] declarations:
*  #[ calculations:
*
	ier1 = 0
	do 10 is1=1,3
	    is2 = is1 + 1
	    if ( is2 .eq. 4 ) is2 = 1
	    is3 = is2 + 1
	    if ( is3 .eq. 4 ) is3 = 1
	    ip1 = is1 + 3
	    ip2 = is2 + 3
	    ip3 = is3 + 3
*
*	    pi.pj, si.sj
*
	    cpiDpj(is1,is1) = cpi(is1)
	    cpiDpj(ip1,ip1) = cpi(ip1)
*
*	    si.s(i+1)
*
	    if ( absc(cdpipj(is1,ip1)) .le. absc(cdpipj(is2,ip1)) ) then
		cpiDpj(is1,is2) = (cdpipj(is1,ip1) + cpi(is2))/2
	    else
		cpiDpj(is1,is2) = (cdpipj(is2,ip1) + cpi(is1))/2
	    endif
	    cpiDpj(is2,is1) = cpiDpj(is1,is2)
*
*	    pi.si
*
	    if ( absc(cdpipj(is2,is1)) .le. absc(cdpipj(is2,ip1)) ) then
		cpiDpj(ip1,is1) = (cdpipj(is2,is1) - cpi(ip1))/2
	    else
		cpiDpj(ip1,is1) = (cdpipj(is2,ip1) - cpi(is1))/2
	    endif
	    cpiDpj(is1,ip1) = cpiDpj(ip1,is1)
*
*	    pi.s(i+1)
*
	    if ( absc(cdpipj(is2,is1)) .le. absc(cdpipj(ip1,is1)) ) then
		cpiDpj(ip1,is2) = (cdpipj(is2,is1) + cpi(ip1))/2
	    else
		cpiDpj(ip1,is2) = (cdpipj(ip1,is1) + cpi(is2))/2
	    endif
	    cpiDpj(is2,ip1) = cpiDpj(ip1,is2)
*
*	    pi.s(i+2)
*
	    if ( (absc(cdpipj(is2,is1)) .le. absc(cdpipj(ip3,is1)) .and.
     +		  absc(cdpipj(is2,is1)) .le. absc(cdpipj(is2,ip2))) .or.
     +	         (absc(cdpipj(ip3,ip2)) .le. absc(cdpipj(ip3,is1)) .and.
     +		  absc(cdpipj(ip3,ip2)).le.absc(cdpipj(is2,ip2))))then
		cpiDpj(ip1,is3) = (cdpipj(ip3,ip2)+cdpipj(is2,is1))/2
	    else
		cpiDpj(ip1,is3) = (cdpipj(ip3,is1)+cdpipj(is2,ip2))/2
	    endif
	    cpiDpj(is3,ip1) = cpiDpj(ip1,is3)
*
*	    pi.p(i+1)
*
	    if ( absc(cdpipj(ip3,ip1)) .le. absc(cdpipj(ip3,ip2)) ) then
		cpiDpj(ip1,ip2) = (cdpipj(ip3,ip1) - cpi(ip2))/2
	    else
		cpiDpj(ip1,ip2) = (cdpipj(ip3,ip2) - cpi(ip1))/2
	    endif
	    cpiDpj(ip2,ip1) = cpiDpj(ip1,ip2)
   10	continue
	ier = ier + ier1
*  #] calculations:
*###] ffcot3:
	end
*###[ ffbglg:
	subroutine ffbglg(ifound,cqi,cqiqj,cqiDqj,ns,i1,i2,i3,ip1,ip3)
***#[*comment:***********************************************************
*									*
*	Find a configuration which contains big logs, i.e. terms which	*
*	would be IR divergent but for the finite width effects.		*
*	We also use the criterium that delta^{s1 s2 s[34]}_{s1 s2 s[34]}*
*	should not be 0 when m^2 is shifted over nwidth*Im(m^2)		*
*									*
*	Input:	cqi(ns)		(complex)	masses, p^2		*
*		cqiqj(ns,ns)	(complex)	diff cqi(i)-cqi(j)      *									*
*		cqiDqj(ns,ns)	(complex)	cqi(i).cqi(j)	      *									*
*		ns		(integer)	size of cqi,cqiqj	*
*		i1,i2,i3	(integer)	combo to be tested	*
*						small,~onshell,~onshell	*
*		ip1,ip3         (integer)	(i1,i2) and (i1,i3) inx	*
*	Output:	ifound		(integer)	0: no divergence, 1: IR	*
*						-1: del(s,s,s)~0	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ifound,ns,i1,i2,i3,ip1,ip3
	DOUBLE COMPLEX cqi(ns),cqiqj(ns,ns),cqiDqj(ns,ns)
*
*	locals vars
*
	integer i123
	DOUBLE PRECISION absc
	DOUBLE COMPLEX cel3,cdm2,cdm3,c
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*  #] declarations:
*  #[ work:
	ifound = 0
	if ( abs(DBLE(cqi(i1))) .lt. -xloss*(DIMAG(cqi(i2)) +
     +		DIMAG(cqi(i3)))
     +	    .and. abs(DBLE(cqiqj(ip1,i2))) .le. -nwidth*DIMAG(cqi(i2))
     +	    .and. abs(DBLE(cqiqj(ip3,i3))) .le. -nwidth*DIMAG(cqi(i3))
     +		) then
	    ifound = 1
	    return
	endif
	if ( nschem.ge.5 .and. cqi(i1).eq.0 ) then
	    i123 = 2**i1 + 2**i2 + 2**i3
	    if ( i123.eq.2**1+2**2+2**3 .or. i123.eq.2**1+2**2+2**4 )
     +			then
		cel3 = - cqiDqj(i1,i2)**2*cqi(i3)
     +			- cqiDqj(i1,i3)**2*cqi(i2)
     +			+ 2*cqiDqj(i1,i2)*cqiDqj(i1,i3)*cqiDqj(i2,i3)
		cdm2 = cqiDqj(i1,i2)*cqiDqj(ip3,i3) +
     +			cqiDqj(i1,i3)*cqiDqj(ip1,i3)
		cdm3 = -cqiDqj(i1,i2)*cqiDqj(ip3,i2) -
     +			cqiDqj(i1,i3)*cqiDqj(ip1,i2)
		if ( 2*absc(cel3) .lt.-nwidth*(absc(cdm2)*DIMAG(cqi(i2))
     +			+ absc(cdm3)*DIMAG(cqi(i3))) ) then
		    ifound = -1
		endif
	    endif
	endif
*  #] work:
*###] ffbglg:
	end
*###[ ffthre:
	subroutine ffthre(ithres,cqi,cqiqj,ns,i1,i2,ip)
***#[*comment:***********************************************************
*									*
*	look for threshold effects.					*
*	ithres = 1: 3 heavy masses					*
*	ithres = 2: 2 masses almost equal and 1 zero			*
*									*
*	Input:	cqi(ns)		(complex)	usual masses,p^2	*
*		cqiqj(ns,ns)	(complex)	cqi(i)-cqi(j)		*
*		ns		(integer)	size			*
*		i1,i2		(integer)	position to be tested	*
*		ip		(integer)	(i1,i2) index		*
*									*
*	Output:	ithres		(integer)	see above, 0 if nothing	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ithres,ns,i1,i2,ip
	DOUBLE COMPLEX cqi(ns),cqiqj(ns,ns)
*
*	local variables
*
	integer ier0
	DOUBLE COMPLEX c
	DOUBLE PRECISION absc,xq1,xq2,xq3,dq1q2,dq1q3,dq2q3,xlam,d1,d2,
     +		sprecx
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*  #] declarations:
*  #[ work:
	ithres = 0
	if ( DIMAG(cqi(i1)).eq.0 .and. DIMAG(cqi(i2)).eq.0 .or.
     +		nschem.le.4 ) return
	if ( DBLE(cqi(i1)).lt.-DIMAG(cqi(i2)) .and.
     +		abs(DBLE(cqiqj(ip,i2))).lt.-nwidth*DIMAG(cqi(i2))
     +	.or. DBLE(cqi(i2)).lt.-DIMAG(cqi(i1)) .and.
     +		abs(DBLE(cqiqj(ip,i1))).lt.-nwidth*DIMAG(cqi(i1)) ) then
	    ithres = 2
	elseif ( nschem.ge.6 .and. DBLE(cqi(i1)).ne.0 .and.
     +		DBLE(cqi(i2)).ne.0 ) then
	    ier0 = 0
	    xq1 = DBLE(cqi(i1))
	    xq2 = DBLE(cqi(i2))
	    xq3 = DBLE(cqi(ip))
	    dq1q2 = DBLE(cqiqj(i1,i2))
	    dq1q3 = DBLE(cqiqj(i1,ip))
	    dq2q3 = DBLE(cqiqj(i2,ip))
	    sprecx = precx
	    precx = precc
	    call ffxlmb(xlam,xq1,xq2,xq3, dq1q2,dq1q3,dq2q3)
	    precx = sprecx
	    d1 = absc(cqiqj(i1,ip) - cqi(i2))
	    d2 = absc(cqiqj(i2,ip) - cqi(i1))
*	    if ( d1 .lt. -nwidth*DIMAG(cqi(i1)) .or.
**     +		 d2 .lt. -nwidth*DIMAG(cqi(i2)) )
**     +		call ffwarn(182,ier0,x1,x1)
	    if ( abs(xlam) .lt. -nwidth*(DBLE(d1)*
     +		    DIMAG(cqi(i1)) + d2*DIMAG(cqi(i2))) ) then
		ithres = 1
	    endif
	endif
*  #] work:
*###] ffthre:
	end
*###[ ffcod3:
	subroutine ffcod3(cpi)
***#[*comment:***********************************************************
*									*
*	Convert real dorproducts into complex ones, adding the		*
*	imaginary parts where appropriate.				*
*									*
*	Input:	cpi(6)			complex		m^2, p^2 	*
*		/ffdots/fpij3(6,6)	real		p.p real	*
*									*
*	Output:	/ffcots/cfpij3(6,6)	complex		p.p complex	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE COMPLEX cpi(6)
*
*	local variables
*
	integer i,i1,i2,ip
*
*	common blocks
*
	include 'ff.h'
*
*  #] declarations:
*  #[ add widths:
*
	do 25 i=1,3
	    ip = i+3
	    i1 = 1 + mod(i,3)
	    i2 = 1 + mod(i1,3)
*	    s.s
	    cfpij3(i,i) = cpi(i)
	    cfpij3(i1,i) = DCMPLX(DBLE(fpij3(i1,i)),
     +	    	(DIMAG(cpi(i1))+DIMAG(cpi(i)))/2)
	    cfpij3(i,i1) = cfpij3(i1,i)
*	    s.p
	    cfpij3(i,ip) = DCMPLX(DBLE(fpij3(i,ip)),
     +		(DIMAG(cpi(i1))-DIMAG(cpi(i)))/2)
	    cfpij3(ip,i) = cfpij3(i,ip)
	    cfpij3(i1,ip) = DCMPLX(DBLE(fpij3(i1,ip)),
     +		(DIMAG(cpi(i1))-DIMAG(cpi(i)))/2)
	    cfpij3(ip,i1) = cfpij3(i1,ip)
	    cfpij3(i2,ip) = DCMPLX(DBLE(fpij3(i2,ip)),
     +		(DIMAG(cpi(i1))-DIMAG(cpi(i)))/2)
	    cfpij3(ip,i2) = cfpij3(i2,ip)
*	    p.p
	    cfpij3(ip,ip) = cpi(ip)
	    cfpij3(ip,i1+3) = fpij3(ip,i1+3)
	    cfpij3(i1+3,ip) = cfpij3(ip,i1+3)
   25	continue
	fodel2 = fdel2
*
*  #] add widths:
*###] ffcod3:
	end
