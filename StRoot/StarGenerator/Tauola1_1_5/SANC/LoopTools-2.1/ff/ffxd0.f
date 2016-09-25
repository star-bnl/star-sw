*--#[ log:
*	$Id: ffxd0.f,v 1.1 2016/09/23 18:39:43 jwebb Exp $
*	$Log: ffxd0.f,v $
*	Revision 1.1  2016/09/23 18:39:43  jwebb
*	Initial commit of Tauola
*	
c Revision 1.4  1996/01/22  13:32:52  gj
c Added sanity check on ier; if it is larger than 16 some routines will not
c compute anything.
c
c Revision 1.3  1995/11/28  13:37:47  gj
c Found wrong sign in ffcdna, fixed typo in ffcrp.
c Killed first cancellation in ffcdna - more to follow
c Added warnings to ffwarn.dat; slightly changed debug output in ffxd0.f
c
c Revision 1.2  1995/10/17  06:55:12  gj
c Fixed ieps error in ffdcrr (ffcxs4.f), added real case in ffcrr, debugging
c info in ffxd0, and warned against remaining errors for del2=0 in ffrot4
c (ffxd0h.f)
c
*--#] log:


*###[ ffxd0:
	subroutine ffxd0(cd0,xpi,ier)
***#[*comment:***********************************************************
*									*
*			  1   /						*
*	calculate cd0 =	----- \dq [(q^2 + 2*s_1.q)*(q^2 + 2*s2.q)	*
*			ipi^2 /      *(q^2 + 2*s3.q)*(q^2 + 2*s4.q)]^-1	*
*									*
*	      |p9							*
*	\p8   V	   p7/							*
*	 \	    /							*
*	  \________/							*
*	  |   m4   |							*
*    =	  |	   |	/____						*
*	m1|	   |m3	\ p10						*
*	  |	   |		all momenta are incoming		*
*	  |________|							*
*	  /  m2	   \							*
*	 /	    \							*
*	/p5	   p6\							*
*									*
*									*
*	following the two-three-point-function method in 't hooft &	*
*	veltman. this is only valid if there is a lambda(pij,mi,mj)>0	*
*									*
*	Input:	xpi = mi^2	   (real)  i=1,4			*
*		xpi = pi.pi	   (real)  i=5,8 (note: B&D metric)	*
*		xpi(9)=s	   (real)  (=p13)			*
*		xpi(10)=t	   (real)  (=p24)			*
*		xpi(11)=u	   (real)  u=p5.p5+..-p9.p9-p10.10 or 0	*
*		xpi(12)=v	   (real)  v=-p5.p5+p6.p6-p7.p7+.. or 0	*
*		xpi(13)=w	   (real)  w=p5.p5-p6.p6+p7.p7-p8.p8+.. *
*	output:	cd0		  (complex)				*
*		ier		  (integer) <50:lost # digits 100=error	*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE PRECISION xpi(13)
	DOUBLE COMPLEX cd0
	integer ier
*
*	local variables
*
	logical luvw(3)
	DOUBLE PRECISION dpipj(10,13)
*
*	common blocks:
*
	include 'ff.h'
*  #] declarations:
*  #[ catch totally massless case:
*
	if (abs(xpi(1) + xpi(2) + xpi(3) + xpi(4)) .lt. 1D-10) then
	  call ffxd0m0(cd0,xpi,ier)
	  return
	endif
*
*  #] catch totally massless case:
*  #[ call ffdif4, ffxd0a:
*
	call ffdif4(dpipj,luvw,xpi)
	call ffxd0a(cd0,xpi,dpipj,ier)
*
*	restore the zeros for u,v,w as we have calculated them
*	ourselves and the user is unlikely to do this...
*
	if ( luvw(1) ) xpi(11) = 0
	if ( luvw(2) ) xpi(12) = 0
	if ( luvw(3) ) xpi(13) = 0
*
*  #] call ffdif4, ffxd0a:
*###] ffxd0:
	end


*###[ ffxd0a:
	subroutine ffxd0a(cd0,xpi,dpipj,ier)
*
*	glue routine which calls ffxd0b with ndiv=0
*
	implicit none
*
*	arguments
*
	integer ier
	DOUBLE PRECISION xpi(13),dpipj(10,13)
	DOUBLE COMPLEX cd0
*
*	locals
*
	DOUBLE COMPLEX cs,cfac
*
*	and go!
*
	call ffxd0b(cs,cfac,xpi,dpipj,0,ier)
	cd0 = cs*cfac
*
*###] ffxd0a:
	end


*###[ ffxd0b:
	subroutine ffxd0b(cs,cfac,xpi,dpipj,ndiv,ier)
***#[*comment:***********************************************************
*									*
*			  1   /						*
*	calculate cd0 =	----- \dq [(q^2 + 2*s_1.q)*(q^2 + 2*s2.q)	*
*			ipi^2 /      *(q^2 + 2*s3.q)*(q^2 + 2*s4.q)]^-1	*
*									*
*	      |p9							*
*	\p8   V	   p7/							*
*	 \	    /							*
*	  \________/							*
*	  |   m4   |							*
*    =	  |	   |	/____						*
*	m1|	   |m3	\ p10						*
*	  |	   |		all momenta are incoming		*
*	  |________|							*
*	  /  m2	   \							*
*	 /	    \							*
*	/p5	   p6\							*
*									*
*									*
*	following the two-three-point-function method in 't hooft &	*
*	veltman. this is only valid if there is a lambda(pij,mi,mj)>0	*
*									*
*	Input:	xpi = mi^2	   (real)  i=1,4			*
*		xpi = pi.pi	   (real)  i=5,8 (note: B&D metric)	*
*		xpi(9)=s	   (real)  (=p13)			*
*		xpi(10)=t	   (real)  (=p24)			*
*		xpi(11)=u	   (real)  u=p5.p5+..-p9.p9-p10.10	*
*		xpi(12)=v	   (real)  v=-p5.p5+p6.p6-p7.p7+..	*
*		xpi(13)=w	   (real)  w=p5.p5-p6.p6+p7.p7-p8.p8+.. *
*		dpipj(10,13)	   (real)  = pi(i) - pi(j)		*
*	output:	cs,cfac		  (complex) cd0 = cs*cfac		*
*		ier		  (integr) 0=ok 1=inaccurate 2=error	*
*	calls:	ffcxs3,ffcxr,ffcrr,...					*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ndiv,ier
	DOUBLE PRECISION xpi(13),dpipj(10,13)
	DOUBLE COMPLEX cs,cfac
*
*	local variables
*
	integer i,j,itype,ini2ir,ier2,idone,ier0
	logical ldel2s
	DOUBLE COMPLEX c,cs1,cs2
	DOUBLE PRECISION absc,xmax,xpip(13),dpipjp(10,13),piDpjp(10,10),
     +		qiDqj(10,10),del2s,lambda0
	save ini2ir,lambda0
*
*	common blocks:
*
	include 'ff.h'
*
*	memory
*
	integer iermem(memory),ialmem(memory),memind,ierini,nscsav,
     +		isgnsa
	logical onssav
	DOUBLE PRECISION xpimem(10,memory),dl4mem(memory)
	DOUBLE COMPLEX csmem(memory),cfcmem(memory)
	save memind,iermem,ialmem,xpimem,dl4mem,nscsav,onssav,csmem,
     +		cfcmem
*
*	statement function:
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*	data
*
	data memind /0/
	data ini2ir /0/
	data lambda0 /1D0/
*
*  #] declarations:
*  #[ initialisations:
	cs = 0
	cfac = 1
	idsub = 0
	idone = 0
*  #] initialisations:
*  #[ check for IR 4point function:
*
	call ffxdir(cs,cfac,idone,xpi,dpipj,4,ndiv,ier)
	if ( idone .le. 0 .and. ndiv .gt. 0 ) then
	    cs = 0
	    cfac = 1
	    ier = 0
	    return
	endif
	if ( idone .gt. 0 ) then
	    return
	endif
*
*  #] check for IR 4point function:
*  #[ rotate to calculable position:
	call ffrot4(irota4,del2s,xpip,dpipjp,piDpjp,xpi,dpipj,qiDqj,4,
     +		itype,ier)
	if ( itype .lt. 0 ) then
	    print *,'ffxd0b:  error:  Cannot handle this ',
     +		' masscombination yet:'
	    print *,(xpi(i),i=1,13)
	    return
	endif
	if ( itype .eq. 1 ) then
	    ldel2s = .TRUE.
	    isgnal = +1
	else
	    ldel2s = .FALSE.
	endif
*  #] rotate to calculable position:
*  #[ treat doubly IR divergent case:
	if ( itype .eq. 2 ) then
*
*	    double IR divergent diagram, i.e. xpi(3)=xpi(4)=xpi(7)=0
*
	    if ( ini2ir .eq. 0 ) then
		ini2ir = 1
		print *,'ffxd0b: using the log(lam) prescription to'
		print *,'  regulate the 2 infrared poles to match'
		print *,'  with soft gluon massive, lam^2 =',lambda2
	    endif
	    ier2 = 0
	    call ffx2ir(cs1,cs2,xpip,dpipjp,ier2)
	    del2s = -lambda2**2/4
*
*	    correct for the wrongly treated IR pole
*
	    cs = cs + (cs1 + cs2)/cfac
	    ier = max(ier,ier2)
	    xmax = max(absc(cs1),absc(cs2))/absc(cfac)
	    if ( absc(cs) .lt. xloss*xmax )
     +		call ffwarn(172,ier,absc(cs),xmax)
	    if ( .not.ldot ) return
	endif
*
*  #] treat doubly IR divergent case:
*  #[ look in memory:
	ierini = ier
	isgnsa = isgnal
*
*	initialise memory
*
	if ( lmem .and. idone .eq. 0 .and. (memind .eq. 0 .or. nschem
     +		.ne. nscsav .or. (onshel .neqv. onssav) ) ) then
	    memind = 0
	    nscsav = nschem
	    onssav = onshel
	    do 2 i=1,memory
		do 1 j=1,10
		    xpimem(j,i) = 0
    1		continue
		ialmem(i) = 0
    2	    continue
	endif
*
	if ( lmem .and. idone .eq. 0 .and. lambda2 .eq. lambda0 ) then
	    do 150 i=1,memory
		do 130 j=1,10
		    if ( xpip(j) .ne. xpimem(j,i) ) goto 150
  130		continue
*		we use ialmem(i)==0 to signal that both are covered as
*		the sign was flipped during the computation
		if ( ialmem(i).ne.isgnal .and. ialmem(i).ne.0 ) goto 150
*		we found an already calculated masscombination ..
*		(maybe check differences as well)
		cs = csmem(i)
		cfac = cfcmem(i)
		ier = ier+iermem(i)
		if ( ldot ) then
		    fdel4s = dl4mem(i)
*		    we forgot to calculate the dotproducts
		    idone = 1
		    goto 51
		endif
		return
  150	    continue
	elseif ( lmem ) then
	    lambda0 = lambda2
	endif
   51	continue
*  #] look in memory:
*  #[ get dotproducts:
*
*	Calculate the dotproducts (in case it comes out of memory the
*	error is already included in ier)
*
	ier0 = ier
	call ffgdt4(piDpjp,xpip,dpipjp,xpi,ier0)
	if ( idone .gt. 0 ) return
	ier = ier0
	if ( ier.ge.100 ) then
	    cs = 0
	    cfac = 1
	    return
	endif
*
*  #] get dotproducts:
*  #[ calculations:
*
	call ffxd0e(cs,cfac,xmax, .FALSE.,ndiv,xpip,dpipjp,piDpjp,del2s,
     +		ldel2s,ier)
*
*  #] calculations:
*  #[ add to memory:
*
*	memory management :-)
*
	if ( lmem ) then
	    memind = memind + 1
	    if ( memind .gt. memory ) memind = 1
	    do 200 j=1,10
		xpimem(j,memind) = xpip(j)
  200	    continue
	    csmem(memind)  = cs
	    cfcmem(memind) = cfac
	    iermem(memind) = ier-ierini
	    ialmem(memind) = isgnal
	    dl4mem(memind) = fdel4s
	    if ( isgnal.ne.isgnsa ) then
		ialmem(memind) = 0
	    endif
	endif
*  #] add to memory:
*###] ffxd0b:
	end


*###[ ffxd0e:
	subroutine ffxd0e(cs,cfac,xmax,lir,ndiv,xpip,dpipjp,piDpjp,
     +		del2s,ldel2s,ier)
***#[*comment:***********************************************************
*									*
*	Break in the calculation of D0 to allow the E0 to tie in in a	*
*	logical position.  This part gets untransformed momenta but	*
*	rotated momenta in and gives the D0 (in two pieces) and the	*
*	maximum term back.						*
*									*
*	Input	xpip	real(13)					*
*		dpipjp	real(10,13)					*
*		piDpjp	real(10,10)					*
*		del2s	real						*
*		ldel2s	logical						*
*		lir	logical		if TRUE it can still be IR-div	*
*		ndiv	integer		number of required divergences	*
*									*
*	Output:	cs	complex		the fourpoint function without	*
*					overall factor (sum of dilogs)	*
*		cfac	complex		this overall factor		*
*		xmax	real		largest term in summation	*
*		ier	integer		usual error flag		*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer ndiv,ier
	logical lir,ldel2s
	DOUBLE PRECISION xpip(13),dpipjp(10,13),piDpjp(10,10),xmax,del2s
	DOUBLE COMPLEX cs,cfac
*
*	local variables
*
	DOUBLE COMPLEX c,cs4(175),cs3(2)
	logical laai
	integer i,ier0,itime,maxlos,init,isoort(16),ipi12(26),
     +		ipi123(2),ipi12t,idone
	DOUBLE PRECISION absc,sdel2s,ai(4),daiaj(4,4),aai(4),
     +		dt3t4,xqi(10),dqiqj(10,10),qiDqj(10,10),xfac
	save maxlos
*
*	common blocks:
*
	include 'ff.h'
*
*	statement function:
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*	data
*
	data init /0/
*  #] declarations:
*  #[ check for IR 4point function:
	if ( lir ) then
*
	    ier0 = ier
	    call ffxdir(cs,cfac,idone,xpip,dpipjp,4,0,ier)
	    if ( idone .le. 0 .and. ndiv .gt. 0 ) then
		cs = 0
		cfac = 1
		xmax = 0
		ier = 0
		return
	    endif
	    if ( idone .gt. 0 ) then
		xmax = abs(cs)*10d0**(-mod((ier0-ier),50))
		return
	    endif
	endif
*
*  #] check for IR 4point function:
*  #[ init:
*
*	initialize cs4:
*
	do 80 i=1,175
	    cs4(i) = 0
   80	continue
	do 90 i=1,26
	    ipi12(i) = 0
   90	continue
	cs = 0
*
*  #] init:
*  #[ transform the masses and momenta:
	itime = 1
   25	continue
*
*	Transform with the A's of gerard 't hooft's transformation:
*
*	NOTE: for some odd reason I cannot vary isgnal,isgn34
*	independently!
*
	isgn34 = isgnal
	sdel2s = isgn34*sqrt(-del2s)
	ier0 = ier
	call ffai(ai,daiaj,aai,laai,del2s,sdel2s,xpip,dpipjp,piDpjp,
     +		ier0)
	if ( ier0 .ge. 100 ) goto 70
	call fftran(ai,daiaj,aai,laai,xqi,dqiqj,qiDqj,del2s,sdel2s,
     +		xpip,dpipjp,piDpjp,ier0)
	if ( ier0 .ge. 100 ) goto 70
	if ( .not.ldel2s ) then
	    dt3t4 = -2*ai(3)*ai(4)*sdel2s
	    if ( dt3t4 .eq. 0 ) then
*		don't know what to do...
		call fferr(85,ier)
		return
	    endif
	else
*	    this value is modulo the delta of xpip(4)=xpip(3)(1+2delta)
	    dt3t4 = -2*ai(4)**2*xpip(3)
	endif

   70	continue
*
*	If we lost too much accuracy try the other root...
*	(to do: build in a mechanism for remembering this later)
*
	if ( init .eq. 0 ) then
	    init = 1
*	    go ahead if we have half the digits left
	    maxlos = -int(log10(precx))/2
	endif
	if ( ier0-ier .gt. maxlos ) then
	    if ( itime .eq. 1 ) then
		itime = 2
		if ( ier0-ier .ge. 100 ) itime = 100
		isgnal = -isgnal
		goto 25
	    else
		if ( ier0-ier .lt. 100 ) then
*		    it does not make any sense to go on, but do it anyway
		elseif ( itime.eq.100 ) then
		    call fferr(72,ier)
		    cfac = 1
		    return
		elseif ( itime.le.2 ) then
*		    the first try was better
		    isgnal = -isgnal
		    itime = 3
		    goto 25
		endif
	    endif
	endif
	ier = ier0
*  #] transform the masses and momenta:
*  #[ calculations:
	call ffxd0p(cs4,ipi12,isoort,cfac,xpip,dpipjp,piDpjp,
     +		xqi,dqiqj,qiDqj,ai,daiaj,ldel2s,ier)
	xfac = -ai(1)*ai(2)*ai(3)*ai(4)/dt3t4
*
*	see the note at the end of this section about the sign
*
	if ( DIMAG(cfac) .eq. 0 ) then
	    cfac = xfac/DBLE(cfac)
	else
	    cfac = DBLE(xfac)/cfac
	endif
*
*	sum'em up:
*
	cs3(1) = 0
	cs3(2) = 0
	xmax = 0
	do 110 i=1,80
	    cs3(1) = cs3(1) + cs4(i)
	    xmax = max(xmax,absc(cs3(1)))
  110	continue
	do 111 i=81,160
	    cs3(2) = cs3(2) + cs4(i)
	    xmax = max(xmax,absc(cs3(2)))
  111	continue
	cs = cs3(1) - cs3(2)
	do 112 i=161,175
	    cs = cs + cs4(i)
	    xmax = max(xmax,absc(cs))
  112	continue
	ipi123(1) = 0
	ipi123(2) = 0
	do 113 i=1,8
	    ipi123(1) = ipi123(1) + ipi12(i)
  113	continue
	do 114 i=9,16
	    ipi123(2) = ipi123(2) + ipi12(i)
  114	continue
	ipi12t = ipi123(1) - ipi123(2)
	do 120 i=17,26
	    ipi12t = ipi12t + ipi12(i)
  120	continue
	cs = cs + ipi12t*DBLE(pi12)
*
*	If the imaginary part is very small it most likely is zero
*	(can be removed, just esthetically more pleasing)
*
	if ( abs(DIMAG(cs)) .lt. precc*abs(DBLE(cs)) )
     +		cs = DCMPLX(DBLE(cs))
*
*	it is much nicer to have the sign of cfac fixed, say positive
*
	if ( DBLE(cfac) .lt. 0 .or. (DBLE(cfac) .eq. 0 .and. DIMAG(cfac)
     +		.lt. 0 ) ) then
	    cfac = -cfac
	    cs = -cs
	endif
*  #] calculations:
*###] ffxd0e:
	end


*###[ ffxd0r:
	subroutine ffxd0r(cd0,xpi,ier)
***#[*comment:***********************************************************
*									*
*	Tries all 12 permutations of the 4pointfunction			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
	integer ier
	DOUBLE PRECISION xpi(13),xqi(13)
	DOUBLE COMPLEX cd0,cd0p
	integer inew(13,6),irota,ier0,ier1,i,j,ialsav
	save inew
	include 'ff.h'
	data inew /1,2,3,4,5,6,7,8,9,10,11,12,13,
     +		   4,1,2,3,8,5,6,7,10,9,11,13,12,
     +		   3,4,1,2,7,8,5,6,9,10,11,12,13,
     +		   2,3,4,1,6,7,8,5,10,9,11,13,12,
     +		   4,2,3,1,10,6,9,8,7,5,12,11,13,
     +		   1,3,2,4,9,6,10,8,5,7,12,11,13/
*  #] declarations:
*  #[ calculations:
	cd0 = 0
	ier0 = ier
	ier = 999
	ialsav = isgnal
	do 30 j = -1,1,2
	    do 20 irota=1,6
		do 10 i=1,13
		    xqi(inew(i,irota)) = xpi(i)
   10		continue
		ier1 = ier0
		ner = 0
		id = id + 1
		isgnal = ialsav
		print '(a,i1,a,i2)','---#[ rotation ',irota,': isgnal ',
     +			isgnal
		call ffxd0(cd0p,xqi,ier1)
		ier1 = ier1 + ner
		print '(a,i1,a,i2,a)','---#] rotation ',irota,
     +			': isgnal ',isgnal,' '
		print '(a,2g28.16,i3)','d0 = ',cd0p,ier1
		if ( ier1 .lt. ier ) then
		    cd0 = cd0p
		    ier = ier1
		endif
   20	    continue
	    ialsav = -ialsav
   30	continue
*  #] calculations:
*###] ffxd0r:
	end


*###[ ffxd0d:
	subroutine ffxd0d(cd0,xpi,piDpj,del3p,del4s,info,ier)
***#[*comment:***********************************************************
*									*
*	Entry point to the four point function with dotproducts given.	*
*	Necessary to avoid cancellations near the borders of phase	*
*	space.								*
*									*
*	Input:	xpi(13)	      real	1-4: mi^2, 5-10: pi^2,s,t	*
*					optional: 11:u, 12:v, 13:w	*
*		info	      integer	0: no extra info		*
*					1: piDpj(i,j), i,j>4 is defined	*
*					2: del3p is also defined	*
*					3: all piDpj are given		*
*					4: del4s is also given		*
*		piDpj(10,10)  real	pi.pj in B&D metric;		*
*					1-4:si.sj=(m_i^2+m_j^2-p_ij^2)/2*
*					cross: si.pjk=si.pj-si.pk	*
*					5-10: pi.pj			*
*		del3p	      real	det(pi.pj)			*
*		del4s	      real	det(si.sj) (~square overall fac)*
*		ier	      integer	#digits accuracy lost in input	*
*	Output:	cd0	      complex	D0				*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer info,ier
	DOUBLE PRECISION xpi(13),piDpj(10,10),del3p,del4s
	DOUBLE COMPLEX cd0
*
*	local vars
*
	integer i,j
*
*	common blocks
*
	include 'ff.h'
*
*  #] declarations:
*  #[ hide information in common blocks:
*
	idot = info
	if ( idot.ne.0 ) then
	    if ( idot.gt.0 .and. idot.le.2 ) then
		do 20 i=5,10
		    do 10 j=5,10
			fpij4(j,i) = piDpj(j,i)
   10		    continue
   20		continue
	    elseif ( idot.ge.3 ) then
		do 40 i=1,10
		    do 30 j=1,10
			fpij4(j,i) = piDpj(j,i)
   30		    continue
   40		continue
	    endif
	    if ( abs(idot).ge.2 ) then
		fdel3 = del3p
	    endif
	    if ( abs(idot).ge.4 ) then
		fdel4s = del4s
	    endif
	endif
*
*  #] hide information in common blocks:
*  #[ call ffxd0:
*
	call ffxd0(cd0,xpi,ier)
*
*	invalidate all the common blocks for the next call
*
	idot = 0
*
*  #] call ffxd0:
*###] ffxd0d:
	end


*###[ ffdif4:
	subroutine ffdif4(dpipj,luvw,xpi)
***#[*comment:***********************************************************
*									*
*	Compute the elements 11-13 in xpi and the differences dpipj	*
*	Note that the digits lost in dpipj are not counted towards	*
*	the total.							*
*									*
*	Input:	xpi(1:10)	real		masses, momenta^2	*
*									*
*	Output:	xpi(11:13)	real		u and similar vars v,w	*
*		luvw(3)		logical		TRUE if xpi(10+i) has	*
*						been computed here	*
*		dpipj(10,13)	real		xpi(i) - xpi(j)		*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	logical luvw(3)
	DOUBLE PRECISION xpi(13),dpipj(10,13)
*
*	local variables
*
	integer i,j
*
*	common blocks
*
	include 'ff.h'
*
*  #] declarations:
*  #[ get differences:
*	simulate the differences in the masses etc..
	if ( xpi(11)  .eq. 0 ) then
	    xpi(11) = xpi(5)+xpi(6)+xpi(7)+xpi(8)-xpi(9)-xpi(10)
	    luvw(1) = .TRUE.
	else
	    luvw(1) = .FALSE.
	endif
	if ( xpi(12)  .eq. 0 ) then
	    xpi(12) = -xpi(5)+xpi(6)-xpi(7)+xpi(8)+xpi(9)+xpi(10)
	    luvw(2) = .TRUE.
	else
	    luvw(2) = .FALSE.
	endif
	if ( xpi(13)  .eq. 0 ) then
	    if ( max(abs(xpi(5)),abs(xpi(7))) .gt.
     +		 max(abs(xpi(9)),abs(xpi(10))) ) then
		xpi(13) = -xpi(12) + 2*(xpi(9)+xpi(10))
	    else
		xpi(13) = -xpi(11) + 2*(xpi(5)+xpi(7))
	    endif
*	    xpi(13) = xpi(5)-xpi(6)+xpi(7)-xpi(8)+xpi(9)+xpi(10)
	    luvw(3) = .TRUE.
	else
	    luvw(3) = .FALSE.
	endif
	    do 20 i=1,13
		do 19 j=1,10
		    dpipj(j,i) = xpi(j) - xpi(i)
   19		continue
   20	    continue
*  #] get differences:
*###] ffdif4:
	end
