*--#[ log:
*	$Id: ffxd0h.f,v 1.1 2016/09/23 18:39:43 jwebb Exp $
*	$Log: ffxd0h.f,v $
*	Revision 1.1  2016/09/23 18:39:43  jwebb
*	Initial commit of Tauola
*	
c Revision 1.6  1996/01/22  13:33:49  gj
c Added the word 'error' to print statements in ffxuvw that u,v,w were wrong
c
c Revision 1.5  1995/12/08  10:48:32  gj
c Changed xloss to xlosn to prevent spurious error messages.
c
c Revision 1.4  1995/11/10  18:55:46  gj
c JUst added some comments in ffrot4
c
c Revision 1.3  1995/10/29  15:37:43  gj
c Revision 1.2  1995/10/17  06:55:13  gj
c Fixed ieps error in ffdcrr (ffcxs4.f), added real case in ffcrr, debugging
c info in ffxd0, and warned against remaining errors for del2=0 in ffrot4
c (ffxd0h.f)
c
*--#] log: 


*###[ ffrot4:
	subroutine ffrot4(irota,del2,xqi,dqiqj,qiDqj,xpi,dpipj,piDpj,ii,
     +		itype,ier)
***#[*comment:***********************************************************
*									*
*	rotates the arrays xpi, dpipj into xqi,dqiqj over irota places	*
*	such that del2(s3,s4)<=0. itype=0 unless del2(s3,s4)=0 (itype=1)*
*	itype=2 if the 4pointfunction is doubly IR-divergent		*
*	((0,0,0)vertex)							*
*									*
*	Input:	xpi(13)		real	momenta squared			*
*		dpipj(10,13)	real	xpi(i) - xpi(j)			*
*		piDpj(10,10)	real	if ( ii>4) pi.pj		*
*		ii		integer	4: from Do, 5: from E0		*
*	Output:	irota		integer	# of positions rotated + 1	*
*		del2		real	delta(s3,s4,s3,s4) chosen	*							*
*		xqi,dqiqj,qiDqj	real	rotated (q->p)			*
*		itype		integer 0:normal, -1:failure, 1:del2=0	*
*					2:doubly IR			*
*		ier		integer	usual error flag		*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer irota,ier,ii,itype
	DOUBLE PRECISION del2,xpi(13),dpipj(10,13),piDpj(10,10),
     +		xqi(13),dqiqj(10,13),qiDqj(10,10)
*
*	local variables
*
	integer i,j,izero,ier0,init
	DOUBLE COMPLEX chulp(4,4)
	save init
*
*	common blocks
*
	include 'ff.h'
*
*	data
*
	data init /0/
*
*  #] declarations:
*  #[ find out which del2 is negative: (or zero)
	izero = 0
	do 40 irota = 1,12
*
*	    first check if we have a doubly IR divergent diagram
*
	    if ( xpi(iold(3,irota)) .eq. 0 .and.
     +		 xpi(iold(4,irota)) .eq. 0 .and.
     +		 xpi(iold(7,irota)) .eq. 0 .and.
     +		 dpipj(iold(1,irota),iold(8,irota)) .eq. 0 .and.
     +		 dpipj(iold(2,irota),iold(6,irota)) .eq. 0 ) then
		del2 = 0
		goto 41
	    endif
*
*	    We can at this moment only handle s3^2 = 0
*	    (Hope to include two masses 0 later)
*	    I hope nothing goes wrong if we leave out:
*		>xpi(iold(1,irota)) .eq. 0 .or.
*     +		 xpi(iold(2,irota)) .eq. 0 .or.
*     +		 <
*	    'cause I can't see why it was included in the first place..
*
	    if (  xpi(iold(4,irota)) .eq. 0 ) goto 40
*
*	    Well, the combination s2=0, p6=s3, p10=s4 gives 1/A2=0 twice
*
	    if ( xpi(iold(2,irota)) .eq. 0 .and.
     +			dpipj(iold( 6,irota),iold(3,irota)) .eq. 0 .and.
     +			dpipj(iold(10,irota),iold(4,irota)) .eq. 0)
     +         goto 40
*
*	    phenomenologically this combo also gives an infinite result
*
	    if ( xpi(iold(1,irota)) .eq. 0 .and.
     +		 xpi(iold(2,irota)) .eq. 0 .and.
     +			dpipj(iold( 8,irota),iold(4,irota)) .eq. 0 .and.
     +			dpipj(iold( 9,irota),iold(3,irota)) .eq. 0)
     +         goto 40
*
*	    I just found out that this gives two times 1/A1 = 0
*
	    if ( xpi(iold(7,irota)) .eq. 0 .and.
     +		 dpipj(iold(9,irota),iold(3,irota))+
     +		 dpipj(iold(4,irota),iold(8,irota)) .eq. 0 )
     +         goto 40

	    if ( xpi(iold(1,irota)) .eq. 0 .and.
     +		 dpipj(iold(9,irota),iold(3,irota)) .eq. 0 .and.
     +		 dpipj(iold(4,irota),iold(8,irota)) .eq. 0 .and.
     +		 .not.lnasty )
     +         goto 40
*
*	    the nasty case wants xpi(1)=0, xpi(2) real:
*
	    if ( lnasty ) then
		if ( xpi(iold(1,irota)).ne.0 .or. DIMAG(
     +			c2sisj(iold(1,irota),iold(2,irota))).ne.0 ) then
		    print *,'no good: nasty but s1!=0 or s2 not real'
		    goto 40
		endif
	    endif
*
	    ier0 = 0
	    call ffxlam(del2,xpi,dpipj,10,
     +		iold(3,irota),iold(4,irota),iold(7,irota))
*
*	    we can only handle del2=0 if p_i^2 = 0 (and thus m_i=m_{i+1})
*
	    if ( del2 .lt. 0 ) then
		itype = 0
		goto 50
	    endif
	    if ( del2 .eq. 0 .and. izero .eq. 0 .and. xpi(iold(7,irota))
     +							.eq. 0 ) then
		izero = irota
	    endif
   40	continue
	    ier = ier + ier0
	    if ( izero .eq. 0 ) then
		call fferr(54,ier)
		itype = -1
		irota = 1
	    else
		irota = izero
		del2 = 0
		itype = 1
		if ( init.lt.10 ) then
		    init = init + 1
	print *,'ffrota: warning: the algorithms for del2=0 have not '
	print *,'        yet been tested thoroughly, and in fact are '
	print *,'        known to contain bugs.'
	print *,' ==> DOUBLECHECK EVERYTHING WITH SMALL SPACELIKE p^2'
		endif
	    endif
	    goto 50
   41	continue
	    itype = 2
   50	continue
*  #] find out which del2 is negative:
*  #[ rotate:
	do 20 i=1,13
	    xqi(i) = xpi(iold(i,irota))
	    do 10 j=1,10
		dqiqj(j,i) = dpipj(iold(j,irota),iold(i,irota))
   10	    continue
   20	continue
	if ( ii .eq. 5 ) then
	    do 120 i=1,10
		do 110 j=1,10
		    qiDqj(j,i) = isgrot(iold(j,irota),irota)*
     +				 isgrot(iold(i,irota),irota)*
     +			piDpj(iold(j,irota),iold(i,irota))
  110		continue
  120	    continue
	endif
	if ( lsmug .or. lnasty ) then
	    do 220 j=1,4
		do 210 i=1,4
		    chulp(i,j) = c2sisj(i,j)
  210		continue
  220	    continue
	    do 240 j=1,4
		do 230 i=1,4
		   c2sisj(i,j) = chulp(iold(i,irota),iold(j,irota))
  230		continue
  240	    continue
	endif
*  #] rotate:
*###] ffrot4:
	end


*###[ ffxlam:
	subroutine ffxlam(xlam,xpi,dpipj,ns,i1,i2,i3)
*************************************************************************
*									*
*	calculate in a numerically stable way				*
*	xlam(xpi(i1),xpi(i2),xpi(i3)) =					*
*		= -((xpi(i1)+xpi(i2)-xpi(i3))/2)^2 + xpi(i1)*xpi(i2)	*
*	or a permutation						*
*	ier is the usual error flag.					*
*									*
*************************************************************************
	implicit none
*
*	arguments:
*
	integer ns,i1,i2,i3
	DOUBLE PRECISION xlam,xpi(ns),dpipj(ns,ns)
*
*	local variables
*
	DOUBLE PRECISION s1,s2
*
*	common blocks
*
	include 'ff.h'
*
*	calculations
*
	if ( abs(xpi(i1)) .gt. max(abs(xpi(i2)),abs(xpi(i3))) ) then
	    s1 = xpi(i2)*xpi(i3)
	    if ( abs(dpipj(i1,i2)) .lt. abs(dpipj(i1,i3)) ) then
		s2 = ((dpipj(i1,i2) - xpi(i3))/2)**2
	    else
		s2 = ((dpipj(i1,i3) - xpi(i2))/2)**2
	    endif
	elseif ( abs(xpi(i2)) .gt. abs(xpi(i3)) ) then
	    s1 = xpi(i1)*xpi(i3)
	    if ( abs(dpipj(i1,i2)) .lt. abs(dpipj(i2,i3)) ) then
		s2 = ((dpipj(i1,i2) + xpi(i3))/2)**2
	    else
		s2 = ((dpipj(i2,i3) - xpi(i1))/2)**2
	    endif
	else
	    s1 = xpi(i1)*xpi(i2)
	    if ( abs(dpipj(i1,i3)) .lt. abs(dpipj(i2,i3)) ) then
		s2 = ((dpipj(i1,i3) + xpi(i2))/2)**2
	    else
		s2 = ((dpipj(i2,i3) + xpi(i1))/2)**2
	    endif
	endif
	xlam = s1 - s2
*###] ffxlam:
	end


*###[ ffdot4:
	subroutine ffdot4(piDpj,xpi,dpipj,ns,ier)
***#[*comment:***********************************************************
*									*
*	calculate the dotproducts pi.pj with				*
*									*
*		pi = si		i1=1,4					*
*		pi = p(i-3)	i1=5,10					*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
	integer ns,ier
	DOUBLE PRECISION xpi(13),dpipj(10,13),piDpj(10,10)
	integer is1,is2,is3,ip1,ip2,ip3,i,j,ier0,ier1
	DOUBLE PRECISION xmax,som,xmxp
	include 'ff.h'
*  #] declarations:
*  #[ check input:
	if ( ns .ne. 10 ) print *,'ffdot4: error: ns <> 10 '
*  #] check input:
*  #[ special case: already known:
	if ( idot.ge.3 ) then
	    do 2 i=1,10
		do 1 j=1,10
		    piDpj(j,i) = isgrot(iold(j,irota4),irota4)*
     +				 isgrot(iold(i,irota4),irota4)*
     +				fpij4(iold(j,irota4),iold(i,irota4))
    1		continue
    2	    continue
	    return
	endif
*  #] special case: already known:
*  #[ indices:
	ier1 = ier
	do 10 is1=1,4
	    is2 = is1 + 1
	    if ( is2 .eq. 5 ) is2 = 1
	    is3 = is2 + 1
	    if ( is3 .eq. 5 ) is3 = 1
	    ip1 = is1 + 4
	    ip2 = is2 + 4
	    if ( mod(is1,2) .eq. 1 ) then
		ip3 = 9
	    else
		ip3 = 10
	    endif
*  #] indices:
*  #[ all in one vertex:
*
*	    pi.pj, si.sj
*
	    piDpj(is1,is1) = xpi(is1)
	    piDpj(ip1,ip1) = xpi(ip1)
*
*	    si.s(i+1)
*
	    if ( xpi(is2) .le. xpi(is1) ) then
		piDpj(is1,is2) = (dpipj(is1,ip1) + xpi(is2))/2
	    else
		piDpj(is1,is2) = (dpipj(is2,ip1) + xpi(is1))/2
	    endif
	    piDpj(is2,is1) = piDpj(is1,is2)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    si.s(i+2)
*
	    if ( is1 .le. 2 ) then
		if ( xpi(is1) .le. xpi(is3) ) then
		    piDpj(is3,is1) = (dpipj(is3,ip3) + xpi(is1))/2
		else
		    piDpj(is3,is1) = (dpipj(is1,ip3) + xpi(is3))/2
		endif
		piDpj(is1,is3) = piDpj(is3,is1)
		ier0 = ier
		ier1 = max(ier1,ier0)
	    endif
*
*	    pi.si
*
	    if ( abs(xpi(ip1)) .le. xpi(is1) ) then
		piDpj(ip1,is1) = (dpipj(is2,is1) - xpi(ip1))/2
	    else
		piDpj(ip1,is1) = (dpipj(is2,ip1) - xpi(is1))/2
	    endif
	    piDpj(is1,ip1) = piDpj(ip1,is1)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    pi.s(i+1)
*
	    if ( abs(xpi(ip1)) .le. xpi(is2) ) then
		piDpj(ip1,is2) = (dpipj(is2,is1) + xpi(ip1))/2
	    else
		piDpj(ip1,is2) = (dpipj(ip1,is1) + xpi(is2))/2
	    endif
	    piDpj(is2,ip1) = piDpj(ip1,is2)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    p(i+2).s(i)
*
	    if ( abs(xpi(ip3)) .le. xpi(is1) ) then
		piDpj(ip3,is1) = (dpipj(is1,is3) + xpi(ip3))/2
	    else
		piDpj(ip3,is1) = (dpipj(ip3,is3) + xpi(is1))/2
	    endif
	    if ( is1 .eq. 2 .or. is1 .eq. 3 )
     +			piDpj(ip3,is1) = -piDpj(ip3,is1)
	    piDpj(is1,ip3) = piDpj(ip3,is1)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*  #] all in one vertex:
*  #[ all in one 3point:
*
*	    pi.s(i+2)
*
	    if ( min(abs(dpipj(is2,is1)),abs(dpipj(ip3,ip2))) .le.
     +		 min(abs(dpipj(ip3,is1)),abs(dpipj(is2,ip2))) ) then
		piDpj(ip1,is3) = (dpipj(ip3,ip2) + dpipj(is2,is1))/2
	    else
		piDpj(ip1,is3) = (dpipj(ip3,is1) + dpipj(is2,ip2))/2
	    endif
	    piDpj(is3,ip1) = piDpj(ip1,is3)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    p(i+1).s(i)
*
	    if ( min(abs(dpipj(is3,is2)),abs(dpipj(ip1,ip3))) .le.
     +		 min(abs(dpipj(ip1,is2)),abs(dpipj(is3,ip3))) ) then
		piDpj(ip2,is1) = (dpipj(ip1,ip3) + dpipj(is3,is2))/2
	    else
		piDpj(ip2,is1) = (dpipj(ip1,is2) + dpipj(is3,ip3))/2
	    endif
	    piDpj(is1,ip2) = piDpj(ip2,is1)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    p(i+2).s(i+1)
*
	    if ( min(abs(dpipj(is1,is3)),abs(dpipj(ip2,ip1))) .le.
     +		 min(abs(dpipj(ip2,is3)),abs(dpipj(is1,ip1))) ) then
		piDpj(ip3,is2) = (dpipj(ip2,ip1) + dpipj(is1,is3))/2
	    else
		piDpj(ip3,is2) = (dpipj(ip2,is3) + dpipj(is1,ip1))/2
	    endif
	    if ( is1 .eq. 2 .or. is1 .eq. 3 )
     +			piDpj(ip3,is2) = -piDpj(ip3,is2)
	    piDpj(is2,ip3) = piDpj(ip3,is2)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*  #] all in one 3point:
*  #[ all external 3point:
	    if ( idot.le.0 ) then
*
*	    pi.p(i+1)
*
	    if ( abs(xpi(ip2)) .le. abs(xpi(ip1)) ) then
		piDpj(ip1,ip2) = (dpipj(ip3,ip1) - xpi(ip2))/2
	    else
		piDpj(ip1,ip2) = (dpipj(ip3,ip2) - xpi(ip1))/2
	    endif
	    piDpj(ip2,ip1) = piDpj(ip1,ip2)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    p(i+1).p(i+2)
*
	    if ( abs(xpi(ip3)) .le. abs(xpi(ip2)) ) then
		piDpj(ip2,ip3) = (dpipj(ip1,ip2) - xpi(ip3))/2
	    else
		piDpj(ip2,ip3) = (dpipj(ip1,ip3) - xpi(ip2))/2
	    endif
	    if ( is1 .eq. 2 .or. is1 .eq. 3 )
     +			piDpj(ip2,ip3) = -piDpj(ip2,ip3)
	    piDpj(ip3,ip2) = piDpj(ip2,ip3)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
*	    p(i+2).p(i)
*
	    if ( abs(xpi(ip1)) .le. abs(xpi(ip3)) ) then
		piDpj(ip3,ip1) = (dpipj(ip2,ip3) - xpi(ip1))/2
	    else
		piDpj(ip3,ip1) = (dpipj(ip2,ip1) - xpi(ip3))/2
	    endif
	    if ( is1 .eq. 2 .or. is1 .eq. 3 )
     +			piDpj(ip3,ip1) = -piDpj(ip3,ip1)
	    piDpj(ip1,ip3) = piDpj(ip3,ip1)
	    ier0 = ier
	    ier1 = max(ier1,ier0)
*
	    else
*
*		idot > 0: copy the dotproducts from fpij4
*
		piDpj(ip1,ip2) = isgrot(iold(ip1,irota4),irota4)*
     +				 isgrot(iold(ip2,irota4),irota4)*
     +				fpij4(iold(ip1,irota4),iold(ip2,irota4))
		piDpj(ip2,ip1) = piDpj(ip1,ip2)
		piDpj(ip1,ip3) = isgrot(iold(ip1,irota4),irota4)*
     +				 isgrot(iold(ip3,irota4),irota4)*
     +				fpij4(iold(ip1,irota4),iold(ip3,irota4))
		piDpj(ip3,ip1) = piDpj(ip1,ip3)
		piDpj(ip2,ip3) = isgrot(iold(ip2,irota4),irota4)*
     +				 isgrot(iold(ip3,irota4),irota4)*
     +				fpij4(iold(ip2,irota4),iold(ip3,irota4))
		piDpj(ip3,ip2) = piDpj(ip2,ip3)
	    endif
   10	continue
*  #] all external 3point:
*  #[ real 4point:
*
*	the awkward 4point dotproducts:
*
	piDpj(9,9) = xpi(9)
	piDpj(10,10) = xpi(10)
	if ( idot.le.0 ) then
*--#[ p5.p7:
	if ( abs(xpi(7)) .lt. abs(xpi(5)) ) then
	    piDpj(5,7) = (-xpi(7) - dpipj(5,11))/2
	else
	    piDpj(5,7) = (-xpi(5) - dpipj(7,11))/2
	endif
	xmax = min(abs(xpi(5)),abs(xpi(7)))
	if ( abs(piDpj(5,7)) .lt. xloss*xmax ) then
*
*	    second try (old algorithm)
*
	    if ( min(abs(dpipj(6,9)),abs(dpipj(8,10))) .le.
     +		 min(abs(dpipj(8,9)),abs(dpipj(6,10))) ) then
		som = (dpipj(6,9) + dpipj(8,10))/2
	    else
		som = (dpipj(8,9) + dpipj(6,10))/2
	    endif
	    xmxp = min(abs(dpipj(6,9)),abs(dpipj(8,9)))
	    if ( xmxp.lt.xmax ) then
		piDpj(5,7) = som
		xmax = xmxp
	    endif
	    ier0 = ier
	    ier1 = max(ier1,ier0)
	endif
	piDpj(7,5) = piDpj(5,7)
*--#] p5.p7:
*--#[ p6.p8:
	if ( abs(xpi(6)) .lt. abs(xpi(8)) ) then
	    piDpj(6,8) = (-xpi(6) - dpipj(8,11))/2
	else
	    piDpj(6,8) = (-xpi(8) - dpipj(6,11))/2
	endif
	xmax = min(abs(xpi(6)),abs(xpi(8)))
	if ( abs(piDpj(6,8)) .lt. xloss*xmax ) then
*
*	    second try (old algorithm)
*
	    if ( min(abs(dpipj(5,9)),abs(dpipj(7,10))) .le.
     +		 min(abs(dpipj(7,9)),abs(dpipj(5,10))) ) then
		som = (dpipj(5,9) + dpipj(7,10))/2
	    else
		som = (dpipj(7,9) + dpipj(5,10))/2
	    endif
	    xmxp = min(abs(dpipj(5,9)),abs(dpipj(7,9)))
	    if ( xmxp.lt.xmax ) then
		piDpj(6,8) = som
		xmax = xmxp
	    endif
	    ier0 = ier
	    ier1 = max(ier1,ier0)
	endif
	piDpj(8,6) = piDpj(6,8)
*--#] p6.p8:
*--#[ p9.p10:
	if ( abs(xpi(9)) .lt. abs(xpi(10)) ) then
	    piDpj(9,10) = (-xpi(9) - dpipj(10,13))/2
	else
	    piDpj(9,10) = (-xpi(10) - dpipj(9,13))/2
	endif
	xmax = min(abs(xpi(9)),abs(xpi(10)))
	if ( abs(piDpj(9,10)) .lt. xloss*xmax ) then
*
*	  second try (old algorithm)
*
	    if ( min(abs(dpipj(5,6)),abs(dpipj(7,8))) .le.
     +		 min(abs(dpipj(7,6)),abs(dpipj(5,8))) ) then
		som = (dpipj(5,6) + dpipj(7,8))/2
	    else
		som = (dpipj(7,6) + dpipj(5,8))/2
	    endif
	    xmxp = min(abs(dpipj(5,6)),abs(dpipj(7,6)))
	    if ( xmxp.lt.xmax ) then
		piDpj(9,10) = som
		xmax = xmxp
	    endif
	    ier0 = ier
	    ier1 = max(ier1,ier0)
	endif
	piDpj(10,9) = piDpj(9,10)
*--#] p9.p10:
	else
*--#[ copy:
*
*	    idot > 1: just copy from fpij4...
*
	    piDpj(5,7) = isgrot(iold(5,irota4),irota4)*
     +			 isgrot(iold(7,irota4),irota4)*
     +			fpij4(iold(5,irota4),iold(7,irota4))
	    piDpj(7,5) = piDpj(5,7)
	    piDpj(6,8) = isgrot(iold(6,irota4),irota4)*
     +			 isgrot(iold(8,irota4),irota4)*
     +			fpij4(iold(6,irota4),iold(8,irota4))
	    piDpj(8,6) = piDpj(6,8)
	    piDpj(9,10)= isgrot(iold(9,irota4),irota4)*
     +			 isgrot(iold(10,irota4),irota4)*
     +			fpij4(iold(9,irota4),iold(10,irota4))
	    piDpj(10,9) = piDpj(9,10)
*--#] copy:
	endif
	ier = ier1
*  #] real 4point:
*###] ffdot4:
	end


*###[ ffgdt4:
	subroutine ffgdt4(piDpj,xpip,dpipjp,xpi,ier)
***#[*comment:***********************************************************
*									*
*	calculate the dotproducts pi.pj with				*
*	and store results in common when asked for			*
*									*
*		pi = si		i1=1,4					*
*		pi = p(i-3)	i1=5,10					*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE PRECISION piDpj(10,10),xpip(13),dpipjp(10,13),xpi(13)
	integer ier
*
*	local variables
*
	integer i,j,ii(6)
	DOUBLE PRECISION dl3p
*
*	common blocks:
*
	include 'ff.h'
*
*  #] declarations:
*  #[ get dotproducts:
*
*	Calculate the dotproducts
*
	call ffdot4(piDpj,xpip,dpipjp,10,ier)
	if ( ldot .and. idot.lt.3 ) then
	    do 65 i=1,10
		do 64 j=1,10
		    fpij4(iold(j,irota4),iold(i,irota4)) =
     +			isgrot(iold(j,irota4),irota4)*
     +			isgrot(iold(i,irota4),irota4)*piDpj(j,i)
   64		continue
   65	    continue
	endif
	if ( ldot ) then
	    if ( abs(idot).lt.2 ) then
		ii(1)= 5
		ii(2)= 6
		ii(3)= 7
		ii(4)= 8
		ii(5)= 9
		ii(6)= 10
		fidel3 = ier
		call ffdl3p(dl3p,piDpj,10,ii,ii)
		fdel3 = dl3p
	    else
		dl3p = fdel3
	    endif
	    if ( dl3p .lt. 0 ) then
		call fferr(44,ier)
		print *,'overall vertex has del3 ',dl3p
		print *,'xpi = ',xpi
	    endif
	endif
*  #] get dotproducts:
*###] ffgdt4:
	end
