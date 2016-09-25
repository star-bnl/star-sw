*###[ ffabcd:
	subroutine ffabcd(aijkl,xpi,dpipj,piDpj,del2s,sdel2s,
     +		in,jn,jin,isji, kn,ln,lkn,islk, ifirst, ier)
***#[*comment:***********************************************************
*									*
*	Calculate the a,b,c,d of the equation for qij.qkl		*
*									*
*	a      = s4.s4^2						*
*									*
*		  si sj  sk sl	/ sm sn	 sm sn	  sm sn	  mu   ro\	*
*	-b/2   = d	d	|d	d      - d	s4   s4	 |	*
*		  mu nu  nu ro	\ mu s4	 ro s4	  sm sn		 /	*
*									*
*	 _	  si sj	 sk sl	/ mu s4	  ro	  mu s4	  ro\		*
*	vD/2   = d	d	|d	s4   +	 d	s4  |		*
*		  mu nu	 nu ro	\ s3 s4		  s3 s4	    /		*
*									*
*	with	sm = s3, sn = s4					*
*		p(jin) = isji*(sj-si)					*
*		p(lkn) = islk*(sl-sk)					*
*									*
*	Input:	xpi(ns)			as usual			*
*		dpipj(ns,ns)		  -"-				*
*		piDpj(ns,ns)		  -"-				*
*		in,jn,jin,isjn		see above			*
*		kn,ln,lkn,islk		see above			*
*									*
*	Output:	del4d2			see above			*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer in,jn,jin,isji,kn,ln,lkn,islk,ifirst,
     +		ier
	DOUBLE PRECISION aijkl,xpi(10),dpipj(10,10),piDpj(10,10),del2s
	DOUBLE PRECISION sdel2s
*
*	local variables:
*
	integer i,j,ji,k,l,lk,isii
	integer ii
	integer iii(6,2)
	save iii
	logical ldet(4)
	DOUBLE PRECISION xa,xb,xc,xd,s(24),del3(4),som,somb,somd,
     +		smax,save,xmax,del2d2,dum,del2i,del2j,
     +		del2ji,d2d2i,d2d2j,d2d2ji
	save del3,ldet
*
*	common blocks:
*
	include 'ff.h'
*
*	data
*
	data iii / 0,3,4,0,7,0,
     +		   0,3,4,0,7,0/
*  #] declarations: 
*  #[ initialisaties:
	if ( ifirst .eq. 0 ) then
	    ifirst = ifirst + 1
	    ldet(2) = .FALSE.
	    ldet(3) = .FALSE.
	    ldet(4) = .FALSE.
	endif
	xa = xpi(4)**2
*  #] initialisaties: 
*  #[ prepare input:
	i = in
	j = jn
	ji = jin
	k = kn
	l = ln
	lk = lkn
*  #] prepare input: 
*  #[ special cases:
	if ( k .eq. 3 ) then
	    xb = 0
	    xc = 0
	    xd = 0
	    goto 990
	elseif ( j .ge. 3 .and. l .ge. 3 ) then
*	    the whole thing collapses to factor*det3
*	    we have a good memory of things already calculated ...
	    if ( .not.ldet(i+k) ) then
		ldet(i+k) = .TRUE.
		iii(1,1) = i
		iii(4,1) = isgn(3,i)*inx(3,i)
		iii(6,1) = isgn(i,4)*inx(i,4)
		iii(1,2) = k
		iii(4,2) = isgn(3,k)*inx(3,k)
		iii(6,2) = isgn(k,4)*inx(k,4)
		call ffdl3s(del3(i+k),piDpj,iii,10)
	    endif
	    if ( l .eq. 4 .and. j .eq. 4 ) then
		xb = xpi(4)**2*del3(i+k)/del2s
		xd = 0
		xc = xb**2/xa
	    elseif ( l .eq. 4 .or. j .eq. 4 ) then
		xb = piDpj(3,4)*xpi(4)*del3(i+k)/del2s
		xd = -xpi(4)*del3(i+k)/sdel2s
		xc = xpi(4)*xpi(3)*del3(i+k)**2/del2s**2
	    else
*		l .eq. 3 .and. j .eq. 3
		xd = -2*piDpj(3,4)*del3(i+k)/sdel2s
		s(1) = xpi(3)*xpi(4)
		s(2) = 2*piDpj(3,4)**2
		som = s(2) - s(1)
		xb = som*del3(i+k)/del2s
		xc = xpi(3)**2*del3(i+k)**2/del2s**2
	    endif
	    goto 900
	endif
	if ( j .eq. 2 .and. l .eq. 4 ) then
	    call ff3dl2(s(1),xpi,dpipj,piDpj, 4, 1,2,5,+1,
     +			k,3,inx(3,k),isgn(3,k), 4, 3,4,7,+1, ier)
	    xb = -xpi(4)*s(1)/del2s
	    iii(1,1) = 1
	    iii(2,1) = 2
	    iii(4,1) = 5
	    iii(5,1) = 10
	    iii(6,1) = 8
	    iii(1,2) = k
	    iii(4,2) = isgn(3,k)*inx(3,k)
	    iii(6,2) = isgn(k,4)*inx(k,4)
	    call ffdl3s(s(1),piDpj,iii,10)
*	    restore values for other users
	    iii(2,1) = 3
	    iii(5,1) = 7
	    xd = -xpi(4)*s(1)/sdel2s
	    goto 800
	endif
*  #] special cases: 
*  #[ normal case b:
*
*	First term:
*
	call ff2dl2(del2d2,dum,xpi,dpipj,piDpj, 4,
     +			i,j,ji,isji, 4, k,l,lk,islk, 10, ier)
	s(1) = -del2d2*del2s
*
*	Second and third term, split i,j
*
	if ( i .eq. 4 ) then
	    del2i = 0
	else
	    ii = inx(4,i)
	    isii = isgn(4,i)
	    call ffdl2s(del2i,piDpj,i,4,ii,isii,3,4,7,+1,10)
	endif
	if ( j .eq. 4 ) then
	    del2j = 0
	else
	    ii = inx(4,j)
	    isii = isgn(4,j)
	    call ffdl2s(del2j,piDpj,j,4,ii,isii,3,4,7,+1,10)
	endif
	call ff2dl2(d2d2i,dum,xpi,dpipj,piDpj, i, k,l,lk,islk, 4,
     +						3,4,7,+1, 10, ier)
	call ff2dl2(d2d2j,dum,xpi,dpipj,piDpj, j, k,l,lk,islk, 4,
     +						3,4,7,+1, 10, ier)
	s(2) = +del2i*d2d2j
	s(3) = -del2j*d2d2i
	somb = s(1) + s(2) + s(3)
	smax = max(abs(s(1)),abs(s(2)),abs(s(3)))
	if ( abs(somb) .ge. xloss*smax ) goto 90
	xmax = smax
	save = somb

*	if the first term is wrong ... forget about it
	if ( abs(somb) .lt. xloss*abs(s(1)) ) goto 80

	call ffdl2t(del2ji,piDpj, ji,4, 3,4,7,+1,+1, 10)
	call ff2dl2(d2d2ji,dum,xpi,dpipj,piDpj, ji, k,l,lk,islk, 4,
     +						3,4,7,+1, 10, ier)
	s(2) = +del2j*d2d2ji
	s(3) = -del2ji*d2d2j
	somb = s(1) + isji*(s(2) + s(3))
	smax = max(abs(s(1)),abs(s(2)),abs(s(3)))
	if ( abs(somb) .ge. xloss*smax ) goto 90
	if ( smax .lt. xmax ) then
	    save = somb
	    xmax = smax
	endif

	s(2) = +del2i*d2d2ji
	s(3) = -del2ji*d2d2i
	somb = s(1) + isji*(s(2) + s(3))
	smax = max(abs(s(1)),abs(s(2)),abs(s(3)))
	if ( abs(somb) .ge. xloss*max(abs(s(1)),abs(s(2)),abs(s(3))) )
     +		goto 90
	if ( smax .lt. xmax ) then
	    save = somb
	    xmax = smax
	endif
   80	continue
*
*	give up:
*
	somb = save
   90	continue
	xb = somb/del2s
*  #] normal case b: 
*  #[ normal case d:
	call ff3dl2(s(1),xpi,dpipj,piDpj, 4, i,j,ji,isji, k,l,lk,islk,
     +						4, 3,4,7,+1, ier)
	if ( i .eq. k .and. j .eq. l ) then
	    somd = -2*s(1)
	else
	    call ff3dl2(s(2),xpi,dpipj,piDpj, 4, k,l,lk,islk,
     +				i,j,ji,isji, 4, 3,4,7,+1, ier)
	    somd = - s(1) - s(2)
	endif
	xd = -somd/sdel2s
*  #] normal case d: 
*  #[ normal case c:
  800	continue
	s(1) = xb - xd
	s(2) = xb + xd
	som = s(1)*s(2)
	xc = som/xa
*  #] normal case c: 
  900	continue
*  #[ and the final answer:
  990	continue
	call ffroot(dum,aijkl,xa,xb,xc,xd,ier)
*  #] and tne final answer: 
*###] ffabcd: 
	end

