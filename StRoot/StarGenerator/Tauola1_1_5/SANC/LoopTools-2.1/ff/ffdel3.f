*###[ ffdel3:
	subroutine ffdel3(del3,piDpj)
***#[*comment:***********************************************************
*									*
*	Calculate del3(piDpj) = det(si.sj)	with			*
*	the momenta as follows:						*
*	p(1-3) = s(i)							*
*	p(4-6) = p(i)							*
*									*
*	Input:	xpi(ns)		(real)	m^2(i),i=1,3; p^2(i-3),i=4,10	*
*		piDpj(ns,ns)	(real)					*
*		ns		(integer)				*
*		ier		(integer)				*
*									*
*	Output:	del3		(real)	det(si.sj)			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	DOUBLE PRECISION del3,piDpj(6,6)
*
*	local variables:
*
	integer mem,nperm
	parameter(mem=10,nperm=16)
	integer i,jj(6),iperm(3,nperm),imem,memarr(mem,3),memind,inow
	DOUBLE PRECISION s(6),xmax,del3p,xmaxp
	save iperm,memind,memarr,inow
*
*	common blocks:
*
	include 'ff.h'
*  #] declarations:
*  #[ data:
	data memind /0/
	data memarr /mem*0,mem*0,mem*1/
	data inow /1/
*
*	these are all permutations that give a non-zero result with the
*	correct sign.  This list was generated with getperm3.
*
	data iperm/
     +		1,2,3,  1,2,5,  1,6,2,  1,4,3,
     +		1,3,5,  1,4,5,  1,6,4,  1,5,6,
     +		2,4,3,  2,3,6,  2,4,5,  2,6,4,
     +		2,5,6,  3,4,5,  3,6,4,  3,5,6/
*  #] data:
*  #[ starting point in memory?:
*
*	see if we know were to start, if not: go on as last time
*
	do 5 i=1,mem
	    if ( id .eq. memarr(i,1) .and. idsub .eq. memarr(i,2) ) then
		inow = memarr(i,3)
		goto 6
	    endif
    5	continue
    6	continue
*  #] starting point in memory?:
*  #[ calculations:
	imem = inow
	del3 = 0
	xmax = 0

   10	continue

	jj(1) = iperm(1,inow)
	jj(3) = iperm(2,inow)
	jj(5) = iperm(3,inow)

	jj(2) = iperm(1,inow)
	jj(4) = iperm(2,inow)
	jj(6) = iperm(3,inow)

	s(1) = +piDpj(jj(1),jj(2))*piDpj(jj(3),jj(4))*piDpj(jj(5),jj(6))
	s(2) = +piDpj(jj(1),jj(4))*piDpj(jj(3),jj(6))*piDpj(jj(5),jj(2))
	s(3) = +piDpj(jj(1),jj(6))*piDpj(jj(3),jj(2))*piDpj(jj(5),jj(4))
	s(4) = -piDpj(jj(1),jj(2))*piDpj(jj(3),jj(6))*piDpj(jj(5),jj(4))
	s(5) = -piDpj(jj(1),jj(6))*piDpj(jj(3),jj(4))*piDpj(jj(5),jj(2))
	s(6) = -piDpj(jj(1),jj(4))*piDpj(jj(3),jj(2))*piDpj(jj(5),jj(6))

	del3p = 0
	xmaxp = 0
	do 20 i=1,6
	    del3p = del3p + s(i)
	    xmaxp = max(xmaxp,abs(s(i)))
   20	continue
	if ( abs(del3p) .lt. xloss*xmaxp ) then
	    if ( inow .eq. imem .or. xmaxp .lt. xmax ) then
		del3 = del3p
		xmax = xmaxp
	    endif
	    inow = inow + 1
	    if ( inow .gt. nperm ) inow = 1
	    if ( inow .eq. imem ) goto 800
	    goto 10
	endif
	del3 = del3p
	xmax = xmaxp
*  #] calculations:
*  #[ into memory:
  800	continue
	memind = memind + 1
	if ( memind .gt. mem ) memind = 1
	memarr(memind,1) = id
	memarr(memind,2) = idsub
	memarr(memind,3) = inow
*  #] into memory:
*###] ffdel3:
	end


*(##[ ffdl3s:
	subroutine ffdl3s(dl3s,piDpj,ii,ns)
***#[*comment:***********************************************************
*									*
*	Calculate dl3s(piDpj) = det(si.sj)	with			*
*	the momenta indicated by the indices ii(1-6,1), ii(1-6,2)	*
*	as follows:							*
*	p(|ii(1,)|-|ii(3,)|) = s(i)					*
*	p(|ii(4,)|-|ii(6,)|) = p(i) = sgn(ii())*(s(i+1) - s(i))		*
*									*
*	At this moment (26-apr-1990) only the diagonal is tried		*
*									*
*	Input:	xpi(ns)		(real)	m^2(i),i=1,3; p^2(i-3),i=4,10	*
*		piDpj(ns,ns)	(real)					*
*		ii(6,2)		(integer)	see above		*
*		ns		(integer)				*
*		ier		(integer)				*
*									*
*	Output:	dl3s		(real)	det(si.sj)			*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer ii(6,2),ns
	DOUBLE PRECISION dl3s,piDpj(ns,ns)
*
*	local variables:
*
	integer mem,nperm
	parameter(mem=10,nperm=16)
	integer i,jj(6),jsgn,iperm(3,nperm),imem,memarr(mem,3),
     +		memind,inow
	DOUBLE PRECISION s(6),xmax,dl3sp,xmaxp
	save iperm,memind,memarr,inow
*
*	common blocks:
*
	include 'ff.h'
*  #] declarations:
*  #[ data:
	data memind /0/
	data memarr /mem*0,mem*0,mem*1/
	data inow /1/
*
*	these are all permutations that give a non-zero result with the
*	correct sign.  This list was generated with getperm3.
*
	data iperm/
     +		1,2,3,  1,2,5,  1,6,2,  1,4,3,
     +		1,3,5,  1,4,5,  1,6,4,  1,5,6,
     +		2,4,3,  2,3,6,  2,4,5,  2,6,4,
     +		2,5,6,  3,4,5,  3,6,4,  3,5,6/
*  #] data:
*  #[ starting point in memory?:
*
*	see if we know were to start, if not: go on as last time
*
	do 5 i=1,mem
	    if ( id .eq. memarr(i,1) .and. idsub .eq. memarr(i,2) ) then
		inow = memarr(i,3)
		goto 6
	    endif
    5	continue
    6	continue
*  #] starting point in memory?:
*  #[ calculations:
	imem = inow
	dl3s = 0
	xmax = 0

   10	continue

	jj(1) = abs(ii(iperm(1,inow),1))
	jj(3) = abs(ii(iperm(2,inow),1))
	jj(5) = abs(ii(iperm(3,inow),1))

	jj(2) = abs(ii(iperm(1,inow),2))
	jj(4) = abs(ii(iperm(2,inow),2))
	jj(6) = abs(ii(iperm(3,inow),2))

	jsgn =  sign(1,ii(iperm(1,inow),1))
     +		*sign(1,ii(iperm(2,inow),1))
     +		*sign(1,ii(iperm(3,inow),1))
     +		*sign(1,ii(iperm(1,inow),2))
     +		*sign(1,ii(iperm(2,inow),2))
     +		*sign(1,ii(iperm(3,inow),2))

	s(1) = +piDpj(jj(1),jj(2))*piDpj(jj(3),jj(4))*piDpj(jj(5),jj(6))
	s(2) = +piDpj(jj(1),jj(4))*piDpj(jj(3),jj(6))*piDpj(jj(5),jj(2))
	s(3) = +piDpj(jj(1),jj(6))*piDpj(jj(3),jj(2))*piDpj(jj(5),jj(4))
	s(4) = -piDpj(jj(1),jj(2))*piDpj(jj(3),jj(6))*piDpj(jj(5),jj(4))
	s(5) = -piDpj(jj(1),jj(6))*piDpj(jj(3),jj(4))*piDpj(jj(5),jj(2))
	s(6) = -piDpj(jj(1),jj(4))*piDpj(jj(3),jj(2))*piDpj(jj(5),jj(6))

	dl3sp = 0
	xmaxp = 0
	do 20 i=1,6
	    dl3sp = dl3sp + s(i)
	    xmaxp = max(xmaxp,abs(s(i)))
   20	continue
	if ( abs(dl3sp) .lt. xloss*xmaxp ) then
	    if ( inow .eq. imem .or. xmaxp .lt. xmax ) then
		dl3s = jsgn*dl3sp
		xmax = xmaxp
	    endif
	    inow = inow + 1
	    if ( inow .gt. nperm ) inow = 1
	    if ( inow .eq. imem ) goto 800
	    goto 10
	endif
	dl3s = jsgn*dl3sp
	xmax = xmaxp
*  #] calculations:
*  #[ into memory:
  800	continue
	memind = memind + 1
	if ( memind .gt. mem ) memind = 1
	memarr(memind,1) = id
	memarr(memind,2) = idsub
	memarr(memind,3) = inow
*  #] into memory:
*)##] ffdl3s:
	end
