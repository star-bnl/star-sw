*###[ ffcel4:
	subroutine ffcel4(del4,xpi,piDpj,ns,ier)
***#[*comment:***********************************************************
*									*
*	Calculate del4(piDpj) = det(si.sj)	with			*
*	the momenta as follows:						*
*	p(1-4) = s(i)							*
*	p(4-10) = p(i)							*
*									*
*	Input:	xpi(ns)		(real)	m^2(i),i=1,3; p^2(i-3),i=4,10	*
*		piDpj(ns,ns)	(real)					*
*		ns		(integer)				*
*		ier		(integer)				*
*									*
*	Output:	del4		(real)	det(si.sj)			*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer ns,ier
	DOUBLE COMPLEX del4,xpi(10),piDpj(10,10)
*
*	local variables:
*
	integer mem,nperm
	parameter(mem=10,nperm=125)
	integer i,jj(8),iperm(4,nperm),imem,jmem,memarr(mem,4),memind,
     +		inow,jnow,icount
	DOUBLE PRECISION xmax,xmaxp,absc,rloss
	DOUBLE COMPLEX s(24),del4p,c
	save iperm,memind,memarr,inow,jnow
*
*	common blocks:
*
	include 'ff.h'
*
*	statement functions:
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*  #] declarations: 
*  #[ data:
	data memind /0/
	data memarr /mem*0,mem*0,mem*1,mem*1/
	data inow /1/
	data jnow /1/
*
*	these are all permutations that give a non-zero result with the
*	correct sign.  This list was generated with getperm4.
*	(note: this used to be well-ordened, but then it had more than
*	19 continuation lines)
*
	data iperm/
     +	1,2,3,4,1,2,3,7,1,2,8,3,1,2,3,10,1,2,6,4,1,2,4,7,1,2,4,9,1,2,6,7
     +	,1,2,8,6,1,2,6,10,1,2,7,8,1,2,7,9,1,2,10,7,1,2,9,8,1,2,10,9,1,3,
     +	4,5,1,3,6,4,1,3,10,4,1,3,7,5,1,3,5,8,1,3,10,5,1,3,6,7,1,3,8,6,1,
     +	3,6,10,1,3,10,7,1,3,8,10,1,4,5,6,1,4,7,5,1,4,9,5,1,4,6,7,1,4,6,9
     +	,1,4,6,10,1,4,10,7,1,4,10,9,1,5,6,7,1,5,8,6,1,5,6,10,1,5,7,8,1,5
     +	,7,9,1,5,10,7,1,5,9,8,1,5,10,9,1,6,8,7,1,6,9,7,1,6,8,9,1,6,8,10,
     +	1,6,9,10,1,7,10,8,1,7,10,9,1,8,9,10,2,3,4,5,2,3,8,4,2,3,9,4,2,3,
     +	7,5,2,3,5,8,2,3,10,5,2,3,8,7,2,3,9,7,2,3,8,9,2,3,8,10,2,3,9,10,2
     +	,4,5,6,2,4,7,5,2,4,9,5,2,4,6,8,2,4,6,9,2,4,8,7,2,4,9,7,2,4,8,9,2
     +	,5,6,7,2,5,8,6,2,5,6,10,2,5,7,8,2,5,7,9,2,5,10,7,2,5,9,8,2,5,10,
     +	9,2,6,8,7,2,6,9,7,2,6,8,9,2,6,8,10,2,6,9,10,2,7,10,8,2,7,10,9,2,
     +	8,9,10,3,4,5,6,3,4,8,5,3,4,9,5,3,4,5,10,3,4,6,8,3,4,6,9,3,4,10,8
     +	,3,4,10,9,3,5,6,7,3,5,8,6,3,5,6,10,3,5,7,8,3,5,7,9,3,5,10,7,3,5,
     +	9,8,3,5,10,9,3,6,8,7,3,6,9,7,3,6,8,9,3,6,8,10,3,6,9,10,3,7,10,8,
     +	3,7,10,9,3,8,9,10,4,5,6,7,4,5,8,6,4,5,6,10,4,5,7,8,4,5,7,9,4,5,1
     +	0,7,4,5,9,8,4,5,10,9,4,6,8,7,4,6,9,7,4,6,8,9,4,6,8,10,4,6,9,10,4
     +	,7,10,8,4,7,10,9,4,8,9,10/
*  #] data: 
*  #[ get starting point from memory:
*
*	see if we know were to start, if not: go on as last time
*
	do 5 i=1,mem
	    if ( id .eq. memarr(i,1) .and. idsub .eq. memarr(i,2) ) then
		inow = memarr(i,3)
		jnow = memarr(i,4)
		goto 6
	    endif
    5	continue
    6	continue
*  #] get starting point from memory: 
*  #[ calculations:
	imem = inow
	jmem = jnow
	del4 = 0
	xmax = 0
	icount = 0
*
   10	continue

	jj(1) = iperm(1,inow)
	jj(3) = iperm(2,inow)
	jj(5) = iperm(3,inow)
	jj(7) = iperm(4,inow)

	jj(2) = iperm(1,jnow)
	jj(4) = iperm(2,jnow)
	jj(6) = iperm(3,jnow)
	jj(8) = iperm(4,jnow)

	s( 1) = +piDpj(jj(1),jj(2))*piDpj(jj(3),jj(4))*
     +		piDpj(jj(5),jj(6))*piDpj(jj(7),jj(8))
	s( 2) = +piDpj(jj(1),jj(4))*piDpj(jj(3),jj(6))*
     +		piDpj(jj(5),jj(2))*piDpj(jj(7),jj(8))
	s( 3) = +piDpj(jj(1),jj(6))*piDpj(jj(3),jj(2))*
     +		piDpj(jj(5),jj(4))*piDpj(jj(7),jj(8))
	s( 4) = -piDpj(jj(1),jj(2))*piDpj(jj(3),jj(6))*
     +		piDpj(jj(5),jj(4))*piDpj(jj(7),jj(8))
	s( 5) = -piDpj(jj(1),jj(6))*piDpj(jj(3),jj(4))*
     +		piDpj(jj(5),jj(2))*piDpj(jj(7),jj(8))
	s( 6) = -piDpj(jj(1),jj(4))*piDpj(jj(3),jj(2))*
     +		piDpj(jj(5),jj(6))*piDpj(jj(7),jj(8))

	s( 7) = -piDpj(jj(1),jj(2))*piDpj(jj(3),jj(4))*
     +		piDpj(jj(7),jj(6))*piDpj(jj(5),jj(8))
	s( 8) = -piDpj(jj(1),jj(4))*piDpj(jj(3),jj(6))*
     +		piDpj(jj(7),jj(2))*piDpj(jj(5),jj(8))
	s( 9) = -piDpj(jj(1),jj(6))*piDpj(jj(3),jj(2))*
     +		piDpj(jj(7),jj(4))*piDpj(jj(5),jj(8))
	s(10) = +piDpj(jj(1),jj(2))*piDpj(jj(3),jj(6))*
     +		piDpj(jj(7),jj(4))*piDpj(jj(5),jj(8))
	s(11) = +piDpj(jj(1),jj(6))*piDpj(jj(3),jj(4))*
     +		piDpj(jj(7),jj(2))*piDpj(jj(5),jj(8))
	s(12) = +piDpj(jj(1),jj(4))*piDpj(jj(3),jj(2))*
     +		piDpj(jj(7),jj(6))*piDpj(jj(5),jj(8))

	s(13) = -piDpj(jj(1),jj(2))*piDpj(jj(7),jj(4))*
     +		piDpj(jj(5),jj(6))*piDpj(jj(3),jj(8))
	s(14) = -piDpj(jj(1),jj(4))*piDpj(jj(7),jj(6))*
     +		piDpj(jj(5),jj(2))*piDpj(jj(3),jj(8))
	s(15) = -piDpj(jj(1),jj(6))*piDpj(jj(7),jj(2))*
     +		piDpj(jj(5),jj(4))*piDpj(jj(3),jj(8))
	s(16) = +piDpj(jj(1),jj(2))*piDpj(jj(7),jj(6))*
     +		piDpj(jj(5),jj(4))*piDpj(jj(3),jj(8))
	s(17) = +piDpj(jj(1),jj(6))*piDpj(jj(7),jj(4))*
     +		piDpj(jj(5),jj(2))*piDpj(jj(3),jj(8))
	s(18) = +piDpj(jj(1),jj(4))*piDpj(jj(7),jj(2))*
     +		piDpj(jj(5),jj(6))*piDpj(jj(3),jj(8))

	s(19) = -piDpj(jj(7),jj(2))*piDpj(jj(3),jj(4))*
     +		piDpj(jj(5),jj(6))*piDpj(jj(1),jj(8))
	s(20) = -piDpj(jj(7),jj(4))*piDpj(jj(3),jj(6))*
     +		piDpj(jj(5),jj(2))*piDpj(jj(1),jj(8))
	s(21) = -piDpj(jj(7),jj(6))*piDpj(jj(3),jj(2))*
     +		piDpj(jj(5),jj(4))*piDpj(jj(1),jj(8))
	s(22) = +piDpj(jj(7),jj(2))*piDpj(jj(3),jj(6))*
     +		piDpj(jj(5),jj(4))*piDpj(jj(1),jj(8))
	s(23) = +piDpj(jj(7),jj(6))*piDpj(jj(3),jj(4))*
     +		piDpj(jj(5),jj(2))*piDpj(jj(1),jj(8))
	s(24) = +piDpj(jj(7),jj(4))*piDpj(jj(3),jj(2))*
     +		piDpj(jj(5),jj(6))*piDpj(jj(1),jj(8))

	del4p = 0
	xmaxp = 0
	do 20 i=1,24
	    del4p = del4p + s(i)
	    xmaxp = max(xmaxp,absc(s(i)))
   20	continue
	if ( absc(del4p) .lt. xloss*xmaxp ) then
	    if ( inow .eq. imem .or. xmaxp .lt. xmax ) then
		del4 = del4p
		xmax = xmaxp
	    endif
*	    as the list is ordered we may have more luck stepping
*	    through with large steps
	    inow = inow + 43
	    jnow = jnow + 49
	    if ( inow .gt. nperm ) inow = inow - nperm
	    if ( jnow .gt. nperm ) jnow = jnow - nperm
	    icount = icount + 1
	    if ( icount.gt.15 .or. inow.eq.imem .or. jnow.eq.jmem
     +			) then
		goto 800
	    endif
	    goto 10
	endif
	del4 = del4p
	xmax = xmaxp
*  #] calculations: 
*  #[ into memory:
	memind = memind + 1
	if ( memind .gt. mem ) memind = 1
	memarr(memind,1) = id
	memarr(memind,2) = idsub
	memarr(memind,3) = inow
	memarr(memind,4) = jnow
  800	continue
*  #] into memory: 
*###] ffcel4:
	end
*###[ ffcl3p:
	subroutine ffcl3p(dl3p,piDpj,ns,ii,jj,ier)
***#[*comment:***********************************************************
*	calculate in a numerically stable way				*
*									*
*	     p1  p2  p3							*
*	delta								*
*	     p1' p2' p3'						*
*									*
*	with pn = xpi(ii(n)), p4 = -p1-p2-p3, p5 = -p1-p2, p6 = p2+p3	*
*	with pn'= xpi(jj(n)), p4'= etc.       (when ns=15 p5=p1+p2)	*
*									*
*	Input:	piDpj	complex(ns,ns)	dotpruducts			*
*		ns	integer		either 10 or 15			*
*		ii,jj	integer(6)	location of pi in piDpj		*
*		ier	integer		number of digits lost so far	*
*	Output:	dl3p	complex		see above			*
*		ier	integer		number of digits lost so far	*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer ns,ii(6),jj(6),ier
	DOUBLE COMPLEX dl3p,piDpj(ns,ns)
*
*	local variables
*
	integer i,j,k,l,iperm(3,16),ii1,ii2,ii3,jj1,jj2,jj3,nl
	DOUBLE PRECISION xmax,smax,absc
	DOUBLE COMPLEX s(6),som,xheck,c
*
*	common blocks
*
	include 'ff.h'
*
*	statement functions:
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*	data
*
	data iperm /1,2,3, 2,4,3, 3,4,1, 4,2,1,
     +		    1,2,6, 6,4,3, 3,1,6, 2,4,6,
     +		    2,5,3, 5,4,1, 1,3,5, 2,4,5,
     +		    1,6,5, 2,5,6, 3,6,5, 4,5,6/
*  #] declarations: 
*  #[ calculations:
	if ( ii(1).eq.jj(1) .and. ii(2).eq.jj(2) .and. ii(3).eq.jj(3) )
     +		then
*
*	    symmetric - fewer possibilities
*
	    nl = 1
	else
	    nl = 16
	endif
*
*	try all (1,16)*16 permutations
*
	xmax = 0
	do 101 l=1,nl
	do 100 i=1,16
	    ii1 = ii(iperm(1,i))
	    ii2 = ii(iperm(2,i))
	    ii3 = ii(iperm(3,i))
	    j = i+l-1
	    if ( j .gt. 16 ) j=j-16
	    jj1 = jj(iperm(1,j))
	    jj2 = jj(iperm(2,j))
	    jj3 = jj(iperm(3,j))
	    s(1) = +piDpj(ii1,jj1)*piDpj(ii2,jj2)*piDpj(ii3,jj3)
	    s(2) = +piDpj(ii2,jj1)*piDpj(ii3,jj2)*piDpj(ii1,jj3)
	    s(3) = +piDpj(ii3,jj1)*piDpj(ii1,jj2)*piDpj(ii2,jj3)
	    s(4) = -piDpj(ii1,jj1)*piDpj(ii3,jj2)*piDpj(ii2,jj3)
	    s(5) = -piDpj(ii3,jj1)*piDpj(ii2,jj2)*piDpj(ii1,jj3)
	    s(6) = -piDpj(ii2,jj1)*piDpj(ii1,jj2)*piDpj(ii3,jj3)
	    som = 0
	    smax = 0
	    do 80 k=1,6
		som = som + s(k)
		smax = max(smax,absc(som))
   80	    continue
	    if ( ns .eq. 15 .and. (i.gt.8 .neqv. j.gt.8) )
     +		som = -som
	    if ( i .eq. 1 .or. smax .lt. xmax ) then
		dl3p = som
		xmax = smax
	    endif
	    if ( absc(dl3p) .ge. xloss*smax ) goto 110
  100	continue
  101	continue
  110	continue
*  #] calculations: 
*###] ffcl3p: 
	end
