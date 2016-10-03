*###[ ffcel2:
	subroutine ffcel2(del2,piDpj,ns,i1,i2,i3,lerr,ier)
*************************************************************************
*	calculate in a numerically stable way				*
*	del2(piDpj(i1,i1),piDpj(i2,i2),piDpj(i3,i3)) =			*
*		= piDpj(i1,i1)*piDpj(i2,i2) - piDpj(i1,i2)^2		*
*		= piDpj(i1,i1)*piDpj(i3,i3) - piDpj(i1,i3)^2		*
*		= piDpj(i2,i2)*piDpj(i3,i3) - piDpj(i2,i3)^2		*
*	ier is the usual error flag.					*
*************************************************************************
	implicit none
*
*	arguments:
*
	integer ns,i1,i2,i3,lerr,ier
	DOUBLE COMPLEX del2,piDpj(ns,ns)
*
*	local variables
*
	DOUBLE COMPLEX s1,s2,cc
	DOUBLE PRECISION absc
*
*	common blocks
*
	include 'ff.h'
*
	absc(cc) = abs(DBLE(cc)) + abs(DIMAG(cc))
*
*	calculations
*
	if ( absc(piDpj(i1,i2)) .lt. absc(piDpj(i1,i3)) .and.
     +	     absc(piDpj(i1,i2)) .lt. absc(piDpj(i2,i3)) ) then
	    s1 = piDpj(i1,i1)*piDpj(i2,i2)
	    s2 = piDpj(i1,i2)**2
	elseif ( absc(piDpj(i1,i3)) .lt. absc(piDpj(i2,i3)) ) then
	    s1 = piDpj(i1,i1)*piDpj(i3,i3)
	    s2 = piDpj(i1,i3)**2
	else
	    s1 = piDpj(i2,i2)*piDpj(i3,i3)
	    s2 = piDpj(i2,i3)**2
	endif
	del2 = s1 - s2
	if ( absc(del2) .lt. xloss*absc(s2) ) then
	    if ( lerr .eq. 0 ) then
*		we know we have another chance
		if ( del2.ne.0 ) then
		    ier = ier + int(log10(xloss*absc(s2)/absc(del2)))
		else
		    ier = ier + int(log10(xloss*absc(s2)/xclogm))
		endif
	    endif
	endif
*###] ffcel2:
	end


*###[ ffcl2p:
	subroutine ffcl2p(delps1,xpi,dpipj,piDpj,
     +		ip1,ip2,ip3,is1,is2,is3,ns)
***#[*comment:***********************************************************
*									*
*	calculate in a numerically stable way				*
*	delta_{ip1,is2}^{ip1,ip2}					*
*	ier is the usual error flag.					*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer ns,ip1,ip2,ip3,is1,is2,is3
	DOUBLE COMPLEX delps1,xpi(ns),dpipj(ns,ns),piDpj(ns,ns)
*
*	local variables
*
	DOUBLE COMPLEX s1,s2,s3,som,c
	DOUBLE PRECISION xmax,absc
*
*	common blocks
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*  #] declarations:
*  #[ stupid tree:
*	1
	s1 = xpi(ip1)*piDpj(ip2,is2)
	s2 = piDpj(ip1,ip2)*piDpj(ip1,is2)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	som = delps1
	xmax = absc(s1)
*	2
	s1 = piDpj(ip1,ip2)*piDpj(ip3,is2)
	s2 = piDpj(ip1,ip3)*piDpj(ip2,is2)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	3
	s1 = piDpj(ip1,ip3)*piDpj(ip1,is2)
	s2 = xpi(ip1)*piDpj(ip3,is2)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	4
	s1 = xpi(ip1)*piDpj(ip2,is1)
	s2 = piDpj(ip1,is1)*piDpj(ip1,ip2)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	5
	s1 = piDpj(ip1,is2)*piDpj(ip2,is1)
	s2 = piDpj(ip1,is1)*piDpj(ip2,is2)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	6
	s1 = piDpj(ip1,ip2)*piDpj(ip3,is1)
	s2 = piDpj(ip1,ip3)*piDpj(ip2,is1)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	7
	s1 = piDpj(ip2,is2)*piDpj(ip3,is1)
	s2 = piDpj(ip2,is1)*piDpj(ip3,is2)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	8
	s1 = piDpj(ip1,ip3)*piDpj(ip1,is1)
	s2 = xpi(ip1)*piDpj(ip3,is1)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*	9
	s1 = piDpj(ip1,is1)*piDpj(ip3,is2)
	s2 = piDpj(ip1,is2)*piDpj(ip3,is1)
	delps1 = s1 - s2
	if ( absc(delps1) .ge. xloss*absc(s1) ) goto 100
	if ( absc(s1) .lt. xmax ) then
	    som = delps1
	    xmax = absc(s1)
	endif
*10	22-nov-1993 yet another one
	if ( dpipj(1,1).eq.0 ) then
	    s1 = +xpi(ip1)*dpipj(is3,is2)/2
	    s2 = -piDpj(ip1,ip2)*dpipj(is2,is1)/2
	    s3 = +xpi(ip1)*piDpj(ip2,ip3)/2
	    delps1 = s1+s2+s3
	    if ( absc(delps1) .ge. xloss*max(absc(s1),absc(s2)) ) 
     +	    	goto 100
	    if ( max(absc(s1),absc(s2)) .lt. xmax ) then
		som = delps1
		xmax = absc(s1)
	    endif
	endif
*	NO possibility
	delps1 = som
  100	continue
*  #] stupid tree:
*###] ffcl2p:
	end


*###[ ffcl2t:
	subroutine ffcl2t(delps,piDpj,in,jn,kn,ln,lkn,islk,iss,ns)
***#[*comment:***********************************************************
*									*
*	calculate in a numerically stable way				*
*									*
*		\delta_{si,sj}^{sk,sl}					*
*									*
*	with p(lk) = islk*(iss*sl - sk)	(islk,iss = +/-1)		*
*	and NO relationship between s1,s2 assumed (so 1/2 the		*
*	possibilities of ffdl2s).					*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer in,jn,kn,ln,lkn,islk,iss,ns
	DOUBLE COMPLEX delps,piDpj(ns,ns)
*
*	local variables
*
	DOUBLE COMPLEX s1,s2,c
	DOUBLE PRECISION absc
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
	if ( in .eq. jn ) then
	    delps = 0D0
	    return
	endif
	s1 = piDpj(kn,in)*piDpj(ln,jn)
	s2 = piDpj(ln,in)*piDpj(kn,jn)
	delps = s1 - s2
	if ( absc(delps) .ge. xloss*absc(s1) ) goto 10
	s1 = piDpj(kn,in)*piDpj(lkn,jn)
	s2 = piDpj(lkn,in)*piDpj(kn,jn)
	delps = iss*islk*(s1 - s2)
	if ( absc(delps) .ge. xloss*absc(s1) ) goto 10
	s1 = piDpj(lkn,in)*piDpj(ln,jn)
	s2 = piDpj(ln,in)*piDpj(lkn,jn)
	delps = islk*(- s1 + s2)
	if ( absc(delps) .ge. xloss*absc(s1) ) goto 10
   10	continue
*  #] calculations:
*###] ffcl2t:
	end


*###[ ffcl3m:
	subroutine ffcl3m(del3mi,ldel,del3,del2,xpi,dpipj,piDpj,ns,ip1n,
     +		ip2n,ip3n,is,itime)
***#[*comment:***********************************************************
*									*
*	Calculate xpi(i)*del2 - del3(piDpj)				*
*									*
*	  /  si	mu \2		(This appears to be one of the harder	*
*	= | d	   |		 determinants to calculate accurately.	*
*	  \  p1	p2 /		 Note that we allow a loss of xloss^2)	*
*									*
*	Input:	ldel		iff .true. del2 and del3 exist		*
*		del3		\delta^{s(1),p1,p2}_{s(1),p1,p2}	*
*		del2		\delta^{p1,p2}_{p1,p2}			*
*		xpi(ns)		standard				*
*		dpipj(ns,ns)	standard				*
*		piDpj(ns,ns)	standard				*
*		ipi		pi = xpi(abs(ipi)) [p3=-p1 +/-p2]	*
*		is		si = xpi(is,is+1,..,is+itime-1)		*
*		itime		number of functions to calculate	*
*									*
*	Output:	del3mi(3)	(\delta^{s_i \mu}_{p_1 p_2})^2		*
*									*
***#]*comment:***********************************************************
*  #[ declarations:
	implicit none
*
*	arguments:
*
	integer ns,ip1n,ip2n,ip3n,is,itime
	logical ldel
	DOUBLE COMPLEX del3mi(itime),del3,del2,xpi(ns),dpipj(ns,ns),
     +		piDpj(ns,ns)
*
*	local variables:
*
	DOUBLE PRECISION smax,xmax,absc
	DOUBLE COMPLEX s(7),som,xsom,c
	integer i,j,k,ip1,ip2,ip3,ipn,is1,is2,isi,is3,ihlp,iqn,
     +		jsgn1,jsgn2,jsgn3,jsgnn,iadj(10,10,3:4),init,nm
	save iadj,init
	logical lmax,ltwist
*
*	common blocks:
*
	include 'ff.h'
*
*	statement function
*
	absc(c) = abs(DBLE(c)) + abs(DIMAG(c))
*
*	data
*
	data iadj /200*0/
	data init /0/
*  #] declarations:
*  #[ initialisations:
	if ( init .eq. 0 ) then
	    init = 1
*
*	    Fill the array with adjacent values: if
*		x = iadj(i,j)
*		k = abs(mod(k,100))
*		jsgnk = sign(x)
*		jsgnj = 1-2*theta(x-100)  (ie -1 iff |x|>100)
*	    then
*		pi(k) = jsgnk*( p(i) - jsgnj*pi(j) )
*
	    do 5 nm=3,4
		do 4 i=1,nm
		    is1 = i
		    is2 = i+1
		    if ( is2 .gt. nm ) is2 = 1
		    is3 = i-1
		    if ( is3 .eq. 0 ) is3 = nm
		    ip1 = is1 + nm
		    iadj(is1,is2,nm) = -ip1
		    iadj(is2,is1,nm) = ip1
		    iadj(ip1,is2,nm) = -is1
		    iadj(is2,ip1,nm) = is1
		    iadj(is1,ip1,nm) = 100+is2
		    iadj(ip1,is1,nm) = 100+is2
		    if ( nm .eq. 3 ) then
			iadj(ip1,is2+3,3) = -100-is3-3
			iadj(is2+3,ip1,3) = -100-is3-3
		    endif
    4		continue
    5	    continue

	    iadj(3,1,4) = -9
	    iadj(1,3,4) = 9
	    iadj(9,1,4) = -3
	    iadj(1,9,4) = 3
	    iadj(3,9,4) = 100+1
	    iadj(9,3,4) = 100+1

	    iadj(2,4,4) = -10
	    iadj(4,2,4) = 10
	    iadj(10,4,4) = -2
	    iadj(4,10,4) = 2
	    iadj(2,10,4) = 100+4
	    iadj(10,2,4) = 100+4

	endif
	if ( ns .eq. 6 ) then
	    nm = 3
	else
	    nm = 4
	endif
*  #] initialisations:
*  #[ easy tries:
	do 40 i=1,itime
	    isi = i+is-1
	    lmax = .FALSE.
*
*	    get xpi(isi)*del2 - del3 ... if del3 and del2 are defined
*
	    if ( ldel ) then
		s(1) = xpi(isi)*del2
		som = s(1) - del3
		smax = absc(s(1))
		if ( absc(som) .ge. xloss**2*smax ) goto 35
		xsom = som
		xmax = smax
		lmax = .TRUE.
	    endif
	    ip1 = ip1n
	    ip2 = ip2n
	    ip3 = ip3n
	    do 20 j=1,3
*
*		otherwise use the simple threeterm formula
*
		s(1) = xpi(ip2)*piDpj(ip1,isi)**2
		s(2) = xpi(ip1)*piDpj(ip2,isi)*piDpj(ip2,isi)
		s(3) = -2*piDpj(ip2,isi)*piDpj(ip2,ip1)*piDpj(ip1,isi)
		som = s(1) + s(2) + s(3)
		smax = max(absc(s(1)),absc(s(2)),absc(s(3)))
		if ( absc(som) .ge. xloss**2*smax ) goto 35
		if ( .not. lmax .or. smax .lt. xmax ) then
		    xsom = som
		    xmax = smax
		    lmax = .TRUE.
		endif
*
*		if there are cancellations between two of the terms:
*		we try mixing with isi.
*
*		First map cancellation to s(2)+s(3) (do not mess up
*		rotations...)
*
		if ( absc(s(1)+s(3)) .lt. absc(s(3))/2 ) then
		    ihlp = ip1
		    ip1 = ip2
		    ip2 = ihlp
		    som = s(1)
		    s(1) = s(2)
		    s(2) = som
		    ltwist = .TRUE.
		else
		    ltwist = .FALSE.
		endif
		if ( absc(s(2)+s(3)) .lt. absc(s(3))/2 ) then
*
*		switch to the vector pn so that si = jsgn1*p1 + jsgnn*pn
*
		k = iadj(isi,ip1,nm)
		if ( k .ne. 0 ) then
		    ipn = abs(k)
		    jsgnn = isign(1,k)
		    if ( ipn .gt. 100 ) then
			ipn = ipn - 100
			jsgn1 = -1
		    else
			jsgn1 = +1
		    endif
		    if ( absc(dpipj(ipn,isi)) .lt. 
     +		    	xloss*absc(piDpj(ip1,isi)) .and.
     +			 absc(piDpj(ipn,ip2)) .lt. 
     +			xloss*absc(piDpj(ip2,isi)) ) then
*		same:	s(1) = xpi(ip2)*piDpj(ip1,isi)**2
			s(2) = jsgnn*piDpj(isi,ip2)*piDpj(ipn,ip2)*
     +								xpi(ip1)
			s(3) = jsgn1*piDpj(isi,ip2)*piDpj(ip1,ip2)*
     +							dpipj(ipn,isi)
			som = s(1) + s(2) + s(3)
			smax = max(absc(s(1)),absc(s(2)),absc(s(3)))
*			print *,'    (isi+ip1) with isi,ip1,ip2,ipn: ',
*     +				isi,ip1,ip2,ipn
*			print *,'xpi(ip2),piDpj(ip1,isi)',xpi(ip2),
*     +				piDpj(ip1,isi)
*			print *,'piDpj(isi,ip2),piDpj(ipn,ip2),xpi(ip1)'
*     +				,piDpj(isi,ip2),piDpj(ipn,ip2),xpi(ip1)
			if ( absc(som) .ge. xloss**2*smax ) goto 35
			if ( smax .lt. xmax ) then
			    xsom = som
			    xmax = smax
			endif
*
*			there may be a cancellation between s(1) and
*			s(2) left.  Introduce a vector q such that
*			pn = jsgnq*q + jsgn2*p2.  We also need the sign
*			jsgn3 in p3 = -p1 - jsgn3*p2
*
			k = iadj(ipn,ip2,nm)
			if ( k .ne. 0 ) then
			    iqn = abs(k)
*not used		    jsgnq = isign(1,k)
			    if ( iqn .gt. 100 ) then
				iqn = iqn - 100
				jsgn2 = -1
			    else
				jsgn2 = +1
			    endif
			    k = iadj(ip1,ip2,nm)
			    if ( k .eq. 0 .or. k .lt. 100 ) then
*				we have p1,p2,p3 all p's
				jsgn3 = +1
			    elseif ( k .lt. 0 ) then
*				ip1,ip2 are 2*s,1*p such that p2-p1=ip3
				jsgn3 = -1
			    else
				jsgn3 = 0
			    endif
*			    we need one condition on the signs for this
*			    to work
			    if ( ip3.ne.0 .and. jsgn1*jsgn2.eq.jsgnn*
     +			    jsgn3 .and. absc(s(3)).lt.xloss*smax ) then
				s(1) = piDpj(ip1,isi)**2*dpipj(iqn,ipn)
				s(2) = -jsgn2*jsgn1*piDpj(ipn,ip2)*
     +					piDpj(ip1,isi)*dpipj(ipn,isi)
*				s(3) stays the same
				s(4) = -jsgn2*jsgn1*piDpj(ipn,ip2)*
     +					xpi(ip1)*piDpj(isi,ip3)
				som = s(1) + s(2) + s(3) + s(4)
				smax = max(absc(s(1)),absc(s(2)),
     +					absc(s(3)),absc(s(4)))
				if (absc(som).ge.xloss**2*smax) goto 35
				if ( smax .lt. xmax ) then
				    xsom = som
				    xmax = smax
				endif
			    endif
			endif
		    endif
		endif
		k = iadj(isi,ip2,nm)
		if ( k .ne. 0 ) then
		    ipn = abs(k)
		    jsgnn = isign(1,k)
		    if ( ipn .gt. 100 ) then
			jsgn1 = -1
			ipn = ipn - 100
		    else
			jsgn1 = +1
		    endif
		    if ( absc(dpipj(ipn,isi)) .lt. 
     +		    	xloss*absc(piDpj(ip2,isi)) .and.
     +			 absc(piDpj(ipn,ip1)) .lt. 
     +			xloss*absc(piDpj(ip1,isi)) ) then
			s(1) = jsgnn*piDpj(isi,ip1)*piDpj(ipn,ip1)*
     +								xpi(ip2)
			s(2) = xpi(ip1)*piDpj(ip2,isi)**2
			s(3) = jsgn1*piDpj(isi,ip1)*piDpj(ip2,ip1)*
     +							dpipj(ipn,isi)
			som = s(1) + s(2) + s(3)
			smax = max(absc(s(1)),absc(s(2)),absc(s(3)))
			print *,'    (isi+ip2) with isi,ip1,ip2,ipn: ',
     +				isi,ip1,ip2,ipn
			if ( absc(som) .ge. xloss**2*smax ) goto 35
			if ( smax .lt. xmax ) then
			    xsom = som
			    xmax = smax
			endif
		    endif
		endif
		endif
*
*		rotate the ipi
*
		if ( ip3 .eq. 0 ) goto 30
		if ( j .ne. 3 ) then
		    if ( .not. ltwist ) then
			ihlp = ip1
			ip1 = ip2
			ip2 = ip3
			ip3 = ihlp
		    else
			ihlp = ip2
			ip2 = ip3
			ip3 = ihlp
		    endif
		endif
   20	    continue
   30	    continue
*  #] easy tries:
*  #[ choose the best value:
*
*	    These values are the best found:
*
	    som = xsom
	    smax = xmax

   35	    continue
	    del3mi(i) = som
   40	continue
*  #] choose the best value:
*###] ffcl3m:
	end
