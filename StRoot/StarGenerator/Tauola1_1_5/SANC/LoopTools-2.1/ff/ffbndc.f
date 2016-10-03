*###[ ffbndc:
	DOUBLE PRECISION function ffbndc(n1,n2,carray)
*************************************************************************
*									*
*	calculate bound = (precc*|a(n1)/a(n1+n2)|^(1/n2) which is the	*
*	maximum value of x in a series expansion sum_(i=n1)^(n1+n2)	*
*	a(i)*x(i) to give a result of accuracy precc (actually of |next	*
*	term| < prec							*
*									*
*************************************************************************
	implicit none
	integer n1,n2
	DOUBLE COMPLEX carray(n1+n2)
	include 'ff.h'
	if ( carray(n1+n2) .eq. 0 ) then
	   print *,'ffbnd: fatal: array not intialized; did you call ',
     +		'ffini?'
	   stop
	endif
	ffbndc = (precc*abs(carray(n1)/carray(n1+n2)))**(1/DBLE(n2))
*###] ffbndc:
	end


