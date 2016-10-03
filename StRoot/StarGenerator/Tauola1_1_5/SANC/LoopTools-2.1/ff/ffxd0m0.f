*###[ ffxd0m0:
	subroutine ffxd0m0(cd0, xpi, ier)
***#[*comment:***********************************************************
*									*
*	D0 function for 4 masses = 0					*
*	input parameters as for ffxd0					*
*									*
*	algorithm taken from						*
*	Denner, Nierste, Scharf, Nucl. Phys. B367 (1991) 637-656	*
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

	DOUBLE PRECISION a, b, c, d
	DOUBLE COMPLEX x(2), z(2), k1, k2, t1, t2
	DOUBLE COMPLEX dl1, dl2, zl, ww, tlg
	DOUBLE COMPLEX k12, k23, k34, k14, k13, k24
	integer j, ipi1(2), ipi2(2), nffeta

	include 'ff.h'

	a = xpi(10)*xpi(7)
	b = xpi(9)*xpi(10) + xpi(5)*xpi(7) - xpi(8)*xpi(6)
	c = xpi(5)*xpi(9)
	d = -xpi(6)

	k1 = DCMPLX(c, precx*d)
	k2 = sqrt(b*b - 4*a*k1)
	x(1) = (-b - k2)/2D0/a
	x(2) = (-b + k2)/2D0/a
	if(abs(x(1)) .gt. abs(x(2))) then
	  x(2) = k1/a/x(1)
	else
	  x(1) = k1/a/x(2)
	endif

	k12 = DCMPLX(-xpi(5), -precx)
	k13 = DCMPLX(-xpi(9), -precx)
	k23 = DCMPLX(-xpi(6), -precx)
	k34 = DCMPLX(-xpi(7), -precx)
	k14 = DCMPLX(-xpi(8), -precx)
	k24 = DCMPLX(-xpi(10), -precx)

	k1 = k34/k13
	k2 = k24/k12
	ww = log(k12) + log(k13) - log(k14) - log(k23)

	do 100 j = 1, 2
	  t1 = 1 + k1*x(j)
	  t2 = 1 + k2*x(j)
	  call ffzzdl(dl1, ipi1(j), zl, t1, ier)
	  call ffzzdl(dl2, ipi2(j), zl, t2, ier)
	  tlg = log(-x(j))
	  z(j) = tlg*(ww - .5D0*tlg) - dl1 - dl2 -
     +      c2ipi*( nffeta(-x(j), k1, ier)*log(t1) +
     +              nffeta(-x(j), k2, ier)*log(t2) )
  100   continue

	ww = z(2) - z(1) +
     +    (ipi1(1) + ipi2(1) - ipi1(2) - ipi2(2))*pi12
	cd0 = ww/a/(x(1) - x(2))
	end

