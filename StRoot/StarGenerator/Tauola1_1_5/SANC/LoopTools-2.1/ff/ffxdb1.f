*###[ ffxdb1:
	subroutine ffxdb1(cdb1, p, m1, m2, ier)
***#[*comment:***********************************************************
*									*
*	DB1 function (derivative of B1)					*
*									*
*	algorithm adapted from Ansgar Denner's bcanew.f			*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE COMPLEX cdb1
	DOUBLE PRECISION p, m1, m2
	integer ier

	DOUBLE COMPLEX ffpvf, ffypvf
	external ffpvf, ffypvf

	DOUBLE COMPLEX xp, xm, yp, ym, r

	include 'ff.h'

	logical initir
	save initir

	data initir /.FALSE./

	if(abs(p) .gt. precx*(m1 + m2)) then

*		IR divergent case
	  if(m2 .eq. 0 .and. p .eq. m1) then
	    if(.not. initir) then
	      initir = .TRUE.
	      print *, "ffxdb1: IR divergent B1', ",
     +          "using cutoff ", lambda2
	    endif
	    cdb1 = .5D0*(3 + log(lambda2/p))/p
	    return
	  endif

	  call ffroots(p, m1, m2, xp, xm, yp, ym, r)
	  if(abs(xp - xm) .gt. sqrt(precx)*abs(xp + xm)) then
	    cdb1 = (ffypvf(2, xp, yp) - ffypvf(2, xm, ym))/r
	  else if(abs(xp) .gt. 10) then
	    cdb1 = DBLE( (2/3D0 +
     +       (2 - 3*xp)*ffpvf(3, xp, yp))/xp**2 )/p
	  else if(abs(yp) .gt. precx) then
	    cdb1 = DBLE( (3/2D0 +
     +       (2 - 3*xp)*ffpvf(1, xp, yp)) )/p
	  else
	    call fferr(101, ier)
	    cdb1 = 999D300
	  endif

*		zero momentum case
	else if(abs(m1 - m2) .gt. precx*(m1 + m2)) then
	  xm = DCMPLX(1D0, -1D-5*precx)*m1/(m1 - m2)
	  ym = DCMPLX(1D0, -1D-5*precx)*m2/(m2 - m1)
	  if(abs(xm) .lt. 10) then
	    cdb1 = -(1/3D0 + ffypvf(2, xm, ym))/(m1 - m2)
	  else
	    cdb1 = -(1/3D0 + ffypvf(3, xm, ym))/m1
	  endif

	else
	  cdb1 = -1/12D0/m1

	endif

	end


*###[ ffxdb11:
	subroutine ffxdb11(cdb11, p, m1, m2, ier)
***#[*comment:***********************************************************
*									*
*	DB11 function (derivative of B11)                               *
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE COMPLEX cdb11
	DOUBLE PRECISION p, m1, m2
	integer ier

	DOUBLE COMPLEX ffpvf, ffypvf
	external ffpvf, ffypvf

	DOUBLE COMPLEX xp, xm, yp, ym, r

	include 'ff.h'

	if(abs(p) .gt. precx*(m1 + m2)) then

	  call ffroots(p, m1, m2, xp, xm, yp, ym, r)
	  if(abs(xp - xm) .gt. sqrt(precx)*abs(xp + xm)) then
	    cdb11 = (ffypvf(3, xm, ym) - ffypvf(3, xp, yp))/r
	  else if(abs(xp) .gt. 10) then
	    cdb11 = DBLE( (-3/4D0 +
     +       (4*xp - 3)*ffpvf(4, xp, yp))/xp**2 )/p
	  else if(abs(yp) .gt. precx) then
	    cdb11 = DBLE( (-4/3D0 +
     +       (4*xp - 3)*ffpvf(2, xp, yp))/p )
	  else
	    call fferr(102, ier)
	    cdb11 = 999D300
	  endif

*		zero momentum case
	else
	  call fferr(102, ier)
	  cdb11 = 999D300

	endif

	end


*###[ ffroots
	subroutine ffroots(p, m1, m2, xp, xm, yp, ym, r)
***#[*comment:***********************************************************
*									*
*	roots of quadratic equation					*
*	p*x^2 + (m2 - m1 - p)*x + m2 - eps =				*
*	  p*(x - xp)*(x - xm) = p*(x - 1 + yp)*(x - 1 + ym)		*
*	i.e. x[pm] = 1 - y[pm]						*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments
*
	DOUBLE PRECISION p, m1, m2
	DOUBLE COMPLEX xp, xm, yp, ym, r

	DOUBLE PRECISION q

	include 'ff.h'

	r = sqrt(dcmplx(p*(p - 2*(m1 + m2)) + (m1 - m2)**2))
	q = p + m1 - m2
	xp = (q + r)/2D0/p
	xm = (q - r)/2D0/p
	if(abs(xm) .gt. abs(xp)) then
	  xp = m1/p/xm
	else if(abs(xp) .gt. abs(xm)) then
	  xm = m1/p/xp
	endif
	xp = xp + DCMPLX(0D0, abs(p*xp)/p*1D-5*precx)
	xm = xm + DCMPLX(0D0, -abs(p*xm)/p*1D-5*precx)

	q = p - m1 + m2
	ym = (q + r)/2D0/p
	yp = (q - r)/2D0/p
	if(abs(ym) .gt. abs(yp)) then
	  yp = m2/p/ym
	else if(abs(yp) .gt. abs(ym)) then
	  ym = m2/p/yp
	endif
	yp = yp + DCMPLX(0D0, -abs(p*yp)/p*1D-5*precx)
	ym = ym + DCMPLX(0D0, abs(p*ym)/p*1D-5*precx)

	end


*###[ ffpvf
	DOUBLE COMPLEX function ffpvf(n, x, y)
***#[*comment:***********************************************************
*									*
*	Passarino-Veltman function f(n, x)				*
*	here third arg y = 1 - x					*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer n
	DOUBLE COMPLEX x, y

	integer m
	DOUBLE COMPLEX xm

	include 'ff.h'

	if(abs(x) .lt. 10) then
	  if(n .eq. 0) then
	    ffpvf = -log(-y/x)
	  else if(x .eq. 0) then
	    ffpvf = -1D0/n
	  else
	    ffpvf = 0
	    xm = 1
	    do 10 m = 0, n - 1
	      ffpvf = ffpvf - xm/(n - m)
	      xm = xm*x
10	    continue
	    ffpvf = ffpvf - xm*log(-y/x)
	  endif
	else
	  ffpvf = 0
	  xm = 1
	  do 20 m = 1, 30
	    xm = xm/x
	    ffpvf = ffpvf + xm/(m + n)
	    if(abs(xm/ffpvf) .lt. precx) return
20	  continue
	endif
	end


*###[ ffypvf
	DOUBLE COMPLEX function ffypvf(n, x, y)
***#[*comment:***********************************************************
*									*
*	y*ffpvf(n, x, y)						*
*									*
***#]*comment:*********************************************************** 
*  #[ declarations:
	implicit none
*
*	arguments
*
	integer n
	DOUBLE COMPLEX x, y

	DOUBLE COMPLEX ffpvf
	external ffpvf

	if(abs(y) .eq. 0) then
	  ffypvf = 0
	else
	  ffypvf = y*ffpvf(n, x, y)
	endif
	end

