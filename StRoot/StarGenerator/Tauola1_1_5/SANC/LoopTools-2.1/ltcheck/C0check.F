* C0check.F
* the scalar three-point function
* these functions are adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* they are used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "C0.F"


	double complex function C0check(p1, p2, p1p2, m1, m2, m3)
	implicit none
	double precision p1, p2, p1p2, m1, m2, m3

	double complex C0ir, C0reg
	external C0ir, C0reg

	if( m1 .eq. 0 .and.
     &      (abs(p1 - m2) + abs(p1p2 - m3)) .lt. CALACC ) then
	  C0check = C0ir(p2, p1, p1p2)
	  return
	endif
	if( m2 .eq. 0 .and.
     &      (abs(p1 - m1) + abs(p2 - m3)) .lt. CALACC ) then
	  C0check = C0ir(p1p2, p1, p2)
	  return
	endif
	if( m3 .eq. 0 .and.
     &      (abs(p2 - m2) + abs(p1p2 - m1)) .lt. CALACC ) then
	  C0check = C0ir(p1, p2, p1p2)
	  return
	endif
	C0check = C0reg(p1, p2, p1p2, m1, m2, m3)
	end

************************************************************************

	double complex function C0reg(p1, p2, p1p2, m1, m2, m3)
	implicit none
	double precision p1, p2, p1p2, m1, m2, m3

#include "ff.h"

	double precision q(5), m(5), mki, mkj, mij, qijk, ar
	double complex a, b, h, h0, h1, h2, h3, h4
	double complex y1, y2, y3, y4, x1, x2, x3, x4
	integer i, j, k

	double complex spence
	integer eta_n
	external spence, eta_n

	q(1) = p1
	q(2) = p2
	q(3) = p1p2
	q(4) = q(1)
	q(5) = q(2)

	m(1) = m1
	m(2) = m2
	m(3) = m3
	m(4) = m(1)
	m(5) = m(2)

	C0reg = 0

* all mom-squares != 0
	if( p1*p2*p1p2 .ne. 0 ) then
	  a = sqrt(dcmplx((p2 - p1 - p1p2)**2 - 4*p1*p1p2))
	  do i = 1, 3
	    j = i + 1
	    k = i + 2
	    mki = m(k) - m(i)
	    mkj = m(k) - m(j)
	    mij = m(i) - m(j)
	    qijk = q(i) - q(j) - q(k)
	    h2 = .5D0/a/q(i)

	    h = q(i)*(qijk + mki + mkj) - mij*(q(j) - q(k))
	    y1 = h2*(h + a*(q(i) - mij))
	    y2 = h2*(h + a*(-q(i) - mij))
	    b = sqrt(dcmplx((q(i) - mij)**2 - 4*q(i)*m(j)))
	    y3 = h2*(h + a*b)
	    y4 = h2*(h - a*b)

	    h0 = q(i)*(q(j)*q(k) + qijk*m(k) + mki*mkj) -
     &        mij*(q(j)*mki - q(k)*mkj)
	    qijk = q(j) - q(k) - q(i)
	    h3 = h0 + q(j)*qijk*m(i) + q(k)*(q(k) - q(i) - q(j))*m(j)
	    if( abs(y3) .lt. abs(y4) ) then
	      y3 = h3/a**2/q(i)/y4
	    else
	      y4 = h3/a**2/q(i)/y3
	    endif
	    if( a*b .ne. 0 ) then
	      y3 = y3 + IEPS/a/b*abs(a*b*y3)
	      y4 = y4 - IEPS/a/b*abs(a*b*y4)
	    else
	      y3 = y3*ONEpEPS
	      y4 = y4*ONEmEPS
	    endif

	    h1 = h2*(h - a*(q(i) - mij))
	    if( abs(y1) .lt. abs(h1) ) then
	      h3 = h0 + q(j)*qijk*m(i) +
     &          (q(k)*(q(i) + q(j)) - (q(i) - q(j))**2)*m(j)
	      y1 = h3/a**2/q(i)/h1
	    endif
	    h1 = h2*(h + a*(q(i) + mij))
	    if( abs(y2) .lt. abs(h1) ) then
	      h3 = h0 + q(k)*(q(k) - q(i) - q(j))*m(j) +
     &          (q(j)*(q(k) + q(i)) - (q(k) - q(i))**2)*m(i)
	      y2 = h3/a**2/q(i)/h1
	    endif

	    C0reg = C0reg +
     &        spence(y2/y3, 0D0) + spence(y2/y4, 0D0) -
     &        spence(y1/y3, 0D0) - spence(y1/y4, 0D0)

	    if( dimag(a) .ne. 0 ) then
	      h3 = IEPS*abs(b)/b
	      x1 = -.5D0*(q(i) - mij + b)/q(i) - h3
	      x2 = -.5D0*(q(i) - mij - b)/q(i) - h3
	      x3 = -.5D0*(-q(i) - mij + b)/q(i) - h3
	      x4 = -.5D0*(-q(i) - mij - b)/q(i) - h3
	      h3 = 1/y3
	      h4 = 1/y4
	      h = log(y1)*(eta_n(x1, x2) + eta_n(h3, h4) -
     &                     eta_n(x1, h3) - eta_n(x2, h4) ) -
     &            log(y2)*(eta_n(x3, x4) + eta_n(h3, h4) -
     &                     eta_n(x3, h3) - eta_n(x4, h4) ) +
     &            log(y3)*(eta_n(x1, h3) - eta_n(x3, h3)) +
     &            log(y4)*(eta_n(x2, h4) - eta_n(x4, h4))
	      if( dimag(a) .gt. 0 .and. q(i) .lt. 0 )
     &          h = h - log(y1/y2)
	      C0reg = C0reg +
     &          dcmplx(0D0, 2*pi)*h
	    endif
	  enddo
	  C0reg = C0reg/a
	  return
	endif

* one mom-square zero
	if( (p2*p1 + p1p2*p2 + p1*p1p2) .ne. 0 ) then
	  if( p1 .ne. 0 ) then
	    if( p2 .eq. 0 ) then
	      m(1) = m2
	      m(2) = m3
	      m(3) = m1
	      q(1) = p2
	      q(2) = p1p2
	      q(3) = p1
	    else
	      m(1) = m3
	      m(2) = m1
	      m(3) = m2
	      q(1) = p1p2
	      q(2) = p1
	      q(3) = p2
	    endif
	    m(4) = m(1)
	    m(5) = m(2)
	    q(4) = q(1)
	    q(5) = q(2)
	  endif
	  ar = q(2) - q(3)
	  do i = 2, 3
	    j = i + 1
	    k = i + 2
	    mki = m(k) - m(i)
	    mkj = m(k) - m(j)
	    mij = m(i) - m(j)
	    qijk = q(i) - q(j) - q(k)

	    if( i .eq. 2 ) then
	      y1 = 2*q(2)*(mki + ar)
	      y2 = 2*q(2)*mki
	    else
	      y1 = 2*q(3)*mkj
	      y2 = 2*q(3)*(mkj - ar)
	    endif
	    h = q(i)*(qijk + mki + mkj) - mij*(q(j) - q(k))
	    b = sqrt(dcmplx((q(i) - mij)**2 - 4*q(i)*m(j)))
	    y3 = h + ar*b
	    y4 = h - ar*b

	    h0 = q(i)*(q(j)*q(k) + qijk*m(k) + mki*mkj) -
     &        mij*(q(j)*mki - q(k)*mkj)
	    h3 = h0 + q(j)*(q(j) - q(k) - q(i))*m(i) + 
     &                q(k)*(q(k) - q(i) - q(j))*m(j)
	    h3 = 4*h3*q(i)
	    if( abs(y3) .lt. abs(y4) ) then
	      y3 = h3/y4
	    else
	      y4 = h3/y3
	    endif
	    qijk = ar/q(i)
	    if( qijk .ne. 0 ) then
	      y3 = y3 + IEPS/qijk*abs(qijk*y3)
	      y4 = y4 - IEPS/qijk*abs(qijk*y4)
	    else
	      y3 = y3*ONEpEPS
	      y4 = y4*ONEmEPS
	    endif

	    C0reg = C0reg +
     &        spence(y2/y3, 0D0) + spence(y2/y4, 0D0) -
     &        spence(y1/y3, 0D0) - spence(y1/y4, 0D0)
	  enddo
	  C0reg = C0reg/ar
	  return
	endif

* two mom-squares zero
	if( p1p2 .eq. 0 ) then
	  if( p2 .ne. 0 ) then
	    m(1) = m3
	    m(2) = m1
	    m(3) = m2
	    q(1) = p1p2
	    q(2) = p1
	    q(3) = p2
	  else
	    m(1) = m2
	    m(2) = m3
	    m(3) = m1
	    q(1) = p2
	    q(2) = p1p2
	    q(3) = p1
	  endif
	  m(4) = m(1)
	  m(5) = m(2)
	  q(4) = q(1)
	  q(5) = q(2)
	endif

	mki = m(2) - m(3)
	mkj = m(2) - m(1)
	mij = m(3) - m(1)

	if( m(2) .ne. m(3) ) then
	  y1 = -q(3) - mkj
	  y2 = -mkj
	  qijk = -mkj - q(3)*m(2)/mki
	  y3 = qijk - IEPS*sign(1D0, -q(3)/mki)*abs(qijk)
	  C0reg = C0reg + spence(y2/y3, 0D0) - spence(y1/y3, 0D0)
	endif

	b = sqrt(dcmplx((q(3) - mij)**2 - 4*q(3)*m(1)))
	h = q(3)*(q(3) + mki + mkj)
	y1 = 2*q(3)*mkj
	y2 = 2*q(3)*(q(3) + mkj)
	y3 = h - q(3)*b
	y4 = h + q(3)*b
	h0 = 4*q(3)**2*(q(3)*m(2) + mki*mkj)
	if( abs(y3) .lt. abs(y4) ) then
	  y3 = h0/y4
	else
	  y4 = h0/y3
	endif
	y3 = y3 - IEPS*abs(y3)
	y4 = y4 + IEPS*abs(y4)

	C0reg = -(C0reg +
     &    spence(y2/y3, 0D0) + spence(y2/y4, 0D0) -
     &    spence(y1/y3, 0D0) - spence(y1/y4, 0D0))/q(3)
	end

************************************************************************

	double complex function C0ir(p2, p1, p1p2)
	implicit none
	double precision p2, p1, p1p2

#include "ff.h"

	double complex spence, ln
	external spence, ln

	double precision a, h1, h2, h3, ps
	double complex c

	if( p1p2 .eq. 0 .or. p1 .eq. 0 ) then
	  print *, "C0ir: mass singular case"
	  C0ir = 999D300
	  return
	endif

	if( p2 .eq. 0 ) then
	  C0ir = -log(p1p2*p1/lambda2**2)*
     &      log(p1/p1p2)/4D0/(p1 - p1p2)
	  return
	endif

	ps = p2 - p1 - p1p2
	a = ps**2 - 4*p1*p1p2
	if( a .lt. 0 )
     &    print *, "C0ir: complex square root not implemented"
	a = sqrt(a)
	if( ps .le. 0 ) then
	  h1 = .5D0*(a - ps)
	else
	  h1 = -2*p1*p1p2/(a + ps)
	endif
	ps = p2 - p1 + p1p2
	if( ps .le. 0 ) then
	  h2 = .5D0*(a - ps)
	else
	  h2 = -2*p2*p1p2/(a + ps)
	endif
	ps = p2 + p1 - p1p2
	if( ps .le. 0 ) then
	  h3 = .5D0*(a - ps)
	else
	  h3 = -2*p1*p2/(a + ps)
	endif

	c = ln(-a/p2, -1D0)
	C0ir = (-pi**2/6D0 +
     &    spence(dcmplx(h2/a), -1D0) + spence(dcmplx(h3/a), -1D0) -
     &    .5D0*(ln(-h2/p2, -1D0)**2 + ln(-h3/p2, -1D0)**2) +
     &    .25D0*(ln(-p1/p2, -1D0)**2 + ln(-p1p2/p2, -1D0)**2) -
     &    c*(ln(-h1/p2, -1D0) - c) +
     &    ln(-lambda2/p2, -1D0)*ln(h1/sqrt(p1*p1p2), 1D0))/a
	end

************************************************************************

	integer function eta_n(c1, c2)
	implicit none
	double complex c1, c2

	integer eta
	external eta

	eta_n = eta(c1, c2, 0D0, 0D0, 0D0)
	end

