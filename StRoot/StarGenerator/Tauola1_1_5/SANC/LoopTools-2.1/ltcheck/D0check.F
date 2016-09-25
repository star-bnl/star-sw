* D0check.F
* the scalar four-point function
* these functions are adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* they are used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "D0.F"


	double complex function D0check(p1, p2, p3, p4, p1p2, p2p3,
     &                                  m1, m2, m3, m4)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m3, m4

	double complex D0ir, D0m0, D0reg
	external D0ir, D0m0, D0reg

	if( m1 .eq. 0 ) then
	  if( abs(p1 - m2) + abs(p4 - m4) .lt. CALACC ) then
	    D0check = D0ir(p1, p2, p3, p4, p1p2, p2p3, m3)
	    return
	  endif
	  if( abs(p1 - m2) + abs(p1p2 - m3) .lt. CALACC ) then
	    D0check = D0ir(p1, p2p3, p3, p1p2, p4, p2, m4)
	    return
	  endif
	  if( abs(p4 - m4) + abs(p1p2 - m3) .lt. CALACC ) then
	    D0check = D0ir(p1p2, p2, p2p3, p4, p1, p3, m2)
	    return
	  endif
	  D0check = D0m0(p3, p4, p1, p2, p1p2, p2p3, m3, m4, m2)
	  return
	endif

	if( m2 .eq. 0 ) then
	  if( abs(p1 - m1) + abs(p2 - m3) .lt. CALACC ) then
	    D0check = D0ir(p1, p4, p3, p2, p2p3, p1p2, m4)
	    return
	  endif
	  if( abs(p1 - m1) + abs(p2p3 - m4) .lt. CALACC ) then
	    D0check = D0ir(p1, p1p2, p3, p2p3, p2, p4, m3)
	    return
	  endif
	  if( abs(p2 - m3) + abs(p2p3 - m4) .lt. CALACC ) then
	    D0check = D0ir(p2, p1p2, p4, p2p3, p1, p3, m1)
	    return
	  endif
	  D0check = D0m0(p4, p1, p2, p3, p2p3, p1p2, m4, m1, m3)
	  return
	endif

	if( m3 .eq. 0 ) then
	  if( abs(p2 - m2) + abs(p3 - m4) .lt. CALACC ) then
	    D0check = D0ir(p2, p1, p4, p3, p1p2, p2p3, m1)
	    return
	  endif
	  if( abs(p2 - m2) + abs(p1p2 - m1) .lt. CALACC ) then
	    D0check = D0ir(p2, p2p3, p4, p1p2, p3, p1, m4)
	    return
	  endif
	  if( abs(p3 - m4) + abs(p1p2 - m1) .lt. CALACC ) then
	    D0check = D0ir(p1p2, p1, p2p3, p3, p2, p4, m2)
	    return
	  endif
	  D0check = D0m0(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m4)
	  return
	endif

	if( m4 .eq. 0 ) then
	  if( abs(p4 - m1) + abs(p3 - m3) .lt. CALACC ) then
	    D0check = D0ir(p3, p2, p1, p4, p2p3, p1p2, m2)
	    return
	  endif
	  if( abs(p4 - m1) + abs(p2p3 - m2) .lt. CALACC ) then
	    D0check = D0ir(p2p3, p2, p1p2, p4, p3, p1, m3)
	    return
	  endif
	  if( abs(p3 - m3) + abs(p2p3 - m2) .lt. CALACC ) then
	    D0check = D0ir(p3, p1p2, p1, p2p3, p4, p2, m1)
	    return
	  endif
	  D0check = D0m0(p2, p3, p4, p1, p2p3, p1p2, m2, m3, m1)
	  return
	endif

	D0check = D0reg(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	end

************************************************************************

	double complex function D0ir(p1, p2, p3, p4, p1p2, p2p3, m3)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3, m3

#include "ff.h"

	double precision m1_, m3_, m4_, d
	double complex xs, x2, x3, y, c, f
	double complex logxs, logx2, logx3, log1x2, log1x3, logy

	double complex ln, spence, bdK
	external ln, spence, bdK

	m1_ = sqrt(p1)
	m4_ = sqrt(p4)
	d = p2p3 - (m1_ - m4_)**2
	f = .5D0/m1_/m4_/(p1p2 - m3)
	if( d .ne. 0 ) then
	  xs = bdK(p2p3, m1_, m4_)
	  logxs = log(xs)
	  f = f*2*xs/(1 - xs**2)
	endif

* massless case
	if( m3 .eq. 0 ) then
	  if( p1 .eq. p2 .and. p3 .eq. p4 ) then
	    D0ir = 2*f*ln(-lambda2/p1p2, 1D0)
	    if( d .ne. 0 ) D0ir = -logxs*D0ir
	    return
	  endif
	  y = m1_/m4_*(p3 - p4 + IEPS)/
     &                (p2 - p1 + IEPS)
	  logy = log(y)
	  c = ln(lambda2/m1_/m4_, 0D0) +
     &      ln((p2 - p1)/p1p2, p1 - p2) + ln((p3 - p4)/p1p2, p4 - p3)
	  if( d .ne. 0 ) then
	    D0ir = f*(pi6 +
     &        logxs*(-.5D0*logxs + 2*log(1 - xs**2) - c) +
     &        spence(xs**2, 0D0) + .5D0*logy**2 -
     &        spence(xs/y, 0D0) -
     &        (logxs + log(1/y))*log(1 - xs/y) -
     &        spence(xs*y, 0D0) -
     &        (logxs + logy)*log(1 - xs*y))
	    return
	  endif
	  D0ir = f*(c - 2 - (1 + y)/(1 - y)*logy)
	  return
	endif

* massive case
	m3_ = sqrt(m3)
	x2 = bdK(p2, m1_, m3_)
	x3 = bdK(p3, m4_, m3_)
	logx2 = log(x2)
	logx3 = log(x3)
	log1x3 = log(1/x3)
	c = ln(m3_*sqrt(lambda2)/(m3 - p1p2), 1D0)
	if( d .ne. 0 ) then
	  log1x2 = log(1/x2)
	  D0ir = f*(.5D0*pi**2 +
     &      2*log(xs)*(log(1 - xs**2) - c) +
     &      spence(xs**2, 0D0) + logx2**2 + logx3**2 -
     &      spence(xs/x2/x3, 0D0) -
     &      (logxs + log1x2 + log1x3)*log(1 - xs/x2/x3) -
     &      spence(xs*x2/x3, 0D0) -
     &      (logxs + logx2 + log1x3)*log(1 - xs*x2/x3) -
     &      spence(xs/x2*x3, 0D0) -
     &      (logxs + log1x2 + logx3)*log(1 - xs/x2*x3) -
     &      spence(xs*x2*x3, 0D0) -
     &      (logxs + logx2 + logx3)*log(1 - xs*x2*x3))
	  return
	endif
	D0ir = f*(2*c -
     &    (1 + x2/x3)/(1 - x2/x3)*(logx2 + log1x3) -
     &    (1 + x2*x3)/(1 - x2*x3)*(logx2 + logx3) - 2)
	end

************************************************************************

	double complex function D0m0(p1, p2, p3, p4, p1p2, p2p3,
     &                               m1, m2, m4)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m4

#include "ff.h"

	double complex D0m00, cspence, cln, k2r
	integer eta_tilde
	external D0m00, cspence, eta_tilde, cln, k2r

	double precision m1_, m2_, m4_
	double precision k12, k13, k14, k23, k24, k34
	double precision ir12, ir14, ir24, ix1(2), ix4(2)
	double complex r12, r14, r24, x4(2), x1
	double complex a, b, c, d, disc
	integer i

	if( m1 .eq. 0 ) then
	  D0m0 = D0m00(p1, p1p2, p3, p2p3, p2, p4, m2, m4)
	  return
	endif
	if( m2 .eq. 0 ) then
	  D0m0 = D0m00(p1, p2, p3, p4, p1p2, p2p3, m1, m4)
	  return
	endif
	if( m4 .eq. 0 ) then
	  D0m0 = D0m00(p4, p3, p2, p1, p1p2, p2p3, m1, m2)
	  return
	endif

	m1_ = sqrt(m1)
	m2_ = sqrt(m2)
	m4_ = sqrt(m4)

	k12 = (m1 + m2 - p1)/m1_/m2_
	k13 = (m1 - p1p2)/m1
	k14 = (m1 + m4 - p4)/m1_/m4_
	k23 = (m2 - p2)/m2_/m1_
	k24 = (m2 + m4 - p2p3)/m2_/m4_
	k34 = (m4 - p3)/m1_/m4_

	r12 = k2r(k12)
	r14 = k2r(k14)
	r24 = k2r(k24)

	a = k34/r24 - k23
	b = k13*(1/r24 - r24) + k12*k34 - k14*k23
	c = k13*(k12 - r24*k14) + r24*k34 - k23
	d = -k34*r24 + k23
	disc = sqrt(dcmplx((k12*k34 - k13*k24 - k14*k23)**2 -
     &    4*(k13*(k13 - k23*(k12 - k14*k24)) +
     &       k23*(k23 - k24*k34) + k34*(k34 - k13*k14))))
	x4(1) = .5D0/a*(-b + disc)
	x4(2) = .5D0/a*(-b - disc)
	if( abs(x4(1)) .gt. abs(x4(2)) ) then
	  x4(2) = c/a/x4(1)
	else
	  x4(1) = c/a/x4(2)
	endif

	if( k12 .lt. -2 ) then
	  ir12 = sign(10D0, 1 - abs(r12))
	else
	  ir12 = 0
	endif
	if( k14 .lt. -2 ) then
	  ir14 = sign(10D0, 1 - abs(r14))
	else
	  ir14 = 0
	endif
	if( k24 .lt. -2 ) then
	  ir24 = sign(10D0, 1 - abs(r24))
	else
	  ir24 = 0
	endif

	ix4(2) = sign(1D0, dble(d))
	ix4(1) = -ix4(2)
	ix1(1) = sign(1D0, ix4(1)*dble(r24))
	ix1(2) = -ix1(1)

	b = dcmplx(k34/k13)
	c = dcmplx(k23/k13)

	D0m0 = 0
	do i = 1, 2
	  x1 = -x4(i)/r24
	  D0m0 = D0m0 + (2*i - 3)*(
     &      cspence(-x4(i), r14, -ix4(i), ir14) +
     &      cspence(-x4(i), 1/r14, -ix4(i), -ir14) -
     &      cspence(x1, r12, -ix1(i), ir12) -
     &      cspence(x1, 1/r12, -ix1(i), -ir12) -
     &      cspence(-x4(i), b, -ix4(i), -k13) +
     &      cspence(x1, c, -ix1(i), -k13) -
     &      dcmplx(0D0, 2*pi)*
     &      eta_tilde(-x4(i), 1/r24, -ix4(i), -ir24)*(
     &        cln((k12 - r24*(k14 + x4(i)) - x1)/d,
     &            dble(-(r24 - 1/r24)*ix4(i)/d)) +
     &        cln(dcmplx(k13), -1D0) ) )
	enddo
	D0m0 = D0m0/m1/m2_/m4_/a/(x4(1) - x4(2))
	end

************************************************************************

	double complex function D0m00(p1, p2, p3, p4, p1p2, p2p3,
     &                                m1, m4)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m4

	double complex D0m000, cspence, k2r, addeps
	external D0m000, cspence, k2r, addeps

	double precision m1_, m4_
	double precision k12, k13, k14, k23, k24, k34
	double complex k12c, k13c, k23c, k24c, k34c
	double complex r14, x4(2)
	double complex a, b, c, disc
	integer i

	if( m1 .eq. 0 ) then
	  D0m00 = D0m000(p4, p1, p2, p3, p2p3, p1p2, m4)
	  return
	endif
	if( m4 .eq. 0 ) then
	  D0m00 = D0m000(p1, p2, p3, p4, p1p2, p2p3, m1)
	  return
	endif

	m1_ = sqrt(m1)
	m4_ = sqrt(m4)

	k12 = (m1 - p1)/m1
	k13 = (m1 - p1p2)/m1
	k14 = (m1 + m4 - p4)/m1_/m4_
	k23 = -p2/m1
	k24 = (m4 - p2p3)/m1_/m4_
	k34 = (m4 - p3)/m1_/m4_

	a = k34*k24 - k23
	b = k13*k24 + k12*k34 - k14*k23
	c = k13*k12 - ONEmEPS*k23
	disc = sqrt(b*b - 4*a*c)
	x4(1) = .5D0/a*(-b + disc)
	x4(2) = .5D0/a*(-b - disc)
	if( abs(x4(1)) .gt. abs(x4(2)) ) then
	  x4(2) = c/a/x4(1)
	else
	  x4(1) = c/a/x4(2)
	endif

	k12c = addeps(k12)
	k13c = addeps(k13)
	k23c = addeps(k23)

	k24c = addeps(k24)/k12c
	k34c = addeps(k34)/k13c
	c = log(k12c) + log(k13c) - log(k23c)

	r14 = k2r(k14)
	r14 = r14*dcmplx(1D0, sign(EPS, dble(1D0/r14 - r14)))

	D0m00 = 0
	do i = 1, 2
	  D0m00 = D0m00 + (2*i - 3)*(
     &      cspence(-x4(i), r14, 0D0, 0D0) +
     &      cspence(-x4(i), 1/r14, 0D0, 0D0) -
     &      cspence(-x4(i), k34c, 0D0, 0D0) -
     &      cspence(-x4(i), k24c, 0D0, 0D0) +
     &      log(-x4(i))*c )
	enddo
	D0m00 = D0m00/m1/m1_/m4_/a/(x4(1) - x4(2))
	end

************************************************************************

	double complex function D0m000(p1, p2, p3, p4, p1p2, p2p3, m1)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1

	double complex D0m0000, cspence, addeps
	external D0m0000, cspence, addeps

	double precision k12, k13, k14, k23, k24, k34
	double complex k12c, k13c, k14c, k23c, k24c, k34c
	double precision a, b
	double complex c, disc, x4(2)
	integer i

	if( m1 .eq. 0 ) then
	  D0m000 = D0m0000(p1, p2, p3, p4, p1p2, p2p3)
	  return
	endif

	k12 = (m1 - p1)/m1
	k13 = (m1 - p1p2)/m1
	k14 = (m1 - p4)/m1
	k23 = -p2/m1
	k24 = -p2p3/m1
	k34 = -p3/m1

	a = k34*k24
	b = k13*k24 + k12*k34 - k14*k23
	c = k13*k12 - ONEmEPS*k23
	disc = sqrt(b*b - 4*a*c)
	x4(1) = .5D0/a*(-b + disc)
	x4(2) = .5D0/a*(-b - disc)
	if( abs(x4(1)) .gt. abs(x4(2)) ) then
	  x4(2) = c/a/x4(1)
	else
	  x4(1) = c/a/x4(2)
	endif

	k12c = addeps(k12)
	k13c = addeps(k13)
	k23c = addeps(k23)
	k14c = addeps(k14)

	k24c = addeps(k24)/k12c
	k34c = addeps(k34)/k13c
	c = log(k12c) + log(k13c) - log(k23c)

	D0m000 = 0
	do i = 1, 2
	  D0m000 = D0m000 + (2*i - 3)*(
     &      cspence(-x4(i), k14c, 0D0, 0D0) -
     &      cspence(-x4(i), k34c, 0D0, 0D0) -
     &      cspence(-x4(i), k24c, 0D0, 0D0) +
     &      log(-x4(i))*c )
	enddo
	D0m000 = D0m000/m1**2/a/(x4(1) - x4(2))
	end

************************************************************************

	double complex function D0m0000(p1, p2, p3, p4, p1p2, p2p3)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3

	double complex cspence, addeps
	external cspence, addeps

	double precision m2
	double precision k12, k13, k14, k23, k24, k34
	double complex k12c, k13c, k14c, k23c, k24c, k34c
	double precision a, b
	double complex c, disc, x4(2)
	integer i

	m2 = abs(p2p3)
	k12 = -p1/m2
	k13 = -p1p2/m2
	k14 = -p4/m2
	k23 = -p2/m2
	k24 = -p2p3/m2
	k34 = -p3/m2

	a = k34*k24
	b = k13*k24 + k12*k34 - k14*k23
	c = k13*k12 + IEPS*k23
	disc = sqrt(b*b - 4*a*c)
	x4(1) = .5D0/a*(-b + disc)
	x4(2) = .5D0/a*(-b - disc)
	if( abs(x4(1)) .gt. abs(x4(2)) ) then
	  x4(2) = c/a/x4(1)
	else
	  x4(1) = c/a/x4(2)
	endif

	k12c = addeps(k12)
	k13c = addeps(k13)
	k23c = addeps(k23)
	k14c = addeps(k14)

	k24c = addeps(k24)/k12c
	k34c = addeps(k34)/k13c
	c = log(k12c) + log(k13c) - log(k23c) - log(k14c)

	D0m0000 = 0
	do i = 1, 2
	  disc = log(-x4(i))
	  D0m0000 = D0m0000 + (2*i - 3)*(
     &      -cspence(-x4(i), k34c, 0D0, 0D0) -
     &      cspence(-x4(i), k24c, 0D0, 0D0) +
     &      disc*(c - .5D0*disc) )
	enddo
	D0m0000 = D0m0000/m2**2/a/(x4(1) - x4(2))
	end

************************************************************************

	double complex function D0reg(p1, p2, p3, p4, p1p2, p2p3,
     &                                m1, m2, m3, m4)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m3, m4

#include "ff.h"

	double complex cspence, cln, k2r
	integer eta
	external cspence, cln, eta, k2r

	double precision m1_, m2_, m3_, m4_
	double precision k12, k13, k14, k23, k24, k34
	double precision ir12, ir14, ir23, ir24, ir34
	double complex r12, r14, r13, r23, r24, r34
	double complex x(2, 4), s(4)
	double precision ix(2, 4), is(4)
	double complex a, b, c, disc
	integer j, k

	m1_ = sqrt(m1)
	m2_ = sqrt(m2)
	m3_ = sqrt(m3)
	m4_ = sqrt(m4)

	k12 = (m1 + m2 - p1)/m1_/m2_
	k13 = (m1 + m3 - p1p2)/m1_/m3_
	k14 = (m1 + m4 - p4)/m1_/m4_
	k23 = (m2 + m3 - p2)/m2_/m3_
	k24 = (m2 + m4 - p2p3)/m2_/m4_
	k34 = (m3 + m4 - p3)/m3_/m4_

#ifdef WARNINGS
	if( k13 .lt. 2 )
     &    print *, "D0reg: case k13 < 0 not implemented."
#endif

	r12 = k2r(k12)
	r13 = 1/k2r(k13)
	r14 = k2r(k14)
	r23 = k2r(k23)
	r24 = 1/k2r(k24)
	r34 = k2r(k34)

	a = k34/r24 - k23 + (k12 - k14/r24)*r13
	b = (1/r13 - r13)*(1/r24 - r24) + k12*k34 - k14*k23
	c = k34*r24 - k23 + (k12 - k14*r24)/r13
	disc = sqrt(b*b - 4*a*c)
	x(1, 4) = .5D0/a*(-b + disc)
	x(2, 4) = .5D0/a*(-b - disc)
	if( abs(x(1, 4)) .gt. abs(x(2, 4)) ) then
	  x(2, 4) = c/a/x(1, 4)
	else    
	  x(1, 4) = c/a/x(2, 4)
	endif

	if( k12 .lt. -2 ) then
	  ir12 = sign(10D0, 1 - abs(r12))
	else
	  ir12 = 0
	endif
	if( k14 .lt. -2 ) then
	  ir14 = sign(10D0, 1 - abs(r14))
	else
	  ir14 = 0
	endif
	if( k23 .lt. -2 ) then
	  ir23 = sign(10D0, 1 - abs(r23))
	else
	  ir23 = 0
	endif
	if( k24 .lt. -2 ) then
	  ir24 = sign(10D0, 1 - abs(r24))
	else if( k24 .eq. -2 ) then
	  ir24 = 10
	else
	  ir24 = 0
	endif
	if( k34 .lt. -2 ) then
	  ir34 = sign(10D0, 1 - abs(r34))
	else
	  ir34 = 0
	endif

	x(1, 1) = x(1, 4)/r24
	x(2, 1) = x(2, 4)/r24
	x(1, 2) = x(1, 4)/r24*r13
	x(2, 2) = x(2, 4)/r24*r13
	x(1, 3) = x(1, 4)*r13
	x(2, 3) = x(2, 4)*r13

	if( dble(x(1, 4)) .gt. 0 ) then
	  ix(1, 4) = 1
	else
	  ix(1, 4) = 0
	endif
	if( dble(x(2, 4)) .gt. 0 ) then
	  ix(2, 4) = -1
	else
	  ix(2, 4) = 0
	endif
	ix(1, 1) = ix(1, 4) + ir24
	if( dble(x(1, 1)) .le. 0 ) ix(1, 1) = -ix(1, 1)
	ix(2, 1) = ix(2, 4) + ir24
	if( dble(x(2, 1)) .le. 0 ) ix(2, 1) = -ix(2, 1)
	ix(1, 3) = ix(1, 4)
	ix(2, 3) = ix(2, 4)
	ix(1, 2) = ix(1, 1)
	ix(2, 2) = ix(2, 1)
 
	s(1) = r12
	s(2) = r23
	s(3) = r34
	s(4) = r14
	is(1) = ir12
	is(2) = ir23
	is(3) = ir34
	is(4) = ir14

	D0reg = 0
	do k = 1, 2
	  do j = 1, 4
	    D0reg = D0reg - (2*mod(j + k, 2) - 1)*(
     &        cspence(-x(k, j), s(j), -ix(k, j), is(j)) +
     &        cspence(-x(k, j), 1/s(j), -ix(k, j), -is(j)) )
	  enddo
	  b = 1 + (k34 + x(k, 3))*x(k, 3)
	  D0reg = D0reg + (2*mod(k, 2) - 1)*(
     &      eta(-x(k, 4), 1/r24, -ix(k, 4), -ir24, -ix(k, 1))*
     &        dcmplx(0D0, 2*pi)*
     &        cln((1 + (k14 + x(k, 4))*x(k, 4))/b, -dble(b)) )
	enddo
	D0reg = D0reg/m1_/m2_/m3_/m4_/disc
	end

************************************************************************

	double complex function k2r(k)
	implicit none
	double precision k

	k2r = .5D0*k*(1 + sqrt(dcmplx(1 - 4/k**2)))
	end

************************************************************************

	double complex function addeps(k)
	implicit none
	double precision k

	addeps = k*dcmplx(1D0, -sign(EPS, k))
	end

************************************************************************

	double complex function bdK(x, m1, m2)
* this is actually -K from the Beenakker/Denner paper for D0ir
	implicit none
	double precision x, m1, m2

	double precision d
	double complex beta

	d = x - (m1 - m2)**2
	if( d .eq. 0 ) then
	  bdK = 1
	else
	  beta = sqrt(1 - 4*m1*m2/(d + IEPS))
	  bdK = (beta - 1)/(beta + 1)
	endif
	end

************************************************************************

	double complex function cspence(z1, z2, im1, im2)
	implicit none
	double complex z1, z2
	double precision im1, im2

#include "ff.h"

	double complex cln, spence
	integer eta
	external cln, spence, eta

	double complex z12
	double precision im12
	integer etas

	z12 = z1*z2
	im12 = im2*sign(1D0, dble(z1))
	if( dble(z12) .gt. .5D0 ) then
	  cspence = spence(1 - z12, 0D0)
	  etas = eta(z1, z2, im1, im2, im12)
	  if( etas .ne. 0 ) cspence = cspence + 
     &      etas*dcmplx(0D0, 2*pi)*
     &        cln(1 - z12, -im12)
	else if( abs(z12) .lt. 1D-4 ) then
	  cspence = pi6 -
     &      spence(z12, 0D0) + (cln(z1, im1) + cln(z2, im2))*z12*
     &        (1 + z12*(.5D0 + z12*(1/3D0 + z12/4D0)))
	else  
	  cspence = pi6 -
     &      spence(z12, 0D0) -
     &      (cln(z1, im1) + cln(z2, im2))*cln(1 - z12, 0D0)
	endif
	end

************************************************************************

	integer function eta_tilde(c1, c2, im1x, im2x)
	implicit none
	double complex c1, c2
	double precision im1x, im2x

	double precision im1, im2

	integer eta
	external eta

	im1 = dimag(c1)
	if( im1 .eq. 0 ) im1 = im1x
	im2 = dimag(c2)
	if( im2 .ne. 0 ) then
	  eta_tilde = eta(c1, c2, im1x, 0D0, 0D0)
	else if( dble(c2) .gt. 0 ) then
	  eta_tilde = 0
	else if( im1 .gt. 0 .and. im2x .gt. 0 ) then
	  eta_tilde = -1
	else if( im1 .lt. 0 .and. im2x .lt. 0 ) then
	  eta_tilde = 1
	else
	  eta_tilde = 0
#ifdef WARNINGS
	  if( im1 .eq. 0 .and. dble(c1) .lt. 0 .or.
     &        im2x .eq. 0 .and. dble(c1*c2) .lt. 0 )
     &      print *, "eta_tilde not defined"
#endif
	endif
	end

