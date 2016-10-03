C-----------------------------------------------------------------------
C...VEGAS
C-----------------------------------------------------------------------
      block data vegdat
C...Makes default parameter assignments for VEGAS
      implicit  real*8 (a - h, o - z)
      REAL*8 xl, xu, acc
      INTEGER*4 ncall, itmx, nprn, ndev
      common /bveg1/ ncall, itmx, nprn, ndev, xl(10), xu(10), acc
      common /bveg2/ it, ndo, si, swgt, schi, xi (100, 10)
      common /bveg3/ alph, ndmx, mds
      common /rnsd/ iseed
      data ncall/1000000/, itmx/100/, nprn/0/, acc/-1.0d0/,
     $    xl/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0
     $      ,0.0d0,0.0d0,0.0d0,0.0d0/,
     $    xu/1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0
     $      ,1.0d0,1.0d0,1.0d0,1.0d0/,
     $    alph/1.5d0/, ndmx/100/, mds/1/, ndev/6/,
     $    ndo/1/, xi/1000 * 1.0d0/, it/0/, si, swgt, schi/3 * 0.0d0/
      end
C-----------------------------------------------------------------------
      subroutine vegas (ndim, fxn, avgi, sd, chi2a)
C...Subroutine performs ndim-dimensional Monte-Carlo integration
C...- by G.P. Lepage Sept 1976 / (rev) Aug 1979
C...- algorithm described in J Comp Phys 27, 192 (1978)
      implicit real*8 (a - h, o - z)
      REAL*8 xl, xu, acc
      INTEGER*4 ncall, itmx, nprn, ndev
      common /bveg1/ ncall, itmx, nprn, ndev, xl(10), xu(10), acc
      common /bveg2/ it, ndo, si, swgt, schi, xi (100, 10)
      common /bveg3/ alph, ndmx, mds
      common /bveg4/ calls, ti, tsi
      dimension d (100, 10), di (100, 10), xin (100), r (100), dx (10),
     $    ia (10), kg (10), dt (10), x (10)
C...Dimension rand(10)
      real*8 ran (10)
      data one/1d0/
      sqrt (a) = dsqrt (a)
      alog (a) = dlog (a)
      abs (a)  = dabs (a)
C...
      ndo = 1
      do 1 j = 1, ndim
 1        xi (1, j) = one
C...
      entry vegas1 (ndim, fxn, avgi, sd, chi2a)
C...- initializes cumulative variables, but not grid
      it = 0
      si = 0d0
      swgt = si
      schi = si
C...
      entry vegas2 (ndim, fxn, avgi, sd, chi2a)
C...- no initialization
      nd = ndmx
      ng = 1
      if (mds .eq. 0)  goto 2
      ng = (ncall / 2d0) ** (1d0 / ndim)
      mds = 1
      if ((2 * ng - ndmx) .lt. 0) goto 2
      mds = - 1
      npg = ng / ndmx + 1
      nd = ng / npg
      ng = npg * nd
 2    k = ng ** ndim
      npg = ncall / k
      if (npg .lt. 2) npg = 2
      calls = npg * k
      dxg = one / ng
      dv2g = (calls * dxg ** ndim) ** 2 / npg / npg / (npg - one)
      xnd = nd
      ndm = nd - 1
      dxg = dxg * xnd
      xjac = one / calls
      do 3 j = 1, ndim
          dx (j) = xu (j) - xl (j)
 3        xjac = xjac * dx (j)
C...Rebin, preserving bin density
      if (nd .eq. ndo) goto 8
      rc = ndo / xnd
      do 7 j = 1, ndim
          k = 0
          xn = 0.0d0
          dr = xn
          i = k
 4        k = k + 1
          dr = dr + one
          xo = xn
          xn = xi (k, j)
 5        if (rc .gt. dr) goto 4
          i = i + 1
          dr = dr - rc
          xin (i) = xn - (xn - xo) * dr
          if (i .lt. ndm) goto 5
          do 6 i = 1, ndm
 6            xi (i, j) = xin (i)
 7        xi (nd, j) = one
      ndo = nd
C...
 8    if (nprn .ge. 0) write (ndev, 200) ndim, calls, it, itmx, acc,
     $    nprn, alph, mds, nd, (xl (j), xu (j), j = 1, ndim)
      if (nprn .ge. 0) write (ndev, 222)
C...
      entry vegas3 (ndim, fxn, avgi, sd, chi2a)
C...- main integration loop
 9    it = it + 1
      ti = 0d0
      tsi = ti
      do 10 j = 1, ndim
          kg (j) = 1
          do 10 i = 1, nd
              d (i, j) = ti
10            di (i, j) = ti
C...
 11   fb = 0d0
      f2b = fb
      k = 0
 12   k = k + 1
      call randa (ndim, ran)
      wgt = xjac
      do 15 j = 1, ndim
          xn=(kg (j) - ran (j)) * dxg + one
          ia (j) = xn
          if (ia (j) .gt. 1) goto 13
          xo = xi (ia (j), j)
          rc = (xn - ia (j)) * xo
          goto 14
 13       xo = xi(ia (j), j) - xi (ia (j) - 1, j)
          rc = xi(ia (j) - 1, j) + (xn - ia (j)) * xo
 14       x (j) = xl (j) + rc * dx (j)
 15       wgt = wgt * xo * xnd
C...
      f = wgt
      f = f * fxn (x, wgt)
      f2 = f * f
CAC
c     print *,'f ',f
c     print *,'f2',f2
c     print *,'fb  a',fb
c     print *,'f2b a',f2b
CAC
      fb = fb + f
      f2b = f2b + f2
CAC
c     print *,'fb  b',fb
c     print *,'f2b b',f2b
CAC
      do 16 j = 1, ndim
          di(ia (j), j) = di (ia (j), j) + f
 16       if (mds .ge. 0) d (ia (j), j) = d (ia (j), j) + f2
      if (k .lt. npg) goto 12
C...
      f2b = sqrt (f2b * npg)
CAC
c     print *,'f2b c',f2b,npg
CAC
      f2b = (f2b - fb) * (f2b + fb)
CAC
c     print *,'f2b d',f2b
CAC
      ti = ti + fb
      tsi = tsi + f2b
      if (mds .ge. 0) goto 18
      do 17 j = 1, ndim
 17       d (ia (j), j) = d (ia (j), j) + f2b
 18   k = ndim
 19   kg (k) = mod (kg (k), ng) + 1
      if (kg (k) .ne. 1) goto 11
      k = k - 1
      if (k .gt. 0) goto 19
C...Compute final results for this iteration
CAC
c     print *,'tsi',tsi,dv2g
CAC
      tsi = tsi * dv2g
      ti2 = ti * ti
      wgt = one / tsi
      si = si + ti * wgt
      swgt = swgt + wgt
      schi = schi + ti2 * wgt
      avgi = si / swgt
      chi2a = (schi - si * avgi) / (it - 0.9999d0)
      sd = sqrt (one / swgt)
C...
      if (nprn .lt. 0) goto 21
      tsi = sqrt (tsi)
      write (ndev, 201) it, ti, tsi, avgi, sd, chi2a
      if (nprn .eq. 0) goto 21
      do 20 j = 1, ndim
 20       write (ndev, 202) j, (xi (i, j), di (i, j),
     $    i = 1 + nprn / 2, nd, nprn)
C...Refine grid
 21   do 23 j = 1, ndim
          xo = d (1, j)
          xn = d (2, j)
          d (1, j) = (xo + xn) / 2.0d0
          dt (j) = d (1, j)
          do 22 i = 2, ndm
              d (i, j) = xo + xn
              xo = xn
              xn = d (i + 1, j)
              d (i, j) = (d (i, j) + xn) / 3.0d0
 22           dt (j) = dt (j) + d (i, j)
          d (nd, j) = (xo + xn) / 2.0d0
 23       dt (j) = dt (j) + d (nd, j)
C...
      do 28 j = 1, ndim
          rc = 0.0d0
          do 24 i = 1, nd
              r(i) = 0d0
              if (d (i, j) .le. 0.0d0) goto 24
              xo = dt (j) / d (i, j)
              r (i) = ((xo - one) / xo / alog (xo)) ** alph
 24           rc = rc + r (i)
          rc = rc / xnd
          k = 0
          xn = 0d0
          dr = xn
          i = k
 25       k = k + 1
          dr = dr + r (k)
          xo = xn
          xn = xi (k, j)
 26       if (rc .gt. dr) goto 25
          i = i + 1
          dr = dr - rc
          xin (i) = xn - (xn - xo) * dr / r (k)
          if (i .lt. ndm) goto 26
          do 27 i = 1, ndm
 27           xi (i, j) = xin (i)
 28       xi (nd, j) = one
C...
      if (it .lt. itmx .and. acc * abs (avgi) .lt. sd) goto 9
 200  format (
     S    /1x, 'Input parameters for Vegas:'/
     $    /1x, 'ndim     = ', i10
     $    /1x, 'ncall    = ', f10.0
     $    /1x, 'it+      = ', i10
     $    /1x, 'itmx     = ', i10
     $    /1x, 'acc      = ', f10.6
     $    /1x, 'nprn     = ', i10 
     $    /1x, 'alph     = ', f10.2
     $    /1x, 'mds      = ', i10
     $    /1x, 'nd       = ', i10/
     $    /1x, '(xl, xu):',/
     $    /(1x, '( ', f18.6, ' , ', f18.6, ' )')
     $    )
 222  format (
     $    //1x, 'Iteration', 9x,
     $    'Integral', 8x,
     $    'Std.-Dev.', 6x,
     $    'Accum.-Int.', 9x,
     $    'Std.-Dev', 12x,
     $    'Chi2a'/
     $    )
 201  format (5x, i5, 2x, 5(g15.6, 2x))
 202  format (
     $    /1x, 'data for axis ' ,i2
     $    /7x, 'x', 5x, 'delta i', 11x, 'x', 5x, 'delta i', 11x,
     $    'x', 5x, 'delta i'
     $    /(1x, f7.6, 1x, f11.4, 5x, f7.6, 1x, f11.4, 5x,
     $    f7.6, 1x, f11.4)
     $    )
      return
      end
C-----------------------------------------------------------------------
      subroutine randa (n, ran)
C...Subroutine generates uniformly distributed random
C...numbers x (i), i = 1, n
      implicit none
      integer n
      real*8 ran (n), randdd
CC    real*8 ran (n), ranf
      integer i
      do i = 1, n
          ran (i) = randdd(n)
CC        ran (i) = ranf(1.1D0)
      enddo
      return
      end
C-----------------------------------------------------------------------
       real*8 function ranf(dummy)
*
*      random number function taken from knuth
*      (seminumerical algorithms).
*      method is x(n)=mod(x(n-55)-x(n-24),1/fmodul)
*      no provision yet for control over the seed number.
*
*      ranf gives one random number between 0 and 1.
*      irn55 generates 55 random numbers between 0 and 1/fmodul.
*      in55  initializes the 55 numbers and warms up the sequence.
*
       implicit real*8 (a-h,o-z)
       parameter (fmodul=1.d-09)
       integer ia(55)
       save ia
       data ncall/0/
       data mcall/55/
       if( ncall.eq.0 ) then
           call in55 ( ia,234612957 )
           ncall = 1
       endif
       if ( mcall.eq.0 ) then
           call irn55(ia)
           mcall=55
       endif
       ranf=ia(mcall)*fmodul
       mcall=mcall-1
       end

       subroutine in55(ia,ix)
       parameter (modulo=1000000000)
       integer ia(55)
       ia(55)=ix
       j=ix
       k=1
       do 10 i=1,54
       ii=mod(21*i,55)
       ia(ii)=k
       k=j-k
       if(k.lt.0)k=k+modulo
       j=ia(ii)
   10  continue
       do 20 i=1,10
       call irn55(ia)
   20  continue
       end

       subroutine irn55(ia)
       parameter (modulo=1000000000)
       integer ia(55)
       do 10 i=1,24
       j=ia(i)+ia(i+31)
       if(j.ge.modulo)j=j-modulo
       ia(i)=j
   10  continue
       do 20 i=25,55
       j=ia(i)+ia(i-24)
       if(j.ge.modulo)j=j-modulo
       ia(i)=j
   20  continue
       end
c************************ Random number generator *******
	real*8 function ranini(n)
	implicit real*8 (a-h,o-z)
	real*8 randdd
C INITIALIZE
	dimension r3(127)
	data r1/2.d0/,s/0.d0/,t/1.d0/,rmc/1.d0/,iw/-1/
	save
	if(n.eq.0)n=12345
C MUST HAVE NON-ZERO INITIALISER
	if(iw.gt.0)goto 30
10	iw=iw+1
	t=.5d0*t
	r1=s
	s=s+t
	if((s.gt.r1).and.(s.lt.1.d0))goto 10
	ikt=(iw-1)/12
	ic=iw-12*ikt
	id=2**(13-ic)
	do 20 i=1,ic
20	rmc=.5d0*rmc
	rm=.015625d0*.015625d0
30	i2=127
	ir=mod(iabs(n),8190)+1
40	r1=0.d0
	do 50 i=1,ikt
	ir=mod(17*ir,8191)
50	r1=(r1+float(ir/2))*rm
	ir=mod(17*ir,8191)
	r1=(r1+float(ir/id))*rmc
	r3(i2)=r1
	i2=i2-1
	if(i2.gt.0)goto 40
	RANINI=R1
	RETURN
	ENTRY RANDDD(N)
C NEW ENTRY POINT
	if(i2.eq.0)i2=127
	RANDDD=r1+r3(i2)
	if(RANDDD.ge.1.)RANDDD=(r1-.5d0)+(r3(i2)-.5d0)
	r1=RANDDD
	r3(i2)=RANDDD
	i2=i2-1
	return
	end
