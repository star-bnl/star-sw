* $Id: tph_fit_pulse.F,v 1.1 1998/01/27 00:49:16 fisyak Exp $
* $Log: tph_fit_pulse.F,v $
* Revision 1.1  1998/01/27 00:49:16  fisyak
* Split sources
*
	Subroutine tph_fit_pulse(tseq, aseq, daseq,
     +                           itdc_lo, itdc_hi, npts_max,
     +				 t0, tau, dt0)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tseq:	tdc values for the pulse to be fit
C     aseq:	adc values for the pulse to be fit
C     daseq:	adc values for the pulse to be fit
C     itdc_lo:	first index to use for tseq, aseq, and daseq arrays
C     itdc_hi:	last  index to use for tseq, aseq, and daseq arrays
C     npts_max:	size of tseq, aseq, and daseq arrays
C
C   Output arguments : 
C
C     t0:	"time-zero" of the pulse (not neccessarily gauss centroid)
C     tau:	"width" of the pulse (not neccessarily gaussian width)
C     dt0:	uncertainty in "time-zero" of the pulse
C	
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C
C	This routine fits a pad-pulse passed in the tseq, aseq arrays
C	and returns the time and width of the pulse.
C	Right now, we use the experimentally-realistic functional form:
C
C			N    -(t-t0)/tau
C	y(t) = A * (t-t0) * e
C
C	for STAR, N=2, and tau ~ 59 ns.  Units used here are time buckets.
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modifications:
c   ==============
C
C   Error conditions : 
C   ================
C
C-----------------------------------------------------------------------
c Passed variables:
c
        integer itdc_lo, itdc_hi      ! Time bucket extent for cluster
	integer npts_max

	real aseq(npts_max)
	real daseq(npts_max)
	real dt0
	real t0
	real tau
	real tseq(npts_max)
C-----------------------------------------------------------------------
c local variables:
c
	integer N
c
	parameter(N=2)          ! for STAR

        integer id                    ! Message ID
        integer imax                  ! Time bucket corresponding to peak
	integer istart                ! First pad
	integer npts                  ! Number of TDC values for hit

	real B                        ! 
        real delta                    ! Separation of time values
	real dy1, dy2, dy3            ! 
	real t1, t2, t3
        real rat
        real root1B
	real y1, y2, y3

        logical retry                 ! .T. iff first try at solution
C-----------------------------------------------------------------------
c functions:
c
	integer lvmax
C-----------------------------------------------------------------------

        npts = itdc_hi - itdc_lo + 1
	imax = lvmax(aseq(itdc_lo),npts) + itdc_lo - 1
c
c RRB: Prevent possible out-of-bounds array accesses
c      by checking array indices before use:
        if (imax.eq.1) then ! max at early edge
           istart = 1
        else if (aseq(imax-1).lt.1.0e-14) then
           istart = imax
        elseif (imax.eq.npts_max) then !lt edge
           istart = npts_max - 2
        elseif (aseq(imax+1).lt.1.0e-14) then !lt edge
           istart = imax - 2
        else                    ! "normal"
           istart = imax - 1
        endif

        retry=.false.

 1      continue
        t1 = tseq(istart)
	t2 = tseq(istart+1)
	t3 = tseq(istart+2)
        delta = t2 - t1         ! (delta also = t3-t2)
	y1 = aseq(istart)
	y2 = aseq(istart+1)
	y3 = aseq(istart+2)
	dy1 = daseq(istart)
	dy2 = daseq(istart+1)
	dy3 = daseq(istart+2)


        rat = y1*y3/(y2*y2)
        if (rat.eq.0.0) goto 99
c       B = (rat)**(1.0/float(N))	! for arbitrary N
	B = sqrt(rat)           ! this is faster for N=2

        if (B.ge.1.0) then      ! this should be rare
           if (retry) then      ! this is already the 2nd chance...
              goto 99
           else                 ! ok, try it again...
              retry=.true.
              istart=istart+1
              goto 1
           endif
        endif

        root1B = sqrt(1.0-B)
        t0 = t2 - delta / root1B
        tau = delta/(log((y1/y2)/(1.0-root1B)**N))
        dt0 = delta/(2.0*root1B**3) * B/float(N) *
     +       sqrt( (dy1/y1)**2 + 4.0*(dy2/y2)**2 + (dy3/dy3)**2)

        return
 99     continue                ! failure...
        id=0
*        call message('TPH_FIT_PULSE failure; going to mean',
*     +       1,id)
         write(6,*) 'TPH_FIT_PULSE failure; going to mean'
c this should be fixed before using for real-----v
        call tph_weighted_mean(tseq, aseq,
     +       daseq, daseq,
     +       itdc_lo, itdc_hi, npts_max,
     +       t0, tau, dt0)
c     should correct for phase here!!!!!!!! *** mal 27jul95
        return
	end
