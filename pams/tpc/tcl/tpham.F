C----------------------------------------------------------------------
C
C                               *
C                              ***
C                             ** **
C                            **   **
C                    *********     *********
C                      ****           ****
C                        ***         ***
C                        **     *     **
C                       **    *****    **
C                      **   **     **   **
C                     **                 **
C
C----------------------------------------------------------------------
C
      INTEGER FUNCTION TPHAM( tsspar_h, tsspar
     +,                       detector_h, detector
     +,                       sector_h, sector
     +,                       pad_plane_h, pad_plane
     +,                       wire_plane_h, wire_plane
     +,                       tppixel_h, tppixel
     +,                       tpmcpix_h, tpmcpix
     +,                       tpseqh, tpseq 
     +,                       tpclusterh, tpcluster 
     +,                       tphith, tphit )

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tsspar_h:
C     tsspar:
C     detector_h:
C     detector:
C     sector_h:
C     setctor:
C     pad_plane_h:
C     pad_plane:
C     wire_plane_h:
C     wire_plane:
C     tpppixel_h
C     tppixel:
C     tpmcpix_h
C     tpmcpix:
C     tpseqh
C     tpseq:
C     tpclusterh
C     tpcluster:
C
C   Output arguments : 
C
C     tphith
C     tphit:
C
C  Returns:
C       TAS user status code, defined in tas_user_codes_inc, one of:
C       tucod_ok       !General successful completion.
C       tucod_fail     !General failure.
C       tucod_abevt    !Abort event.
C       tucod_abgo     !Abort GO event looping.
C       tucod_exit     !Force program termination.
C       tucod_unkmode  !Unknown mode in call to TAE_USER.
C     
C   Functional Description : 
C    This module reconstructs hits given cluster data in tpseq/tpcluster
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   25-28 Nov 1994 - MA Lisa:
C     -	Clusters with .ge. min_hit_seq (=2) sequences are kept as hits,
C	instead of hard-wiring a .ge. 3 requirement
C     -	Additional user array, npix_pad, filled by TPH_GET_CLS_DATA and
C	passed to TPH_FIT_CLUSTER
C     -	Restructured algorithm of extracting spatial information,
C	simplifying it, and using more realistic parametrization of
C	the pulse in time.  Now, for each cluster, this routine just
C	calls two routines, TPH_GET_CLS_DATA and TPH_FIT_CLUSTER.
C	The first fills user arrays ipad,itdc,iadc,npix_pad with information
C	about sequences in the cluster, and the second extracts spatial
C	information from these arrays.  The old (obsolete) routines
C	originally used are stored at the end of this file (search for ****).
C     -	New variable added to TPHIT table and filled below, TPHIT.PHI,
C	which indicates the "orientation" of the hit with respect to
C	the padplane.  (it is atan(delta_time/delta_pad)).  It is hoped
C	that this variable, along with prf and zrf, will give information
C	about the crossing and dip angles, alpha and lambda.
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tss_tsspar_pars.inc"
#include "tss_tsspar_st.inc"

#include "tpg_detector_pars.inc"
#include "tpg_detector_st.inc"
#include "tpg_sector_pars.inc"
#include "tpg_sector_st.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"
#include "tpg_wire_plane_pars.inc"
#include "tpg_wire_plane_st.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"
#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"
#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"
#include "tcl_tphit_pars.inc"
#include "tcl_tphit_st.inc"

C-----------------------------------------------------------------------
      RECORD/ table_head_st/ tsspar_h
      RECORD/ tsspar_row_st/ tsspar(*)

      RECORD/ table_head_st/ detector_h
      RECORD/ detector_row_st/ detector(*)
      RECORD/ table_head_st/ sector_h
      RECORD/ sector_row_st/ sector(*)
      RECORD/ table_head_st/ pad_plane_h
      RECORD/ pad_plane_row_st/ pad_plane(*)
      RECORD/ table_head_st/ wire_plane_h
      RECORD/ wire_plane_row_st/ wire_plane(*)

      RECORD/ table_head_st/ tppixel_h
      RECORD/ tppixel_row_st/ tppixel(*)
      RECORD/ table_head_st/ tpmcpix_h
      RECORD/ tpmcpix_row_st/ tpmcpix(*)

      RECORD/ table_head_st/ tpseqh
      RECORD/ tpseq_row_st/ tpseq(*)
      RECORD/ table_head_st/ tpclusterh
      RECORD/ tpcluster_row_st/ tpcluster(*)
      RECORD/ table_head_st/ tphith
      RECORD/ tphit_row_st/ tphit(*)

C-----------------------------------------------------------------------
c
c        functions

         logical tpg_global_to_local
     +,          tpg_global_to_local_p
     +,          tpg_pad_of_x
     +,          tpg_local_to_global

         double precision tpg_row_to_y
     +,                   tpg_pad_to_x

         real tph_calc_prf_sigma
     +,       tph_calc_prf_pos
c
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
        integer nclpix_max
        parameter (nclpix_max = 1000)
        real ipad(nclpix_max),itdc(nclpix_max),iadc(nclpix_max)
	integer npix_pad(50)

        integer isec,irow,itrk,krow

        integer jtpcl, nclspix

        integer mxrow
        parameter (mxrow = 45)
        integer id_pad(mxrow)

        integer ntim, nseq, jseq

        real pad_width, time_bucket, time_bin, tval
        real shaper_bin
        real rstat

	integer min_hit_seq/2/	! this could go into switch table

        double precision invec(3),outvec(3)

        logical first, lstat
C-----------------------------------------------------------------------
        data first/.true./
C-----------------------------------------------------------------------
c
c       set status
c
        tpham = tucod_ok

         if(first)then
c
c           make id_pad array...
c
            first = .false.
            id_pad(1) = 0
            do irow = 2, mxrow
               id_pad(irow) = id_pad(irow-1)
     +                        + pad_plane(1).npads(irow-1)
            enddo
c
c->         constants...
c
            time_bucket = detector(1).drift_length/tsspar(1).ntime
            time_bin = time_bucket/tsspar(1).v_drift

            shaper_bin = tsspar(1).tau1/time_bin
        endif

        do jtpcl = 1, tpclusterh.nok

           nseq = tpcluster(jtpcl).nseq
           jseq = tpcluster(jtpcl).jseq
           isec = tpcluster(jtpcl).secpad/100
           irow = tpcluster(jtpcl).secpad-isec*100
           if(mod(isec,2).eq.0)then
              krow = irow + pad_plane(1).nrow_in
           else
              krow = irow
           endif

           if(nseq.ge.min_hit_seq)then

              call tph_get_cls_data(jseq, nseq, id_pad, krow
     +,                             tpseqh, tpseq 
     +,                             tppixel_h, tppixel
     +,                             tpmcpix_h, tpmcpix
     +,                             nclspix, ipad, itdc, iadc, itrk
     +,				    npix_pad)
 
              tphith.nok = tphith.nok + 1
              tphit(tphith.nok).cluster = jtpcl
              tphit(tphith.nok).row = krow
              tphit(tphith.nok).track = itrk   ! for now this is MC info

	      call tph_fit_cluster(nseq, nclspix, npix_pad,
     +				   ipad, itdc, iadc, tphith, tphit)

           else
c
c             do not store clusters with < min_hit_seq  sequences in them
c
              goto 999
           endif
c
c          finally, transform everything to global coordinates !
c
c          account for "lower-edge" binning in z coordinate
c
           invec(3)=dble(tphit(tphith.nok).z*time_bucket)
           invec(2)=tpg_row_to_y(irow,isec)
           invec(1)=tpg_pad_to_x(tphit(tphith.nok).x,irow,isec)

           lstat=tpg_local_to_global(isec,invec,outvec)

           tphit(tphith.nok).x = real(outvec(1))
           tphit(tphith.nok).y = real(outvec(2))
           tphit(tphith.nok).z = real(outvec(3))
c
c          zrf in units of time bins --> cm
c
           tphit(tphith.nok).zrf 
     +                         = (tphit(tphith.nok).zrf)*time_bucket
c
c          prf in units of pads --> cm
c
c
c          perhaps consistency with simulation demands use of pad_sep_in/out ???
c	   yes, I think so - mal
c
           if(mod(isec,2).eq.0)then
              pad_width = pad_plane(1).pad_sep_out
           else
              pad_width = pad_plane(1).pad_sep_in
           endif
           tphit(tphith.nok).prf 
     +                         = (tphit(tphith.nok).prf) * pad_width

c	also, calculate "orientation" of hit in terms of spatial quantities

	if (tphit(tphith.nok).phi.gt.-990.0)
     +	   tphit(tphith.nok).phi = atand(
     +				  	tphit(tphith.nok).phi *
     +					time_bucket / pad_width )

c
c       finally, should calculate dx, dy, and dz ...
c       note that sigma_prf IS NOT sigma_x, etc...

 999       continue
        enddo

c
        return
        end


C-----------------------------------------------------------------------
        Subroutine tph_get_cls_data(jseq, nseq, id_pad, irow
     +,                            tpseqh, tpseq 
     +,                            tppixel_h, tppixel
     +,                            tpmcpix_h, tpmcpix
     +,                            nclspix, ipad, itdc, iadc, itrk
     +,				   npix_pad)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     jseq:
C     nseq:
C     id_pad:
C     irow:
C     tpseqh:
C     tpseq:
C     tpppixel_h
C     tppixel:
C     tpmcpix_h
C     tpmcpix:
C
C   Output arguments : 
C
C     nclspix:
C     ipad:
C     itdc:
C     iadc:
C     itrk:
C     npix_pad:	(added 23 Nov 94) number of pixels in segment on each pad
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C    This subroutine unpacks the data associated with a given cluster
C    into the caller arrays.
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C   23nov94 - m lisa - added output argument of npix_pad=number of pixels
C			in each sequence
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"
#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

C-----------------------------------------------------------------------

      RECORD/ table_head_st/ tppixel_h
      RECORD/ tppixel_row_st/ tppixel(*)
      RECORD/ table_head_st/ tpmcpix_h
      RECORD/ tpmcpix_row_st/ tpmcpix(*)

      RECORD/ table_head_st/ tpseqh
      RECORD/ tpseq_row_st/ tpseq(*)

C-----------------------------------------------------------------------
        integer lvmax,iucomp

        real iadc(*),itdc(*),ipad(*)
        integer id_pad(*), itrk
        integer jseq,nseq,nclspix,irow
	integer npix_pad(*)
C-----------------------------------------------------------------------
        integer ldatum
        integer*2 idatum(2),tdc_lo,tdc_hi
        equivalence(idatum(1),ldatum)
        integer*2 seqmask,botmask

        integer maxtrk, ntrk, ktrk, imax
        parameter (maxtrk=100)
        integer ltrk(maxtrk)
        real nocc(maxtrk)

        integer kseq,kpix,kseqpix
        integer jpix,iclspix,indx
        integer mtrk,jndx

        integer id
	integer iseq_num
C-----------------------------------------------------------------------
        data id/0/
        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        iclspix = 0
        ntrk = 0
        call vzero(ltrk,maxtrk)
        call vzero(nocc,maxtrk)

	iseq_num = 0
        do kseq = jseq, jseq+nseq-1
           jpix = tpseq(kseq).jpix
           iclspix = iclspix + 1

           ldatum = tppixel(jpix).datum
c
c          for MC data!
c
           ktrk = tpmcpix(jpix).mcid
           if(ntrk.gt.0 .and. ntrk.lt.maxtrk)then
              mtrk = iucomp(ktrk,ltrk,ntrk)
              if(mtrk.eq.0 .and. ntrk.lt.maxtrk)then
                 ntrk = ntrk + 1
                 ltrk(ntrk) = ktrk
                 nocc(ntrk) = 1.
              elseif(mtrk.gt.0 .and. mtrk.le.maxtrk)then
                 nocc(mtrk) = nocc(mtrk) + 1.
              else
                 id = 0
                 call message('TPHAM-I-GCD, ntrk.gt.maxtrk',1,id)
                 return
              endif
           elseif(ntrk.eq.0)then
              ntrk = ntrk + 1
              ltrk(ntrk) = ktrk
              nocc(ntrk) = 1.
           else
              id = 0
              call message('TPHAM-I-GCD, ntrk.gt.maxtrk',1,id)
              return
           endif

           iadc(iclspix) = real(idatum(1))

           tdc_lo = iand(idatum(2),botmask)
           tdc_hi = ibits(idatum(2),14,2)*'100'X
           itdc(iclspix) = real(tdc_lo + tdc_hi) 

           ipad(iclspix) = real(tpseq(kseq).secpad-id_pad(irow))

           kseqpix = iand(idatum(2),seqmask)/'100'X
	   iseq_num = iseq_num+1
	   npix_pad(iseq_num) = kseqpix

           do indx = 2, kseqpix
              iclspix = iclspix + 1
              ldatum = tppixel(jpix+indx-1).datum
              iadc(iclspix) = real(idatum(1))
              itdc(iclspix) = real(itdc(iclspix-1)+1.)
              ipad(iclspix) = real(ipad(iclspix-1))
c
c             for MC data!
c
              ktrk = tpmcpix(jpix).mcid
              mtrk = iucomp(ktrk,ltrk,ntrk)
              if(mtrk.eq.0 .and. ntrk.lt.maxtrk)then
                 ntrk = ntrk + 1
                 ltrk(ntrk) = ktrk
                 nocc(ntrk) = 1.
              elseif(mtrk.gt.0 .and. mtrk.le.maxtrk)then
                 nocc(mtrk) = nocc(mtrk) + 1.
              else
                 id = 0
                 call message('TPHAM-I-GCD, ntrk.gt.maxtrk',1,id)
                 return
              endif

           enddo
        enddo

        nclspix = iclspix
        imax = lvmax(nocc,ntrk)
        itrk = ltrk(imax)

        return
        end


C-----------------------------------------------------------------------
	Subroutine tph_fit_cluster(nseq, nclspix, npix_pad,
     +				ipad, itdc, iadc, 
     +				tphith, tphit)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     nseq:	number of sequences in cluster
C     nclspix:	number of pixels in cluster
C     npix_pad: number of pixels in each sequence in cluster
C     ipad:	for each pixel, what is pad #
C     itdc:	for each pixel, what is bucket #
C     iadc:	for each pixel, what is adc value
C
C   Output arguments : 
C
C     tphith:	
C     tphit:	tpc hit table.. this fills (in local coordinates):
C			x     - position in pad direction
C			z     - position in time direction
C			dx,dz - uncertainties
C			q     - sum of occupied pixels
C			dq    - ??? not filled - how to do it???
C			prf   - width of gaussian in pad direction
C			zrf   - width in time direction
C			phi   - orientation of the hit w.r.t. padplane
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C	This subroutine replaces what had been a messy set of routine
C	calls in the main code.  It should help in fitting in a more
C	truly 2-dimentional way.
C	The pad direction is fit with a Gaussian parametrization.
C	The time direction is fit with a form reflecting real signals
C	measured in the lab.
C
C   Originally written 23 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tphit_pars.inc"
#include "tcl_tphit_st.inc"

	RECORD / table_head_st/ tphith
	RECORD / tphit_row_st/  tphit(*)

C-----------------------------------------------------------------------
	integer nseq,nclspix
	real ipad(*),itdc(*),iadc(*)
	integer npix_pad(*)

	real centroid,sigma,uncertainty,sum
C-----------------------------------------------------------------------
	integer lvmax
	integer iseq,iptr_lo,iptr_hi
	integer itim,ipix
	real padsum(50),padnum(50),padtime(50),padwidth(50)
	real tseq(100),aseq(100)	! for passing to pulse-fitting routine
	real t0,tau
	real hitsum
	real mom1,mom2
	integer ngood_times
	real adc_sum
	integer id,imax
	real dylin(50),slope,intercept
	real dslope,dintercept
	real padtime_arr(50),padwidth_arr(50)
	real padsum_arr(50),padnum_arr(50)
C-----------------------------------------------------------------------

c loop over sequences to (1) make projection in pad direction 	(padnum,padsum)
c			 (2) sum all adc counts in hit 		 (hitsum)
c			 (3) determine and store t0 for each pad (padtime)
c
c (here it is assumed that each sequence is on a different pad)

	hitsum = 0.0
	ngood_times = 0
	iptr_lo = 1
	do iseq = 1,nseq
	  padsum(iseq) = 0.0
	  padnum(iseq) = ipad(iptr_lo)
	  iptr_hi = iptr_lo + npix_pad(iseq) - 1
	  itim = 0
	  do ipix=iptr_lo,iptr_hi
	    itim = itim+1
	    padsum(iseq) = padsum(iseq) + iadc(ipix)
	    tseq(itim) = itdc(ipix)
	    aseq(itim) = iadc(ipix)
	  enddo
	  iptr_lo = iptr_hi + 1
	  hitsum = hitsum + padsum(iseq)
	  if (npix_pad(iseq).ge.3) then		! little time info if <3 bkts
	    call tph_fit_pulse(tseq, aseq, itim, t0, tau)
	    padtime(iseq) = t0
	    padwidth(iseq) = tau
	    if (tau.gt.0.0) then
	      ngood_times = ngood_times + 1
	      padtime_arr(ngood_times)  = padtime(iseq)
	      padwidth_arr(ngood_times) = padwidth(iseq)
	      padsum_arr(ngood_times)   = padsum(iseq)
	      padnum_arr(ngood_times)   = padnum(iseq)
	      dylin(ngood_times)	= 1.0/sqrt(padsum(iseq)) ! ad hoc
	    endif
	  else
	    padwidth(iseq) = -1.0
	  endif
	enddo

	tphit(tphith.nok).q = hitsum

c calculate hit position along pad row: fit to gaussian.  If that does not
c   work, do a weighted mean
c
	sigma = -1.0
	if (nseq.ge.3) call tph_3point_gauss(padnum, padsum, nseq,
     +					     centroid, sigma)
	if (sigma.lt.0.0) then
	  call tph_weighted_mean(padnum, padsum, nseq,
     +				 centroid, sigma)
	  sigma = -sigma		! use sign to flag different algorithm
	endif

	tphit(tphith.nok).x = centroid
	tphit(tphith.nok).prf = sigma

c Calculate hit position along time direction from sequences with well-defined
c  t0's

	if (ngood_times.ge.1) then
	  call tph_weighted_mean(padtime_arr, padsum_arr, ngood_times,
     +				 t0, mom2)
	  call tph_weighted_mean(padwidth_arr, padsum_arr, ngood_times,
     +				 tau, mom2)
	else
	  id = 0
	  call message('TPH_FIT_CLUSTER no good time peaks - taking max',
     +			1, id)
	  imax = lvmax(iadc,nclspix)
	  t0 = itdc(imax)
	  tau = 0.0
	endif

	tphit(tphith.nok).z   = t0
	tphit(tphith.nok).zrf = tau

c now linear fit to padtime vs padnum:
	if (ngood_times.gt.1) then	
 	  call tph_lin_fit(padnum_arr, padtime_arr, dylin, ngood_times,
     +			   slope, dslope, intercept, dintercept)
	else
	  slope = -1000.0
	endif

	tphit(tphith.nok).phi = slope

	return
	end

C-----------------------------------------------------------------------
	Subroutine tph_fit_pulse(tseq, aseq, itim,
     +				 t0, tau)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tseq:	tdc values for the pulse to be fit
C     aseq:	adc values for the pulse to be fit
C     itim:	number of points in the pulse
C
C   Output arguments : 
C
C     t0:	"time-zero" of the pulse (not neccessarily gauss centroid)
C     tau:	"width" of the pulse (not neccessarily gaussian width)
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
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
	real tseq(*),aseq(*)
	integer itim
	real t0,tau
C-----------------------------------------------------------------------
	integer lvmax
	integer N
	parameter(N=2)
	real X
	real y1,y2,y3,t1,t2,t3
	real a,b,c		! coefficients in quadratic equation
	integer imax
	integer id,istart,jb
	real radicand
	real sum
C-----------------------------------------------------------------------

	imax = lvmax(aseq,itim)
	if ((imax.ge.2.).and.(imax.lt.itim)) then
	  istart=imax-1
	elseif (imax.eq.1) then
c	  id = 0
c	  call message('TPH_FIT_PULSE peak at first bucket',
c     +			1, id)
	  istart=1
	else
c	  id = 0
c	  call message('TPH_FIT_PULSE peak at last bucket',
c     +			1, id)
	  istart=itim-2
	endif
	t1 = tseq(istart)
	t2 = tseq(istart+1)
	t3 = tseq(istart+2)
	y1 = aseq(istart)
	y2 = aseq(istart+1)
	y3 = aseq(istart+2)

c	X = (y1*y3/(y2*y2))**(1.0/float(N))	! for arbitrary N
	X = sqrt(y1*y3/(y2*y2))			! this is faster for N=2

	a = X-1.0
	if (a.eq.0.0) then			! this shoud be very rare
c	  This happens if y1=y2=y3.  If so, try one more time by
c	    shifting istart later in time (if possible)
c
	  if (itim.eq.3) then		   ! no room to shift
	    id = 0
	    call message('TPH_FIT_PULSE a=0; going to mean',
     +			1,id)
	    goto 99
	  elseif (istart.ne.itim-2) then   ! shift sample later in time
	    istart=istart+1
	  else				   ! if we must, shift sample earlier
	    istart=istart-1
	  endif
	  t1 = tseq(istart)
	  t2 = tseq(istart+1)
	  t3 = tseq(istart+2)
	  y1 = aseq(istart)
	  y2 = aseq(istart+1)
	  y3 = aseq(istart+2)
c	  X = (y1*y3/(y2*y2))**(1.0/float(N))	! for arbitrary N
	  X = sqrt(y1*y3/(y2*y2))		! this is faster for N=2
	  a = X-1.0
	  if (a.eq.0.0) then	! if FOUR points are the same, give up!
	    id = 0
	    call message('TPH_FIT_PULSE a=0; going to mean',
     +			1,id)
	    goto 99
	  endif
	endif
	b = 2.0*t2*(1-X)
	c = X*t2*t2 - t1*t3

	radicand = b*b - 4.0*a*c
	if (radicand.gt.0.0) then
c 	  2 solutions exist (normal) we want the *earlier* one
	  if (a.lt.0) then
	    t0 = (-b + sqrt(radicand) ) / (2.0*a)
	  else
	    t0 = (-b - sqrt(radicand) ) / (2.0*a)
	  endif
	elseif (radicand.eq.0) then
c 	  1 solution (shouldn't happen)
	  id = 0
	  call message('TPH_FIT_PULSE one solution?',
     +			1,id)
	  t0 = -b/(2.0*a)
	else
c 	  no real solutions... error
	  id = 0
	  call message('TPH_FIT_PULSE no solutions! going to mean',
     +			1,id)
	  goto 99
	endif

c calculate "width" tau
	tau = 1.0/(log(y2/y3)-float(N)*log((t2-t0)/(t3-t0)))

	return

 99	continue	! what to do if normal fit fails

	t0 = 0.0
	sum = 0.0
	do jb=1,itim
	  t0 = t0 + aseq(jb)*tseq(jb)
	  sum = sum + aseq(jb)
	enddo
	t0 = t0/sum
	tau = -1.0

	return
	end


C-----------------------------------------------------------------------
	Subroutine tph_3point_gauss(x, y, npts,
     +				centroid, sigma)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     y		- y(x) distribution to fit to gaussian
C     x		- x coordinate for each point
C     npts	- number of samples in distribution
C
C   Output arguments : 
C
C     centroid 	- Gaussian centroid from 3-point fit to y(x)
C     sigma	- Gaussian width from 3-point fit to y(x)
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C
C	Performs 3-point gaussian fit to passed distribution y(x)
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C
C	If the distribution is not well described by a gaussian, (i.e.
C	it has a peak on one edge), a negative sigma is returned and no
C	centroid is calculated
C
C-----------------------------------------------------------------------
	real y(*),x(*)
	integer npts
	real centroid,sigma
C-----------------------------------------------------------------------
	integer lvmax
	integer ipt,imax,istart,id
	real y1,y2,y3,delta,r213

	imax = lvmax(y,npts)

	if ((imax.ge.2).and.(imax.lt.npts)) then
	  istart=imax-1
	elseif (imax.eq.1) then
	  id = 0
	  call message('TPH_3POINT_GAUSS peak at first point',
     +			1,id)
c	  istart=1
	  sigma = -1.0
	else
	  id = 0
	  call message('TPH_3POINT_GAUSS peak at last point',
     +			1,id)
c	  istart=npts-2
	  sigma = -1.0
	endif

	y1 = y(istart)
	y2 = y(istart+1)
	y3 = y(istart+2)

	delta = x(istart+1) - x(istart)
	if (delta.ne.1.0) then
	  id = 0
	  call message('TPH_3POINT_GAUSS unexpected delta .ne. 1',
     +			1,id)
	endif

	r213 = log(y2*y2/(y1*y3))
	centroid = x(istart+1) + (delta/2.0) * log(y3/y1)/r213
	sigma = delta/sqrt(r213)

	return
	end



C-----------------------------------------------------------------------
	Subroutine tph_lin_fit(x, y, dy, npts,
     +			       slope, dslope,
     +			       intercept, dintercept)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     x		- x coordinate of each point
C     y		- y(x) distribution to fit to line
C     dy	- uncertainty in y
C     npts	- number of samples in distribution
C
C   Output arguments : 
C
C     slope	- slope of best fit line
C     dslope	- uncertainty in slope
C     intercept - y-intercept of best fit line
C     dintercept- uncertainty in intercept
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C
C	returns slope and intercept of best fit line to (x,y,dy) data
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C
C-----------------------------------------------------------------------
	real x(*),y(*),dy(*)
	integer npts
	real slope,intercept,dslope,dintercept
C-----------------------------------------------------------------------
	real sxx,sxy,sx,sy,s,denom
	integer ipt
C-----------------------------------------------------------------------

        sxx = 0.0
        sxy = 0.0
        sx = 0.0
        sy = 0.0
        s = 0.0
        do ipt=1,npts
           sxx = sxx + x(ipt)*x(ipt)/dy(ipt)**2
           sxy = sxy + x(ipt)*y(ipt)/dy(ipt)**2
           sx  = sx  + x(ipt)/dy(ipt)**2
           sy  = sy  + y(ipt)/dy(ipt)**2
           s   = s   + 1/dy(ipt)**2
        enddo
        denom = s*sxx-sx*sx
        slope = (s*sxy - sx*sy)/denom
        intercept = (sxx*sy - sx*sxy)/denom
        dslope = sqrt(s/denom)
        dintercept = sqrt(sxx/denom)

	return
	end


C-----------------------------------------------------------------------
	Subroutine tph_weighted_mean(x, y, npts,
     +				     mom1, mom2)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     x		- x coordinate of each point
C     y		- y(x) distribution
C     npts	- number of samples in distribution
C
C   Output arguments : 
C
C     mom1	- first moment of y(x) distribution (mean x)
C     mom2	- second moment of y(x) distribution (rms)
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C
C	takes first and second moment of input distribution
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C
C-----------------------------------------------------------------------
	real x(*),y(*)
	integer npts
	real mom1,mom2
C-----------------------------------------------------------------------
	real ysum
	integer ipt
C-----------------------------------------------------------------------

	mom1 = 0.0
	mom2 = 0.0
	ysum = 0.0
	do ipt=1,npts
	  ysum = ysum + y(ipt)
	  mom1 = mom1 + x(ipt)*y(ipt)
	  mom2 = mom2 + x(ipt)*x(ipt)*y(ipt)
	enddo
	mom1 = mom1 / ysum
	mom2 = mom2 / ysum
	mom2 = sqrt(mom2 - mom1*mom1)
	return
	end


c ****** what follows here are old routines no longer in use ******
c********************************************************************
C-----------------------------------------------------------------------
        Subroutine tph_calc_cntr_of_grvty(nclspix, ipad, itdc, iadc
     +,                            tphith, tphit )
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     nclspix:
C     ipad:
C     itdc:
C     iadc:
C
C   Output arguments : 
C
C     tphith:
C     tphit:
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C    This subroutine calculates the center of gravity of the data 
C    presented in the caller arrays.
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tphit_pars.inc"
#include "tcl_tphit_st.inc"

C-----------------------------------------------------------------------

      RECORD/ table_head_st/ tphith
      RECORD/ tphit_row_st/ tphit(*)

C-----------------------------------------------------------------------
        real vmax

        real iadc(*),itdc(*),ipad(*)
        integer nclspix
C-----------------------------------------------------------------------
        integer ipix
        real    sum,tdc,pad,tdc2,pad2
        real    max_adc
C-----------------------------------------------------------------------
        sum = 0.
        tdc = 0.
        pad = 0.
        tdc2 = 0.
        pad2 = 0.

        do ipix = 1, nclspix
           sum = sum + iadc(ipix)
           pad = pad + ipad(ipix)*iadc(ipix)
           tdc = tdc + itdc(ipix)*iadc(ipix)
        enddo
        pad = pad/sum
        tdc = tdc/sum

        tphit(tphith.nok).q = sum
c       max_adc = vmax(iadc,nclspix)
c       tphit(tphith.nok).q = max_adc

        tphit(tphith.nok).x = pad
        tphit(tphith.nok).z = tdc

        do ipix = 1, nclspix
           pad2 = pad2 + iadc(ipix)*(ipad(ipix)-pad)**2
           tdc2 = tdc2 + iadc(ipix)*(itdc(ipix)-tdc)**2
        enddo

        tphit(tphith.nok).prf = pad2/sum
        tphit(tphith.nok).zrf = tdc2/sum

        return
        end

C-----------------------------------------------------------------------
        Subroutine tph_proj_pad(jseq, nseq, id_pad, irow
     +,                            tpseqh, tpseq 
     +,                            tppixel_h, tppixel
     +,                            ipad, iadc)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     jseq:
C     nseq:
C     id_pad:
C     irow:
C     tpseqh:
C     tpseq:
C     tppixel_h:
C     tppixel:
C
C   Output arguments : 
C
C     ipad:
C     iadc:
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C    This subroutine projects pixel data presented in the 
C    caller arrays onto the pad dimension.
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

C-----------------------------------------------------------------------

      RECORD/ table_head_st/ tppixel_h
      RECORD/ tppixel_row_st/ tppixel(*)

      RECORD/ table_head_st/ tpseqh
      RECORD/ tpseq_row_st/ tpseq(*)

C-----------------------------------------------------------------------
        real iadc(*),ipad(*)
        integer id_pad(*)
        integer jseq,nseq,irow,nclspix
C-----------------------------------------------------------------------
        integer ldatum
        integer*2 idatum(2),tdc_lo,tdc_hi
        equivalence(idatum(1),ldatum)
        integer*2 seqmask,botmask

        integer kseq,kpix,kseqpix
        integer jpix,iclspix,indx
        integer iseq

C-----------------------------------------------------------------------
        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        iseq = 0

        do kseq = jseq, jseq+nseq-1
           iseq = iseq + 1
           jpix = tpseq(kseq).jpix

           ldatum = tppixel(jpix).datum
           iadc(iseq) = real(idatum(1))

           ipad(iseq) = real(tpseq(kseq).secpad-id_pad(irow))

           kseqpix = iand(idatum(2),seqmask)/'100'X

           do indx = 2, kseqpix
              ldatum = tppixel(jpix+indx-1).datum
              iadc(iseq) = iadc(iseq) + real(idatum(1))
           enddo

        enddo

        return
        end

C-----------------------------------------------------------------------
        Real Function tph_calc_prf_sigma(nseq, ipad, iadc)
C-----------------------------------------------------------------------
      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     nseq:
C     ipad:
C     iadc:
C
C   Output arguments : 
C
C  Returns:
C       The response width, in units of pixels, of the data 
C       presented in the caller arrays.
C     
C   Functional Description : 
C    This function returns the response width, in units of 
C    pixels, of the data presented in the caller arrays 
C    under the assumption of a Gaussian response function.
C    
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------

      integer lvmax
C-----------------------------------------------------------------------
        real iadc(*),ipad(*)
        integer nseq
C-----------------------------------------------------------------------
        integer imax, id
        real    x,y,pad_width
C-----------------------------------------------------------------------

        pad_width = 0.
        imax = lvmax(iadc,nseq)

        if(imax.ge.2 .and. imax.lt.nseq)then
           x = iadc(imax)**2
           y = iadc(imax-1)*iadc(imax+1)
           pad_width = 1./log(x/y)
        elseif(imax .eq. 1)then
c
c          the peak of the pulse is on the "leftmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRFS imax=1; leftmost'
     +,                 1, id)
        elseif(imax .eq. nseq)then
c
c          the peak of the pulse is on the "rightmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRFS imax=nseq; rightmost'
     +,                 1, id)
        else
           id = 0
           call message('TPHAM-E-PRFS error in 3 pad prf calculation'
     +,                 1, id)
        endif

        tph_calc_prf_sigma = pad_width

        return
        end

C-----------------------------------------------------------------------
        Real Function tph_calc_prf_pos(nseq, ipad, iadc, sigma)
C-----------------------------------------------------------------------
      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     nseq:
C     ipad:
C     iadc:
C     sigma:
C
C   Output arguments : 
C
C
C  Returns:
C       The response centroid, in units of pixels, of the data 
C       presented in the caller arrays.
C     
C   Functional Description : 
C    This function returns the response centroid, in units of 
C    pixels, of the data presented in the caller arrays 
C    under the assumption of a Gaussian response function.
C    
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
      integer lvmax


C-----------------------------------------------------------------------
        real iadc(*),ipad(*)
        integer nseq
        real sigma
C-----------------------------------------------------------------------
        integer imax
        real    p1,p2,p3
        real    c1,c2,c3
        real    pad
        integer id
C-----------------------------------------------------------------------

        tph_calc_prf_pos = 0.
        imax = lvmax(iadc,nseq)

        if(imax.ge.2 .and. imax.lt.nseq)then
           c1 = iadc(imax-1)
           c2 = iadc(imax)
c          c3 = iadc(imax+1)

           p1 = ipad(imax-1)
           p2 = ipad(imax)
c          p3 = ipad(imax+1)

           tph_calc_prf_pos = (p1+p2)/2.-sigma*log(c1/c2)
        elseif(imax .eq. 1)then
c
c          the peak of the pulse is on the "leftmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRFP imax=1; leftmost'
     +,                 1, id)
        elseif(imax .eq. nseq)then
c
c          the peak of the pulse is on the "rightmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRFP imax=nseq; rightmost'
     +,                 1, id)
        else
           id = 0
           call message('TPHAM-E-PRFP error in 3 pad prf calculation'
     +,                 1, id)
           return
        endif

        return
        end

C-----------------------------------------------------------------------
        Subroutine tph_proj_tim(nclspix, ipad, itdc, iadc, ntim)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     nclspix:
C     ipad:
C     itdc:
C
C   Output arguments : 
C
C     itdc:
C     ntim:
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C    This subroutine projects pixel data presented in the 
C    caller arrays onto the time dimension.
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
        real vmin,vmax

        real iadc(*),itdc(*),ipad(*)
        integer nclspix, ntim
C-----------------------------------------------------------------------

        real rndx, min_time, max_time
        integer nbuck
        parameter (nbuck = 100)
        real ladc(nbuck),ltdc(nbuck)

        integer ldatum
        integer*2 idatum(2)
        equivalence(idatum(1),ldatum)
        integer*2 seqmask,botmask

        integer indx
C-----------------------------------------------------------------------
        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        min_time = vmin(itdc,nclspix)
        max_time = vmax(itdc,nclspix)

        ntim = int(max_time-min_time)

        if((max_time-min_time).gt.4.)then
           return
        endif
c
c       if need be, perform time axis projection...
c
        do rndx = min_time, max_time
           ltdc(int(rndx-min_time+1.)) = rndx
           ladc(int(rndx-min_time+1.)) = 0.
        enddo

        do indx = 1, nclspix
           ladc(int(itdc(indx)-min_time+1.))
     +      = ladc(int(itdc(indx)-min_time+1.))+iadc(indx)
        enddo
c
c       now put the projected spectrum into "standard" arrays
c
        do rndx = min_time, max_time
           iadc(int(rndx-min_time+1.))=ladc(int(rndx-min_time+1.))
           itdc(int(rndx-min_time+1.))=ltdc(int(rndx-min_time+1.))
        enddo

        return
        end

