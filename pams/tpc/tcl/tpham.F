*23456789*123456789*123456789*123456789*123456789*123456789*123456789*12
*        1         2         3         4         5         6         7
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
C----------------------------------------------------------------------
C
      INTEGER FUNCTION TPHAM( tclpar_h,        tclpar
     +,			      tsspar_h,        tsspar
     +,                       detector_h,      detector
     +,                       pad_plane_h,     pad_plane
     +,                       tppixel_h,       tppixel
     +,                       tpmcpix_h,       tpmcpix
     +,                       tpseq_h,         tpseq 
     +,                       tpcluster_h,     tpcluster 
     +,                       tphit_h,         tphit 
     +,                       tphit_aux_h,     tphit_aux)

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tsspar_h:
C     tsspar:
C     detector_h:
C     detector:
C     pad_plane_h:
C     pad_plane:
C     tpppixel_h
C     tppixel:
C     tpmcpix_h
C     tpmcpix:
C     tpseq_h
C     tpseq:
C     tpcluster_h
C     tpcluster:
C
C   Output arguments : 
C
C     tphit_h
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
C   This module reconstructs hits given cluster data in tpseq/tpcluster
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
C	The first fills user arrays ipad,itdc,iadc,npix_pad with
C	information
C	about sequences in the cluster, and the second extracts spatial
C	information from these arrays.  The old (obsolete) routines
C	originally used are stored at the end of this file (search for
C	 ****).
C     -	New variable added to TPHIT table and filled below, TPHIT.PHI,
C	which indicates the "orientation" of the hit with respect to
C	the padplane.  (it is atan(delta_time/delta_pad)).  It is hoped
C	that this variable, along with prf and zrf, will give
C	information
C	about the crossing and dip angles, alpha and lambda.
C
C    13 Dec 1994 - MA Lisa
C     -	Changed the way x is calculated from the sector/pad #'s.
C	Now I do the job of the tpg routine tpg_pad_to_x here, since
C	now that routine demands integer pad#
C
C    17,19 Jan 1995 - malisa
C     -	Now fill id and row entries in a way consistent with fast
C	simulator
C	(row = 100*((SECTOR+1)/2)+ROW)
C
C    13 Mar 1995
C     -	Fill dx and dy according to reasonable prescription.
C
C    30 Mar 1995
C     - Also fill new entry in tphit table, nseq=nseq of contributing
C	cluster
C     -	now using consistent convention of sector=1..24, row=1..45 for
C	all
C       tables from tpseq,tpcluster on
C
C    21 Apr 1995
C     -	uncertainty in position along pad row for hits fit w/ 3-point
C	gaussian
C	now caculated in a non-ad-hoc fashion, depends on signal:noise
C
C    June 1995
C     - npix_pad is now called npix_seq, since it just
C       indicates where in the ipad,iadc,itdc arrays the pad number
C	(ipad)
C       changes.  This just makes things faster later on.
C     - itrk is now an *array* passed back from tph_get_cls_data,
C	telling
C       which MC track contributed most to a pixel-- that way, the
C	track
C       associated with a given *hit* can be assessed after
C	deconvolution
C     - also, no reason to keep iadc,itdc,ipad arrays as reals... put
C	them
C       integer -- that should also speed things up
C     - tph_fit_cluster now first figures out how many hits are in
C       the cluster, then parses the fitting job out to
C	fit_isolated_cluster
C       (now existing) if isolated or deconvolute_cluster (will exist)
C     - the field tphit.lambda is now filled, using a vertex ansatz
C       (lambda=atan(z/radius_of_row)
c
c     June-Dec 1995 - lots of stuff
c
c     Dec 1995
c      - now transform tphit.q into units of energy loss
c      - Also, when taking mean for z-position, the offset of the
c        mean has been shown to be independent of lambda and diffusion,
c        and the value is related to the shaper response function.  The
c        value is no longer hard-wired in fit_isolated_cluster, but
c        is subtracted at the top level
c
c     7mar96
c      - update to work with gstar and new geometry routines.
c      - old version, which works fine with old geant and geometry routines
c        is stored as tpham.F.pre_gstar
c
c     4apr96
c      - we finally get rid of the convention in which the pad is numbered
c	 *in the sector*.  Now it is the pad number in the row...
c
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tpham.inc"
C-----------------------------------------------------------------------
c
c        functions
      integer tgc_row_to_y,tgc_pad_to_x,tgc_local_to_global

c
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
        integer nclpix_max
        parameter (nclpix_max = 10000)
c
        integer ipad(nclpix_max),itdc(nclpix_max),iadc(nclpix_max)
        integer itrk(nclpix_max)
	integer npix_seq(300)
        integer ipad_lo, ipad_hi, itdc_lo, itdc_hi
        integer isec,irow,nhits,ihit
        integer jtpcl, nclspix
        integer nseq, jseq

        real pad_width, time_bucket, time_bin
	real rad,dx_prime
        real white_noise,filtered_noise
        real de_scale
        real invec(3),outvec(3)

        logical first

c------------ for deconvolution -----------
        integer max_hits_in_cluster
        parameter(max_hits_in_cluster=100)
        real qq(max_hits_in_cluster)
        real x(max_hits_in_cluster)
        real dx(max_hits_in_cluster)
        real prf(max_hits_in_cluster)
        real z(max_hits_in_cluster)
        real dz(max_hits_in_cluster)
        real zrf(max_hits_in_cluster)
        real phi(max_hits_in_cluster)
        real track(max_hits_in_cluster)
        real mu1(max_hits_in_cluster)
        real mu2(max_hits_in_cluster)
        real mu3(max_hits_in_cluster)
        real mu4(max_hits_in_cluster)
	real mu5(max_hits_in_cluster)

        integer fflagg


C-----------------------------------------------------------------------
        data first/.true./
C-----------------------------------------------------------------------
        integer istatus

	save first,time_bucket,time_bin
C-----------------------------------------------------------------------
c
c       set status
c
        tpham = STAFCV_OK

        if(first)then
           first = .false.
c     
c->         constants...
c
           time_bucket = detector(1).drift_length/tsspar(1).ntime
           time_bin = time_bucket/tsspar(1).v_drift
           
        endif

c noise in ADC counts...
c sca and digitization noise
	white_noise = sqrt((tsspar(1).sca_rms/tsspar(1).scale)**2+0.5)
c noise that comes through shaper
        filtered_noise = tsspar(1).white_rms/tsspar(1).scale

c Loop over the sequences in this cluster:
        do jtpcl = 1, tpcluster_h.nok
           nseq = tpcluster(jtpcl).nseq
           jseq = tpcluster(jtpcl).jseq
           isec = tpcluster(jtpcl).tpc_row/100
           irow = tpcluster(jtpcl).tpc_row-isec*100
           if(nseq.ge.tclpar(1).min_nseq)then

              call tph_get_cls_data(jseq, nseq, irow
     +,                             tpseq_h, tpseq 
     +,                             tppixel_h, tppixel
     +,                             tpmcpix_h, tpmcpix
     +,                             nclspix, ipad, itdc, iadc, itrk
     +,				    nclpix_max
     +,				    npix_seq
     +,				    ipad_lo, ipad_hi, itdc_lo, itdc_hi)
 
	      call tph_fit_cluster(tclpar,nseq, nclspix, npix_seq,
     +             ipad, itdc, iadc, itrk,
     +             white_noise, filtered_noise, nhits,
     +             max_hits_in_cluster,
     +             qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +             irow, ipad_lo, ipad_hi, itdc_lo, itdc_hi)

           else
c
c             do not store clusters with < min_hit_seq  sequences in them
c
              goto 999
           endif


c
c perhaps consistency with simulation demands use of pad_sep_in/out ???
c yes, I think so - mal
c
           if(irow.gt.pad_plane(1).nrow_in)then
              pad_width = pad_plane(1).pad_sep_out
              de_scale = tsspar(1).ave_ion_pot*tsspar(1).scale
     +             /(tsspar(1).gain_out*tsspar(1).wire_coupling_out)
           else
              pad_width = pad_plane(1).pad_sep_in
              de_scale = tsspar(1).ave_ion_pot*tsspar(1).scale
     +             /(tsspar(1).gain_in*tsspar(1).wire_coupling_in)
           endif


c store (nothing else for now) the estimated number of hits contributing 
c  to cluster in TPCLUSTER table....
           tpcluster(jtpcl).nhits  = nhits


           fflagg=0
           if (nhits.gt.1) fflagg=1
           DO ihit=1,nhits
              if (tphit_h.nok.ge.tphit_h.maxlen) then
c ??? Direct write is forbidden
                 write(6,*)'TPHIT table full - quitting at cluster',
     +                jtpcl
                 return
              endif
              tphit_h.nok = tphit_h.nok+1
	      tphit(tphit_h.nok).id = tphit_h.nok ! mal 17jan95
              tphit(tphit_h.nok).cluster = jtpcl
              tphit(tphit_h.nok).nseq = nseq
              tphit(tphit_h.nok).row = 100*isec+irow ! mal 30mar95
              tphit(tphit_h.nok).q     = qq(ihit)
              tphit(tphit_h.nok).x     = x(ihit)
              tphit(tphit_h.nok).dx    = dx(ihit)
              tphit(tphit_h.nok).prf   = prf(ihit)
              tphit(tphit_h.nok).z     = z(ihit)
              tphit(tphit_h.nok).dz    = dz(ihit)
              tphit(tphit_h.nok).zrf   = zrf(ihit)
              tphit(tphit_h.nok).phi   = phi(ihit)
              tphit(tphit_h.nok).track = track(ihit)
              tphit(tphit_h.nok).flag  = fflagg
              
c     
c     finally, transform everything to global coordinates !
c     
c     account for "lower-edge" binning in z coordinate
c     
              invec(3)=tphit(tphit_h.nok).z*time_bucket
              
c     correct (in average way) for time-offset -- (this is not beautiful)
c     22jun95 mal
c     the offset is shown to be (notebook2 p110, + files..) (n+1)*tau, where
c     n is the (effective) number of stages in the amplifier, and tau is the
c     (effective) time constant.  For STAR, n=2, and tau is set by tss.
              if (tclpar(1).tfit.eq.0) ! average value used
     +             invec(3) = invec(3) - 
     +             3.0*tsspar(1).tau*tsspar(1).v_drift
              
              
              istatus = tgc_row_to_y(float(irow),
     +             invec(2),pad_plane_h,pad_plane)
              
              istatus = tgc_pad_to_x(tphit(tphit_h.nok).x,
     +             float(irow),invec(1),pad_plane_h,pad_plane)
              
              istatus = tgc_local_to_global(isec,invec,outvec)
              
              
              tphit(tphit_h.nok).x = outvec(1)
              tphit(tphit_h.nok).y = outvec(2)
              tphit(tphit_h.nok).z = outvec(3) 
              
c     
c     zrf in units of time bins --> cm
c     
              tphit(tphit_h.nok).zrf 
     +             = (tphit(tphit_h.nok).zrf)*time_bucket
c     
c     prf in units of pads --> cm
c     
              tphit(tphit_h.nok).prf 
     +             = (tphit(tphit_h.nok).prf) * pad_width
              
c     also, calculate "orientation" of hit in terms of spatial quantities
              
              tphit(tphit_h.nok).phi = atand(
     +             tphit(tphit_h.nok).phi *
     +             time_bucket / pad_width )
              
c     must also rotate dx,dy into global coordinates...
              tphit(tphit_h.nok).dz = time_bucket*tphit(tphit_h.nok).dz
              dx_prime = pad_width*tphit(tphit_h.nok).dx
c     for explanation of what follows, see bottom of notebook p95
              rad = sqrt((tphit(tphit_h.nok).x)**2+
     +             (tphit(tphit_h.nok).y)**2)
              tphit(tphit_h.nok).dx = dx_prime *
     +             abs(tphit(tphit_h.nok).y)/rad
              tphit(tphit_h.nok).dy = dx_prime *
     +             abs(tphit(tphit_h.nok).x)/rad
              
              
c     while we have these numbers, fill the lambda field of tphit,
c     with the ansatz that the track comes from (0,0,0)
              tphit(tphit_h.nok).lambda = 
     +             - abs(atand(tphit(tphit_h.nok).z/rad)) ! lambda < 0 always
              
c     and I can calcualte alpha as well -- see notebook 2, pp 25-32 for
c     explanation...
              if (tand(tphit(tphit_h.nok).lambda).ne.0.0) then
                 if (tphit(tphit_h.nok).lambda.ge.10.0) then
                    tphit(tphit_h.nok).alpha = atand(
     +                   0.5*tand(tphit(tphit_h.nok).phi)/
     +                   tand(tphit(tphit_h.nok).lambda))
                 else
                    if (tphit(tphit_h.nok).flag.eq.0) then
                       tphit(tphit_h.nok).alpha = atand(
     +                      tand(tphit(tphit_h.nok).phi)/
     +                      tand(tphit(tphit_h.nok).lambda))
                    else
                       tphit(tphit_h.nok).alpha = atand(
     +                      4.0*tand(tphit(tphit_h.nok).phi)/
     +                      tand(tphit(tphit_h.nok).lambda))
                    endif
                 endif
              else
                 tphit(tphit_h.nok).alpha = 0.0
              endif
              
c     also, need a fudge factor for the uncertainties dx dy dz.
c     this should be understood and fixed, but for now....
              tphit(tphit_h.nok).dx = tphit(tphit_h.nok).dx 
     +             * tclpar(1).dxy_fact
              tphit(tphit_h.nok).dy = tphit(tphit_h.nok).dy 
     +             * tclpar(1).dxy_fact
              tphit(tphit_h.nok).dz = tphit(tphit_h.nok).dz 
     +             * tclpar(1).dz_fact
              
              
c     also put q (magnitude of hit) into units of energy loss
              tphit(tphit_h.nok).q = tphit(tphit_h.nok).q * de_scale
              
              
C     Added an auxiliary diagnostic hit table to evaluate "various quality
C     studies" of hit finder.   DSW 08/29/97.
              tphit_aux_h.nok = tphit_h.nok
	      tphit_aux(tphit_h.nok).id     = tphit_h.nok		
              tphit_aux(tphit_h.nok).flag   = tphit(tphit_h.nok).flag
              tphit_aux(tphit_h.nok).cluster= tphit(tphit_h.nok).cluster
              tphit_aux(tphit_h.nok).nseq   = tphit(tphit_h.nok).nseq
              tphit_aux(tphit_h.nok).row    = tphit(tphit_h.nok).row
              tphit_aux(tphit_h.nok).npads  = int(mu5(ihit))       
              tphit_aux(tphit_h.nok).x      = tphit(tphit_h.nok).x 
              tphit_aux(tphit_h.nok).y      = tphit(tphit_h.nok).y
              tphit_aux(tphit_h.nok).z      = tphit(tphit_h.nok).z 
              tphit_aux(tphit_h.nok).dx     = tphit(tphit_h.nok).dx 
              tphit_aux(tphit_h.nok).dy     = tphit(tphit_h.nok).dy 
              tphit_aux(tphit_h.nok).dz     = tphit(tphit_h.nok).dz 
              tphit_aux(tphit_h.nok).prf    = tphit(tphit_h.nok).prf 
              tphit_aux(tphit_h.nok).zrf    = tphit(tphit_h.nok).zrf 
              tphit_aux(tphit_h.nok).q      = tphit(tphit_h.nok).q 
              tphit_aux(tphit_h.nok).xave   = mu1(ihit)            
              tphit_aux(tphit_h.nok).sigma  = mu2(ihit)      
              tphit_aux(tphit_h.nok).skew   = mu3(ihit)      
              tphit_aux(tphit_h.nok).kurto  = mu4(ihit)       
c explicitly put them to zero here-- mal 30aug97
c              tphit_aux(tphit_h.nok).xave   = 0.0            
c              tphit_aux(tphit_h.nok).sigma  = 0.0
c              tphit_aux(tphit_h.nok).skew   = 0.0
c              tphit_aux(tphit_h.nok).kurto  = 0.0
              

           ENDDO


 999       continue
        enddo

c
        return
        end


C-----------------------------------------------------------------------
        Subroutine tph_get_cls_data(jseq, nseq, irow
     +,                            tpseq_h, tpseq 
     +,                            tppixel_h, tppixel
     +,                            tpmcpix_h, tpmcpix
     +,                            nclspix, ipad, itdc, iadc, itrk
     +,				   nclpix_max
     +,				   npix_seq
     +,				   ipad_lo, ipad_hi, itdc_lo, itdc_hi)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     jseq:
C     nseq:
C     irow:
C     tpseq_h:
C     tpseq:
C     tpppixel_h
C     tppixel:
C     tpmcpix_h
C     tpmcpix:
C     nclpix_max  (added 1feb95 by mal) maximum #pixels in a cluster
C
C   Output arguments : 
C
C     nclspix:
C     ipad:
C     itdc:
C     iadc:
C     itrk:
C     npix_seq:	(added 23 Nov 94) number of pixels in segment on each pad
C     ipad_lo: pad number lower limit for cluster (undefined if null cluster)
C     ipad_hi: pad number upper limit for cluster (undefined if null cluster)
C     itdc_lo: TDC lower limit (1-512) for cluster (undefined if null cluster)
C     itdc_hi: TDC upper limit (1-512) for cluster (undefined if null cluster)
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
C   23nov94 - m lisa - added output argument of npix_seq=number of pixels
C			in each sequence
C
C   30mar95 now sector=1..24,row=1..45
C
c   9apr96 -- malisa 
c	account for the fact that data is stored such that tdc=0..511
c	NOT 1..512.  The rest of the hitfinder assumes minimum tdc
c	is 1.  This is the only routine where tdc information is unpacked,
c	so I just add one to it here...
c
c   03-NOV-1997 --- R.Bossingham
c       * Find/return lower and upper tdc, pad-number limits;
c         change subroutine arguments accordingly.
c       * Update max. pads in a row and max. time buckets.
c       * Explicitly save variables that must be non-volatile
c       * Remove some obsolete (commented) code.
c
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tcl_tpseq.inc"
      RECORD      /TABLE_HEAD_ST/                 tpseq_h
      RECORD       /TCL_TPSEQ_ST/                tpseq(*)
#include "tss_tppixel.inc"
      RECORD      /TABLE_HEAD_ST/               tppixel_h
      RECORD     /TSS_TPPIXEL_ST/              tppixel(*)
#include "tss_tpmcpix.inc"
      RECORD      /TABLE_HEAD_ST/               tpmcpix_h
      RECORD     /TSS_TPMCPIX_ST/              tpmcpix(*)



C-----------------------------------------------------------------------
        integer iadc(*)
        integer ipad(*)
        integer ipad_lo, ipad_hi
        integer irow
        integer itdc(*)
        integer itdc_lo, itdc_hi
        integer itrk(*)
        integer jseq
        integer nclspix
	integer npix_seq(*)
        integer nseq
C-----------------------------------------------------------------------
c RRB: correct 184->182, change 1024->512
        integer npads_max, nbuck_max
        parameter (npads_max=182,nbuck_max=512)
c
	integer tdc_factor
	parameter (tdc_factor='400'X)
c
	integer numseq_factor
	parameter (numseq_factor='100000'X)
C
        integer iclspix
        integer id
        integer indx
	integer iseq_num
        integer jpix
        integer kseq, kseqpix
        integer ktrk
        integer ldatum
	integer nclpix_max
c
c 24mar95 -now data is packed in a new way in tppixel
C-----------------------------------------------------------------------
        data id/0/
C-----------------------------------------------------------------------

        iclspix = 0
	iseq_num = 0
        ipad_lo = npads_max
        ipad_hi = 1
        itdc_lo = nbuck_max
        itdc_hi = 1
        do kseq = jseq, jseq+nseq-1
           jpix = tpseq(kseq).jpix
           iclspix = iclspix + 1

	   if (iclspix.gt.nclpix_max) then
	     id = 0
	     call message('TPHAM-!-TMP too many pixels!',1,id)
	     return
	   endif

           ldatum = tppixel(jpix).datum
c
c          for MC data!
c
           ktrk = tpmcpix(jpix).mcid
           itrk(iclspix) = ktrk
c new way to unpack data
           kseqpix = ldatum/numseq_factor
           itdc(iclspix) = (ldatum - numseq_factor*kseqpix)/tdc_factor
c ??? Maybe mask this (for speed)
           iadc(iclspix) = ldatum - numseq_factor*kseqpix 
     +		- tdc_factor*itdc(iclspix)
c
           itdc(iclspix) = itdc(iclspix)+1 ! added 9apr96 malisa
           itdc_lo = MIN(itdc_lo,itdc(iclspix))
           itdc_hi = MAX(itdc_hi,itdc(iclspix)+kseqpix-1)
C
           ipad(iclspix) = tpseq(kseq).secpad
           ipad_lo = MIN(ipad_lo,ipad(iclspix))
           ipad_hi = MAX(ipad_hi,ipad(iclspix))
C
	   iseq_num = iseq_num+1
	   npix_seq(iseq_num) = kseqpix
C
C Step through one sequence on one pad:
           do indx = 2, kseqpix
              iclspix = iclspix + 1
	      if (iclspix.gt.nclpix_max) then
	        id = 0
	        call message('TPHAM-!-TMP too many pixels!',1,id)
	        return
	      endif
              ldatum = tppixel(jpix+indx-1).datum
c ??? Maybe mask this (for speed)
              itdc(iclspix) = itdc(iclspix-1)+1
              iadc(iclspix) = ldatum - numseq_factor*kseqpix 
     +		- tdc_factor*itdc(iclspix-1)
              ipad(iclspix) = ipad(iclspix-1)
c
c             for MC data!
c
              ktrk = tpmcpix(jpix).mcid
              itrk(iclspix) = ktrk
           enddo
        enddo

        nclspix = iclspix

        return
        end


C-----------------------------------------------------------------------
      Subroutine tph_fit_cluster(tclpar, nseq, nclspix, npix_seq,
     +     ipad, itdc, iadc, itrk,
     +     white_noise,  filtered_noise, nhits,
     +     max_hits_in_cluster,
     +     qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +     irow, ipad_lo, ipad_hi, itdc_lo, itdc_hi)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:   tcl switch table
C     nseq:	number of sequences in cluster
C     nclspix:	number of pixels in cluster
C     npix_seq: number of pixels in each sequence in cluster
C     ipad:	for each pixel, what is pad #
C     itdc:	for each pixel, what is bucket #
C     iadc:	for each pixel, what is adc value
C     itrk:     for each pixel, what MC track contributed most
C     white_noise:
C     filtered_noise:
C     max_hits_in_cluster: max # hits allowed in an overlapping pattern
C
C     irow
C     ipad_lo: Lower limit for pad number in cluster
C     ipad_hi: Upper limit for pad number in cluster
C     itdc_lo: Lower limit for time bucket in cluster
C     itdc_hi: Upper limit for time bucket in cluster
C
C   Output arguments : 
C
C     nhits - estimated number of hits in this cluster
C     qq         - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C     phi       - "orientation" of cluster
C     track     - MC track contributing the most pixels
C     mu1
C     mu2
C     mu3
C     mu4
C     mu5
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
#include "PAM.inc"
#include "tcl_tclpar.inc"
      RECORD      /TABLE_HEAD_ST/                tclpar_h
      RECORD      /TCL_TCLPAR_ST/               tclpar(*)



      integer nseq,nclspix
      integer ipad(*),itdc(*),iadc(*),itrk(*)
      integer npix_seq(*),nhits
      integer irow
      integer ipad_lo, ipad_hi, itdc_lo, itdc_hi
      integer max_hits_in_cluster

      real white_noise,filtered_noise
      real qq(*)
      real x(*)
      real dx(*)
      real prf(*)
      real z(*)
      real dz(*)
      real zrf(*)
      real phi(*)
      real track(*)
      real mu1(*)
      real mu2(*)
      real mu3(*)
      real mu4(*)
      real mu5(*)
c-----------------------------------------------------------------------
c*** skip the definition for the neural net too..... RRB: 03-NOV-1997
c    integer nhits_nnet        ! neural net estimator function
                                ! of #hits in cluster

      logical look_for_many,triage_cuts
c----------------------------------------------------------------

c*** skip the neural net for now..... mal 21jun95
***        nhits = nhits_nnet(nseq,nclspix,npix_seq,ipad,itdc,iadc)
	! this is experimental

      nhits=0
      if (tclpar(1).decon.ne.0) then
         if (tclpar(1).triage.eq.1) then
            look_for_many = triage_cuts(ipad,itdc,iadc,nclspix,tclpar)
         else
            look_for_many = .true.
         endif
         if (look_for_many) then
            call mountain_finder(tclpar, nseq, nclspix, npix_seq,
     +           ipad, itdc, iadc, itrk,
     +           white_noise, filtered_noise,
     +           nhits,
     +           qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +           ipad_lo, ipad_hi, itdc_lo, itdc_hi)
c ??? Direct write is forbidden
            if (nhits.gt.max_hits_in_cluster) 
     +           write(6,*)'TPHAM - ERROR! array overrun! - fix this!'
         endif
         if (nhits.eq.0) then
            call tph_fit_isolated_cluster(tclpar, nclspix,
     +           ipad, itdc, iadc, itrk,
     +           white_noise, filtered_noise,
     +           qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +           ipad_lo, ipad_hi, itdc_lo, itdc_hi)
            nhits=1
         endif
c
c remove the following after done using it--
c  just useful for outputing triage parameter for training
c*****************
         if (tclpar(1).decon.eq.-1)
     +        call ouput_practice(ipad,itdc,iadc,nclspix,irow,nhits)
c*****************
      else
c 30aug97 - bug introduced by UW folks - must include the "mu's" in ALL
c  calls to tph_fit_isolated_cluster!! - fixed mal
         call tph_fit_isolated_cluster(tclpar, nclspix,
     +        ipad, itdc, iadc, itrk,
     +        white_noise, filtered_noise,
     +        qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +        ipad_lo, ipad_hi, itdc_lo, itdc_hi)
         nhits=1
      endif
      
      return
      end


C-----------------------------------------------------------------------
      logical function triage_cuts(ipad,itdc,iadc,nclspix,tclpar)
c
c performs quick cuts on cluster data to determine whether to go ahead
c  with multi-peak search in cluster.  This is supposed to speed up the
c  processing by not trying multi-peak searches on clusters that have
c  no possibility to have more than one in them (that we can find).
c
c Inputs:
c     ipad: pad number array for pixels in cluster
c     itdc: tdc number array for pixels in cluster
c     iadc: adc number array for pixels in cluster
c     nclspix: # pixels in cluster
c     tclpar: switch table for cluster/hitfinder.  has cut values.
c
c Returns: true if should proceed with multi-peak search. false otherwise
c
      implicit none
#include "PAM.inc"
#include "tcl_tclpar.inc"
      RECORD      /TABLE_HEAD_ST/                tclpar_h
      RECORD      /TCL_TCLPAR_ST/               tclpar(*)

      integer ipad(*),itdc(*),iadc(*),nclspix
      real ppa,tta,pa,ta,a ! these are produce sums (ta = tdc(i)*adc(i))
      real rms_pad,rms_tdc
      integer ipix

      ppa=0.0
      tta=0.0
      pa=0.0
      ta=0.0
      a=0.0
      do ipix=1,nclspix
         ppa = ppa + ipad(ipix)*ipad(ipix)*iadc(ipix)
         tta = tta + itdc(ipix)*itdc(ipix)*iadc(ipix)
         pa  = pa  + ipad(ipix)*iadc(ipix)
         ta  = ta  + itdc(ipix)*iadc(ipix)
         a   = a   + iadc(ipix)
      enddo
      rms_pad = sqrt(ppa/a - (pa/a)**2)
      rms_tdc = sqrt(tta/a - (ta/a)**2)

      triage_cuts = (rms_pad+rms_tdc.gt.tclpar(1).triage_rmscut)
      end



C-----------------------------------------------------------------------
      subroutine ouput_practice(ipad,itdc,iadc,nclspix,irow,nhits)
c
c just calculates some integer moments and outputs them for playing with
c
      implicit none

      integer ipad(*),itdc(*),iadc(*),nclspix,irow,nhits
c
c see pp21-22 of STAR tcl notebook #2
c
c      integer rms2_pad,rms2_tdc,rms2_mix,tdc_mean
      real rms2_pad,rms2_tdc,rms2_mix,tdc_mean
c      integer ppa,tta,pa,ta,a,pta ! these are produce sums (ta = tdc(i)*adc(i)
      real ppa,tta,pa,ta,a,pta ! these are produce sums (ta = tdc(i)*adc(i))
      integer ipix

      ppa=0.0
      tta=0.0
      pa=0.0
      ta=0.0
      a=0.0
      pta=0.0
      do ipix=1,nclspix
         ppa = ppa + ipad(ipix)*ipad(ipix)*iadc(ipix)
         tta = tta + itdc(ipix)*itdc(ipix)*iadc(ipix)
         pa  = pa  + ipad(ipix)*iadc(ipix)
         ta  = ta  + itdc(ipix)*iadc(ipix)
         a   = a   + iadc(ipix)
         pta = pta + ipad(ipix)*itdc(ipix)*iadc(ipix)
      enddo
      rms2_pad = sqrt(ppa/a - (pa/a)**2)
      rms2_tdc = sqrt(tta/a - (ta/a)**2)
      rms2_mix = sqrt(pta/a - pa*ta/(a**2))
      tdc_mean = ta/a

c ??? Direct write is forbidden
      write(55,*)rms2_pad,rms2_tdc,rms2_mix,tdc_mean,irow,nhits

      return
      end
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      Subroutine mountain_finder(tclpar, nseq, nclspix, npix_seq,
     +     ipad, itdc, iadc, itrk,
     +     white_noise, filtered_noise,
     +     nhits,
     +     qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +     ipad_lo, ipad_hi, itdc_lo, itdc_hi)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:   switch table for tcl
C     nseq:	number of sequences in cluster
C     nclspix:	number of pixels in cluster
C     npix_seq: number of pixels in each sequence in cluster
C     ipad:	for each pixel, what is pad #
C     itdc:	for each pixel, what is bucket #
C     iadc:	for each pixel, what is adc value
C     itrk:     for each pixel, what MC track contributed most
C     white_noise: noise on adc values
C     filtered_noise: noise on adc values
C
C   Output arguments : 
C
C     nhits     - number of peaks found
C     q         - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C     phi       - "orientation" of cluster
C     track     - MC track contributing the most pixels
C     mu1
C     mu2
C     mu3
C     mu4
C     mu5
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
c
c   modificattions:
c   ==============
c   03-NOV-1997 --- R.Bossingham
c       * Use lower and upper tdc, pad-number limits to speed code;
c         change arguments accordingly
c       * Update max. pads in a row and max. time buckets.
c       * Add comments
c       * Allow reset of edges of "allowed" array
c       * Explicitly save the "allowed" array
C-----------------------------------------------------------------------
c passed inputs
#include "PAM.inc"
#include "tcl_tclpar.inc"
      RECORD      /TABLE_HEAD_ST/                tclpar_h
      RECORD      /TCL_TCLPAR_ST/               tclpar(*)



      integer nseq,nclspix
      integer ipad(*),itdc(*),iadc(*),itrk(*)
      integer npix_seq(*),nhits
      integer ipad_lo, ipad_hi, itdc_lo, itdc_hi
      real white_noise,filtered_noise
c passed outputs
      real qq(*)
      real x(*)
      real dx(*)
      real prf(*)
      real z(*)
      real dz(*)
      real zrf(*)
      real phi(*)
      real track(*)
      real mu1(*)
      real mu2(*)
      real mu3(*)
      real mu4(*)
      real mu5(*)

c local variables
      integer maxpad,maxbuck,n_allow
c RRB: correct 184->182, change 1024->512
      parameter (maxpad=182,maxbuck=512)
c     parameter (maxpad=184,maxbuck=1024) ! upped from 512 4jan95
      parameter (n_allow=maxpad*maxbuck)
c
      integer height(1:maxpad,1:maxbuck)
      integer track_pix(1:maxpad,1:maxbuck)
      logical allowed(maxpad,maxbuck)
c
      integer i,j
      integer jpad,jtdc,jpix
      integer kpad,ktdc
      integer index_list(10000),indx
      integer nmax,imax
      integer nmax_max          ! maximum number of local maxima
      parameter(nmax_max=100)
      integer max_pos(2,nmax_max) ! position (pad,tdc) of local maxima
      integer max_height(nmax_max)
      integer iclosest_pad,iclosest_tdc,i_minimum_height

c the following arrays are for passing to fit_isolated_clusters for
c  each found mountain top
      real qtemp(1),xtemp(1),dxtemp(1),prftemp(1)
      real ztemp(1),dztemp(1),zrftemp(1),phitemp(1),tracktemp(1)
      real mu1temp(1),mu2temp(1),mu3temp(1),mu4temp(1),mu5temp(1)
      integer nclspix_temp
      integer ipad_temp(30),itdc_temp(30),iadc_temp(30),itrk_temp(30)
      integer ipad_lo_temp
      integer ipad_hi_temp
      integer itdc_lo_temp
      integer itdc_hi_temp

      integer dpad,dtdc,valley
      real slope,peak_to_valley

      integer iwriteout
c
      data allowed/n_allow*.true./
c
C-----------------------------------------------------------------------
      save allowed
C-----------------------------------------------------------------------

c Clear a rectangular area containing the cluster with the arrays
c height(1:maxpad,1:maxbuck) and track_pix(1:maxpad,1:maxbuck):
      do j=itdc_lo,itdc_hi
         do i=ipad_lo,ipad_hi
            height(i,j) = 0
            track_pix(i,j) = 0
         enddo
      enddo

c Load the cluster:
      do jpix=1,nclspix
         height(ipad(jpix),itdc(jpix)) = iadc(jpix)
         track_pix(ipad(jpix),itdc(jpix)) = itrk(jpix)
      enddo

c list pixels in descending order of adc occupancy...
      call sortzv(iadc,index_list,nclspix,-1,10,0)

      iclosest_pad=tclpar(1).mf_cpad
      iclosest_tdc=tclpar(1).mf_ctim
      i_minimum_height=tclpar(1).mf_min


c find maxima...
c here, index_list are the indices into ipad,itdc,iadc, in decending
c     order, of the pixels with maximum occupancy.  So, for each pixel
c     that comes up, just check to see if that pixel is allowed.
c     If so, then it is a mountain-top.
c Each pixel that comes up on my ordered list should "lay waste" to
c     surrounding territory, so
c     that if I get a pixel in my ordered list which is allowed, then
c     it is a probably a local maximum-- the only way it is not is
c     when there is a pleteau of equal adc values.  Then, there may
c     be >1 maximum assigned to it.  Also, a true local max adjacent to a
c     plateau may give rise to "fake" local maxima on the plateau.
c

      nmax=0
      do jpix=1,nclspix
         indx=index_list(jpix)
         jpad=ipad(indx)
         jtdc=itdc(indx)
         if (height(jpad,jtdc).lt.i_minimum_height) goto 20 ! done
         if (allowed(jpad,jtdc)) then ! possible local max
c before storing, we check peak:valley for pixels along lines which
c  connect this local max to the bigger (previous) ones... note that
c  this check does not attempt to kill the biggest (first) max found.
c if peak:valley test fails, we do not store and just jump to line 19
            if (tclpar(1).mf_pv.gt.1.0) then ! (if not, then do not even 
                                             !  check - peak is ALWAYS > vall)
               do imax=1,nmax
                  dpad=max_pos(1,imax)-jpad
                  dtdc=max_pos(2,imax)-jtdc
                  if (dpad.ne.0) then ! walk along line connecting peaks
                     slope = float(dtdc)/float(dpad)
                     valley = height(jpad,jtdc)
                     if (iabs(dpad).gt.iabs(dtdc)) then ! loop over pad
                                                        ! tdc depends
                                                        ! on pad
c RRB: Must have at least 3 pads for next loop...
                        do kpad=min(jpad,max_pos(1,imax))+1,
     +                          max(jpad,max_pos(1,imax))-1
                           ktdc=jtdc+nint(slope*float(kpad-jpad))
                           valley = min(valley,height(kpad,ktdc))
                        enddo
                     else                   ! loop over tdc
                                            ! pad depends on tdc
c RRB: Must have at least 3 buckets for next loop...
                        do ktdc=min(jtdc,max_pos(2,imax))+1,
     +                          max(jtdc,max_pos(2,imax))-1
                           kpad=jpad+nint(float(ktdc-jtdc)/slope)
                           valley = min(valley,height(kpad,ktdc))
                        enddo
                     endif
                  else          ! peaks on same pad.. walk in time only
                     valley = height(jpad,jtdc)
                     do ktdc=min(jtdc,max_pos(2,imax))+1,
     +                       max(jtdc,max_pos(2,imax))-1
                        valley=min(valley,height(jpad,ktdc))
                     enddo
                  endif

c RRB: ??? Need a more sophisticated algorithm, allowing for ADC & noise?
c--------------------------------------------
c we should NOT be using peak/valley!!!
c instead use peak-valley (i.e. peak MINUS valley)
c It was my stupidity.  malisa 4sep97
c                  if (valley.ne.0) then
c                     peak_to_valley = 
c     +                    float(height(jpad,jtdc))/float(valley)
c                     if (peak_to_valley.lt.tclpar(1).mf_pv)
c     +                    goto 19 ! FAILED
c                  endif
                  peak_to_valley = height(jpad,jtdc)-valley
                  if (peak_to_valley.lt.tclpar(1).mf_pv)
     +                 goto 19  ! FAILED
c--------------------------------------------
               enddo
            endif
c ok, passed peak:valley test... store this peak
            nmax=nmax+1
            if (nmax.gt.nmax_max) then
               nmax = nmax_max
c ??? Direct write is forbidden
               write(6,*)'Hey!... too many peaks... I am bailing...'
               do iwriteout=1,nclspix
c ??? Direct write is forbidden
                  write(56,*)' '
c ??? Direct write is forbidden
                  write(56,*)iwriteout,
     +                 ipad(iwriteout),itdc(iwriteout),iadc(iwriteout)
               enddo
               goto 20
            endif
            max_pos(1,nmax)=jpad
            max_pos(2,nmax)=jtdc
            max_height(nmax)=iadc(indx)
 19         continue
c lay waste to adjacent area (may be large)...
c RRB: Reverse memory access order; mark false only within cluster region:
            do ktdc=max(jtdc-(iclosest_tdc-1),itdc_lo),
     +              min(jtdc+(iclosest_tdc-1),itdc_hi)
               do kpad=max(jpad-(iclosest_pad-1),ipad_lo),
     +                 min(jpad+(iclosest_pad-1),ipad_hi)
                  allowed(kpad,ktdc) = .false.
               enddo
            enddo
         else
c lay waste to some adjacent area even if not a peak......
c RRB: i.e., "allowed" was false for pixel
c RRB: mark false only within cluster region:
            do ktdc=max(jtdc-1,itdc_lo),
     +              min(jtdc+1,itdc_hi)
               do kpad=max(jpad-1,ipad_lo),
     +                 min(jpad+1,ipad_hi)
                  allowed(kpad,ktdc) = .false.
               enddo
            enddo
         endif
      enddo

 20   continue

c calculate "positions"...
      nhits=nmax
      if (nmax.eq.1) then       ! use whole cluster in fitting
         call tph_fit_isolated_cluster(tclpar, nclspix,
     +        ipad,itdc,iadc,itrk,
     +        white_noise, filtered_noise,
     +        qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +        ipad_lo, ipad_hi, itdc_lo, itdc_hi)
      else                      ! just use pixels around local maximum
         do imax=1,nmax
            jpad=max_pos(1,imax)
            jtdc=max_pos(2,imax)
            nclspix_temp = 0
            ipad_lo_temp = ipad_hi
            ipad_hi_temp = ipad_lo
            itdc_lo_temp = itdc_hi
            itdc_hi_temp = itdc_lo
            do kpad=max(jpad-(iclosest_pad-1),ipad_lo),
     +              min(jpad+(iclosest_pad-1),ipad_hi)
               do ktdc=max(jtdc-(iclosest_tdc-1),itdc_lo),
     +                 min(jtdc+(iclosest_tdc-1),itdc_hi)
                  if (height(kpad,ktdc).ne.0) then
c ??? What keeps these arrays from overflowing?
c ??? Probably need check on iclosest_adc value...
                     nclspix_temp=nclspix_temp+1
                     ipad_temp(nclspix_temp)=kpad
                     itdc_temp(nclspix_temp)=ktdc
                     iadc_temp(nclspix_temp)=height(kpad,ktdc)
                     itrk_temp(nclspix_temp)=track_pix(kpad,ktdc)
                     ipad_lo_temp = MIN(ipad_lo_temp,kpad)
                     ipad_hi_temp = MAX(ipad_hi_temp,kpad)
                     itdc_lo_temp = MIN(itdc_lo_temp,ktdc)
                     itdc_hi_temp = MAX(itdc_hi_temp,ktdc)
                  endif
               enddo
            enddo
            call tph_fit_isolated_cluster(tclpar, nclspix_temp,
     +           ipad_temp,itdc_temp,iadc_temp,itrk_temp,
     +           white_noise, filtered_noise,
     +           qtemp,xtemp,dxtemp,prftemp,ztemp,dztemp,zrftemp,
     +           phitemp,tracktemp,
     +           mu1temp,mu2temp,mu3temp,mu4temp,mu5temp,
     +           ipad_lo_temp, ipad_hi_temp, itdc_lo_temp, itdc_hi_temp)

c ??? Could we just put these in the calling arguments, instead?
            qq(imax)     = qtemp(1)
            x(imax)     = xtemp(1)
            dx(imax)    = dxtemp(1)
            prf(imax)   = prftemp(1)
            z(imax)     = ztemp(1)
            dz(imax)    = dztemp(1)
            zrf(imax)   = zrftemp(1)
            phi(imax)   = phitemp(1)
            track(imax) = tracktemp(1)
            mu1(imax)   = mu1temp(1) 
            mu2(imax)   = mu2temp(1) 
            mu3(imax)   = mu3temp(1) 
            mu4(imax)   = mu4temp(1) 
            mu5(imax)   = mu5temp(1) 
         enddo
      endif

c finally, put back array allowed the way you found it.
c RRB: Now simplified; also, memory access order reversed
      do jtdc=itdc_lo,itdc_hi
         do jpad=ipad_lo,ipad_hi
            allowed(jpad,jtdc) = .true.
         enddo
      enddo
      return
      end



C-----------------------------------------------------------------------
      Subroutine tph_fit_isolated_cluster(tclpar, nclspix,
     +				ipad, itdc, iadc, itrk,
     +				white_noise, filtered_noise,
     +                          qq,x,dx,prf,z,dz,zrf,phi,track,
     +                          mu1,mu2,mu3,mu4,mu5,
     +                          ipad_lo, ipad_hi, itdc_lo, itdc_hi)

C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:         switch table for tcl
C     nclspix:        number of pixels in cluster
C     ipad:	      for each pixel, what is pad #
C     itdc:	      for each pixel, what is bucket #
C     iadc:	      for each pixel, what is adc value
C     itrk:           for each pixel, what MC track contributed most
C     white_noise:    noise on adc values
C     filtered_noise: noise on adc values
C     ipad_lo:        pad number lower limit for cluster
C     ipad_hi:        pad number upper limit for cluster
C     itdc_lo:        TDC lower limit (1-512) for cluster
C     itdc_hi:        TDC upper limit (1-512) for cluster
C
C   Output arguments : 
C
C     qq         - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C     phi       - "orientation" of cluster
C     track     - MC track contributing the most pixels
C     mu1
C     mu2
C     mu3
C     mu4
C     mu5
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
c   03-NOV-1997 --- R.Bossingham
c       * Use lower and upper tdc, pad-number limits to speed code;
c         change arguments accordingly
c       * Update max. pads in row -> 182, max. time buckets -> 512.
c       * Replace most calls to vzero with in-line code (for speed)
c       * Replace calls to vfloat with in-line code (for speed)
c       * Add comments
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------


c passed inputs
#include "PAM.inc"
#include "tcl_tclpar.inc"
      RECORD      /TABLE_HEAD_ST/                tclpar_h
      RECORD      /TCL_TCLPAR_ST/               tclpar(*)



	integer nclspix
	integer ipad(*),itdc(*),iadc(*),itrk(*)
        integer ipad_lo, ipad_hi, itdc_lo, itdc_hi

c passed outputs
        real qq(*)
        real x(*)
        real dx(*)
        real prf(*)
        real z(*)
        real dz(*)
        real zrf(*)
        real phi(*)
        real track(*)
        real mu1(*)
        real mu2(*)
        real mu3(*)
        real mu4(*)
        real mu5(*)
        integer tph_3point_gauss

	real centroid,sigma,uncertainty
        real white_noise,filtered_noise
C-----------------------------------------------------------------------

        integer npads_max,nbuck_max
        parameter(npads_max=182,nbuck_max=512)
c
        integer nmc_tracks_max
        parameter(nmc_tracks_max=2000)
c
        integer adc_sum
        integer iadc_ipix
        integer id
        integer imax
        integer imc_track(nmc_tracks_max)
        integer indx
        integer ipad_ipix
        integer ipad_range
        integer ipix
        integer ir
        integer itdc_ipix
        integer iucomp
        integer ktrk
        integer lvsimx
        integer mtrk
        integer nmc_tracks
        integer nocc_track(nmc_tracks_max)
        integer numpix_pad_proj(npads_max)
        integer numpix_time_proj(nbuck_max)
        integer pad_proj(npads_max)
        integer pad_n
        integer tdc_x_adc(npads_max)
        integer time_proj(nbuck_max)
c
        real d_rvec_cor(nbuck_max)             ! SCA noise
        real d_rvec_uncor(nbuck_max)           ! SAS noise
        real diff(4)
        real identity_vector(nbuck_max)        ! useful for x-axis of proj.
        real rmu(4)
        real rvec(nbuck_max),d_rvec(nbuck_max) ! temp arrays
        real slope,b,badness                   ! for linear fit
c
        logical first/.true./
        logical lookat_mc
C-----------------------------------------------------------------------
        save first
        save identity_vector
        save lookat_mc
C-----------------------------------------------------------------------

c set up the identity vector
        if (first) then
           first = .false.
           do indx=1,nbuck_max
              identity_vector(indx) = float(indx)
           enddo
           lookat_mc = (tclpar(1).mc.eq.1)
        endif

c loop over all pixels, in the process:
c 1) sum all adc counts == q
c 2) project onto padrow (will fit projection to find x,dx,prf)
c 3) project onto time axis (will fit projection to find z,dz,zrf)
c 4) get time average for each pad (fit each and obtain orientation)
c 5) sum up adc contribs from all MC tracks (will check to see who gave most)

        ipad_range = ipad_hi - ipad_lo + 1
c
        do indx=ipad_lo,ipad_hi
           pad_proj(indx) = 0
           tdc_x_adc(indx) = 0
           numpix_pad_proj(indx) = 0
        end do
c
        do indx=itdc_lo,itdc_hi
           time_proj(indx) = 0
           numpix_time_proj(indx) = 0
        end do
c
        adc_sum = 0
        do ipix=1,nclspix
           iadc_ipix = iadc(ipix)
           adc_sum = adc_sum + iadc_ipix
           ipad_ipix = ipad(ipix)
           itdc_ipix = itdc(ipix)

           pad_proj(ipad_ipix)  = pad_proj(ipad_ipix)  + iadc_ipix
           tdc_x_adc(ipad_ipix) = tdc_x_adc(ipad_ipix)
     +                          + itdc_ipix*iadc_ipix
           numpix_pad_proj(ipad_ipix)  = numpix_pad_proj(ipad_ipix)  +1
           time_proj(itdc_ipix) = time_proj(itdc_ipix) + iadc_ipix
           numpix_time_proj(itdc_ipix) = numpix_time_proj(itdc_ipix) +1
        enddo
	if (adc_sum.eq.0) stop 'Hey -- ADCSUM=0!!!!'

c mal 30aug97 - maybe a problem with decon=0 running - does this 
c screw up for very wide clusters??????????
C------------------------------------------------------------------
C     Added by DSW on July 6, 1997 to study the pad resolution shootup
C     at large pad crossing angles.
	do ir=1,4
           rmu(ir) = 0.
        enddo
        do pad_n=ipad_lo, ipad_hi
           rmu(1)  =  rmu(1) +  pad_proj(pad_n)*pad_n
        enddo
        rmu(1) =  rmu(1)/float(adc_sum)

C     Calculate  sigma, skewness & kurtosis;
C RRB: Speed calc. by reordering loop and carrying previous results forward:
        do pad_n=ipad_lo, ipad_hi
           diff(1) = float(pad_n) - rmu(1)
           do ir = 2,4
              diff(ir) = diff(1)*diff(ir-1)
              rmu(ir) =  rmu(ir) + float(pad_proj(pad_n))*diff(ir)
           enddo
        enddo
C
        do ir = 2,4
           rmu(ir) = rmu(ir)/float(adc_sum)
        enddo

        mu1(1) = rmu(1)
        mu2(1) = sqrt(rmu(2))
        mu3(1) = rmu(3)/rmu(2)**1.5
        mu4(1) = rmu(4)/rmu(2)**2 - 3.
        mu5(1) = float(ipad_hi - ipad_lo + 1 )
C------------------------------------------------------------------
c mal 30aug97 - just put the damn things to zero-- they seem to be randomly
c assigned if not!!
c	mu1(1) = 0.0
c	mu2(1) = 0.0
c	mu3(1) = 0.0
c	mu4(1) = 0.0
c	mu5(1) = 0.0

c     q:
        qq(1) = adc_sum

c x,dx,prf: - try 3-point gauss... if does not work, do weighted mean
        do indx=ipad_lo,ipad_hi
           rvec(indx) = FLOAT(pad_proj(indx))
           if (pad_proj(indx).ne.0) then
              d_rvec(indx) = 
     +          sqrt((float(numpix_pad_proj(indx))*filtered_noise)**2+
     +          (float(numpix_pad_proj(indx))*white_noise**2))
           else
              d_rvec(indx)=0.0
           endif
           d_rvec_cor(indx) = 0.0
        enddo
cmal 30aug97        if (tph_3point_gauss(identity_vector, rvec, d_rvec,
cmal/rrb     +       ipad_lo, ipad_hi, npads_max,
cmal/rrb     +       centroid, sigma, uncertainty).ne.1) 
cmal     +       call tph_weighted_mean(identity_vector, rvec,
	       call tph_weighted_mean(identity_vector, rvec,
     +          d_rvec_cor, d_rvec,
     +          ipad_lo, ipad_hi, npads_max,
     +          centroid, sigma, uncertainty)
        x(1) = centroid
        dx(1) = uncertainty
        prf(1) = sigma

c z,dz,zrf: - do weighted mean
c  for uncertainties, see notebook2 p113
c
        do indx=itdc_lo,itdc_hi
           rvec(indx) = FLOAT(time_proj(indx))
           if (time_proj(indx).ne.0) then
              d_rvec_cor(indx) = 
     +             sqrt(float(numpix_time_proj(indx)))*filtered_noise
              d_rvec_uncor(indx) = 
     +             sqrt(float(numpix_time_proj(indx)))*white_noise
           else
              d_rvec_cor(indx) = 0.0
              d_rvec_uncor(indx) = 0.0
           endif
        enddo
c        if (tclpar(1).tfit.eq.1) then ! fancy fit - not recommended...
c           call tph_fit_pulse(identity_vector, rvec,
c     +                        d_rvec,
c     +                        itdc_lo, itdc_hi, nbuck_max,
c     +                        centroid, sigma, uncertainty)
c       else                          ! simple weighted mean
        call tph_weighted_mean(identity_vector, rvec,
     +       d_rvec_cor, d_rvec_uncor,
     +       itdc_lo, itdc_hi, nbuck_max,
     +       centroid, sigma, uncertainty)
c ??? Curious line?
        centroid = centroid     ! phase correction done at top level....
c       endif
        z(1) = centroid
        dz(1) = uncertainty
        zrf(1) = sigma

c orientation phi:
c ??? If pad_proj *is* 0, rvec is peculiar...but I left existing *logic*
        do indx=ipad_lo,ipad_hi
           d_rvec(indx) = FLOAT(pad_proj(indx))
           if (pad_proj(indx).ne.0) then
              rvec(indx) = FLOAT(tdc_x_adc(indx))/d_rvec(indx)
           else
              rvec(indx) = FLOAT(tdc_x_adc(indx))
           end if
        enddo
	call lfitw(identity_vector(ipad_lo), rvec(ipad_lo),
     +             d_rvec(ipad_lo), ipad_range, 0,
     +             slope, b, badness)
        phi(1) = slope


c track: - who contributed the most pixels (MC data)
c this following is only useful for MC data-- turn it off with 
c  switch if interested in real data (or real data performance)
        if (lookat_mc) then
           call vzero(imc_track,nmc_tracks_max)
           call vzero(nocc_track,nmc_tracks_max)
           nmc_tracks = 0
           do ipix=1,nclspix
              ktrk=itrk(ipix)
              mtrk=iucomp(ktrk,imc_track,nmc_tracks)
              if (mtrk.eq.0) then
                 if (nmc_tracks.lt.nmc_tracks_max) then
                    nmc_tracks=nmc_tracks+1
                    imc_track(nmc_tracks) = ktrk
                    nocc_track(nmc_tracks) = 1
                 else           ! too many contributing tracks... forget it
                    id=0
                    call message('TPHAM-I-2MANY, ntrk.gt.max',1,id)
                 endif
              else
                 nocc_track(mtrk) = nocc_track(mtrk)+1
              endif
           enddo
           imax = lvsimx(nocc_track,nmc_tracks,1)
           track(1) = imc_track(imax)
        endif

        return
        end





C-----------------------------------------------------------------------
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
C     dt0:	uncertainty in "time-zero" of the pulse
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
        integer itdc_lo, itdc_hi
	integer npts_max
	real tseq(npts_max),aseq(npts_max),daseq(npts_max)
	real t0,tau,dt0
C-----------------------------------------------------------------------
	integer lvmax
	integer npts
	integer N
	parameter(N=2)          ! for STAR
	real B
	real y1,y2,y3,t1,t2,t3,dy1,dy2,dy3
	integer imax
	integer id,istart
        real delta,root1B,rat
        logical retry
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
        call message('TPH_FIT_PULSE failure; going to mean',
     +       1,id)
c this should be fixed before using for real-----v
        call tph_weighted_mean(tseq, aseq,
     +       daseq, daseq,
     +       itdc_lo, itdc_hi, npts_max,
     +       t0, tau, dt0)
c     should correct for phase here!!!!!!!! *** mal 27jul95
        return
	end


C-----------------------------------------------------------------------
	integer function tph_3point_gauss(x, y, dy, ipad_lo, ipad_hi,
     +				ipad_max, centroid, sigma, uncertainty)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     y		- y(x) distribution to fit to gaussian
C     dy	- uncertainties on y(x)
C     x		- x coordinate for each point
C     ipad_lo	- first pad number in cluster
C     ipad_hi	- last pad number in cluster
C     ipad_max	- last pad number in row
C
C   Output arguments : 
C
C     centroid 	- Gaussian centroid from 3-point fit to y(x)
C     sigma	- Gaussian width from 3-point fit to y(x)
C     uncertainty - uncertainty in centroid
C
C  Returns:
C
C     +1 if successful, -1 if unable to fit Gaussian
C     
C   Functional Description : 
C
C	Performs 3-point gaussian fit to passed distribution y(x)
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modifications :
c   =============
C
C   Modified: 20-DEC-1996 by R.Bossingham, Lawrence Berkeley Natl. Laboratory
C             If the Gaussian width is more than 83% of the pad width,
C             return a function value of -2; do not calculate position.
C
C             03-NOV-1997 by R.Bossingham
C             Add calling arguments so as to clearly distinguish
C               a) range of pads involved in cluster
C               b) length of pad row
C             and change logic to use this information properly.
C             
C
C   Error conditions : 
C   ================
C
C     If the distribution is not well described by a gaussian, (i.e.
C	it has a peak on one edge and is not monotonic) the function
C       returns a value of -1
C
C     If the width is so large that a weighted mean algorithm is likely
C       to be more accurate, the function returns a value of -2
C
C-----------------------------------------------------------------------
	real y(*),x(*),dy(*)
        integer ipad_lo
        integer ipad_hi
        integer ipad_max
	real centroid,sigma,uncertainty,x0
C-----------------------------------------------------------------------
	integer lvmax
	integer imax,istart,id
	integer npts
	real y1,y2,y3,delta,r213

        npts = ipad_hi - ipad_lo + 1
        if (npts.lt.3) then
           tph_3point_gauss = -1
           return
        else
           tph_3point_gauss = 1
        endif
c
c Find pad with largest signal:
	imax = lvmax(y(ipad_lo),npts) + ipad_lo - 1
        if (imax.eq.1) then             ! maximum at first point
           if (y(2).le.y(3)) then
              id = 0
              tph_3point_gauss = -1
              call message('TPH_3POINT_GAUSS bad shape', 1,id)
              return
           endif
           istart = 1
	else if (imax.eq.ipad_max) then ! maximum at last point
           if (y(imax-1).le.y(imax-2)) then
              id = 0
              tph_3point_gauss = -1
              call message('TPH_3POINT_GAUSS bad shape', 1,id)
              return
           endif
           istart = imax - 2
        else                            ! normal - max not at edge
           istart = imax - 1
	endif

	y1 = y(istart)
	y2 = y(istart+1)
	y3 = y(istart+2)

        if (y1*y3.lt.1.0e-10) then ! zero occupancy - no can do
           tph_3point_gauss = -1
           return
        endif

C RRB: Slightly recode to avoid direct comparison of floating pt. numbers
	delta = x(istart+1) - x(istart)
	if (ABS(delta-1.).gt.1.E-7) then
	  id = 0
	  call message('TPH_3POINT_GAUSS unexpected delta .ne. 1', 1,id)
          tph_3point_gauss = -1
	  return
	endif

	r213 = log(y2*y2/(y1*y3))
        if (r213.le.0.0) then
           id = 0
           tph_3point_gauss = -1
           call message('TPH_3POINT_GAUSS bad shape - r213', 1,id)
           return

********************** let's try to take it out....**********************
C
C From Monte Carlo, if the fitted Gaussian is wider than about 83%
C of the pad pitch in X, the weighted mean algorithm is more accurate;
C we return a diagnostic to force its use.  [ 1/sqrt(1.452)=0.83 ].
C	elseif (r213.le.1.452) then
C           id = 0
C           tph_3point_gauss = -2
C           return
        endif

	x0 = (delta/2.0) * log(y3/y1)/r213
	centroid = x(istart+1) + x0
	sigma = delta/sqrt(r213)

	uncertainty = sqrt( (dy(istart)*(delta/2.0-x0)/y(istart))**2 +
     +			    (dy(istart+1)*(2.0*x0)/y(istart+1))**2 +
     +			    (dy(istart+2)*(delta/2.0+x0)/y(istart+2))**2
     +				)/r213

	return
	end




C-----------------------------------------------------------------------
	Subroutine tph_weighted_mean(x, y, dy_cor,dy_uncor,
     +     ilo, ihi, imax,
     +     mom1, mom2, uncertainty)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     x		- x coordinate of each point
C     y		- y(x) distribution
C     dy_cor	- uncertainty in y(x) that is CORRELATED
C     dy_uncor	- uncertainty in y(x) that is UNCORRELATED
C     ilo	- index of first sample in distribution
C     ihi	- index of last sample in distribution
C     imax	- max. possible index for sample
C
C   Output arguments : 
C
C     mom1	- first moment of y(x) distribution (mean x)
C     uncertainty - uncertainty of mean
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
C     27dec95 - mal - calculate uncertainty in mean using
C                     correlated and uncorrelated uncertainty in y
C
C   Error conditions : 
C   ================
C
C-----------------------------------------------------------------------
	real x(*),y(*),dy_cor(*),dy_uncor(*)
        integer ilo, ihi, imax
	real mom1,mom2,uncertainty_cor,uncertainty_uncor,uncertainty
C-----------------------------------------------------------------------
	real ysum
	integer ipt
	integer npts      ! Not currently used
C-----------------------------------------------------------------------

        npts = ihi - ilo + 1

	mom1 = 0.0
	mom2 = 0.0
	ysum = 0.0
	do ipt=ilo,ihi
	  ysum = ysum + y(ipt)
	  mom1 = mom1 + x(ipt)*y(ipt)
          mom2 = mom2 + x(ipt)*(x(ipt)*y(ipt))
	enddo
	mom1 = mom1 / ysum
        mom2 = sqrt(mom2/ysum - mom1**2)
c
c the uncertainty from the correlated noise and uncorrelated noise
c  is estimated below...
	uncertainty_cor = 0.0
	uncertainty_uncor = 0.0
	do ipt=ilo,ihi
	  uncertainty_cor = uncertainty_cor+
     +          abs(x(ipt)-mom1)*dy_cor(ipt)
	  uncertainty_uncor = uncertainty_uncor+
     +          ((x(ipt)-mom1)*dy_uncor(ipt))**2
	enddo
        uncertainty_cor = uncertainty_cor/ysum
	uncertainty_uncor = sqrt(uncertainty_uncor)/ysum

	uncertainty = sqrt(uncertainty_cor**2+uncertainty_uncor**2)

	return
	end
