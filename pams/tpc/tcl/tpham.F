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
C
C----------------------------------------------------------------------
C
      INTEGER FUNCTION TPHAM( tclpar_h, tclpar
     +,			      tsspar_h, tsspar
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

#include "tcl_tclpar_pars.inc"
#include "tcl_tclpar_st.inc"
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


      RECORD/ table_head_st/ tclpar_h
      RECORD/ tclpar_row_st/ tclpar(*)
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

c
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
        integer nclpix_max
        parameter (nclpix_max = 10000)
        integer ipad(nclpix_max),itdc(nclpix_max),iadc(nclpix_max)
        integer itrk(nclpix_max)
	integer npix_seq(300)

        integer isec,irow,krow,nhits,ihit

        integer jtpcl, nclspix

        integer mxrow
        parameter (mxrow = 45)
        integer id_pad(mxrow)

        integer ntim, nseq, jseq

        real pad_width, time_bucket, time_bin, tval
        real rstat
	real rad,dx_prime

        real white_noise,filtered_noise
        real de_scale

        double precision invec(3),outvec(3)

        logical first, lstat


c------------ for deconvolution -----------
        integer max_hits_in_cluster
        parameter(max_hits_in_cluster=100)
        real q(max_hits_in_cluster)
        real x(max_hits_in_cluster)
        real dx(max_hits_in_cluster)
        real prf(max_hits_in_cluster)
        real z(max_hits_in_cluster)
        real dz(max_hits_in_cluster)
        real zrf(max_hits_in_cluster)
        real phi(max_hits_in_cluster)
        real track(max_hits_in_cluster)
        integer fflagg


C-----------------------------------------------------------------------
        data first/.true./
C-----------------------------------------------------------------------
	integer ichek	! mal -** temp
        integer iisec,iirow,jjsec

	save first,time_bucket,time_bin,id_pad	

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
     +             + pad_plane(1).npads(irow-1)
           enddo
c     
c->         constants...
c
           time_bucket = detector(1).drift_length/tsspar(1).ntime
           time_bin = time_bucket/tsspar(1).v_drift
c           rms_noise = sqrt((tsspar(1).white_rms/tsspar(1).scale)**2 +
c     +          (tsspar(1).sca_rms/tsspar(1).scale)**2+1.0) ! add 1 for digit
           
        endif

c noise in ADC counts...
c sca and digitization noise
	white_noise = sqrt((tsspar(1).sca_rms/tsspar(1).scale)**2+0.5)
c noise that comes through shaper
        filtered_noise = tsspar(1).white_rms/tsspar(1).scale

        do jtpcl = 1, tpclusterh.nok

c	   if (mod(jtpcl,ichek).eq.1) write(13,*)'cluster #',jtpcl	! mal 

           nseq = tpcluster(jtpcl).nseq
           jseq = tpcluster(jtpcl).jseq
           isec = tpcluster(jtpcl).tpc_row/100
           irow = tpcluster(jtpcl).tpc_row-isec*100
c     note-- now, sector=1..24, row=1..45  mal 30mar95
c           if(mod(isec,2).eq.0)then
c              krow = irow + pad_plane(1).nrow_in
c           else
c              krow = irow
c           endif
           krow=irow
           if(nseq.ge.tclpar(1).min_nseq)then

              call tph_get_cls_data(jseq, nseq, id_pad, krow
     +,                             tpseqh, tpseq 
     +,                             tppixel_h, tppixel
     +,                             tpmcpix_h, tpmcpix
     +,                             nclspix, ipad, itdc, iadc, itrk
     +,				    nclpix_max
     +,				    npix_seq)
 


	      call tph_fit_cluster(tclpar,nseq, nclspix, npix_seq,
     +             ipad, itdc, iadc, itrk,
     +             white_noise, filtered_noise, nhits,
     +             max_hits_in_cluster,
     +             q,x,dx,prf,z,dz,zrf,phi,track,irow)

           else
c
c             do not store clusters with < min_hit_seq  sequences in them
c
              goto 999
           endif


c now I have to deal with tpg convention....
c it wants sector=1..48, row=1..13 for inner, 1..32 for outer sector
c mal 30mar95
           if (irow.gt.pad_plane(1).nrow_in) then ! outer sector
              iisec=isec*2
              iirow=irow-pad_plane(1).nrow_in
           else
              iisec=isec*2-1
              iirow=irow
           endif
c
c          perhaps consistency with simulation demands use of pad_sep_in/out ???
c	   yes, I think so - mal
c
           if(irow.gt.pad_plane(1).nrow_in)then
              pad_width = pad_plane(1).pad_sep_out
              de_scale = tsspar(1).ave_ion_pot*tsspar(1).scale
     +             /tsspar(1).gain_out
           else
              pad_width = pad_plane(1).pad_sep_in
              de_scale = tsspar(1).ave_ion_pot*tsspar(1).scale
     +             /tsspar(1).gain_in
           endif


c store (nothing else for now) the estimated number of hits contributing 
c  to cluster in TPCLUSTER table....
           tpcluster(jtpcl).nhits  = nhits


           fflagg=0
           if (nhits.gt.1) fflagg=1
           DO ihit=1,nhits
              if (tphith.nok.ge.tphit_len) then
                 write(6,*)'TPHIT table full - quitting at cluster',
     +                jtpcl
                 return
              endif
              tphith.nok = tphith.nok+1
	      tphit(tphith.nok).id = tphith.nok		! mal 17jan95
              tphit(tphith.nok).cluster = jtpcl
              tphit(tphith.nok).nseq = nseq
              tphit(tphith.nok).row = 100*isec+krow     	! mal 30mar95
              tphit(tphith.nok).q     = q(ihit)
              tphit(tphith.nok).x     = x(ihit)
              tphit(tphith.nok).dx    = dx(ihit)
              tphit(tphith.nok).prf   = prf(ihit)
              tphit(tphith.nok).z     = z(ihit)
              tphit(tphith.nok).dz    = dz(ihit)
              tphit(tphith.nok).zrf   = zrf(ihit)
              tphit(tphith.nok).phi   = phi(ihit)
              tphit(tphith.nok).track = track(ihit)
              tphit(tphith.nok).flag  = fflagg

c     
c     finally, transform everything to global coordinates !
c     
c     account for "lower-edge" binning in z coordinate
c     
              invec(3)=dble(tphit(tphith.nok).z*time_bucket)
              
c     correct (in average way) for time-offset -- (this is not beautiful)
c     22jun95 mal
cccc  invec(3) = invec(3) - 0.88 now we do this in fit_isolated_cluster
c the offset is shown to be (notebook2 p110, + files..) (n+1)*tau, where
c  n is the (effective) number of stages in the amplifier, and tau is the
c  (effective) time constant.  For STAR, n=2, and tau is set by tss.
              if (tclpar(1).tfit.eq.0) ! average value used
     +             invec(3) = invec(3) - 
     +             dble(3.0*tsspar(1).tau*tsspar(1).v_drift)
              
              invec(2)=tpg_row_to_y(iirow,iisec)
              
              
c     
c     mal 13dec94 - tpg_pad_to_x wants pad passed as integer which screws me
c     up, so I just do tpg's job explicitly...
c     invec(1)=tpg_pad_to_x(tphit(tphith.nok).x,irow,isec)  
              invec(1) = sector(iisec).xrow(iirow) + 
     +             (tphit(tphith.nok).x-1.0)*sector(iisec).pad_x_space
              
              lstat=tpg_local_to_global(iisec,invec,outvec)
              
              if (.not.lstat)
     +             write(6,*)'TPHAM - GLOBAL TRANSFORM FAILED'
              
              tphit(tphith.nok).x = real(outvec(1))
              tphit(tphith.nok).y = real(outvec(2))
              tphit(tphith.nok).z = real(outvec(3))
              
c     
c     zrf in units of time bins --> cm
c     
              tphit(tphith.nok).zrf 
     +             = (tphit(tphith.nok).zrf)*time_bucket
c     
c     prf in units of pads --> cm
c     
              tphit(tphith.nok).prf 
     +             = (tphit(tphith.nok).prf) * pad_width
              
c     also, calculate "orientation" of hit in terms of spatial quantities
              
              tphit(tphith.nok).phi = atand(
     +             tphit(tphith.nok).phi *
     +             time_bucket / pad_width )
              
c     must also rotate dx,dy into global coordinates...
              tphit(tphith.nok).dz = time_bucket*tphit(tphith.nok).dz
              dx_prime = pad_width*tphit(tphith.nok).dx
c     for explanation of what follows, see bottom of notebook p95
              rad = sqrt((tphit(tphith.nok).x)**2+
     +             (tphit(tphith.nok).y)**2)
              tphit(tphith.nok).dx = dx_prime *
     +             abs(tphit(tphith.nok).y)/rad
              tphit(tphith.nok).dy = dx_prime *
     +             abs(tphit(tphith.nok).x)/rad

              
c     while we have these numbers, fill the lambda field of tphit,
c     with the ansatz that the track comes from (0,0,0)
              tphit(tphith.nok).lambda = 
     +             - abs(atand(tphit(tphith.nok).z/rad)) ! lambda < 0 always

c     and I can calcualte alpha as well -- see notebook 2, pp 25-32 for
c     explanation...
              if (tand(tphit(tphith.nok).lambda).ne.0.0) then
                 if (tphit(tphith.nok).lambda.ge.10.0) then
                    tphit(tphith.nok).alpha = atand(
     +                   0.5*tand(tphit(tphith.nok).phi)/
     +                   tand(tphit(tphith.nok).lambda))
                 else
                    if (tphit(tphith.nok).flag.eq.0) then
                       tphit(tphith.nok).alpha = atand(
     +                      tand(tphit(tphith.nok).phi)/
     +                      tand(tphit(tphith.nok).lambda))
                    else
                       tphit(tphith.nok).alpha = atand(
     +                      4.0*tand(tphit(tphith.nok).phi)/
     +                      tand(tphit(tphith.nok).lambda))
                    endif
                 endif
              else
                 tphit(tphith.nok).alpha = 0.0
              endif

c also, need a fudge factor for the uncertainties dx dy dz.
c  this should be understood and fixed, but for now....
              tphit(tphith.nok).dx = tphit(tphith.nok).dx 
     +             * tclpar(1).dxy_fact
              tphit(tphith.nok).dy = tphit(tphith.nok).dy 
     +             * tclpar(1).dxy_fact
              tphit(tphith.nok).dz = tphit(tphith.nok).dz 
     +             * tclpar(1).dz_fact
              

c also put q (magnitude of hit) into units of energy loss
              tphit(tphith.nok).q = tphit(tphith.nok).q * de_scale

c     here, at the very end, I make up for the sins of Bob Hackenburg, who
c     spurns convention, and makes the fast and slow simulator routes have
c     different conventions for sector numbering... best if this becomes
c     unneccesary!!!!!!!!!! 26may95 mal
              jjsec=tphit(tphith.nok).row
              jjsec=(jjsec-irow)/100
              if (jjsec.le.12) then
                 jjsec=jjsec+12
              else
                 jjsec=jjsec-12
              endif
              tphit(tphith.nok).row=jjsec*100+irow
ccccccccc
              
           ENDDO


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
     +,				   nclpix_max
     +,				   npix_seq)
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

        integer iadc(*),itdc(*),ipad(*)
        integer id_pad(*), itrk(*)
        integer jseq,nseq,nclspix,irow
	integer npix_seq(*)
C-----------------------------------------------------------------------
        integer ldatum
c 24mar95 -now data is packed in a new way in tppixel
c        integer*2 idatum(2),tdc_lo,tdc_hi
c        equivalence(idatum(1),ldatum)
c        integer*2 seqmask,botmask
	integer tdc_factor/'400'X/
	integer numseq_factor/'100000'X/

c        integer maxtrk, ntrk, ktrk, imax
        integer ktrk
c        parameter (maxtrk=2000)
c        integer ltrk(maxtrk)		! array of all MC tracks contrib to cluster
c        real nocc(maxtrk)		! array of occurrances of those tracks

        integer kseq,kpix,kseqpix
        integer jpix,iclspix,indx
        integer mtrk,jndx
	integer nclpix_max

        integer id
	integer iseq_num
C-----------------------------------------------------------------------
        data id/0/
c        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        iclspix = 0
c        ntrk = 0
c        call vzero(ltrk,maxtrk)
c        call vzero(nocc,maxtrk)

	iseq_num = 0
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
c this below is a good way to do it but save it for after deconvolution
c           if(ntrk.gt.0 .and. ntrk.lt.maxtrk)then
c              mtrk = iucomp(ktrk,ltrk,ntrk)
c              if(mtrk.eq.0 .and. ntrk.lt.maxtrk)then
c                 ntrk = ntrk + 1
c                 ltrk(ntrk) = ktrk
c                 nocc(ntrk) = 1.
c              elseif(mtrk.gt.0 .and. mtrk.le.maxtrk)then
c                 nocc(mtrk) = nocc(mtrk) + 1.
c              else
c                 id = 0
c                 call message('TPHAM-I-GCD--A, ntrk.gt.maxtrk',1,id)
c                 return
c              endif
c           elseif(ntrk.eq.0)then
c              ntrk = ntrk + 1
c              ltrk(ntrk) = ktrk
c              nocc(ntrk) = 1.
c           else
c              id = 0
c              call message('TPHAM-I-GCD--B, ntrk.gt.maxtrk',1,id)
c	      write(6,*)ntrk,maxtrk,ktrk
c              return
c           endif

c           iadc(iclspix) = real(idatum(1))
c
c           tdc_lo = iand(idatum(2),botmask)
c           tdc_hi = ibits(idatum(2),14,2)*'100'X
c           itdc(iclspix) = real(tdc_lo + tdc_hi) 
c           kseqpix = iand(idatum(2),seqmask)/'100'X
c new way to unpack data
           kseqpix = ldatum/numseq_factor
           itdc(iclspix) = (ldatum - numseq_factor*kseqpix)/
     +		tdc_factor
           iadc(iclspix) = ldatum - numseq_factor*kseqpix 
     +		- tdc_factor*itdc(iclspix)

           ipad(iclspix) = tpseq(kseq).secpad-id_pad(irow)

	   iseq_num = iseq_num+1
	   npix_seq(iseq_num) = kseqpix

           do indx = 2, kseqpix
              iclspix = iclspix + 1
	      if (iclspix.gt.nclpix_max) then
	        id = 0
	        call message('TPHAM-!-TMP too many pixels!',1,id)
	        return
	      endif
              ldatum = tppixel(jpix+indx-1).datum
c              iadc(iclspix) = real(idatum(1))
              itdc(iclspix) = itdc(iclspix-1)+1
              iadc(iclspix) = ldatum - numseq_factor*kseqpix 
     +		- tdc_factor*itdc(iclspix)
              ipad(iclspix) = ipad(iclspix-1)
c
c             for MC data!
c
              ktrk = tpmcpix(jpix).mcid
              itrk(iclspix) = ktrk
c this below is a good way to do it... save it for after deconvolution
c              mtrk = iucomp(ktrk,ltrk,ntrk)
c              if(mtrk.eq.0 .and. ntrk.lt.maxtrk)then
c                 ntrk = ntrk + 1
c                 ltrk(ntrk) = ktrk
c                 nocc(ntrk) = 1.
c              elseif(mtrk.gt.0 .and. mtrk.le.maxtrk)then
c                 nocc(mtrk) = nocc(mtrk) + 1.
c              else
c                 id = 0
c                 call message('TPHAM-I-GCD--C, ntrk.gt.maxtrk',1,id)
c                 return
c              endif

           enddo
        enddo

        nclspix = iclspix
c        imax = lvmax(nocc,ntrk)
c        itrk = ltrk(imax)

c	write(13,*)'ntrk,iclspix for this cluster.....',ntrk,iclspix	! mal -** temp

        return
        end


C-----------------------------------------------------------------------
      Subroutine tph_fit_cluster(tclpar, nseq, nclspix, npix_seq,
     +     ipad, itdc, iadc, itrk,
     +     white_noise,  filtered_noise, nhits,
     +     max_hits_in_cluster,
     +     q,x,dx,prf,z,dz,zrf,phi,track,irow)
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
C     rms_noise: rms noise on adc values
C     max_hits_in_cluster: max # hits allowed in an overlapping pattern
C
C   Output arguments : 
C
C     nhits - estimated number of hits in this cluster
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
#include "tcl_tclpar_pars.inc"
#include "tcl_tclpar_st.inc"
      RECORD/ tclpar_row_st/ tclpar(*)

      integer nseq,nclspix
      integer ipad(*),itdc(*),iadc(*),itrk(*)
      integer npix_seq(*),nhits
      integer irow

      real white_noise,filtered_noise

      integer nhits_nnet        ! neural net estimator function
                                ! of #hits in cluster


      integer max_hits_in_cluster
c        parameter(max_hits_in_cluster=30)
      real q(max_hits_in_cluster)
      real x(max_hits_in_cluster)
      real dx(max_hits_in_cluster)
      real prf(max_hits_in_cluster)
      real z(max_hits_in_cluster)
      real dz(max_hits_in_cluster)
      real zrf(max_hits_in_cluster)
      real phi(max_hits_in_cluster)
      real track(max_hits_in_cluster)

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
     +           q,x,dx,prf,z,dz,zrf,phi,track)
            if (nhits.gt.max_hits_in_cluster) 
     +           write(6,*)'TPHAM - ERROR! array overrun! - fix this!'
         endif
         if (nhits.eq.0) then
            call tph_fit_isolated_cluster(tclpar, nclspix,
     +           ipad, itdc, iadc, itrk,
     +           white_noise, filtered_noise,
     +           q,x,dx,prf,z,dz,zrf,phi,track)
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
         call tph_fit_isolated_cluster(tclpar, nclspix,
     +        ipad, itdc, iadc, itrk,
     +        white_noise, filtered_noise,
     +        q,x,dx,prf,z,dz,zrf,phi,track)
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
#include "tas_structures.inc"
#include "tcl_tclpar_pars.inc"
#include "tcl_tclpar_st.inc"
      RECORD/ tclpar_row_st/ tclpar(*)

      integer ipad(*),itdc(*),iadc(*),nclspix
      real ppa,tta,pa,ta,a,pta ! these are produce sums (ta = tdc(i)*adc(i))
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
      character*1 junk

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

      write(55,*)rms2_pad,rms2_tdc,rms2_mix,tdc_mean,irow,nhits

      return
      end
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      Subroutine mountain_finder(tclpar, nseq, nclspix, npix_seq,
     +     ipad, itdc, iadc, itrk,
     +     white_noise, filtered_noise,
     +     nhits,
     +     q,x,dx,prf,z,dz,zrf,phi,track)
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
C     rms_noise: rms noise on adc values
C
C   Output arguments : 
C
C     nhits     - number of peaks found
C     q         - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C     phi       - "orientation" of cluster
C     track     - MC track contributing the most pixels
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C-----------------------------------------------------------------------
c passed inputs

#include "tas_structures.inc"
#include "tcl_tclpar_pars.inc"
#include "tcl_tclpar_st.inc"
      RECORD/ tclpar_row_st/ tclpar(*)

      integer nseq,nclspix
      integer ipad(*),itdc(*),iadc(*),itrk(*)
      integer npix_seq(*),nhits
      real white_noise,filtered_noise
c passed outputs
      real q(*)
      real x(*)
      real dx(*)
      real prf(*)
      real z(*)
      real dz(*)
      real zrf(*)
      real phi(*)
      real track(*)
c local variables
      integer maxpad,maxbuck
      parameter (maxpad=184,maxbuck=1024) ! upped from 512 4jan95
      integer height(1:maxpad,1:maxbuck)
      integer track_pix(1:maxpad,1:maxbuck)
      logical allowed(0:maxpad+1,0:maxbuck+1) ! go 1 beyond to avoid overwrite
                                              ! memory when laying waste
c      data allowed/95604*.true./
      data allowed/190836*.true./ ! had to increase when up from 512 to 1024
      integer jpad,jtdc,jpix
      integer kpad,ktdc
      integer index_list(10000),indx
      integer previous_height,nmax,imax
      integer nmax_max          ! maximum number of local maxima
      parameter(nmax_max=100)
      integer max_pos(2,nmax_max) ! position (pad,tdc) of local maxima
      integer max_height(nmax_max)
      integer j_maxpad,j_minpad,j_maxtdc,j_mintdc
      integer itemp,maxize,minize
      integer iclosest_pad,iclosest_tdc,i_minimum_height

c the following arrays are for passing to fit_isolated_clusters for
c  each found mountain top
      real qtemp(1),xtemp(1),dxtemp(1),prftemp(1)
      real ztemp(1),dztemp(1),zrftemp(1),phitemp(1),tracktemp(1)
      integer nclspix_temp
      integer ipad_temp(30),itdc_temp(30),iadc_temp(30),itrk_temp(30)

      integer dpad,dtdc,valley
      real slope,peak_to_valley

      integer iwriteout
C-----------------------------------------------------------------------

c fill the reference 2-d arrays...
      call vzero(track_pix,maxpad*maxbuck)
      call vzero(height,maxpad*maxbuck)
      do jpix=1,nclspix
         height(ipad(jpix),itdc(jpix))=iadc(jpix)
         track_pix(ipad(jpix),itdc(jpix))=itrk(jpix)
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
                        do kpad=min(jpad,max_pos(1,imax))+1,
     +                       max(jpad,max_pos(1,imax))-1
                           ktdc=jtdc+nint(slope*float(kpad-jpad))
                           valley = min(valley,height(kpad,ktdc))
                        enddo
                     else                   ! loop over tdc
                                            ! pad depends on tdc
                        do ktdc=min(jtdc,max_pos(2,imax))+1,
     +                       max(jtdc,max_pos(2,imax))-1
                           kpad=jpad+nint(float(ktdc-jtdc)/slope)
                           valley = min(valley,height(kpad,ktdc))
                        enddo
                     endif
                  else          ! peaks on same pad.. walk in time only
                     valley = height(jpad,jtdc)
                     do ktdc=min(jtdc,max_pos(2,imax))+1,
     +                    max(jtdc,max_pos(2,imax))-1
                        valley=min(valley,height(jpad,ktdc))
                     enddo
                  endif
                  if (valley.ne.0) then
                     peak_to_valley = 
     +                    float(height(jpad,jtdc))/float(valley)
                     if (peak_to_valley.lt.tclpar(1).mf_pv)
     +                    goto 19 ! FAILED
                  endif
               enddo
            endif
c ok, passed peak:valley test... store this peak
            nmax=nmax+1
            if (nmax.gt.nmax_max) then
               nmax = nmax_max
               write(6,*)'Hey!... too many peaks... I am bailing...'
               do iwriteout=1,nclspix
                  write(56,*)' '
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
            do kpad=max(jpad-(iclosest_pad-1),1),
     +           min(jpad+(iclosest_pad-1),maxpad)
               do ktdc=max(jtdc-(iclosest_tdc-1),1)
     +              ,min(jtdc+(iclosest_tdc-1),maxbuck)
                  allowed(kpad,ktdc) = .false.
               enddo
            enddo
         else
c lay waste to some adjacent area even if not a peak......
            allowed(jpad+1,jtdc-1)=.false.
            allowed(jpad+1,jtdc)  =.false.
            allowed(jpad+1,jtdc+1)=.false.
            allowed(jpad  ,jtdc-1)=.false.
            allowed(jpad  ,jtdc  )=.false.
            allowed(jpad  ,jtdc+1)=.false.
            allowed(jpad-1,jtdc-1)=.false.
            allowed(jpad-1,jtdc)  =.false.
            allowed(jpad-1,jtdc+1)=.false.
         endif
      enddo

 20   continue

c calculate "positions"...
      nhits=nmax
      if (nmax.eq.1) then       ! use whole cluster in fitting
         call tph_fit_isolated_cluster(tclpar, nclspix,
     +        ipad,itdc,iadc,itrk,
     +        white_noise, filtered_noise,
     +        q,x,dx,prf,z,dz,zrf,phi,track)
      else                      ! just use pixels around local maximum
         do imax=1,nmax
            jpad=max_pos(1,imax)
            jtdc=max_pos(2,imax)
            nclspix_temp = 0
            do kpad=max(jpad-(iclosest_pad-1),1),
     +           min(jpad+(iclosest_pad-1),maxpad)
               do ktdc=max(jtdc-(iclosest_tdc-1),1),
     +              min(jtdc+(iclosest_tdc-1),maxbuck)
                  if (height(kpad,ktdc).ne.0) then
                     nclspix_temp=nclspix_temp+1
                     ipad_temp(nclspix_temp)=kpad
                     itdc_temp(nclspix_temp)=ktdc
                     iadc_temp(nclspix_temp)=height(kpad,ktdc)
                     itrk_temp(nclspix_temp)=track_pix(kpad,ktdc)
                  endif
               enddo
            enddo
            call tph_fit_isolated_cluster(tclpar, nclspix_temp,
     +           ipad_temp,itdc_temp,iadc_temp,itrk_temp,
     +           white_noise, filtered_noise,
     +           qtemp,xtemp,dxtemp,prftemp,ztemp,dztemp,zrftemp,
     +           phitemp,tracktemp)
            q(imax)     = qtemp(1)
            x(imax)     = xtemp(1)
            dx(imax)    = dxtemp(1)
            prf(imax)   = prftemp(1)
            z(imax)     = ztemp(1)
            dz(imax)    = dztemp(1)
            zrf(imax)   = zrftemp(1)
            phi(imax)   = phitemp(1)
            track(imax) = tracktemp(1)
         enddo
      endif

c finally, put back array allowed the way you found it.
      itemp=maxize(ipad(1),nclspix)
      j_maxpad=min(ipad(itemp)+iclosest_pad,maxpad)
      itemp=minize(ipad(1),nclspix)
      j_minpad=max(ipad(itemp)-iclosest_pad,1)

      itemp=maxize(itdc(1),nclspix)
      j_maxtdc=min(itdc(itemp)+iclosest_tdc,maxbuck)
      itemp=minize(itdc(1),nclspix)
      j_mintdc=max(itdc(itemp)-iclosest_tdc,1)
      do jpad=j_minpad,j_maxpad
         do jtdc=j_mintdc,j_maxtdc
            allowed(jpad,jtdc) = .true.
         enddo
      enddo
      return
      end



C-----------------------------------------------------------------------
      Subroutine tph_fit_isolated_cluster(tclpar, nclspix,
     +				ipad, itdc, iadc, itrk,
     +				white_noise, filtered_noise,
     +                          q,x,dx,prf,z,dz,zrf,phi,track)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:   switch table for tcl
C     nclspix:	number of pixels in cluster
C     ipad:	for each pixel, what is pad #
C     itdc:	for each pixel, what is bucket #
C     iadc:	for each pixel, what is adc value
C     itrk:     for each pixel, what MC track contributed most
C     rms_noise: rms noise on adc values
C
C   Output arguments : 
C
C     q         - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C     phi       - "orientation" of cluster
C     track     - MC track contributing the most pixels
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


c passed inputs
#include "tas_structures.inc"
#include "tcl_tclpar_pars.inc"
#include "tcl_tclpar_st.inc"
      RECORD/ tclpar_row_st/ tclpar(*)

	integer nclspix
	integer ipad(*),itdc(*),iadc(*),itrk(*)
	integer nhits

c passed outputs
        real q(*)
        real x(*)
        real dx(*)
        real prf(*)
        real z(*)
        real dz(*)
        real zrf(*)
        real phi(*)
        real track(*)

        integer tph_3point_gauss

	real centroid,sigma,uncertainty,sum,dt0
        real white_noise,filtered_noise
C-----------------------------------------------------------------------

        integer ipix,adc_sum
        integer npads_max,nbuck_max
        parameter(npads_max=184,nbuck_max=1024)
        integer pad_proj(npads_max),time_proj(nbuck_max)
        integer numpix_pad_proj(npads_max)
        integer numpix_time_proj(nbuck_max)
        integer tdc_x_adc(npads_max)
        integer nmc_tracks_max,nmc_tracks
        parameter(nmc_tracks_max=2000)
        integer imc_track(nmc_tracks_max),nocc_track(nmc_tracks_max)

        real identity_vector(nbuck_max) ! useful for x-axis of projections
        real rvec(nbuck_max),d_rvec(nbuck_max) ! temp arrays
        real d_rvec_cor(nbuck_max),d_rvec_uncor(nbuck_max) ! SAS, SCA noise
        logical first/.true./
        integer mtrk,iucomp,ktrk

        real slope,b,badness    ! for linear fit

        integer imax,lvsimx
        integer id,indx

        logical lookat_mc
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

	call vzero(numpix_pad_proj,npads_max)
	call vzero(numpix_time_proj,nbuck_max)
        call vzero(pad_proj,npads_max)
        call vzero(time_proj,nbuck_max)
        call vzero(tdc_x_adc,npads_max)
        adc_sum = 0
        do ipix=1,nclspix
           adc_sum = adc_sum+iadc(ipix)
           pad_proj(ipad(ipix)) = pad_proj(ipad(ipix))+iadc(ipix)
           numpix_pad_proj(ipad(ipix))=numpix_pad_proj(ipad(ipix))+1
           numpix_time_proj(itdc(ipix))=numpix_time_proj(itdc(ipix))+1
           time_proj(itdc(ipix)) = time_proj(itdc(ipix))+iadc(ipix)
           tdc_x_adc(ipad(ipix)) = tdc_x_adc(ipad(ipix))+
     +          itdc(ipix)*iadc(ipix)
        enddo
c q:
        q(1) = adc_sum

c x,dx,prf: - try 3-point gauss... if does not work, do weighted mean
        call vfloat(pad_proj,rvec,npads_max)
        do indx=1,npads_max
           if (pad_proj(indx).ne.0) then
              d_rvec(indx) = 
     +          sqrt((float(numpix_pad_proj(indx))*filtered_noise)**2+
     +          (float(numpix_pad_proj(indx))*white_noise**2))
           else
              d_rvec(indx)=0.0
           endif
           d_rvec_cor(indx) = 0.0
        enddo
        if (tph_3point_gauss(identity_vector,rvec,d_rvec,
     +       npads_max,centroid,sigma,uncertainty).ne.1) 
     +       call tph_weighted_mean(identity_vector,rvec,
     +       d_rvec_cor,d_rvec,
     +       npads_max,centroid,sigma,uncertainty)
        x(1) = centroid
        dx(1) = uncertainty
        prf(1) = sigma

c z,dz,zrf: - do weighted mean
c  for uncertainties, see notebook2 p113
        call vfloat(time_proj,rvec,nbuck_max)
        do indx=1,nbuck_max
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
c           call tph_fit_pulse(identity_vector,rvec,d_rvec,
c     +       nbuck_max,centroid,sigma,uncertainty)
c       else                          ! simple weighted mean
        call tph_weighted_mean(identity_vector,rvec,
     +       d_rvec_cor,d_rvec_uncor,
     +       nbuck_max,centroid,sigma,uncertainty)
        centroid = centroid     ! phase correction done at top level....
c     endif
        z(1) = centroid
        dz(1) = uncertainty
        zrf(1) = sigma

c orientation phi:
	call vfloat(tdc_x_adc,rvec,npads_max)
	call vfloat(pad_proj,d_rvec,npads_max)
        do indx=1,npads_max
           if (pad_proj(indx).ne.0) rvec(indx) = rvec(indx)/d_rvec(indx)
        enddo
	call lfitw(identity_vector,rvec,d_rvec,npads_max,0,
     +       slope,b,badness)
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
	Subroutine tph_fit_pulse(tseq, aseq, daseq, npts_max,
     +				 t0, tau, dt0)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tseq:	tdc values for the pulse to be fit
C     aseq:	adc values for the pulse to be fit
C     daseq:	adc values for the pulse to be fit
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
	integer npts_max
	real tseq(npts_max),aseq(npts_max),daseq(npts_max)
	real t0,tau,dt0
C-----------------------------------------------------------------------
	integer lvmax
	integer N
	parameter(N=2)          ! for STAR
	real B
	real y1,y2,y3,t1,t2,t3,dy1,dy2,dy3
	integer imax
	integer id,istart
        real delta,root1B,rat
        logical retry
C-----------------------------------------------------------------------

	imax = lvmax(aseq,npts_max)

        if ((imax.eq.1).or.(aseq(imax-1).lt.1.0e-14)) then ! max at early edge
           istart=imax
        elseif ((imax.eq.npts_max).or.(aseq(imax+1).lt.1.0e-14)) then !lt edge
           istart=imax-2
        else                    ! "normal"
           istart=imax-1
        endif

        retry=.false.

 1      t1 = tseq(istart)
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
        call tph_weighted_mean(tseq,aseq,daseq,daseq,npts_max,
     +       t0,tau,dt0)
c     should correct for phase here!!!!!!!! *** mal 27jul95
        return
	end


C-----------------------------------------------------------------------
	integer function tph_3point_gauss(x, y, dy, npts,
     +				centroid, sigma, uncertainty)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     y		- y(x) distribution to fit to gaussian
C     dy	- uncertainties on y(x)
C     x		- x coordinate for each point
C     npts	- number of samples in distribution
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
c   modificattions:
c   ==============
C
C   Error conditions : 
C   ================
C
C     If the distribution is not well described by a gaussian, (i.e.
C	it has a peak on one edge and is not monotonic) the function
C       returns a value of -1
C
C-----------------------------------------------------------------------
	real y(*),x(*),dy(*)
	integer npts
	real centroid,sigma,uncertainty,x0
C-----------------------------------------------------------------------
	integer lvmax
	integer ipt,imax,istart,id
	real y1,y2,y3,delta,r213

        tph_3point_gauss = 1
        
        if (npts.lt.3) then
           tph_3point_gauss = -1
           return
        endif

	imax = lvmax(y,npts)
        
	if ((imax.ge.2).and.(imax.lt.npts)) then ! normal - max not at edge
           istart=imax-1
        elseif (imax.eq.1) then ! maximum occupancy at first point
           if (y(2).le.y(3)) then
              id = 0
              tph_3point_gauss = -1
              call message('TPH_3POINT_GAUSS bad shape',
     +             1,id)
              return
           endif
           istart=1
	else                    ! maximum occupancy at last point
           if (y(npts-1).le.y(npts-2)) then
              id = 0
              tph_3point_gauss = -1
              call message('TPH_3POINT_GAUSS bad shape',
     +             1,id)
              return
           endif
           istart=npts-2
	endif

	y1 = y(istart)
	y2 = y(istart+1)
	y3 = y(istart+2)

        if (y1*y3.lt.1.0e-10) then ! zero occupancy - no can do
           tph_3point_gauss=-1
           return
        endif

	delta = x(istart+1) - x(istart)
	if (delta.ne.1.0) then
	  id = 0
	  call message('TPH_3POINT_GAUSS unexpected delta .ne. 1',
     +			1,id)
          tph_3point_gauss = -1
	  return
	endif

	r213 = log(y2*y2/(y1*y3))
        if (r213.le.0.0) then
           id = 0
           tph_3point_gauss = -1
           call message('TPH_3POINT_GAUSS bad shape - r213',
     +          1,id)
           return
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
     +     npts,
     +     mom1, mom2, uncertainty)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     x		- x coordinate of each point
C     y		- y(x) distribution
C     dy_cor	- uncertainty in y(x) that is CORRELATED
C     dy_uncor	- uncertainty in y(x) that is UNCORRELATED
C     npts	- number of samples in distribution
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
	integer npts
	real mom1,mom2,uncertainty_cor,uncertainty_uncor,uncertainty
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
        mom2 = sqrt(mom2/ysum - mom1**2)
c
c the uncertainty from the correlated noise and uncorrelated noise
c  is estimated below...
	uncertainty_cor = 0.0
	uncertainty_uncor = 0.0
	do ipt=1,npts
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


c ****** what follows here are old routines no longer in use ******
c********************************************************************


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


      integer function nhits_nnet(nseq,nclspix,npix_seq,ipad,itdc,iadc)
c
c this is an experimental neural net function that should return the number of hits
c  that have contributed to a given cluster *****
c
c
C     nseq:	number of sequences in cluster
C     nclspix:	number of pixels in cluster
C     npix_seq: number of pixels in each sequence in cluster
C     ipad:	for each pixel, what is pad #
C     itdc:	for each pixel, what is bucket #
C     iadc:	for each pixel, what is adc value
c
      implicit none
c this include file contains neural net architecture
#include "tcl_nnet.inc"
      
      integer nseq,nclspix
      integer ipad(*),itdc(*),iadc(*)
      integer npix_seq(*)

      integer minpad,maxpad,mintdc,maxtdc
      integer ix,iy,ig,ng,nsamp
      parameter(ng=12,nsamp=12)
      real g(1:ng)
      real a(1:nsamp,1:nsamp)   ! for passing to correlated moments routine
      real f_nnet
      integer ipic

      nhits_nnet = 1            ! this is the default value
      
c (1) pack cluster in array a...
c find minimum and maximum pad, time bucket #
      do ix=1,nsamp
         do iy=1,nsamp
            a(ix,iy) = 0.0
         enddo
      enddo
      minpad=9999
      maxpad=-9999
      mintdc=9999
      maxtdc=-9999
      do ipic=1,nclspix
         if (ipad(ipic).lt.minpad) minpad=ipad(ipic)
         if (ipad(ipic).gt.maxpad) maxpad=ipad(ipic)
         if (itdc(ipic).lt.mintdc) mintdc=itdc(ipic)
         if (itdc(ipic).gt.maxtdc) maxtdc=itdc(ipic)
      enddo
      if ((maxpad-minpad+1).gt.nsamp) then
         write(6,*)'ERROR - nsamp too small (pad)'
         return
      endif
      if ((maxtdc-mintdc+1).gt.nsamp) then
         write(6,*)'ERROR - nsamp too small (tdc)'
         return
      endif
      do ipic=1,nclspix
         ix = ipad(ipic) - minpad + 1
         iy = itdc(ipic) - mintdc + 1
         a(ix,iy) = iadc(ipic)
      enddo

c (2) calculate reduced correlated moments (net preprocessor)      
      call calculate_correlated_moments(a,nsamp,g,ng)
      do ig=1,ng                ! correlated moments are net input
         s(0,ig) = g(ig)
      enddo

c (3) process through net
      do il=1,n_layer
         do in=1,nneur(il)
            sum = 0.0
            do jn=1,nneur(il-1)
               sum = sum + w(il,in,jn)*s(il-1,jn)
            enddo
            s(il,in) = f_nnet(sum - t(il,in),il)
         enddo
      enddo

      nhits_nnet = nint(s(n_layer,1))
      
      end

c----------------------------------------------
      real function f_nnet(x,l)
      implicit none
      real x
      integer l
c     transfer function, which depends on layer
      goto (1,2,3) l
 1    f_nnet = tanh(x)
      return
 2    f_nnet = tanh(x)
      return
 3    f_nnet = x
      return
      end
c----------------------------------------------



      subroutine calculate_correlated_moments(a,nsamp,g,ng)
      implicit none
      integer nsamp,ng
      integer nsamp_i,nsamp_j
      integer iprime,jprime,ipq_combo
      real a(1:nsamp,1:nsamp),g(1:ng)
      integer ip,im,m,i,j,index1,index2,iq
      integer ik,k
      real gmom,asum,ave_j,ave_i
c     
c     based on p116 in STAR notebook
c     
      
      nsamp_i = nsamp
      nsamp_j = nsamp
      
      do ipq_combo=1,3
         if (ipq_combo.eq.1) then
            ip = 2
            iq = 0
            index1 = 0
         elseif (ipq_combo.eq.2) then
            ip = 0
            iq = 2
            index1=4
         elseif (ipq_combo.eq.3) then
            ip = 2
            iq = 2
            index1=8
         endif
         index2 = 0
         do im=0,2,2
            m = nsamp_i/2**im
            do ik=0,2,2
               k = nsamp_i/2**ik
               index2=index2+1
c     ok, now we have decided what is p,q and m,k... now calculate G_(p,q)(m,k)
               gmom = 0.0
               do i=1,nsamp_i-m+1
                  do j=1,nsamp_j-k+1
                     asum = 0.0
                     ave_i = 0.0
                     ave_j = 0.0
                     do iprime=i,i+m-1
                        do jprime=j,j+k-1
                           asum = asum + a(iprime,jprime)
                           ave_i = ave_i+iprime*a(iprime,jprime)
                           ave_j = ave_j+jprime*a(iprime,jprime)
                        enddo
                     enddo
                     if (asum.gt.0.0) then
                        ave_i = ave_i/asum
                        ave_j = ave_j/asum
                        do iprime=i,i+m-1
                           do jprime=j,j+k-1
                              gmom = gmom + 
     +                             (float(iprime)-ave_i)**ip *
     +                             (float(jprime)-ave_j)**iq *
     +                             a(iprime,jprime)/asum
                           enddo
                        enddo
                     endif
                  enddo
               enddo
c     scale it pretty now...
               gmom = ((float(m)/float(nsamp_i))*
     +              (float(k)/float(nsamp_j))*
     +              gmom)**(1.0/float((ip+1)*(iq+1)))
               g(index1+index2) = gmom
            enddo
         enddo
      enddo
      return
      end




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
c        integer*2 idatum(2),tdc_lo,tdc_hi
c        equivalence(idatum(1),ldatum)
c        integer*2 seqmask,botmask
c data now packed in new way.... malisa 24mar95
        integer nseq_factor/'100000'X/
        integer tdc_factor/'400'X/
        integer itdc

        integer kseq,kpix,kseqpix
        integer jpix,iclspix,indx
        integer iseq

C-----------------------------------------------------------------------
c        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        iseq = 0

        do kseq = jseq, jseq+nseq-1
           iseq = iseq + 1
           jpix = tpseq(kseq).jpix

           ldatum = tppixel(jpix).datum
           kseqpix = ldatum/nseq_factor
           itdc = (ldatum - kseqpix*nseq_factor)/tdc_factor
c           iadc(iseq) = real(idatum(1))
           iadc(iseq) = real(ldatum - kseqpix*nseq_factor -
     +		 itdc*tdc_factor)

           ipad(iseq) = real(tpseq(kseq).secpad-id_pad(irow))

c           kseqpix = iand(idatum(2),seqmask)/'100'X

           do indx = 2, kseqpix
              ldatum = tppixel(jpix+indx-1).datum
              itdc = (ldatum - kseqpix*nseq_factor)/tdc_factor
              iadc(iseq) = iadc(iseq) + real(ldatum - 
     +		kseqpix*nseq_factor - itdc*tdc_factor)
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

