* $Id: tph.F,v 1.1 1998/05/22 00:18:18 hardtke Exp $
* $Log: tph.F,v $
* Revision 1.1  1998/05/22 00:18:18  hardtke
* modified hit finder with new data format
*
* Revision 1.51  1998/03/20 06:05:22  sakrejda
* 10.0e-6 changed to 1.0e-6 in tpeam and tph, maching changes in tss
*
* Revision 1.50  1998/02/20 22:18:43  sakrejda
* tsspar.v_drift replaced with a call to tgc_drift_velocity
*
* Revision 1.49  1998/02/09 20:11:01  sakrejda
* Test mailing list
*
* Revision 1.48  1998/01/27 00:49:19  fisyak
* Split sources
* testing for Yuri 1998/02/09
*
      INTEGER FUNCTION TPH(tcl_sector_index_h, tcl_sector_index
     +,                       tclpar_h,        tclpar
     +,			      tsspar_h,        tsspar
     +,                       detector_h,      detector
     +,                       pad_plane_h,     pad_plane
     +,                       pixel_data_in_h,   pixel_data_in
     +,                       pixel_data_out_h,  pixel_data_out
     +,                       tpseq_h,         tpseq 
     +,                       tpcluster_h,     tpcluster 
     +,                       tphit_h,         tphit 
     +,                       tphit_aux_h,     tphit_aux)

      IMPLICIT NONE

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
C   Input arguments  (One to a line with definition after ! ) 
C
C     tsspar_h:
C     tsspar:
C     detector_h:
C     detector:
C     pad_plane_h:
C     pad_plane:
C     pad_data_in_h
C     pad_data_in:
C     pad_data_out_h
C     pad_data_out:
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
c   modifications:
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
c        is stored as tph.F.pre_gstar
c
c     4apr96
c      - we finally get rid of the convention in which the pad is numbered
c	 *in the sector*.  Now it is the pad number in the row...
c
C     22Dec1997-WAL
C      - Change to real data approach - i.e. every pad can have a different
C        z calibration.  For now do it by converting every pixel to z
C        when passing the cluster time averages to tph_weighted_mean.
C
C     19Feb1998-IMS
C     replaced tsspar.v_drift by a call to tgc_drift_velocity
C
C     21May1998-D. Hardtke
C      -tpham become tph for new data structures.  Now must be called on
C       a sector by sector basis.  The current sector being processed is
C       controlled by tcl_sector_index.  Note that for the first sector
C       to be processed in an event, tcl_sector_index MUST be negative.
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tph.inc"
C-----------------------------------------------------------------------
c
c functions:
        integer tgc_pad_to_x
        integer tgc_row_to_y
        integer tgc_local_to_global
c
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
        integer nclpix_max            ! Max. pixels in cluster
        integer nrows                 ! Number of rows per TPC supersector
c
        parameter (nclpix_max=10000)
        parameter (nrows=45)
c
        integer iadc(nclpix_max)      ! ADC's for cluster pixels
        integer ihit                  ! Hit index
        integer ipad(nclpix_max)      ! Pads for cluster pixels
        integer ipad_lo, ipad_hi      ! Pad number extent for cluster
        integer irow                  ! Row number for cluster (1-45)
        integer isec                  ! Sector number for cluster (1-24)
        integer itdc(nclpix_max)      ! Time buckets for cluster pixels
        integer itrk(nclpix_max)      ! Track ID for MC data
        integer itdc_lo, itdc_hi      ! Time bucket extent for cluster
        integer jseq                  ! Index for tpseq
        integer jtpcl                 ! Sequence index within cluster
        integer nclspix               ! Number of pixels in cluster
        integer nhits                 ! Number of hits in cluster
        integer npads                 ! Number of pads in cluster's pad row
	integer npix_seq(300)         ! Number of pixels in sequence on pad
        integer nseq                  ! Number of sequences in cluster
        integer pads_per_row(nrows)   ! Listing of number of TPC pads by row

        real de_scale                 ! Energy loss / charge
	real dx_prime                 ! Uncertainty in x, global coord.
        real filtered_noise           ! Noise from SAS chip, after shaping
        real invec(3)                 ! Local x,y,z vector
        real outvec(3)                ! Global x,y,z vector
        real pad_width                ! Pad width (cm)
	real rad                      ! Radius at hit (cm)
        real white_noise              ! SCA and digitization noise

        logical first                 ! .T. iff first call to this routine

c------------ for deconvolution -----------
        integer max_hits_in_cluster   ! Max. number of hits per cluster
        parameter(max_hits_in_cluster=100)
c
        integer fflagg                ! 1 IFF >1 hit; else 0
        integer istatus

        real qq(max_hits_in_cluster)  ! Sum of ADC counts
        real x(max_hits_in_cluster)   ! Position along row (pad units)
        real dx(max_hits_in_cluster)  ! Uncertainty in x
        real prf(max_hits_in_cluster) ! Width along x
        real z(max_hits_in_cluster)   ! Position along drift (bucket units)
        real dz(max_hits_in_cluster)  ! Uncertainty in z
        real zrf(max_hits_in_cluster) ! Width along z
        real phi(max_hits_in_cluster) ! Orientation of cluster
        real track(max_hits_in_cluster)  ! MC track contributing most
        real mu1(max_hits_in_cluster) ! 1st moment of hit (mean)
        real mu2(max_hits_in_cluster) ! 2nd moment of hit (rms)
        real mu3(max_hits_in_cluster) ! 3rd moment of hit (skewness)
        real mu4(max_hits_in_cluster) ! 4th moment of hit (kurtosis)
	real mu5(max_hits_in_cluster) ! Number of pads for hit
C-----------------------------------------------------------------------
        real dvel                     !drift velocity
        real dummy1                   !it is what it says
        integer tgc_drift_velocity    !function that returns as a first
c                                     !argument the drift velocity
c                                     !two next arguments dummy             
C-----------------------------------------------------------------------
        integer CurrentSector,istart,last_cluster_index
        integer Nrows_in
        parameter(Nrows_in=13)
        save last_cluster_index
        data last_cluster_index / 1 /
       
C-----------------------------------------------------------------------
        data first/.true./
        data pads_per_row
     &       / 88,  96, 104, 112, 118,
     &        126, 134, 142, 150, 158,
     &        166, 174, 182,  98, 100,
     &        102, 104, 106, 106, 108,
     &        110, 112, 112, 114, 116,
     &        118, 120, 122, 122, 124,
     &        126, 128, 128, 130, 132,
     &        134, 136, 138, 138, 140,
     &        142, 144, 144, 144, 144 /
C-----------------------------------------------------------------------
	save first
C-----------------------------------------------------------------------
c
c       set status
c
        tph = STAFCV_OK

        if(first)then
           first = .false.
c     
c->         constants...
c
        endif

c noise in ADC counts...
c sca and digitization noise
	white_noise = sqrt((tsspar(1).sca_rms/tsspar(1).scale)**2+0.5)
c noise that comes through shaper
        filtered_noise = tsspar(1).white_rms/tsspar(1).scale

c Loop over the sequences in this cluster:
        If (tcl_sector_index(1).CurrentSector.lt.0) then
         CurrentSector = -tcl_sector_index(1).CurrentSector
         istart = 1
        else 
         CurrentSector = tcl_sector_index(1).CurrentSector
         istart = last_cluster_index
        endif

        do jtpcl = istart, tpcluster_h.nok
           nseq = tpcluster(jtpcl).nseq
           jseq = tpcluster(jtpcl).jseq
           isec = tpcluster(jtpcl).tpc_row/100
         if(isec.eq.CurrentSector) then      
           irow = tpcluster(jtpcl).tpc_row-isec*100
c ??? Check index?
           npads = pads_per_row(irow)
           if(nseq.ge.tclpar(1).min_nseq)then

            if (irow.gt.Nrows_in) then
              call tph_get_cluster_data(jseq, nseq, irow, isec, npads
     +,                             tpseq_h, tpseq 
     +,                             pixel_data_out_h, pixel_data_out
     +,                             nclspix, ipad, itdc, iadc, itrk
     +,				    nclpix_max, npix_seq, ipad_lo
     +,				    ipad_hi, itdc_lo, itdc_hi)
 
            else
              call tph_get_cluster_data(jseq, nseq, irow, isec, npads
     +,                             tpseq_h, tpseq 
     +,                             pixel_data_in_h, pixel_data_in
     +,                             nclspix, ipad, itdc, iadc, itrk
     +,				    nclpix_max, npix_seq, ipad_lo
     +,				    ipad_hi, itdc_lo, itdc_hi)
            endif
 
	      call tph_fit_cluster(tclpar,nseq, nclspix, npix_seq,
     +             ipad, itdc, iadc, itrk, 
     +             white_noise, filtered_noise, nhits,
     +             max_hits_in_cluster, qq, x, dx, prf, z, dz, zrf,
     +             phi,track,mu1,mu2,mu3,mu4,mu5, irow, isec, npads,
     +             ipad_lo, ipad_hi, itdc_lo, itdc_hi)

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
c
c store (nothing else for now) the estimated number of hits contributing 
c  to cluster in TPCLUSTER table....
           tpcluster(jtpcl).nhits  = nhits

           if (nhits.gt.1) then
              fflagg = 1
           else
              fflagg = 0
           end if
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
              invec(3)=tphit(tphit_h.nok).z
              
c     correct (in average way) for time-offset -- (this is not beautiful)
c     22jun95 mal
c     the offset is shown to be (notebook2 p110, + files..) (n+1)*tau, where
c     n is the (effective) number of stages in the amplifier, and tau is the
c     (effective) time constant.  For STAR, n=2, and tau is set by tss.
c             replace tsspar(1).v_drift by a call to
c             tgc_drift_velocity call, should be figured out and properly
c             fixed by having this call in the initialisation phase
              istatus = tgc_drift_velocity(dvel,dummy1,dummy1)
              if (tclpar(1).tfit.eq.0) ! average value used
     +             invec(3) = invec(3) - 
c                  the following line was commented out and replaced by
c                  one line after that
c                  iwona sakrejda
c     +             3.0*tsspar(1).tau*tsspar(1).v_drift
     +             3.0*tsspar(1).tau*dvel* 1.0e-6
              
              
              istatus = tgc_row_to_y(float(irow),
     +             invec(2),pad_plane_h,pad_plane)
              
              istatus = tgc_pad_to_x(tphit(tphit_h.nok).x,
     +             float(irow),invec(1),pad_plane_h,pad_plane)
              
              istatus = tgc_local_to_global(isec,invec,outvec)
              
              tphit(tphit_h.nok).x = outvec(1)
              tphit(tphit_h.nok).y = outvec(2)
              tphit(tphit_h.nok).z = outvec(3) 
c     
c     zrf already in units of cm
c     
c     prf in units of pads --> cm
c     
              tphit(tphit_h.nok).prf 
     +             = (tphit(tphit_h.nok).prf) * pad_width
              
c     also, calculate "orientation" of hit in terms of spatial quantities
              
              tphit(tphit_h.nok).phi = atand(
     +             tphit(tphit_h.nok).phi / pad_width )
              
c     must also rotate dx,dy into global coordinates...
              dx_prime = pad_width*tphit(tphit_h.nok).dx
c     for explanation of what follows, see bottom of notebook p95
              rad = sqrt((tphit(tphit_h.nok).x)**2+
     +             (tphit(tphit_h.nok).y)**2)
              tphit(tphit_h.nok).dx = dx_prime *
     +             abs(tphit(tphit_h.nok).y)/rad
              tphit(tphit_h.nok).dy = dx_prime *
     +             abs(tphit(tphit_h.nok).x)/rad
c              
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

          Else if (isec.gt.CurrentSector) then
           last_cluster_index = jtpcl 
           goto 555           
          Else         ! isec < Current Sector
           continue
          ENDIF        ! isec  = Current Sector

 999       continue
        enddo
c
 555    continue
        return
        end
