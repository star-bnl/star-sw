* $Id: tph.F,v 1.23 2000/02/18 20:07:57 snelling Exp $
* $Log: tph.F,v $
* Revision 1.23  2000/02/18 20:07:57  snelling
* Changed tph function into tpc functions y(using database)
*
* Revision 1.22  2000/02/03 00:05:13  snelling
* Changed warning reporting, improved switching between 3point Gaussian fit
* and weighted mean.
*
* Revision 1.21  1999/11/20 20:52:58  snelling
* added content to the tphit table and removed hitclus table
*
* Revision 1.20  1999/10/15 00:53:50  nevski
* pgf->g77 cleanup
*
*
      integer function tph(tcl_sector_index_h, tcl_sector_index
     +,                       tclpar_h,        tclpar
     +,			      tsspar_h,        tsspar
     +,                       pad_plane_h,     pad_plane
     +,                       pixel_data_in_h,   pixel_data_in
     +,                       pixel_data_out_h,  pixel_data_out
     +,                       tpseq_h,         tpseq 
     +,                       tpcluster_h,     tpcluster 
     +,                       tphit_h,         tphit) 

      IMPLICIT NONE

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
C   Error conditions : 
C   ================
c-----------------------------------------------------------------------
#include "tph.inc"
c-----------------------------------------------------------------------
c
c functions:
      real    tand, atand
      integer tpc_pad_to_x
      integer tpc_row_to_y
      integer tpc_local_to_global
      integer tpc_drift_velocity !function that returns as a first
c                                !argument the drift velocity
c                                !two next arguments dummy             
c
c      integer tgc_pad_to_x
c      integer tgc_row_to_y
c      integer tgc_local_to_global
c      integer tgc_drift_velocity !function that returns as a first
c                                !argument the drift velocity
c                                !two next arguments dummy             
c
c-----------------------------------------------------------------------
      integer nclpix_max        ! Max. pixels in a cluster
      integer npix_seq_max      ! Max. pixels in a sequence
      integer nrows             ! Number of rows per TPC supersector
      integer max_hits_in_cluster ! Max. number of hits per cluster
      integer Nrows_in
c
      parameter (nclpix_max = 10000)
      parameter (npix_seq_max = 300)
      parameter (nrows = 45)
      parameter (max_hits_in_cluster = 100)
      parameter (Nrows_in = 13)
c
      integer iadc(nclpix_max)  ! ADC's for cluster pixels
      integer ihit              ! Hit index
      integer ipad(nclpix_max)  ! Pads for cluster pixels
      integer ipad_lo, ipad_hi  ! Pad number extent for cluster
      integer irow              ! Row number for cluster (1-45)
      integer isec              ! Sector number for cluster (1-24)
      integer itdc(nclpix_max)  ! Time buckets for cluster pixels
      integer itrk(nclpix_max)  ! Track ID for MC data
      integer itdc_lo, itdc_hi  ! Time bucket extent for cluster
      integer jseq              ! Index for tpseq
      integer jtpcl             ! Sequence index within cluster
      integer nclspix           ! Number of pixels in cluster
      integer nhits             ! Number of hits in cluster
      integer npads             ! Number of pads in cluster's pad row
      integer npix_seq(npix_seq_max) ! Number of pixels in sequence on pad
      integer nseq              ! Number of sequences in cluster
      integer pads_per_row(nrows) ! Listing of number of TPC pads by row
      real de_scale             ! Energy loss / charge
      real dx_prime             ! Uncertainty in x, global coord.
      real filtered_noise       ! Noise from SAS chip, after shaping
      real invec(3)             ! Local x,y,z vector
      real outvec(3)            ! Global x,y,z vector
      real pad_width            ! Pad width (cm)
      real rad                  ! Radius at hit (cm)
      real white_noise          ! SCA and digitization noise
      
      logical first             ! .T. iff first call to this routine
      
c------------for deconvolution -----------
c     
      integer fflagg            ! 1 IFF >1 hit; else 0
      integer istatus
      
      real qq(max_hits_in_cluster) ! Sum of ADC counts
      real x(max_hits_in_cluster) ! Position along row (pad units)
      real dx(max_hits_in_cluster) ! Uncertainty in x
      real prf(max_hits_in_cluster) ! Width along x
      real z(max_hits_in_cluster) ! Position along drift (bucket units)
      real dz(max_hits_in_cluster) ! Uncertainty in z
      real zrf(max_hits_in_cluster) ! Width along z
      real phi(max_hits_in_cluster) ! Orientation of cluster
      real track(max_hits_in_cluster) ! MC track contributing most
      real mu1(max_hits_in_cluster) ! 1st moment of hit (mean)
      real mu2(max_hits_in_cluster) ! 2nd moment of hit (rms)
      real mu3(max_hits_in_cluster) ! 3rd moment of hit (skewness)
      real mu4(max_hits_in_cluster) ! 4th moment of hit (kurtosis)
      real mu5(max_hits_in_cluster) ! Number of pads for hit
      integer ipad_hit_lo(max_hits_in_cluster) ! First pad number for the hits
      integer ipad_hit_hi(max_hits_in_cluster) ! Last pad number for the hits
      integer itdc_hit_lo(max_hits_in_cluster) ! First time bucket for the hits
      integer itdc_hit_hi(max_hits_in_cluster) ! Last time bucket for the hits
c-----------------------------------------------------------------------
      real dvel                 !drift velocity
      real dummy1               !it is what it says
c-----------------------------------------------------------------------
      integer CurrentSector,istart,last_cluster_index
      save last_cluster_index
      data last_cluster_index / 1 /

      logical debug 
      data    debug /.false./
c-----------------------------------------------------------------------
      save first
c-----------------------------------------------------------------------
      character*132 m132
c-----------------------------------------------------------------------
      data first /.true./
      data pads_per_row
     &     / 88,  96, 104, 112, 118,
     &     126, 134, 142, 150, 158,
     &     166, 174, 182,  98, 100,
     &     102, 104, 106, 106, 108,
     &     110, 112, 112, 114, 116,
     &     118, 120, 122, 122, 124,
     &     126, 128, 128, 130, 132,
     &     134, 136, 138, 138, 140,
     &     142, 144, 144, 144, 144 /
c-----------------------------------------------------------------------
c     
c     set status
c     
      tph = STAFCV_OK
      
      if (first) then
         first = .false.
      endif
      
c     noise in ADC counts...
c     sca and digitization noise
      white_noise = sqrt((tsspar(1).sca_rms/tsspar(1).scale)**2+0.5)
c     noise that comes through shaper
      filtered_noise = tsspar(1).white_rms/tsspar(1).scale

      if (tcl_sector_index(1).CurrentSector .lt. 0) then
         CurrentSector = -tcl_sector_index(1).CurrentSector
         istart = 1
      else 
         CurrentSector = tcl_sector_index(1).CurrentSector
         istart = last_cluster_index
      endif

c     Loop over the clusters:
      do jtpcl = istart, tpcluster_h.nok
         nseq = tpcluster(jtpcl).nseq
         jseq = tpcluster(jtpcl).jseq
         isec = tpcluster(jtpcl).tpc_row/100
         if(isec .eq. CurrentSector) then      
            irow = mod(tpcluster(jtpcl).tpc_row,100)
c     ??? Check index?
            npads = pads_per_row(irow)
            if(nseq .ge. tclpar(1).min_nseq) then
               if (irow .gt. Nrows_in) then
                  call tph_get_cluster_data(jseq, nseq, irow, isec,
     &                 npads, tpseq_h, tpseq, 
     &                 pixel_data_out_h, pixel_data_out,
     &                 nclspix, ipad, itdc, iadc, itrk,
     &                 nclpix_max, npix_seq, ipad_lo,
     &                 ipad_hi, itdc_lo, itdc_hi)
               else
                  call tph_get_cluster_data(jseq, nseq, irow, isec,
     &                 npads, tpseq_h, tpseq,
     &                 pixel_data_in_h, pixel_data_in,
     &                 nclspix, ipad, itdc, iadc, itrk,
     &                 nclpix_max, npix_seq, ipad_lo,
     &                 ipad_hi, itdc_lo, itdc_hi)
               endif

c     Check TDC range (0-511) if outside something really wrong
               if(itdc_hi .gt. 511) then
                  write(m132,*) 'TPH - itdc_hi', itdc_hi
                  call StError(m132)
                  itdc_hi = 511
               endif
               if(itdc_lo .lt. 0) then
                  write(m132,*) 'TPH - itdc_lo', itdc_lo
                  call StError(m132)
                  itdc_lo = 0
               endif
               
c     Skip clusters on the end pads (should be done on hit level?).
              if(ipad_lo .eq. 1)then
                 if(debug) then
                    write(m132,*) 
     &                   'TPH - dropping pad 1 cluster in row: ',
     &                   irow
                    call StWarning(m132)
                 endif
                 goto 999
              elseif(ipad_hi .eq. npads)then
                 if(debug) then
                    write(m132,*)
     &                  'TPH - drop end pad cluster, pad: ',
     &                   npads, ' row: ', irow
                    call StWarning(m132)
                 endif
                 goto 999
              endif

              nhits =0 

	      call tph_fit_cluster(tclpar,nseq, nclspix, npix_seq,
     &             ipad, itdc, iadc, itrk, 
     &             white_noise, filtered_noise, nhits,
     &             max_hits_in_cluster, qq, x, dx, prf, z, dz, zrf,
     &             phi,track,mu1,mu2,mu3,mu4,mu5, irow, isec, npads,
     &             ipad_lo, ipad_hi, itdc_lo, itdc_hi,
     &             ipad_hit_lo, ipad_hit_hi, itdc_hit_lo, itdc_hit_hi)

           else
c     do not store clusters with < min_hit_seq  sequences in them
c     thought tcl would take care of that ?
              goto 999
           endif

c     perhaps consistency with simulation demands use of pad_sep_in/out ???

           if(irow .gt. pad_plane(1).nrow_in) then
              pad_width = pad_plane(1).pad_sep_out
              de_scale = tsspar(1).ave_ion_pot*tsspar(1).scale
     &             /(tsspar(1).gain_out*tsspar(1).wire_coupling_out)
           else
              pad_width = pad_plane(1).pad_sep_in
              de_scale = tsspar(1).ave_ion_pot*tsspar(1).scale
     &             /(tsspar(1).gain_in*tsspar(1).wire_coupling_in)
           endif

c     Fill TPCLUSTER Table
           tpcluster(jtpcl).nhits  = nhits
           
           if (nhits .gt. 1) then
              fflagg = 1
           else
              fflagg = 0
           end if
           
c     fill array if fit-isolated cluster is called without mountainfinder
c     if nhits gt 1 then this array is already filled.
           if(nhits .eq. 1) then
              ipad_hit_lo(ihit) = ipad_lo
              ipad_hit_hi(ihit) = ipad_hi
              itdc_hit_lo(ihit) = itdc_lo
              itdc_hit_hi(ihit) = itdc_hi
           endif
           

c     Fill TPHIT Table
           do ihit = 1, nhits

c     Check for not writing over table bounderies              
              if (tphit_h.nok .ge. tphit_h.maxlen) then
                 write(m132,*)'TPH table full - quitting at cluster: ',
     &                jtpcl
                 call StWarning(m132)
                 return
              endif
              
              tphit_h.nok = tphit_h.nok+1
              
              tphit(tphit_h.nok).cluster    = jtpcl
              tphit(tphit_h.nok).flag       = fflagg
	      tphit(tphit_h.nok).id         = tphit_h.nok
c     filled by global
              tphit(tphit_h.nok).id_globtrk = 0
c     filled here only for MC data
              tphit(tphit_h.nok).track      = track(ihit)
c     index values
              tphit(tphit_h.nok).row        = 100*isec+irow
              tphit(tphit_h.nok).nseq       = nseq
c     hit characteristics (used for tracking)
              tphit(tphit_h.nok).npads =
     &             ipad_hit_hi(ihit) - ipad_hit_lo(ihit) + 1     
              tphit(tphit_h.nok).minpad = ipad_hit_lo(ihit)
              tphit(tphit_h.nok).maxpad = ipad_hit_hi(ihit)     
              tphit(tphit_h.nok).ntmbk=
     &             itdc_hit_hi(ihit) - itdc_hit_lo(ihit) + 1         
              tphit(tphit_h.nok).mintmbk = itdc_hit_lo(ihit)
              tphit(tphit_h.nok).maxtmbk = itdc_hit_hi(ihit)
              
c     Put q (magnitude of hit) into units of energy loss
              tphit(tphit_h.nok).q = qq(ihit) * de_scale 
              tphit(tphit_h.nok).dq = 0.
              
c     Transform x(ihit) to local x coordinates invec(1)              
              istatus = tpc_pad_to_x(x(ihit),
     &             float(irow), invec(1), pad_plane_h, pad_plane)
              
c     Transform row to y coordinate invec(2)              
              invec(2) = 0.
              istatus = tpc_row_to_y(float(irow),
     &             invec(2), pad_plane_h, pad_plane)

c     Correct z for weighted mean function used to determine it 
c     (using the drift velocity and tau)
              invec(3) = z(ihit)
              istatus = tpc_drift_velocity(dvel, dummy1, dummy1)
              if (tclpar(1).tfit .eq. 0) then 
                 invec(3) = z(ihit) - 
     &                3.0 * tsspar(1).tau * dvel * 1.0E-6
              endif

c     Transform local to global coordinates !
              istatus = tpc_local_to_global(isec, invec, outvec)

c     Fill hit coordinates
              tphit(tphit_h.nok).x = outvec(1)
              tphit(tphit_h.nok).y = outvec(2)
              tphit(tphit_h.nok).z = outvec(3) 
              
c     zrf already in units of cm
              tphit(tphit_h.nok).zrf = zrf(ihit)
c     transform prf in units of pads --> cm
              tphit(tphit_h.nok).prf = prf(ihit) * pad_width
              
c     Calculate "orientation" of hit in terms of spatial quantities
              tphit(tphit_h.nok).phi = atand(phi(ihit) / pad_width)
c     fill the lambda field of tphit,
c     with the ansatz that the track comes from (0,0,0)
              tphit(tphit_h.nok).lambda = 
     &             abs(atand(tphit(tphit_h.nok).z/rad)) ! lambda > 0 always
c     Set dlambda to 0 can be filled later by global
              tphit(tphit_h.nok).dlambda    = 0.
              
c     Calculate alpha as well -- see Mikes notebook 2, pp 25-32 for
c     explanation... (Where are they???)
              if (tand(tphit(tphit_h.nok).lambda) .ne. 0.0) then
                 if (abs(tphit(tphit_h.nok).phi) .ge. 10.0) then
                    tphit(tphit_h.nok).alpha = atand(
     &                   0.5*tand(tphit(tphit_h.nok).phi)/
     &                   tand(tphit(tphit_h.nok).lambda))
                 else
                    if (tphit(tphit_h.nok).flag .eq. 0) then
                       tphit(tphit_h.nok).alpha = atand(
     &                      tand(tphit(tphit_h.nok).phi)/
     &                      tand(tphit(tphit_h.nok).lambda))
                    else
                       tphit(tphit_h.nok).alpha = atand(
     &                      4.0*tand(tphit(tphit_h.nok).phi)/
     &                      tand(tphit(tphit_h.nok).lambda))
                    endif
                 endif
              else 
                 tphit(tphit_h.nok).alpha = 0.0
              endif 
c     Set dalpha zero can be filled later by global
              tphit(tphit_h.nok).dalpha = 0.

c     Must also rotate dx, dy into global coordinates
              dx_prime = pad_width * dx(ihit)
              rad = sqrt((tphit(tphit_h.nok).x)**2 +
     &             (tphit(tphit_h.nok).y)**2)
              tphit(tphit_h.nok).dx = dx_prime *
     &             abs(tphit(tphit_h.nok).y) / rad
              tphit(tphit_h.nok).dy = dx_prime *
     &             abs(tphit(tphit_h.nok).x) / rad
c     dz already in global coordinates
              tphit(tphit_h.nok).dz = dz(ihit)

c     also, need a fudge factor for the uncertainties dx dy dz.
c     this should be understood and fixed.
              tphit(tphit_h.nok).dx = 0.1
c             tphit(tphit_h.nok).dx+ tclpar(1).dxy_fact
              tphit(tphit_h.nok).dy = 0.1
c             tphit(tphit_h.nok).dy+ tclpar(1).dxy_fact
              tphit(tphit_h.nok).dz = 0.2
              
           enddo

        else if (isec .gt. CurrentSector) then
           goto 555           
        else                    ! isec < Current Sector
           continue
        endif                   ! isec  = Current Sector
        
 999    continue
      enddo
      
 555  continue
      last_cluster_index = jtpcl 
      
      return
      end
