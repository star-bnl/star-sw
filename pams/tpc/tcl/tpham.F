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
C
C   Output arguments : 
C
C   Functional Description : 
C    This module reconstructs hits given cluster data in tpseq/tpcluster
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

#include "tcl_tppixel_pars.inc"
#include "tcl_tppixel_st.inc"
#include "tcl_tpmcpix_pars.inc"
#include "tcl_tpmcpix_st.inc"

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
        parameter (nclpix_max = 100)
        real ipad(nclpix_max),itdc(nclpix_max),iadc(nclpix_max)

        integer isec,irow,itrk,krow

        integer jtpcl, nclspix

        integer mxrow
        parameter (mxrow = 45)
        integer id_pad(mxrow)

        integer ntim, nseq, jseq

        real pad_width, time_bucket, time_bin, tval

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

           if(nseq.ge.3)then

              call tph_get_cls_data(jseq, nseq, id_pad, krow
     +,                             tpseqh, tpseq 
     +,                             tppixel_h, tppixel
     +,                             tpmcpix_h, tpmcpix
     +,                             nclspix, ipad, itdc, iadc, itrk)
 
              tphith.nok = tphith.nok + 1
              tphit(tphith.nok).cluster = jtpcl
              tphit(tphith.nok).row = krow
              tphit(tphith.nok).track = itrk   ! for now this is MC info

              call tph_calc_cntr_of_grvty(nclspix, ipad, itdc, iadc
     +,                                   tphith, tphit )

c
c             now use Gaussian prf assumption in order to refine 
c             time dimension prf/coordinate; could use alternative
c             methods here!
c             ONLY IF ntime <= 4 !
c
              call tph_proj_tim(nclspix, ipad, itdc, iadc 
     +,                             ntim)
              if(ntim.le.4)then
c
c                warning, these units are in time buckets; later convert to cm...
c
                 tphit(tphith.nok).zrf = tph_calc_prf_sigma(ntim, itdc
     +,                                                           iadc)

                 tphit(tphith.nok).z = tph_calc_prf_pos(ntim, itdc, iadc
     +,                                          tphit(tphith.nok).zrf)
              endif

c
c             now use Gaussian prf assumption in order to refine 
c             pad dimension prf/coordinate; could use alternative
c             methods here!
c
              call tph_proj_pad(jseq, nseq, id_pad, krow
     +,                             tpseqh, tpseq 
     +,                             tppixel_h, tppixel
     +,                             ipad, iadc)
c
c             presumption of three pad hits and Gaussian prf-> 
c             recalculate position and prf width using parameterisation
c
              tphit(tphith.nok).prf = tph_calc_prf_sigma(nseq, ipad
     +,                                                        iadc)
              tphit(tphith.nok).x = tph_calc_prf_pos(nseq, ipad, iadc
     +,                                          tphit(tphith.nok).prf)

           elseif(nseq.eq.2)then
c
c             treat two pad hits separately
c
              continue
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
c          zrf in units of time bins --> cm**2
c
           tphit(tphith.nok).zrf 
     +                         = sqrt(tphit(tphith.nok).zrf)*time_bucket
c
c          prf in units of pads --> cm**2
c
c
c          perhaps consistency with simulation demands use of pad_sep_in/out ???
c
           if(mod(isec,2).eq.0)then
              pad_width = pad_plane(1).pad_wid_in
           else
              pad_width = pad_plane(1).pad_wid_out
           endif
           tphit(tphith.nok).prf 
     +                         = sqrt(tphit(tphith.nok).prf) * pad_width
c
c       finally, should calculate dx, dy, and dz ...
c       note that sigma_prf IS NOT sigma_x, etc...

        enddo

c
        return
        end


C-----------------------------------------------------------------------
        Subroutine tph_get_cls_data(jseq, nseq, id_pad, irow
     +,                            tpseqh, tpseq 
     +,                            tppixel_h, tppixel
     +,                            tpmcpix_h, tpmcpix
     +,                            nclspix, ipad, itdc, iadc, itrk)
C-----------------------------------------------------------------------

      IMPLICIT NONE

#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tppixel_pars.inc"
#include "tcl_tppixel_st.inc"
#include "tcl_tpmcpix_pars.inc"
#include "tcl_tpmcpix_st.inc"

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
        integer lvmax

        real iadc(*),itdc(*),ipad(*)
        integer id_pad(*), itrk
        integer jseq,nseq,nclspix,irow
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
C-----------------------------------------------------------------------
        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        iclspix = 0
        ntrk = 0

        do kseq = jseq, jseq+nseq-1
           jpix = tpseq(kseq).jpix
           iclspix = iclspix + 1

           ldatum = tppixel(jpix).datum
c
c          for MC data!
c
           ktrk = tpmcpix(jpix).mcid
           if(ntrk.gt.0)then
              mtrk = ntrk
              do jndx = 1, mtrk
                 if(ltrk(jndx).eq.ktrk)then
                    nocc(jndx)=nocc(jndx)+1.
                 else
                    ntrk = ntrk + 1
                    ltrk(ntrk) = ktrk
                    nocc(ntrk) = 1.
                 endif
              enddo
           else
              ntrk = ntrk + 1
              ltrk(ntrk) = ktrk
              nocc(ntrk) = 1.
           endif

           iadc(iclspix) = real(idatum(1))

           tdc_lo = iand(idatum(2),botmask)
           tdc_hi = ibits(idatum(2),14,2)*'100'X
           itdc(iclspix) = real(tdc_lo + tdc_hi) 

           ipad(iclspix) = real(tpseq(kseq).secpad-id_pad(irow))

           kseqpix = iand(idatum(2),seqmask)/'100'X

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
              mtrk = ntrk
              do jndx = 1, mtrk
                 if(ltrk(jndx).eq.ktrk)then
                    nocc(jndx)=nocc(jndx)+1.
                 else
                    ntrk = ntrk + 1
                    ltrk(ntrk) = ktrk
                    nocc(ntrk) = 1.
                 endif
              enddo

           enddo
        enddo

        nclspix = iclspix
        imax = lvmax(nocc,ntrk)
        itrk = ltrk(imax)

        return
        end

C-----------------------------------------------------------------------
        Subroutine tph_calc_cntr_of_grvty(nclspix, ipad, itdc, iadc
     +,                            tphith, tphit )
C-----------------------------------------------------------------------

      IMPLICIT NONE

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

c       tphit(tphith.nok).q = sum
        max_adc = vmax(iadc,nclspix)
        tphit(tphith.nok).q = max_adc

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

#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tppixel_pars.inc"
#include "tcl_tppixel_st.inc"

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
        elseif(imax+1 .le. nseq)then
c
c          the peak of the pulse is on the "leftmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRF tpc hit at leftmost pad row edge'
     +,                 1, id)
        elseif(imax-1 .ge. 1)then
c
c          the peak of the pulse is on the "rightmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRF tpc hit at rightmost pad row edge'
     +,                 1, id)
        else
           id = 0
           call message('TPHAM-E-PRF error in 3 pad prf calculation'
     +,                 1, id)
        endif

        tph_calc_prf_sigma = pad_width

        return
        end

C-----------------------------------------------------------------------
        Real Function tph_calc_prf_pos(nseq, ipad, iadc, sigma)
C-----------------------------------------------------------------------
      IMPLICIT NONE

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
        elseif(imax+1 .le. nseq)then
c
c          the peak of the pulse is on the "leftmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRF tpc hit at leftmost pad row edge'
     +,                 1, id)
        elseif(imax-1 .ge. 1)then
c
c          the peak of the pulse is on the "rightmost" edge of a pad row
c          should use calculated pad-response width and 2-pad position
c          estimate
c
           id = 0
           call message('TPHAM-I-PRF tpc hit at rightmost pad row edge'
     +,                 1, id)
        else
           id = 0
           call message('TPHAM-E-PRF error in 3 pad prf calculation'
     +,                 1, id)
           return
        endif

        return
        end

C-----------------------------------------------------------------------
        Subroutine tph_proj_tim(nclspix, ipad, itdc, iadc, ntim)
C-----------------------------------------------------------------------

      IMPLICIT NONE

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

