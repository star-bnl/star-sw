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
        parameter (nclpix_max = 1000)
        real ipad(nclpix_max),itdc(nclpix_max),iadc(nclpix_max)

        integer isec,irow,itrk,krow

        integer jtpcl, nclspix

        integer mxrow
        parameter (mxrow = 45)
        integer id_pad(mxrow)

        integer ntim, nseq, jseq

        real pad_width, time_bucket, time_bin, tval
        real shaper_bin
        real rstat

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
                 rstat = tph_calc_prf_sigma(ntim, itdc
     +,                                                           iadc)
                 if(rstat .ne. 0.)then
                    tphit(tphith.nok).zrf = rstat
                 endif
                 rstat = tph_calc_prf_pos(ntim, itdc, iadc
     +,                                          tphit(tphith.nok).zrf)
                 if(rstat .ne. 0.)then
                    tphit(tphith.nok).z = rstat
                 endif
              endif
c
c             final adjustment to z coordinate--we must add the shaper response 
c             in time bins!!!
c  
              tphit(tphith.nok).z = tphit(tphith.nok).z - 0.7*shaper_bin
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
              rstat = tph_calc_prf_sigma(nseq, ipad
     +,                                                        iadc)
              if(rstat .ne. 0.)then
                 tphit(tphith.nok).prf = rstat
              endif
              rstat = tph_calc_prf_pos(nseq, ipad, iadc
     +,                                          tphit(tphith.nok).prf)
              if(rstat .ne. 0.)then
                 tphit(tphith.nok).x = rstat
              endif
           elseif(nseq.le.2)then
c
c             treat two pad hits separately
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
              pad_width = pad_plane(1).pad_wid_out
           else
              pad_width = pad_plane(1).pad_wid_in
           endif
           tphit(tphith.nok).prf 
     +                         = sqrt(tphit(tphith.nok).prf) * pad_width
c
c       finally, should calculate dx, dy, and dz ...
c       note that sigma_prf IS NOT sigma_x, etc...

 999       continue   ! error return for two-pad hits!
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
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
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
        integer lvmax,iucomp

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

        integer id
C-----------------------------------------------------------------------
        data id/0/
        data seqmask/'3F00'X/,botmask/'FF'X/
C-----------------------------------------------------------------------

        iclspix = 0
        ntrk = 0
        call vzero(ltrk,maxtrk)
        call vzero(nocc,maxtrk)

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

