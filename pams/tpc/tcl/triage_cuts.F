* $Id: triage_cuts.F,v 1.1 1998/01/27 00:49:20 fisyak Exp $
* $Log: triage_cuts.F,v $
* Revision 1.1  1998/01/27 00:49:20  fisyak
* Split sources
*
      logical function triage_cuts(ipad,itdc,iadc,nclspix,tclpar)
c
c performs quick cuts on cluster data to determine whether to go ahead
c  with multi-peak search in cluster.  This is supposed to speed up the
c  processing by not trying multi-peak searches on clusters that have
c  no possibility to have more than one in them (that we can find).
c
c Inputs:
c     ipad:     pad number array for pixels in cluster
c     itdc:     tdc number array for pixels in cluster
c     iadc:     adc number array for pixels in cluster
c     nclspix:  # pixels in cluster
c     tclpar:   switch table for cluster/hitfinder.  has cut values.
c
c Returns: true if should proceed with multi-peak search. false otherwise
c
      implicit none
#include "PAM.inc"
#include "tcl_tclpar.inc"
      RECORD      /TABLE_HEAD_ST/                tclpar_h
      RECORD      /TCL_TCLPAR_ST/               tclpar(*)

C-----------------------------------------------------------------------
c passed variables:
c
        integer iadc(*)               ! ADC's for cluster pixels
        integer ipad(*)               ! Pads for cluster pixels
        integer itdc(*)               ! Time buckets for cluster pixels
        integer nclspix               ! Number of pixels in cluster
c
C-----------------------------------------------------------------------
c local variables:
c
        integer ipix                  ! Pixel index within cluster

        real a                        ! ADC sum in cluster
        real pa                       ! ADC*Pad sum in cluster
        real ppa                      ! ADC*Pad^2 sum in cluster
        real rms_pad                  ! RMS of pad number in cluster
        real rms_tdc                  ! RMS of TDC in cluster
        real ta                       ! ADC*TDC sum in cluster
        real tta                      ! ADC*TDC^2 sum in cluster
C-----------------------------------------------------------------------

        a   = 0.
        pa  = 0.
        ppa = 0.
        ta  = 0.
        tta = 0.
        do ipix=1,nclspix
           a   = a   + iadc(ipix)
           pa  = pa  + ipad(ipix)*iadc(ipix)
           ppa = ppa + ipad(ipix)*(ipad(ipix)*iadc(ipix))
           ta  = ta  + itdc(ipix)*iadc(ipix)
           tta = tta + itdc(ipix)*(itdc(ipix)*iadc(ipix))
        enddo
        rms_pad = sqrt(ppa/a - (pa/a)**2)
        rms_tdc = sqrt(tta/a - (ta/a)**2)

        triage_cuts = (rms_pad+rms_tdc.gt.tclpar(1).triage_rmscut)
        return
        end
