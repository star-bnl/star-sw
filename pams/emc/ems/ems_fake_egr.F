      INTEGER*4 FUNCTION EMS_FAKE_EGR(
     1       tpt_track_h,         tpt_track ,
     2       egr_globtrk_h,       egr_globtrk )
c:>------------------------------------------------------------------
C:FILE:         ems_fake_egr.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University 
C:BUGS:         None known.
C:HISTORY:      02aug96-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_FAKE_EGR
C:DESCRIPTION:  Just copies TPC track info into egr_globtrk
C:ARGUMENTS:    tpt_track
C:ARGUMENTS:    egr_globtrk
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "ems_fake_egr.inc"
C
      INTEGER i,j

      egr_globtrk_h.nok = tpt_track_h.nok
      do i = 1,tpt_track_h.nok
       egr_globtrk(i).icharge = tpt_track(i).q          
       egr_globtrk(i).id      = tpt_track(i).id
       egr_globtrk(i).ndegf   = tpt_track(i).nfit
       egr_globtrk(i).sflag   = tpt_track(i).flag
       egr_globtrk(i).chisq   = (tpt_track(i).chisq(1)
     1                        +  tpt_track(i).chisq(2))/2.
       do j = 1,15
        egr_globtrk(i).cov(j) = tpt_track(i).cov(j)
       end do
       egr_globtrk(i).invpt   = tpt_track(i).invp
       egr_globtrk(i).phi0    = tpt_track(i).phi0
       egr_globtrk(i).psi     = tpt_track(i).psi
       egr_globtrk(i).r0      = tpt_track(i).r0
       egr_globtrk(i).tanl    = tpt_track(i).tanl
       egr_globtrk(i).z0      = tpt_track(i).z0
      end do

      EMS_FAKE_EGR=STAFCV_OK
999   RETURN
      END
