       Integer*4 FUNCTION egr_impactcl(
     1           vertex_h,            vertex ,
     2           egrpar_h,            egrpar ,
     3           globtrk_h,          globtrk )
C>-----------------------------------------------------------------------
C   Input arguments : 
C
C     globtrk_h:  global track structure header
C     globtrk:    global track structure 
C     vertex_h:   dst vertex table header
C     vertex:     dst vertex table, co-ords and pointers to aux tables
C     proximity   parameter specifiying the radial distance from the
C                 primary vertex to which the track is to be extrapolated.
C                 If the extrapolation fails then a value of STAFCV_BAD
C                 is returned
C
C   Output : 
C     globtrk().chisq(2) = confidence level that globtrk points to primary vertex
C     
C   DESCRIPTION  : 
C                  The helix defined by globtrk is extrapolated
C                  to radial distance from primary vertex = proximity.
C                  A constrained fit of the track from
C                  proximity to the primary vertex is done by
C                  requiring the track to pass through the primary
C                  vertex. A confidence level based on the globtrk
C                  chisquare + change of chisquare from the constrained
C                  fit is returned.
C                  
C   Created  April, 2000  by A. C. Saulys
C   Error conditions    : STAFCV_BAD
C<----------------------------------------------------------------------

      IMPLICIT NONE

#include "egr_impactcl.inc"
#include "StTrackMethod.inc"
#include "phys_constants.inc"
#include "math_constants.inc"


      INTEGER impfit

      REAL proximity, rc, phi, step

      INTEGER i,iret,ndegf,ndef

      REAL    bfield(3)
      REAL    mainv(3)
      REAL    pveci(5),pvecis(5),rver1,bchis
      REAL    Prob_cern
      REAL    fcov(6,6) ! covariance matrix of fit
      CHARACTER*132 m132

      DOUBLE PRECISION covi(15)

C---executable statements
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C

C  preset in case of failure

      EGR_IMPACTCL = STAFCV_BAD      

      if((globtrk_h.nok).eq.0) return ! no tracks
      if((vertex_h.nok).eq.0) return ! no vertices


CC:  Load Primary vertex position coordinates:
         do i = 1,vertex_h.nok
            if(vertex(i).vtx_id .eq. 1) then
               mainv(1) = vertex(i).x
               mainv(2) = vertex(i).y
               mainv(3) = vertex(i).z
               go to 100
            end if
         end do
         return
100      continue

C

      call gufld(mainv,bfield)
c
      DO i=1,globtrk_h.nok
       if(globtrk(i).iflag.gt.0)then ! work with good tracks only

C      this will only work if Kalman fit was used
        if((globtrk(i).method.and.2**kKalmanFitId).ne.0)then
         globtrk(i).chisq(2) = 0.0 ! preset in case of failure
         rc = 1./globtrk(i).curvature
         ndef = 2*globtrk(i).n_fit_point-5

C -- use Kalman extrapolator to near primary vertex position
C -- use pion hypthesis, until we have pid
          CALL TPCINI(8,0.001,0.01)
          rver1 = 3.0
CCC        rver1 = proximity
C -- set up working arrays
          phi = globtrk(i).phi0*C_RAD_PER_DEG
C --  put phi 0-2pi
          if(phi.lt.0)phi = phi + C_2PI
          pvecis(1) = globtrk(i).r0*phi                   !r*phi
          pvecis(2) = globtrk(i).z0                       !z
          pvecis(3) = globtrk(i).tanl                     !tanl
          pvecis(4) = globtrk(i).psi*C_RAD_PER_DEG        !psi
          pvecis(5) = globtrk(i).icharge*globtrk(i).invpt ! q/pt
          step = 2.0
          call kalex(step,globtrk(i).r0,pvecis,globtrk(i).covar,rver1,pveci,covi,globtrk(i).pid,iret)
          if(iret.eq.0)then ! successful extrapolation

C --      calculate the impact parameter chisq for charged track at primary vertex

            if(impfit(10,rver1,pveci,covi,bfield(3),mainv,bchis,fcov))then
              ndegf = ndef+2 ! two extra df for fit
              globtrk(i).chisq(2) = Prob_cern(globtrk(i).chisq(1)*ndef+bchis,ndegf)
              EGR_IMPACTCL =STAFCV_OK
            endif ! if(impfit
         endif ! if(iret
        endif ! if( method
       endif ! iflag.gt.0
      ENDDO

 999  CONTINUE
      RETURN
      END





