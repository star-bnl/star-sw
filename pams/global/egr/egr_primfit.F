       Integer*4 FUNCTION egr_primfit(
     1           vertex_h,            vertex ,
     2           egrpar_h,            egrpar ,
     3           globtrk_h,          globtrk ,
     4           primtrk_h,          primtrk )
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
C     primtrk = primary track fit
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

#include "egr_primfit.inc"
#include "StTrackMethod.inc"
#include "StVertexId.inc"
#include "phys_constants.inc"
#include "math_constants.inc"


      INTEGER impfit

      REAL proximity, rc, phi, step

      INTEGER i,iret,ndegf,ndef,index
      INTEGER imsgw, idgeant
      REAL    bfield(3)
      REAL    mainv(3)
      REAL    pveci(5),pvecis(5),rver1,bchis
      REAL    Prob_cern
      REAL    covar(6,6) ! covariance matrix of fit - 6th entry is alen
      REAL    r0,r1,charge,psi1,psi1b,alen
      REAL    CONSB

      REAL    csign, psi, r, xb, yb, xy0(2), xyatv(2), trk1(6), trk2(6)
      REAL    px, py, pz, pt

      CHARACTER*132 m132

      DOUBLE PRECISION covi(15)

      DATA imsgw/0/
C---executable statements
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C

C  preset in case of failure

      EGR_PRIMFIT = STAFCV_BAD      

      if((globtrk_h.nok).eq.0) return ! no tracks
      if((vertex_h.nok).eq.0) return ! no vertices


CC:  Load Primary vertex position coordinates:
         do i = 1,vertex_h.nok
            if(vertex(i).vtx_id .eq. kEventVtxId
     +           .and. vertex(i).iflag .eq. 1) then
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
      CONSB = C_D_CURVATURE*bfield(3)
c
      primtrk_h.nok=0
      DO i=1,globtrk_h.nok
       index = i
C -- test for size
       if (index .gt. primtrk_h.maxlen) go to 900
       primtrk_h.nok=index
C -- make copy as default
       primtrk(index) = globtrk(i)
       primtrk(index).iflag = globtrk(i).iflag
       if(globtrk(i).iflag.gt.0)then ! work with good tracks only
C --    this will only work if Kalman fit was used
        if(iand(globtrk(i).method,2**kKalmanFitId).ne.0)then
CCC --- restore this when substituting for egr_impactcl
CCC         globtrk(i).chisq(2) = 0.0 ! preset in case of failure
         primtrk(index).chisq(2) = 0.0 ! preset in case of failure
         rc = 1./globtrk(i).curvature
         ndef = 2*globtrk(i).n_fit_point-5

C -- use Kalman extrapolator to near primary vertex position
C -- use pion hypthesis, until we have pid
          idgeant = 8
          CALL TPCINI(idgeant,0.001,0.01)
          rver1 = 3.0
C -- should probably get this from egrpar
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

C -- Set up GEANT energy loss calculation for this track
          csign = bfield(3)*pvecis(5) ! b_z * charge
          psi = pvecis(4)+sign(1.0,csign)*C_PI_2 !mangled psi
          r = 1.0/abs(C_D_CURVATURE*csign)

          xb = globtrk(i).r0*cos(phi)
          yb = globtrk(i).r0*sin(phi)
          xy0(1) = xb - r*cos(psi)
          xy0(2) = yb - r*sin(psi)
C -- project track to near mainv
          call ev0_project_track(xy0,abs(r),mainv,xyatv)
C -- calculate track parameters near mainv
          trk1(1) = globtrk(i).r0
          trk1(2) = phi
          trk1(3) = globtrk(i).z0
          trk1(4) = pvecis(4)
          trk1(5) = pvecis(3)
          trk1(6) = globtrk(i).icharge
          call ev0_update_track_param(xy0,abs(r),xyatv,trk1,trk2,bfield(3))
          pt = 1/globtrk(i).invpt
          px = pt*cos(trk2(4))
          py = pt*sin(trk2(4))
          pz = pt*trk2(5)
          call mytrack(mainv,px,py,pz,idgeant)
          call kalex(step,globtrk(i).r0,pvecis,globtrk(i).covar,rver1,pveci,covi,idgeant,iret)
          primtrk(index).iflag = -iret
          if(iret.eq.0)then ! successful extrapolation
C --      calculate the impact parameter chisq for charged track at primary vertex
            primtrk(index).iflag = -20000 ! preset in case of failure
            if(impfit(10,rver1,pveci,covi,bfield(3),mainv,bchis,covar,alen).ne.0)then
              primtrk(index).iflag = globtrk(i).iflag
              
              primtrk(index).method =2**kKalmanFitId
              ndegf = ndef+2 ! two extra df for fit
              primtrk(index).id=globtrk(i).id
              primtrk(index).n_max_point=globtrk(i).n_max_point + 1
              primtrk(index).n_fit_point=globtrk(i).n_fit_point + 1

              r0 = sqrt(mainv(1)**2 + mainv(2)**2)
              if(r0.lt.0.01)r0 = 0.01
              phi = atan2(mainv(2),mainv(1))
C --          put phi 0-2pi
              if(phi.lt.0)phi = phi + C_2PI
              primtrk(index).phi0 = phi/C_RAD_PER_DEG
              primtrk(index).r0 = r0
              primtrk(index).z0 = mainv(3)
              primtrk(index).tanl = pveci(3)                 !tanl
              charge = sign(1.0,pveci(5))
              r1 = 1./(pveci(5)*CONSB) ! signed radius of curvature

              psi1 = pveci(4) + alen/r1
              if(psi1.gt. C_2PI)psi1 = psi1-C_2PI
              if(psi1.lt.0)psi1 = psi1+C_2PI ! at mainv
              primtrk(index).psi = psi1/C_RAD_PER_DEG
              primtrk(index).icharge = globtrk(i).icharge 
              primtrk(index).invpt = ABS(pveci(5))

C -- the covariance matrix is evaluated at proximity
              primtrk(index).covar(1) = covar(1,1)
              primtrk(index).covar(2) = covar(1,2)
              primtrk(index).covar(3) = covar(1,3)
              primtrk(index).covar(4) = covar(1,4)
              primtrk(index).covar(5) = covar(1,5)
              primtrk(index).covar(6) = covar(2,2)
              primtrk(index).covar(7) = covar(2,3)
              primtrk(index).covar(8) = covar(2,4)
              primtrk(index).covar(9) = covar(2,5)
              primtrk(index).covar(10) = covar(3,3)
              primtrk(index).covar(11) = covar(3,4)
              primtrk(index).covar(12) = covar(3,5)
              primtrk(index).covar(13) = covar(4,4)
              primtrk(index).covar(14) = covar(4,5)
              primtrk(index).covar(15) = covar(5,5)

              primtrk(index).chisq(1) = globtrk(i).chisq(1)*ndef + bchis

C -- NOTE - globtrk(i).chisq(2) is the probability that globtrk is primary
              primtrk(index).chisq(2) = Prob_cern(primtrk(index).chisq(1),ndef+2) 
              globtrk(i).chisq(2) = primtrk(index).chisq(2) 
              primtrk(index).chisq(1) = primtrk(index).chisq(1)/(ndef+2)
              primtrk(index).impact = globtrk(i).impact ! use original impact
              primtrk(index).curvature = abs(1/r1)
C        Don't copy bit map it is filled later on in the maker (LSB)
              primtrk(index).pid = idgeant
              EGR_PRIMFIT =STAFCV_OK
            endif ! if(impfit
         endif ! if(iret
        endif ! if( method
       endif ! iflag.gt.0
      ENDDO
      GO TO 999

 900  EGR_PRIMFIT = STAFCV_BAD
      call message('EGR_PRIMFIT-W: Too many prim 
     +   track - aborted.',1,imsgw)


 999  CONTINUE
      RETURN
      END





