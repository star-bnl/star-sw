      SUBROUTINE ev0_trak2pvec(r,trk1,pvec,beta)
      IMPLICIT NONE
C   Input arguments  :  r       !radius of the above circle
C                       trk1(6) !old track parameters
C                       beta    !z component of magnetic field
C   Output arguments :  pvec(5) !updated track parameters
C   Functional Description :    Loads up pvec for Kalman filter 
C                               extrapolation to another point.
C   Created  June, 1999 A. Saulys   
C   Error conditions : None
C     trk(1) -> r0  (radius from 0,0)
C     trk(2) -> phi (angle about 0,0)
C     trk(3) -> z
C     trk(4) -> psi (the circle angle)
C     trk(5) -> tanl
C     trk(6) -> charge of track ( = =- 1.0)


C---------------------------------------------------------------------
#include "math_constants.inc"
#include "phys_constants.inc"

      REAL r,trk1(6),pvec(5),beta,phi

C-----calculate the new track parameters
      
C --  put phi 0-2pi
      phi = trk1(2)
      if(trk1(2).lt.0)phi = phi + C_2PI
      pvec(1) = trk1(1)*phi !r*phi
      pvec(2) = trk1(3) !z
      pvec(3) = trk1(5) !tanl
      pvec(4) = trk1(4) !psi
      pvec(5) = trk1(6)/(C_D_CURVATURE*r*beta) ! q/pt

      RETURN
      END
