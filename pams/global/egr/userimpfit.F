      SUBROUTINE USERIMPFIT (Itstp, Isw, X, Nxp, C, N1cp, Ncp, Ifail)

************************************************************************
*  USERIMPFIT                                                          *
*                                                                      *
*  Created     : 14-Feb-2000   Author : A. C. Saulys                   *
*                                                                      *
*  Function    : Calculates constraints for impact parameter fit       *
*                                                                      *
*  References  : None                                                  *
*                                                                      *
*  Arguments   :                                                       *
*      Itst    0=normal mode,1=test mode                               *
*      Isw     X & C switches                                          *
*      X   -   Vector of values                                        *
*      Nxp -   size of X                                               *
*      C   -   Constraint matrix                                       *
*      N1cp    1st size of C                                           *
*      Ncp     2nd size of C                                           *
*                                                                      *
*  Errors      :                                                       *
*      Ifail   0 = OK                                                  *
*              1 = too big step, cut step is needed                    *
*              9 = stop the fit                                        *
*                                                                      *
*                                                                      *
*       X(1)    phi1*rxy1                                              *
*       X(2)    z1                                                     *
*       X(3)    tanl1                                                  *
*       X(4)    psi1                                                   *
*       X(5)    q1/pt1                                                 *
*                                                                      *
*       X(6)   arclen1                                                 *
*                                                                      *
*       Xb(7)  X at primary vertex                                     *
*       Xb(8)  Y at primary vertex                                     *
*       Xb(9)  Z at primary vertex                                     *
*                                                                      *
*       Xb(10)  rxy1                                                   *
*                                                                      *
************************************************************************
      IMPLICIT NONE

#include "math_constants.inc"
#include "phys_constants.inc"


      REAL           CONSB,Xb
      COMMON/FITV0C/ CONSB,Xb(30)

      REAL*8    X(*)
      REAL*8    C(N1cp,Ncp)

      INTEGER   Itstp, Isw, Nxp, N1cp, Ncp, Ifail, Isw(Nxp+Ncp)

* Local variables
*__________________
*

      REAL*8 R1, charge
      REAL*8 spsi1, cpsi1
      REAL*8 spsi1b, cpsi1b
      REAL*8 psi1b, psi1
      REAL*8 x1b, y1b, x01, y01, z01
      REAL*8 x1, y1
      REAL*8 sphi1b, cphi1b
      REAL*8 spsi1a, cpsi1a, psi1a
      REAL*8 pt1, px1, py1, pz1
      REAL*8 px, py, pz, ptotsq, bx, by, bz

      REAL*8 XX(16)
      INTEGER j

      DO j    = 1,Nxp
        XX(j) = Xb(j) + X(j)
      ENDDO

      CALL VZERO (C(1,1), N1cp*Ncp*2)

      charge = sign(1.0,XX(5))
      r1 = 1./(XX(5)*CONSB) ! signed radius of curvature
      pt1 = 1./abs(XX(5))
      psi1b = XX(4)+charge*C_PI_2 !mangled psi
      if(psi1b.gt. C_2PI)psi1b = psi1b-C_2PI
      if(psi1b.lt.0)psi1b = psi1b+C_2PI

      x1b = Xb(10)*cos(XX(1)/Xb(10))
      y1b = Xb(10)*sin(XX(1)/Xb(10))

      cpsi1b = cos(psi1b)
      spsi1b = sin(psi1b)

      x01 = x1b - charge*r1*cpsi1b
      y01 = y1b - charge*r1*spsi1b

      psi1 = XX(6)/r1 + psi1b
      if(psi1.gt. C_2PI)psi1 = psi1-C_2PI
      if(psi1.lt.0)psi1 = psi1+C_2PI

      cpsi1 = cos(psi1)
      spsi1 = sin(psi1)

      x1 = x1b + charge*r1*(cpsi1-cpsi1b)  
      y1 = y1b + charge*r1*(spsi1-spsi1b)  

      sphi1b = sin(XX(1)/Xb(10))
      cphi1b = cos(XX(1)/Xb(10))

      psi1a = psi1-charge*C_PI_2 !mangled psi
      spsi1a = sin(psi1a)
      cpsi1a = cos(psi1a)

      px1 = pt1*cpsi1a
      py1 = pt1*spsi1a
      pz1 = pt1*XX(3)

* residuals

      C(1,1) =   x01 + charge*r1*cpsi1 - Xb(7)   ! dx1
      C(1,2) =   y01 + charge*r1*spsi1 - Xb(8)   ! dy1
      C(1,3) =   XX(2) - XX(6)*XX(3) - Xb(9)   ! dz

* derivatives

      C(1+1,1) =   -sphi1b                        ! dFX1/dphr1
      C(1+2,1) =   0.0                            ! dFX1/dz1
      C(1+3,1) =   0.0                            ! dFx1/dtanl1
      C(1+4,1) =   charge*r1*(spsi1b-spsi1)       ! dFX1/dpsi1
      C(1+5,1) =   charge*(cpsi1b-cpsi1-XX(6)*spsi1/r1)*(CONSB*r1**2)! dFX1/d(q/pt1)

      C(1+1,2) =   cphi1b                         ! dFY1/dphr1
      C(1+2,2) =   0.0                            ! dFY1/dz1
      C(1+3,2) =   0.0                            ! dFY1/dtanl1
      C(1+4,2) =   -charge*r1*(cpsi1b-cpsi1)      ! dFY1/dpsi1
      C(1+5,2) =   charge*(spsi1b-spsi1+XX(6)*cpsi1/r1)*(CONSB*r1**2)! dFY1/d(q/pt1)

      C(1+1,3) =   0.0                            ! dFZ1/dphr1
      C(1+2,3) =   1.0                            ! dFZ1/dz1
      C(1+3,3) =   -XX(6)                         ! dFZ1/dtanl1
      C(1+4,3) =   0.0                            ! dFZ1/dpsi1
      C(1+5,3) =   0.0                            ! dFZ1/d(q/pt1)

      C(1+6,1) = -charge*spsi1                    ! dFX1/dalen1
      C(1+6,2) =  charge*cpsi1                    ! dFY1/dalen1
      C(1+6,3) = -XX(3)                           ! dFZ1/dalen1

      Ifail = 0
  999 RETURN
      END






