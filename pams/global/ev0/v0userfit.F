      SUBROUTINE V0USERFIT (Itstp, Isw, X, Nxp, C, N1cp, Ncp, Ifail)

************************************************************************
*  USERFIT                                                             *
*                                                                      *
*  Created     : 11-Oct-1999   Author : A. C. Saulys                   *
*                                                                      *
*  Function    : Calculates constraints for vertex fit                 *
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
*       X(6)    phi2*rxy2                                              *
*       X(7)    z2                                                     *
*       X(8)    tanl2                                                  *
*       X(9)    psi2                                                   *
*       X(10)   q1/pt2                                                *
*                                                                      *
*       X(11)    xvertex                                               *
*       X(12)    yvertex                                               *
*       X(13)    zvertex                                               *
*                                                                      *
*       X(14)   arclen1
*       X(15)   arclen2
*                                                                      *
*       Xb(16)  rxy1
*       Xb(17)  rxy2
*
*       Xb(18)  psi1 at vertex
*       Xb(19)  psi2 at vertex
*
*       Xb(20)  X at primary vertex
*       Xb(21)  Y at primary vertex
*       Xb(22)  Z at primary vertex
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

      REAL*8 R1, R2
      REAL*8 spsi1, cpsi1, spsi2, cpsi2
      REAL*8 spsi1b, cpsi1b, spsi2b, cpsi2b
      REAL*8 psi1b, psi2b, psi1, psi2
      REAL*8 x1b, y1b, x2b, y2b, x01, y01, x02, y02, z01, z02
      REAL*8 x1, x2, y1, y2
      REAL*8 sphi1b, sphi2b, cphi1b, cphi2b
      REAL*8 spsi1a, spsi2a, cpsi1a, cpsi2a, psi1a, psi2a
      REAL*8 DX, DY, DZ, pt1, pt2, px1, px2, py1, py2, pz1, pz2
      REAL*8 px, py, pz, ptotsq, bx, by, bz
      REAL*8 ptot, dist

      REAL*8 dbxdpsi1, dbxdpsi2, dbydpsi1, dbydpsi2, dbzdpsi1, dbzdpsi2
      REAL*8 dptotdqovpt1, dptotdqovpt2, dbxdqovpt1, dbxdqovpt2
      REAL*8 dbydqovpt1, dbydqovpt2, dbzdqovpt1, dbzdqovpt2

      REAL*8 dpxdalen1, dpxdalen2, dpydalen1, dpydalen2
      REAL*8 dptotdalen1, dptotdalen2
      REAL*8 dbxdalen1,dbxdalen2,dbydalen1,dbydalen2,dbzdalen1,dbzdalen2
      REAL*8 dpxdqovpt1,dpxdqovpt2,dpydqovpt1
      REAL*8 dpydqovpt2,dpzdqovpt1,dpzdqovpt2
      REAL*8 s1,s2

      REAL*8 XX(16)
       
      INTEGER j

      DO j    = 1,Nxp
        XX(j) = Xb(j) + X(j)
      ENDDO

      CALL VZERO (C(1,1), N1cp*Ncp*2)

      r1 = 1./(XX(5)*CONSB) ! signed radius of curvature
      r2 = 1./(XX(10)*CONSB) ! signed radius of curvature
      pt1 = 1./abs(XX(5))
      pt2 = 1./abs(XX(10))
      psi1b = XX(4)+sign(1.0,XX(5))*C_PI_2 !mangled psi
      psi2b = XX(9)+sign(1.0,XX(10))*C_PI_2 !mangled psi
      if(psi1b.gt. C_2PI)psi1b = psi1b-C_2PI
      if(psi1b.lt.0)psi1b = psi1b+C_2PI
      if(psi2b.gt.C_2PI)psi2b = psi2b-C_2PI
      if(psi2b.lt.0)psi2b = psi2b+C_2PI

      x1b = Xb(16)*cos(XX(1)/Xb(16))
      y1b = Xb(16)*sin(XX(1)/Xb(16))
      x2b = Xb(17)*cos(XX(1+5)/Xb(17))
      y2b = Xb(17)*sin(XX(1+5)/Xb(17))

      cpsi1b = cos(psi1b)
      cpsi2b = cos(psi2b)
      spsi1b = sin(psi1b)
      spsi2b = sin(psi2b)

      x01 = x1b - r1*cpsi1b
      y01 = y1b - r1*spsi1b
      x02 = x2b + r2*cpsi2b
      y02 = y2b + r2*spsi2b

      psi1 = XX(14)/r1 + psi1b
      psi2 = XX(15)/r2 + psi2b
      if(psi1.gt. C_2PI)psi1 = psi1-C_2PI
      if(psi1.lt.0)psi1 = psi1+C_2PI
      if(psi2.gt.C_2PI)psi2 = psi2-C_2PI
      if(psi2.lt.0)psi2 = psi2+C_2PI

      cpsi1 = cos(psi1)
      cpsi2 = cos(psi2)
      spsi1 = sin(psi1)
      spsi2 = sin(psi2)

      x1 = x1b + r1*(cpsi1-cpsi1b)  
      x2 = x2b - r2*(cpsi2-cpsi2b)  
      y1 = y1b + r1*(spsi1-spsi1b)  
      y2 = y2b - r2*(spsi2-spsi2b) 

      sphi1b = sin(XX(1)/Xb(16))
      sphi2b = sin(XX(6)/Xb(17))
      cphi1b = cos(XX(1)/Xb(16))
      cphi2b = cos(XX(6)/Xb(17))

      DX = XX(11) - Xb(20)
      DY = XX(12) - Xb(21)
      DZ = XX(13) - Xb(22)

      psi1a = psi1-sign(1.0,XX(5))*C_PI_2 !mangled psi
      psi2a = psi2-sign(1.0,XX(10))*C_PI_2 !mangled psi
      spsi1a = sin(psi1a)
      cpsi1a = cos(psi1a)
      spsi2a = sin(psi2a)
      cpsi2a = cos(psi2a)

      px1 = pt1*cpsi1a
      py1 = pt1*spsi1a
      pz1 = pt1*XX(3)

      px2 = pt2*cpsi2a
      py2 = pt2*spsi2a
      pz2 = pt2*XX(8)

      px = px1 + px2
      py = py1 + py2
      pz = pz1 + pz2
      ptotsq = px**2 + py**2 + pz**2
      ptot = sqrt(ptotsq)

      dist = sqrt(DX**2+DY**2+DZ**2)
      bx = -dist*px/ptot + XX(11) - Xb(20) 
      by = -dist*py/ptot + XX(12) - Xb(21) 
      bz = -dist*pz/ptot + XX(13) - Xb(22) 

      dbxdpsi1 = dist*pt1*(px*(py*cpsi1a-px*spsi1a)/ptotsq+spsi1a)/ptot
      dbxdpsi2 = dist*pt2*(px*(py*cpsi2a-px*spsi2a)/ptotsq+spsi2a)/ptot
      dbydpsi1 = dist*pt1*(py*(py*cpsi1a-px*spsi1a)/ptotsq-cpsi1a)/ptot
      dbydpsi2 = dist*pt2*(py*(py*cpsi2a-px*spsi2a)/ptotsq-cpsi2a)/ptot
      dbzdpsi1 = dist*pt1*pz*(py*cpsi1a-px*spsi1a)/ptot**3
      dbzdpsi2 = dist*pt2*pz*(py*cpsi2a-px*spsi2a)/ptot**3

      s1 = sign(1.0,XX(5))
      s2 = sign(1.0,XX(10))
      dpxdqovpt1 = -s1*cpsi1a/XX(5)**2 - spsi1a*XX(14)*CONSB/abs(XX(5))
      dpxdqovpt2 = -s2*cpsi2a/XX(10)**2 - spsi2a*XX(15)*CONSB/abs(XX(10))
      dpydqovpt1 = -s1*spsi1a/XX(5)**2 + cpsi1a*XX(14)*CONSB/abs(XX(5))
      dpydqovpt2 = -s2*spsi2a/XX(10)**2 + cpsi2a*XX(15)*CONSB/abs(XX(10))
      dpzdqovpt1 = -s1*XX(3)/XX(5)**2
      dpzdqovpt2 = -s2*XX(8)/XX(10)**2

      dptotdqovpt1 = (px*dpxdqovpt1+py*dpydqovpt1+pz*dpzdqovpt1)/ptot
      dptotdqovpt2 = (px*dpxdqovpt2+py*dpydqovpt2+pz*dpzdqovpt2)/ptot
      dbxdqovpt1 = -dist*(dpxdqovpt1-px*dptotdqovpt1/ptot)/ptot
      dbxdqovpt2 = -dist*(dpxdqovpt2-px*dptotdqovpt2/ptot)/ptot
      dbydqovpt1 = -dist*(dpydqovpt1-py*dptotdqovpt1/ptot)/ptot
      dbydqovpt2 = -dist*(dpydqovpt2-py*dptotdqovpt2/ptot)/ptot
      dbzdqovpt1 = -dist*(dpzdqovpt1-pz*dptotdqovpt1/ptot)/ptot
      dbzdqovpt2 = -dist*(dpzdqovpt2-pz*dptotdqovpt2/ptot)/ptot

      dpxdalen1 = -spsi1a
      dpxdalen2 = -spsi2a
      dpydalen1 =  cpsi1a
      dpydalen2 =  cpsi2a
      dptotdalen1 = px*dpxdalen1+py*dpydalen1
      dptotdalen2 = px*dpxdalen2+py*dpydalen2
      dbxdalen1 = -dist*pt1*(dpxdalen1+px*dptotdalen1/ptotsq)/(ptot*r1)
      dbxdalen2 = -dist*pt2*(dpxdalen2+px*dptotdalen2/ptotsq)/(ptot*r2)
      dbydalen1 = -dist*pt1*(dpydalen1+py*dptotdalen1/ptotsq)/(ptot*r1)
      dbydalen2 = -dist*pt2*(dpydalen2+py*dptotdalen2/ptotsq)/(ptot*r2)
      dbzdalen1 = -dist*pt1*(pz*dptotdalen1/ptotsq)/(ptot*r1)
      dbzdalen2 = -dist*pt2*(pz*dptotdalen2/ptotsq)/(ptot*r2)
      
* residuals

      C(1,1) =   x01 + r1*cpsi1 - XX(11)   ! dx1
      C(1,1+3) = x02 - r2*cpsi2 - XX(11)   ! dx2
      C(1,2) =   y01 + r1*spsi1 - XX(12)   ! dy1
      C(1,2+3) = y02 - r2*spsi2 - XX(12)   ! dy2
      C(1,3) =   XX(2) - XX(14)*XX(3) - XX(13)   ! dz
      C(1,3+3) = XX(7) - XX(15)*XX(3+5) - XX(13)   ! dz
      C(1,7)   = bx**2 + by**2 + bz**2  ! db**2
 
* derivatives

      C(1+1,1) =   -sphi1b                        ! dFX1/dphr1
      C(1+2,1) =   0.0                            ! dFX1/dz1
      C(1+3,1) =   0.0                            ! dFx1/dtanl1
      C(1+4,1) =   r1*(spsi1b-spsi1)              ! dFX1/dpsi1
      C(1+5,1) =   (cpsi1b-cpsi1-XX(14)*spsi1/r1)*(CONSB*r1**2)! dFX1/d(q/pt1)

      C(1+1,2) =   cphi1b                         ! dFY1/dphr1
      C(1+2,2) =   0.0                            ! dFY1/dz1
      C(1+3,2) =   0.0                            ! dFY1/dtanl1
      C(1+4,2) =   -r1*(cpsi1b-cpsi1)             ! dFY1/dpsi1
      C(1+5,2) =   (spsi1b-spsi1+XX(14)*cpsi1/r1)*(CONSB*r1**2)! dFY1/d(q/pt1)

      C(1+1,3) =   0.0                            ! dFZ1/dphr1
      C(1+2,3) =   1.0                            ! dFZ1/dz1
      C(1+3,3) =   -XX(14)                        ! dFZ1/dtanl1
      C(1+4,3) =   0.0                            ! dFZ1/dpsi1
      C(1+5,3) =   0.0                            ! dFZ1/d(q/pt1)

      C(1+6,4) =   -sphi2b                        ! dFX2/dphr2
      C(1+7,4) =   0.0                            ! dFX2/dz2
      C(1+8,4) =   0.0                            ! dFX2/dtanl2
      C(1+9,4) =   -r2*(spsi2b-spsi2)             ! dFX2/dpsi2
      C(1+10,4)=  -(cpsi2b-cpsi2-XX(15)*spsi2/r2)*(CONSB*r2**2)! dFX2/d(q/pt2)

      C(1+6,5) =   cphi2b                         ! dFY2/dphr2
      C(1+7,5) =   0.0                            ! dFY2/dz2
      C(1+8,5) =   0.0                            ! dFY2/dtanl2
      C(1+9,5) =   r2*(cpsi2b-cpsi2)              ! dFY2/dpsi2
      C(1+10,5)=  -(spsi2b-spsi2+XX(15)*cpsi2/r2)*(CONSB*r2**2)! dFY1/d(q/pt1)

      C(1+6,6) =   0.0                            ! dFZ2/dphr2
      C(1+7,6) =   1.0                            ! dFZ2/dz2
      C(1+8,6) =   -XX(15)                        ! dFZ2/dtanl2
      C(1+9,6) =   0.0                            ! dFZ2/dpsi2
      C(1+10,6) =  0.0                            ! dFZ2/d(q/pt1)

      C(1+1,7) =   0.0                            ! dbs/dphr1
      C(1+2,7) =   0.0                            ! dbs/dz1
      C(1+3,7) =   2.*dist*pt1*(bz*(pz**2/ptotsq-1)
     &                        +bx*px*pz/ptotsq
     &                        +by*py*pz/ptotsq)/ptot           ! dbs/dtanl1
      C(1+4,7) =   2.*(bx*dbxdpsi1+by*dbydpsi1+bz*dbzdpsi1)    ! dbs/dpsi1
      C(1+5,7) =   2.*(bx*dbxdqovpt1+by*dbydqovpt1+bz*dbzdqovpt1) ! dbs/d(q/pt1)

      C(1+6,7) =   0.0                            ! dbs/dphr2
      C(1+7,7) =   0.0                            ! dbs/dz2
      C(1+8,7) =   2.*dist*pt2*(bz*(pz**2/ptotsq-1)
     &                        +bx*px*pz/ptotsq
     &                        +by*py*pz/ptotsq)/ptot           ! dbs/dtanl2
      C(1+9,7) =   2.*(bx*dbxdpsi2+by*dbydpsi2+bz*dbzdpsi2)    ! dbs/dpsi2
      C(1+10,7) =  2.*(bx*dbxdqovpt2+by*dbydqovpt2+bz*dbzdqovpt2) ! dbs/d(q/pt2)

      C(1+11,1) = -1.0                            ! dFX1/dX
      C(1+11,2) =  0.0                            ! dFX1/dY
      C(1+11,3) =  0.0                            ! dFX1/dZ

      C(1+12,1) =  0.0                            ! dFY1/dX
      C(1+12,2) = -1.0                            ! dFY1/dY
      C(1+12,3) =  0.0                            ! dFY1/dZ

      C(1+13,1) =  0.0                            ! dFZ1/dX
      C(1+13,2) =  0.0                            ! dFZ1/dY
      C(1+13,3) = -1.0                            ! dFZ1/dZ

      C(1+11,4) = -1.0                            ! dFX2/dX
      C(1+11,5) =  0.0                            ! dFX2/dY
      C(1+11,6) =  0.0                            ! dFX2/dZ

      C(1+12,4) =  0.0                            ! dFY2/dX
      C(1+12,5) = -1.0                            ! dFY2/dY
      C(1+12,6) =  0.0                            ! dFY2/dZ

      C(1+13,4) =  0.0                            ! dFZ2/dX
      C(1+13,5) =  0.0                            ! dFZ2/dY
      C(1+13,6) = -1.0                            ! dFZ2/dZ

      C(1+11,7) =  2.*(bx-DX*(bx*px+by*py+bz*pz)/(ptot*dist))     ! dbs/dX
      C(1+12,7) =  2.*(by-DY*(bx*px+by*py+bz*pz)/(ptot*dist))     ! dbs/dX
      C(1+13,7) =  2.*(bz-DZ*(bx*px+by*py+bz*pz)/(ptot*dist))     ! dbs/dX

      C(1+14,7) =  2.*(bx*dbxdalen1+by*dbydalen1+bz*dbzdalen1)    ! dbs/dalen1
      C(1+15,7) =  2.*(bx*dbxdalen2+by*dbydalen2+bz*dbzdalen2)    ! dbs/dalen2

      C(1+14,1) = -spsi1                          ! dFX1/dalen1
      C(1+14,2) =  cpsi1                          ! dFY1/dalen1
      C(1+14,3) = -XX(3)                          ! dFZ1/dalen1

      C(1+15,4) =  spsi2                          ! dFX2/dalen2
      C(1+15,5) = -cpsi2                          ! dFY2/dalen2
      C(1+15,6) = -XX(3+5)                        ! dFZ2/dalen2

      Ifail = 0
  999 RETURN
      END






