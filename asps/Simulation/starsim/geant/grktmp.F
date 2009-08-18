      SUBROUTINE GRKTMP(CHARGE, STEP, M0, VECT, VOUT, EOUT)
C
      IMPLICIT NONE
C.
C.    ******************************************************************
C.    *                                                                *
C.    * 4-th order Runge-Kutta stepping routine for a magnetic monopole*
C.    * in a magnetic field.                                           *
C.    *                                                                *
C.    * We solve the system of equations                               *
C.    *     dx(i)/ds = c*p(i)/p;  dp(i)/ds = c*k*g*B(i)*m/p            *
C.    * for each spatial dimension, where                              *
C.    *     x() is the position 3-vector [cm],                         *
C.    *     s   is arc length/c [sec],                                 *
C.    *     c   is the speed of light in vacuum: 2.9979e10 [cm/sec],   *
C.    *     p() is the momentum 3-vector [GeV/c],                      *
C.    *     p   is the norm of p(),                                    *
C.    *     k   is a constant resulting from the conversion of g, B,   *
C.    *         m and p to Gaussian units: 2.9979e-4 [GeV-esu/erg],    *
C.    *     g   is the magnetic charge in multiples of the electron    *
C.    *         charge,                                                *
C.    *     B() is the ambient magnetic field from GUFLD [kgauss],     *
C.    *     m   is the monopole mass (p**2+m0**2)**0.5 [GeV/c**2].     *
C.    *                                                                *
C.    * All quantities are evaluated in the rest frame of the detector.*
C.    * We use arc-length/c as the independent variable to ensure that *
C.    * the step size is << 1.                                         *
C.    *                                                                *
C.    *  Parameters:                                                   *
C.    *   input                                                        *
      REAL             CHARGE ! magnetic charge in units of e          *
      REAL             STEP   ! arc length of the step asked in [cm]   *
      REAL             M0     ! monopole invariant mass in [Gev/c**2]  *
      REAL             VECT(*)! (x,y,z,px/p,py/p,pz/p,p)_initial       *
C.    *                         p in [GeV/c]                           *
C.    *                                                                *
C.    *   output                                                       *
      REAL             VOUT(*)! (x,y,z,px/p,py/p,pz/p,p)_final         *
C.    *                         Same units as VECT.                    *
      REAL             EOUT   ! Final energy in [GeV]                  *
C.    *                                                                *
C.    *   ==> Called by: GTMONP                                        *
C.    *       Author: Steve Tether **********                          *
C.    *                                                                *
C.    ******************************************************************
C
      INTEGER          IX,IY,IZ,IPX,IPY,IPZ,IPP
      PARAMETER       (IX=1,IY=2,IZ=3,IPX=4,IPY=5,IPZ=6,IPP=7)
      REAL C
      PARAMETER (C = 2.9979E10)
C
      REAL H, P
      INTEGER I
      REAL Q0(6), Q(6), QT(6), DQ0(6), DQT(6), DQM(6)
*
* *** Prepare the vector of initial values.
*
      DO I = 1, 3
         Q0(I) = VECT(I)
         Q0(I+3) = VECT(I+3) * VECT(IPP)
      END DO
      H = STEP / C
      CALL GRKDMP(CHARGE, M0, Q0, DQ0)
C     WRITE(*,*) 'H = ', H
C     WRITE(*,*) 'Q0 = ', Q0
C     WRITE(*,*) 'DQ0 = ', DQ0
*
*** What follows is adapted from Numerical Recipes.
*
      DO I = 1, 6
         QT(I) = Q0(I) + 0.5 * H * DQ0(I)
      END DO
      CALL GRKDMP(CHARGE, M0, QT, DQT)
C     WRITE(*,*) 'QT = ', QT
C     WRITE(*,*) 'DQT = ', DQT
      DO I = 1, 6
         QT(I) = Q0(I) + 0.5 * H * DQT(I)
      END DO
      CALL GRKDMP(CHARGE, M0, QT, DQM)
C     WRITE(*,*) 'QT = ', QT
C     WRITE(*,*) 'DQM = ', DQM
      DO I = 1, 6
         QT(I) = Q0(I) + H * DQM(I)
         DQM(I) = DQT(I) + DQM(I)
      END DO
      CALL GRKDMP(CHARGE, M0, QT, DQT)
C     WRITE(*,*) 'QT = ', QT
C     WRITE(*,*) 'DQM = ', DQM
C     WRITE(*,*) 'DQT = ', DQT
      DO I = 1, 6
         Q(I) = Q0(I) + (H/6.0) * (DQ0(I) + DQT(I) + 2.0 * DQM(I))
      END DO
C     WRITE(*,*) 'Q = ', Q
*
* *** Return the answer in GEANT standard form.
*
      DO I = 1, 3
         VOUT(I) = Q(I)
      END DO
      P = SQRT(Q(4)**2 + Q(5)**2 + Q(6)**2)
      DO I = 4, 6
         VOUT(I) = Q(I) / P
      END DO
      VOUT(IPP) = P
      EOUT = SQRT(P**2 + M0**2)
      END



      SUBROUTINE GRKDMP(CHARGE, M0, Q, DQ)
C.
C.    ******************************************************************
C.    * Calculate derivatives for GRKTMP.                              *
C.    *                                                                *
C.    *  Parameters:                                                   *
C.    *   input                                                        *
      REAL             CHARGE ! magnetic charge                        *
      REAL             M0     ! monopole invariant mass                *
      REAL             Q(*)   ! (x,y,z,px,py,pz)                       *
C.    *                                                                *
C.    *   output                                                       *
      REAL             DQ(*)  ! d(x,y,z,px,py,pz)/ds at Q              *
C.    *                                                                *
C.    *   ==> Called by: GRKTMP                                        *
C.    *       Author: Steve Tether **********                          *
C.    *                                                                *
C.    ******************************************************************
*
      REAL C, K
      PARAMETER (C = 2.9979E10, K = 2.9979E-4)

      REAL P, M
      REAL R(3), B(3)
      INTEGER I

      P = SQRT(Q(4)**2 + Q(5)**2 + Q(6)**2)
      M = SQRT(M0**2 + (Q(4)**2 + Q(5)**2 + Q(6)**2))

* Coordinate components.
* dx(i)/ds = c * p(i) / p
      DO I = 1, 3
         DQ(I) = C * Q(I+3) / P
      END DO

* Get the magnetic field at (x, y, z).
* gufld() requires the position in cm and returns the field
* components in kgauss.
      DO I = 1, 3
         R(I) = Q(I)
      END DO
      CALL GUFLD(R, B)

* Momentum components
* dp(i)/ds = const * m * g * B(i) / p
      DO I = 4, 6
         DQ(I) = C * K * M * CHARGE * B(I-3) / P
C        WRITE(*,*) 'm, charge, B, P = ', M, CHARGE, B(I-3), P
      END DO
      END
