#include "geant321/pilot.h"
      SUBROUTINE GPARMP (CHARGE, STEP, MASS, VECT, VOUT, EOUT)
      IMPLICIT NONE
* 
*     ******************************************************************
*     *                                                                *
*     *  Performes the tracking for one step for magnetic monopoles,   *
*     *  i.e. particles wich carry magnetic but no electric charge,    *
*     *  in a constant magnetic field taken at the midpoint of the step*
*     *  The field is assumed to have just a z-component!!!            *
*     *                                                                *
*     *  units are kgauss,centimeters,gev/c                            *
*     *                                                                *
*     *                                                                *
*     *  Parameters:                                                   *
*     *   input                                                        *
      REAL             CHARGE ! charge magnetic(!)                     *
      REAL             STEP   ! arc length of the step asked           *
      REAL             MASS   ! monopole mass                          *
      REAL             VECT(*)! (x,y,z,px/p,py/p,pz/p,p)_initial       *
*     *                         VECT(8) accesses GCTRAK->GETOT !       *
*     *                         VECT(9) accesses GCTRAK->GEKIN !       *
*     *                                                                *
*     *   output                                                       *
      REAL             VOUT(*)! (x,y,z,px/p,py/p,pz/p,p)_final         *
      REAL             EOUT   ! final energy                           *
*     *                                                                *
*     *    ==>Called by : <USER>, GTMONP                               *
*     *       Authors:    P.Schieferdecker, M.Mulhearn                 *
*     *                                                                *
*     ******************************************************************
* 
*     variables:
*
      INTEGER          IX,IY,IZ,IPX,IPY,IPZ,IPP
      PARAMETER        (IX=1,IY=2,IZ=3,IPX=4,IPY=5,IPZ=6,IPP=7)
      REAL             XYZ(3),H(3),BZ
      INTEGER          I
      DOUBLE PRECISION X,Y,Z,T,R,PHI
      DOUBLE PRECISION PT0,PZ0,ET0
      DOUBLE PRECISION P,PZ,PT,V,VZ,VT,GAMMA
      DOUBLE PRECISION TAU1, TAU2, DT
      DOUBLE PRECISION R0,R1,R2,Z0,Z1,Z2
      DOUBLE PRECISION S,TSTEP,DX1,DX2
      DOUBLE PRECISION ARG
*     
*     parameters:
*
      DOUBLE PRECISION LSIHEP,TSIHEP,BKGHEP
      DOUBLE PRECISION HALF,PI
      DOUBLE PRECISION TOL; 
      PARAMETER       (HALF   = .5D0          )
      PARAMETER       (PI     = 3.141592654D0 )
      PARAMETER       (LSIHEP = 5.06773D+13   )
      PARAMETER       (TSIHEP = 1.51927D+12   )
      PARAMETER       (BKGHEP = 5.91572D-18   )
      PARAMETER        (TOL=1.0D-2)
* 
*     functions:
*
      EXTERNAL         MONOTRAJ
*
*     ------------------------------------------------------------------
*
      XYZ(1)    = VECT(IX) + 0.5D0 * STEP * VECT(IPX)
      XYZ(2)    = VECT(IY) + 0.5D0 * STEP * VECT(IPY)
      XYZ(3)    = VECT(IZ) + 0.5D0 * STEP * VECT(IPZ)
      CALL GUFLD (XYZ, H)
      BZ  =BKGHEP*H(3)
*
*     linear transport for small charge or magnetic field:
*      
      IF (ABS(CHARGE).LE.1D-6.OR.ABS(H(3)).LE.1D-6) GOTO 20    
*
*     calculate some essential quantities:
*     
      STEP=STEP*LSIHEP
      PT0= SQRT((VECT(IPX)*VECT(IPP))**2+(VECT(IPY)*VECT(IPP))**2)
      PZ0= VECT(IPZ)*VECT(IPP)
      ET0= SQRT(PT0*PT0+MASS*MASS)
      TAU1 = ET0 / (CHARGE * BZ)
      TAU2 = PT0 / (CHARGE * BZ)
      DT   = PZ0 / (CHARGE * BZ)
      IF(VECT(IPX).EQ.0.AND.VECT(IPY).EQ.0) THEN
         PHI = 0
      ELSE
         PHI = ATAN2(VECT(IPY),VECT(IPX))
      ENDIF
*
*     calculate time interval that results in correct step size:
*
      S = 0
      T = 0
      TSTEP = STEP
 10   CALL MONOTRAJ(R0,Z0,T              , DT,TAU1,TAU2)
      CALL MONOTRAJ(R1,Z1,T + HALF*TSTEP , DT,TAU1,TAU2)
      CALL MONOTRAJ(R2,Z2,T + TSTEP      , DT,TAU1,TAU2)
      DX1 = SQRT((Z2-Z0)**2+(R2-R0)**2)
      DX2 = SQRT((Z1-Z0)**2+(R1-R0)**2)+SQRT((Z2-Z1)**2+(R2-R1)**2)
      IF (ABS(DX1-DX2) > TOL*DX1) THEN
         TSTEP = HALF*TSTEP
         GOTO 10
      ENDIF
      IF (DX1 < (STEP-S)) THEN
         S = S + DX1
         T = T + TSTEP
*     try increasing step size for next step:
*        TSTEP = 2*STEP
      ELSE
         TSTEP = HALF*TSTEP
         GOTO 10
      ENDIF    
      IF (S < STEP*(1.0D0-TOL)) GOTO 10
*
*     calculate next point in trajectory:
*
      CALL MONOTRAJ(R,Z,T,DT,TAU1,TAU2)
      X=R*COS(PHI)
      Y=R*SIN(PHI)
*
*     prepare output:
*
      STEP=STEP/LSIHEP
      X   =   X/LSIHEP
      Y   =   Y/LSIHEP
      Z   =   Z/LSIHEP
      PZ  =   PZ0 + CHARGE*BZ*T
      PT  =   PT0
      P   =   SQRT(PZ**2 + PT**2)
      VOUT(IX) = VECT(IX) + X
      VOUT(IY) = VECT(IY) + Y
      VOUT(IZ) = VECT(IZ) + Z
      VOUT(IPP) = P
      VOUT(IPX) = PT*COS(PHI) / P
      VOUT(IPY) = PT*SIN(PHI) / P
      VOUT(IPZ) = PZ          / P
      EOUT      = SQRT(MASS**2+P**2)
*
      GOTO 999
*
*     Linear transport for small charge or magnetic field
*      
 20   CONTINUE
      DO 30 I   = 1,3
        VOUT(I) = VECT(I) + STEP * VECT(I+3)
        VOUT(I+3) = VECT(I+3)
 30   CONTINUE
      VOUT(IPP)= VECT(IPP)
      EOUT = SQRT(MASS**2 + VOUT(IPP)**2)     
*
 999  END
*
*     inverse hyperbolic sine:      
*
      DOUBLE PRECISION FUNCTION INVSINH(X)
      IMPLICIT NONE
      DOUBLE PRECISION X
      DOUBLE PRECISION TEMP
      TEMP  = X+SQRT(X**2+1D0)
      IF (TEMP.LE.0D0) THEN
         INVSINH=X
      ELSE 
         INVSINH=LOG(TEMP)
      ENDIF
      RETURN
      END
*
*     trajectory:
*
      SUBROUTINE MONOTRAJ(R,Z,T,DT,TAU1,TAU2)
      IMPLICIT NONE
      DOUBLE PRECISION R,Z,T,DT,TAU1,TAU2
      DOUBLE PRECISION INVSINH
      EXTERNAL         INVSINH
      DOUBLE PRECISION ARG1, ARG2     
      ARG1 = (T+DT) / TAU1
      ARG2 = DT / TAU1
      Z = TAU1 * (SQRT(1+ARG1*ARG1) - SQRT(1+ARG2*ARG2))
      R = TAU2 * (INVSINH(ARG1)      - INVSINH(ARG2)   )
      RETURN
      END















