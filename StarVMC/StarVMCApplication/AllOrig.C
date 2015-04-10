*
* $Id: trprfn.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trprfn.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:35  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE TRPRFN(X1,P1,H1,X2,P2,H2,CH,XL,R,MVAR,IFLAG,ITRAN,IERR)
************************************************************************
*
*
*     SUBR. TRPRFN(X1,P1,H1,X2,P2,H2,CH,XL,*R*,MVAR,IFLAG,ITRAN,IERR*)
*
*     Origin W.Wittek    EMCSW/81/18
*
*     Finite step length case coded by V.Innocente ( Feb. 88 )
*
*     code improved:                   V.Innocente ( April. 90 )
*                   inline code replaces external function
*     code improved:                   V.Innocente ( January 91 )
*                   effect of energy loss added
*
*_______________________________________________________________________
*
* *** ERROR PROPAGATION ALONG A PARTICLE TRAJECTORY IN A MAGNETIC FIELD
*     ROUTINE ASSUMES THAT IN THE INTERVAL (X1,X2) THE QUANTITIES 1/P
*     AND (HX,HY,HZ) ARE CONSTANT.
*
* *** IFLAG  =  -1   INITIALIZATION, TRANSFORMATION OF ERROR MATRIX FROM
*                    EXTERNAL TO SC VARIABLES
*            =   0   ERROR PROPAGATION FROM X1 TO X2
*            =   1   TRANSFORMATION OF ERROR MATRIX FROM SC TO
*                    EXTERNAL VARIABLES
*
*     ITRAN          USED FOR IFLAG = 0 OR 1 ONLY
*            =   0   TRANSFORMATION MATRIX IS UPDATED ,BUT ERROR MATRIX IS NOT
*                    TRANSFORMED
*           =    1   TRANSF. MATRIX IS UPDATED  AND ERROR MATRIX IS TRANSFORMED
*
*     MVAR           SPECIFIES TYPE OF EXTERNAL VARIABLES
*            =   0   ( 1/P,LAMBDA,PHI,YT, ZT ;   SC   )
*            =   1   ( 1/P,  Y',  Z',  Y,  Z ; SPLINE )
*
* *** X1, P1, H1     X,Y,Z COMPONENTS OF POSITION, MOMENTUM AND MAGNETIC   INPUT
*                    FIELD VECTOR/GRADIENT AT STARTING POINT OF INTERVAL
*     X2, P2, H2     ......  AT END POINT OF INTERVAL                      INPUT
*     CH             CHARGE OF PARTICLE                                    INPUT
*     XL             PATHLENGTH FROM X1 TO X2   ( NEGATIVE IF OPPOSITE
*                    TO ACTUAL MOVEMENT OF PARTICLE )                      INPUT
*     R              ERROR MATRIX  (TRIANGLE)                       INPUT/OUTPUT
*     B              5 * 5 TRANSFORMATION MATRIX FOR ERRORS IN
*                    SC VARIABLES                                         OUTPUT
*
* *** IERR   =  1    ILLEGAL VALUE OF MVAR                                OUTPUT
*               2    MOMENTUM IS ZERO
*               3    H*ALFA/P AT X1 AND X2 DIFFER TOO MUCH
*               4    PARTICLE MOVES IN Z - DIRECTION
*
************************************************************************
*
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  X1,P1,H1,X2,P2,H2,R,CH,PS,PC,XL,SPX
#endif
#include "geant321/trcom3.inc"
#include "geant321/gcunit.inc"
      DIMENSION X1(3),P1(3),H1(9),X2(3),P2(3),H2(9)
      DIMENSION R(15),PS(3),PC(3)
*
      DIMENSION T1(3),T2(3),U1(3),U2(3),V1(3),V2(3),HN(9)
      DIMENSION AN1(3),AN2(3),DX(3)
      DIMENSION HV1(3),HU1(3)
*
      SAVE INIT,DELHP6,CFACT8
*
      DATA INIT/0/
#if !defined(CERNLIB_SINGLE)
      DATA DELHP6/300.D0/
*
      DATA CFACT8 / 2.997925 D-4 /
#endif
#if defined(CERNLIB_SINGLE)
      DATA DELHP6/300./
*
      DATA CFACT8 / 2.997925 E-4 /
#endif
*
*____________________________________________________________________
*
      IERR=0
      IF(IFLAG) 10, 20, 80
*
* *** TRANSFORM ERROR MATRIX FROM EXTERNAL TO INTERNAL VARIABLES;
*
   10 NEW=1
      IF(MVAR.NE.1) GO TO 11
      PA1=SQRT(P1(1)**2+P1(2)**2+P1(3)**2)
      IF(PA1.EQ.0.) GO TO 902
      PS(1)=1./PA1
      IF(P1(1).EQ.0.) GO TO 904
      PS(2)=P1(2)/P1(1)
      PS(3)=P1(3)/P1(1)
      SPX=1.
      IF(P1(1).LT.0.) SPX=-1.
      CALL TRSPSC(PS,R,PC,R,H1,CH,IERR,SPX)
      GO TO 19
*
   11 IF(MVAR.NE.0) GO TO 901
   19 GO TO 900
*
* *** ERROR PROPAGATION ON A HELIX ASSUMING SC VARIABLES
 
*
   20 PA1=SQRT(P1(1)**2+P1(2)**2+P1(3)**2)
      PA2=SQRT(P2(1)**2+P2(2)**2+P2(3)**2)
      IF(PA1*PA2.EQ.0.) GO TO 902
C      DPA = PA2 - PA1
      PM1=1./PA1
      PM2=1./PA2
      DPM = PM2 - PM1
*
      DO 201 I=1,3
        T1(I) = P1(I)*PM1
        T2(I) = P2(I)*PM2
201   CONTINUE
*
      SINL=T2(3)
      SINL0=T1(3)
*
      COSL=SQRT(ABS(1.-SINL**2))
      IF(COSL.EQ.0.) GO TO 904
      COSL1=1./COSL
      COSL0=SQRT(ABS(1.-SINL0**2))
*
* *** DEFINE TRANSFORMATION MATRIX BETWEEN X1 AND X2 FOR
* *** NEUTRAL PARTICLE OR FIELDFREE REGION
*
      DO 26 I=1,5
         DO 15 K=1,5
            A(I,K)=0.
   15    CONTINUE
         A(I,I)=1.
   26 CONTINUE
      A(4,3)=XL*COSL
      A(5,2)=XL
*
      IF(CH.EQ.0.) GO TO 45
      HA1=SQRT(H1(1)**2+H1(2)**2+H1(3)**2)
      HA2=SQRT(H2(1)**2+H2(2)**2+H2(3)**2)
      HAM1=HA1*PM1
      HAM2=HA2*PM2
      HAMX=MAX(HAM1,HAM2)
      IF(HAMX.EQ.0.) GO TO 45
*
*
*
* *** CHECK WHETHER H*ALFA/P IS TOO DIFFERENT AT X1 AND X2
*
*
      IF(HA2.NE.0.) THEN
         GAM=(H2(1)*T2(1)+H2(2)*T2(2)+H2(3)*T2(3))/HA2
      ELSE
         GAM=(H1(1)*T1(1)+H1(2)*T1(2)+H1(3)*T1(3))/HA1
      ENDIF
*
      ALFA2=1.-GAM**2
*
      DH2=(H1(1)*PM1-H2(1)*PM2)**2+
     1    (H1(2)*PM1-H2(2)*PM2)**2+
     1    (H1(3)*PM1-H2(3)*PM2)**2
      IF(DH2*ALFA2.GT.DELHP6**2) GO TO 903
*
* *** DEFINE AVERAGE MAGNETIC FIELD AND GRADIENT
*
      PM12=(PM1+PM2)*0.5
      P12=1./(2.*PM12)
      HN(1)=(H1(1)*PM1+H2(1)*PM2)*P12*CH*CFACT8
      HN(2)=(H1(2)*PM1+H2(2)*PM2)*P12*CH*CFACT8
      HN(3)=(H1(3)*PM1+H2(3)*PM2)*P12*CH*CFACT8
CC    HN(4)=(H1(4)*PM1+H2(4)*PM2)*P12*CH*CFACT8
CC    HN(5)=(H1(5)*PM1+H2(5)*PM2)*P12*CH*CFACT8
CC    HN(6)=(H1(6)*PM1+H2(6)*PM2)*P12*CH*CFACT8
CC    HN(7)=(H1(7)*PM1+H2(7)*PM2)*P12*CH*CFACT8
CC    HN(8)=(H1(8)*PM1+H2(8)*PM2)*P12*CH*CFACT8
CC    HN(9)=(H1(9)*PM1+H2(9)*PM2)*P12*CH*CFACT8
*
      HM = SQRT(HN(1)**2+HN(2)**2+HN(3)**2)
      OVER = 1./HM
      HN(1) = OVER*HN(1)
      HN(2) = OVER*HN(2)
      HN(3) = OVER*HN(3)
      PAV = .5*(PA1+PA2)
      Q = - HM/PAV
      THETA = Q*XL
      SINT = SIN(THETA)
      COST = COS(THETA)
      GAMMA=HN(1)*T2(1)+HN(2)*T2(2)+HN(3)*T2(3)
      AN2(1) = HN(2)*T2(3)-HN(3)*T2(2)
      AN2(2) = HN(3)*T2(1)-HN(1)*T2(3)
      AN2(3) = HN(1)*T2(2)-HN(2)*T2(1)
*
      AU = 1./SQRT(T1(1)**2+T1(2)**2)
      U1(1) = -AU*T1(2)
      U1(2) =  AU*T1(1)
      U1(3) =  0.D0
      V1(1) = -T1(3)*U1(2)
      V1(2) =  T1(3)*U1(1)
      V1(3) =  T1(1)*U1(2)-T1(2)*U1(1)
*
      AU = 1./SQRT(T2(1)**2+T2(2)**2)
      U2(1) = -AU*T2(2)
      U2(2) =  AU*T2(1)
      U2(3) =  0.D0
      V2(1) = -T2(3)*U2(2)
      V2(2) =  T2(3)*U2(1)
      V2(3) =  T2(1)*U2(2)-T2(2)*U2(1)
*
      DX(1) = X1(1) - X2(1)
      DX(2) = X1(2) - X2(2)
      DX(3) = X1(3) - X2(3)
*
*
* *** COMPLETE TRANSFORMATION MATRIX BETWEEN ERRORS AT X1 AND X2
* *** FIELD GRADIENT PERPENDICULAR TO TRACK IS PRESENTLY NOT
* *** TAKEN INTO ACCOUNT
*
   30 CONTINUE
      QP  = Q*PAV
      ANV = -(HN(1)*U2(1)+HN(2)*U2(2)            )
      ANU =  (HN(1)*V2(1)+HN(2)*V2(2)+HN(3)*V2(3))
      OMCOST = 1.-COST
      TMSINT = THETA-SINT
*
      HU1(1) =            -HN(3)*U1(2)
      HU1(2) = HN(3)*U1(1)
      HU1(3) = HN(1)*U1(2)-HN(2)*U1(1)
*
      HV1(1) = HN(2)*V1(3)-HN(3)*V1(2)
      HV1(2) = HN(3)*V1(1)-HN(1)*V1(3)
      HV1(3) = HN(1)*V1(2)-HN(2)*V1(1)
*
***   1/P
*
      A(1,1) = 1.-DPM*PAV*(1.+(T2(1)*DX(1)+T2(2)*DX(2)+T2(3)*DX(3))/XL)
     +           +2.*DPM*PAV
*
      A(1,2) =  -DPM/THETA*
     1           ( TMSINT*GAMMA*(HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3)) +
     2             SINT*(V1(1)*T2(1)+V1(2)*T2(2)+V1(3)*T2(3)) +
     3             OMCOST*(HV1(1)*T2(1)+HV1(2)*T2(2)+HV1(3)*T2(3)) )
*
      A(1,3) =  -COSL0*DPM/THETA*
     1           ( TMSINT*GAMMA*(HN(1)*U1(1)+HN(2)*U1(2)            ) +
     2             SINT*(U1(1)*T2(1)+U1(2)*T2(2)            ) +
     3             OMCOST*(HU1(1)*T2(1)+HU1(2)*T2(2)+HU1(3)*T2(3)) )
*
      A(1,4) =  -DPM/XL*(U1(1)*T2(1)+U1(2)*T2(2)            )
*
      A(1,5) =  -DPM/XL*(V1(1)*T2(1)+V1(2)*T2(2)+V1(3)*T2(3))
*
***   Lambda
*
      A(2,1) = -QP*ANV*(T2(1)*DX(1)+T2(2)*DX(2)+T2(3)*DX(3))
     +         *(1.+DPM*PAV)
*
      A(2,2) = COST*(V1(1)*V2(1)+V1(2)*V2(2)+V1(3)*V2(3)) +
     +         SINT*(HV1(1)*V2(1)+HV1(2)*V2(2)+HV1(3)*V2(3)) +
     1         OMCOST*(HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3))*
     A                (HN(1)*V2(1)+HN(2)*V2(2)+HN(3)*V2(3)) +
     2         ANV*( -SINT*(V1(1)*T2(1)+V1(2)*T2(2)+V1(3)*T2(3)) +
     3         OMCOST*(V1(1)*AN2(1)+V1(2)*AN2(2)+V1(3)*AN2(3)) -
     4         TMSINT*GAMMA*(HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3)) )
*
      A(2,3) = COST*(U1(1)*V2(1)+U1(2)*V2(2)            ) +
     +         SINT*(HU1(1)*V2(1)+HU1(2)*V2(2)+HU1(3)*V2(3)) +
     1         OMCOST*(HN(1)*U1(1)+HN(2)*U1(2)            )*
     A                (HN(1)*V2(1)+HN(2)*V2(2)+HN(3)*V2(3)) +
     2         ANV*( -SINT*(U1(1)*T2(1)+U1(2)*T2(2)            ) +
     3         OMCOST*(U1(1)*AN2(1)+U1(2)*AN2(2)             ) -
     4         TMSINT*GAMMA*(HN(1)*U1(1)+HN(2)*U1(2)            ) )
      A(2,3) = COSL0*A(2,3)
*
      A(2,4) = -Q*ANV*(U1(1)*T2(1)+U1(2)*T2(2)            )
*
      A(2,5) = -Q*ANV*(V1(1)*T2(1)+V1(2)*T2(2)+V1(3)*T2(3))
*
***   Phi
*
      A(3,1) = -QP*ANU*(T2(1)*DX(1)+T2(2)*DX(2)+T2(3)*DX(3))*COSL1
     +         *(1.+DPM*PAV)
*
      A(3,2) = COST*(V1(1)*U2(1)+V1(2)*U2(2)            ) +
     +         SINT*(HV1(1)*U2(1)+HV1(2)*U2(2)             ) +
     1         OMCOST*(HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3))*
     A                (HN(1)*U2(1)+HN(2)*U2(2)            ) +
     2         ANU*( -SINT*(V1(1)*T2(1)+V1(2)*T2(2)+V1(3)*T2(3)) +
     3         OMCOST*(V1(1)*AN2(1)+V1(2)*AN2(2)+V1(3)*AN2(3)) -
     4         TMSINT*GAMMA*(HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3)) )
      A(3,2) = COSL1*A(3,2)
*
      A(3,3) = COST*(U1(1)*U2(1)+U1(2)*U2(2)            ) +
     +         SINT*(HU1(1)*U2(1)+HU1(2)*U2(2)             ) +
     1         OMCOST*(HN(1)*U1(1)+HN(2)*U1(2)            )*
     A                (HN(1)*U2(1)+HN(2)*U2(2)            ) +
     2         ANU*( -SINT*(U1(1)*T2(1)+U1(2)*T2(2)            ) +
     3         OMCOST*(U1(1)*AN2(1)+U1(2)*AN2(2)             ) -
     4         TMSINT*GAMMA*(HN(1)*U1(1)+HN(2)*U1(2)            ) )
      A(3,3) = COSL1*COSL0*A(3,3)
*
      A(3,4) = -Q*ANU*(U1(1)*T2(1)+U1(2)*T2(2)            )*COSL1
*
      A(3,5) = -Q*ANU*(V1(1)*T2(1)+V1(2)*T2(2)+V1(3)*T2(3))*COSL1
*
***   Yt
*
      A(4,1) = PAV*(U2(1)*DX(1)+U2(2)*DX(2)            )
     +         *(1.+DPM*PAV)
*
      A(4,2) = (   SINT*(V1(1)*U2(1)+V1(2)*U2(2)            ) +
     1           OMCOST*(HV1(1)*U2(1)+HV1(2)*U2(2)             ) +
     2           TMSINT*(HN(1)*U2(1)+HN(2)*U2(2)            )*
     3                  (HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3)) )/Q
*
      A(4,3) = (   SINT*(U1(1)*U2(1)+U1(2)*U2(2)            ) +
     1           OMCOST*(HU1(1)*U2(1)+HU1(2)*U2(2)             ) +
     2           TMSINT*(HN(1)*U2(1)+HN(2)*U2(2)            )*
     3                  (HN(1)*U1(1)+HN(2)*U1(2)            ) )*COSL0/Q
*
      A(4,4) = (U1(1)*U2(1)+U1(2)*U2(2)            )
*
      A(4,5) = (V1(1)*U2(1)+V1(2)*U2(2)            )
*
***   Zt
*
      A(5,1) = PAV*(V2(1)*DX(1)+V2(2)*DX(2)+V2(3)*DX(3))
     +         *(1.+DPM*PAV)
*
      A(5,2) = (   SINT*(V1(1)*V2(1)+V1(2)*V2(2)+V1(3)*V2(3)) +
     1           OMCOST*(HV1(1)*V2(1)+HV1(2)*V2(2)+HV1(3)*V2(3)) +
     2           TMSINT*(HN(1)*V2(1)+HN(2)*V2(2)+HN(3)*V2(3))*
     3                  (HN(1)*V1(1)+HN(2)*V1(2)+HN(3)*V1(3)) )/Q
*
      A(5,3) = (   SINT*(U1(1)*V2(1)+U1(2)*V2(2)            ) +
     1           OMCOST*(HU1(1)*V2(1)+HU1(2)*V2(2)+HU1(3)*V2(3)) +
     2           TMSINT*(HN(1)*V2(1)+HN(2)*V2(2)+HN(3)*V2(3))*
     3                  (HN(1)*U1(1)+HN(2)*U1(2)            ) )*COSL0/Q
*
      A(5,4) = (U1(1)*V2(1)+U1(2)*V2(2)            )
*
      A(5,5) = (V1(1)*V2(1)+V1(2)*V2(2)+V1(3)*V2(3))
   45 CONTINUE
*
* *** NEW = 0  TRANSFORMATION MATRIX IS UPDATED
*           1  TRANSFORMATION MATRIX IS INITIALIZED
*
      IF(NEW.EQ.0) GO TO 23
      NEW=0
      DO 25 I=1,5
         DO 24 K=1,5
            B(I,K)=A(I,K)
   24    CONTINUE
   25 CONTINUE
      GO TO 27
   23 CONTINUE
*
      CALL XMM55(A,B,B)
*
   27 CONTINUE
   80 IF(ITRAN.EQ.0) GO TO 90
*
*
      J=0
      DO 22 I=1,5
         DO 21 K=I,5
            J=J+1
            S(J)=R(J)
   21    CONTINUE
   22 CONTINUE
*
*
* *** TRANSFORM ERROR MATRIX
*
      CALL SSMT5T(B,S,S)
*
      NEW=1
 
      J=0
      DO 41 I=1,5
         DO 40 K=I,5
            J=J+1
            R(J)=S(J)
   40    CONTINUE
   41 CONTINUE
*
   90 IF(IFLAG.LE.0) GO TO 900
*
*
* *** TRANSFORM ERROR MATRIX FROM INTERNAL TO EXTERNAL VARIABLES;
*
*
      NEW=1
      IF(MVAR.NE.1) GO TO 91
      PC(1)=PM2
      PC(2)=ASIN(P2(3)*PC(1))
      IF (ABS (P2(1)) .LT. 1.E-30) P2(1) = 1.E-30
      PC(3)=ATAN2(P2(2),P2(1))
      CALL TRSCSP(PC,R,PS,R,H2,CH,IERR,SPX)
      GO TO 900
*
   91 IF(MVAR.NE.0) GO TO 901
      GO TO 900
*
* *** ERROR EXITS
*
  901 IERR=1
      GO TO 999
  902 IERR=2
      GO TO 999
  903 IERR=3
C     IF(INIT.NE.0) GO TO 30
*     WRITE (LOUT, 998) DH2,ALFA2,XL
  998 FORMAT('0',' *** S/R TRPROP   DELTA(H*ALFA/P)',5X
     1,'EXCEEDS TOLERANCE    '/'0',3E12.5//' **********    ',///)
      INIT=1
      GO TO 30
  904 IERR=4
  999 WRITE (LOUT, 1000) IERR
 1000 FORMAT(1H ,' *** S/R ERPROP   IERR =',I5)
*
  900 CONTINUE
      END
 
*
* $Id: trprop.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trprop.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:35  mclareni
* Add geane321 source directories
*
*
*      MODIFIED FOR MAGFIELD GRADIENT 02-APR-82
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
C
      SUBROUTINE TRPROP(X1,P1,H1,X2,P2,H2,CH,XL,R,MVAR,IFLAG,ITRAN,IERR)
C
C *** ERROR PROPAGATION ALONG A PARTICLE TRAJECTORY IN A MAGNETIC FIELD
C     ROUTINE ASSUMES THAT IN THE INTERVAL (X1,X2) THE QUANTITIES 1/P
C     AND (HX,HY,HZ) ARE RATHER CONSTANT. DELTA(PHI) MUST NOT BE TOO LARGE
C
C     Authors: A. Haas and W. Wittek
C
C *** IFLAG  =  -1   INITIALIZATION, TRANSFORMATION OF ERROR MATRIX FROM
C                    EXTERNAL TO SC VARIABLES
C            =   0   ERROR PROPAGATION FROM X1 TO X2
C            =   1   TRANSFORMATION OF ERROR MATRIX FROM SC TO
C                    EXTERNAL VARIABLES
C
C     ITRAN          USED FOR IFLAG = 0 OR 1 ONLY
C            =   0   TRANSFORMATION MATRIX IS UPDATED ,BUT ERROR MATRIX IS NOT
C                    TRANSFORMED
C           =    1   TRANSF. MATRIX IS UPDATED  AND ERROR MATRIX IS TRANSFORMED
C
C     MVAR           SPECIFIES TYPE OF EXTERNAL VARIABLES
C            =   0   ( 1/P,LAMBDA,PHI,YT, ZT ;   SC   )
C            =   1   ( 1/P,  Y',  Z',  Y,  Z ; SPLINE )
C
C *** X1, P1, H1     X,Y,Z COMPONENTS OF POSITION, MOMENTUM AND MAGNETIC   INPUT
C                    FIELD VECTOR/GRADIENT AT STARTING POINT OF INTERVAL
C     X2, P2, H2     ......  AT END POINT OF INTERVAL                      INPUT
C     CH             CHARGE OF PARTICLE                                    INPUT
C     XL             PATHLENGTH FROM X1 TO X2   ( NEGATIVE IF OPPOSITE
C                    TO ACTUAL MOVEMENT OF PARTICLE )                      INPUT
C     R              ERROR MATRIX  (TRIANGLE)                       INPUT/OUTPUT
C     B              5 * 5 TRANSFORMATION MATRIX FOR ERRORS IN
C                    SC VARIABLES                                         OUTPUT
C
C *** IERR   =  1    ILLEGAL VALUE OF MVAR                                OUTPUT
C               2    MOMENTUM IS ZERO
C               3    H*ALFA/P AT X1 AND X2 DIFFER TOO MUCH
C                    OR DELTA PHI IS TOO LARGE
C               4    PARTICLE MOVES IN Z - DIRECTION
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  X1,P1,H1,X2,P2,H2,R,CH,PS,PC,XL,SPX
#endif
#include "geant321/trcom3.inc"
#include "geant321/gcunit.inc"
      DIMENSION X1(3),P1(3),H1(9),X2(3),P2(3),H2(9)
      DIMENSION R(15),PS(3),PC(3),HN(9)
C
      SAVE INIT,DELHP6,CFACT8
*
      DATA INIT/0/
#if defined(CERNLIB_SINGLE)
      DATA DELHP6/300./,DELFI6/0.1/
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA DELHP6/300.D0/,DELFI6/0.1D0/
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      IERR=0
      IF(IFLAG) 10, 20, 80
C
C *** TRANSFORM ERROR MATRIX FROM EXTERNAL TO INTERNAL VARIABLES;
C
   10 NEW=1
      IF(MVAR.NE.1) GO TO 11
      PA1=SQRT(P1(1)**2+P1(2)**2+P1(3)**2)
      IF(PA1.EQ.0.) GO TO 902
      PS(1)=1./PA1
      IF(P1(1).EQ.0.) GO TO 904
      PS(2)=P1(2)/P1(1)
      PS(3)=P1(3)/P1(1)
      SPX=1.
      IF(P1(1).LT.0.) SPX=-1.
      CALL TRSPSC(PS,R,PC,R,H1,CH,IERR,SPX)
      GO TO 19
C
   11 IF(MVAR.NE.0) GO TO 901
   19 GO TO 900
C
C *** ERROR PROPAGATION ON A HELIX ASSUMING SC VARIABLES
 
C
   20 PA1=SQRT(P1(1)**2+P1(2)**2+P1(3)**2)
      PA2=SQRT(P2(1)**2+P2(2)**2+P2(3)**2)
      IF(PA1*PA2.EQ.0.) GO TO 902
      PM1=1./PA1
      PM2=1./PA2
C
      TN(1)=P1(1)+P2(1)
      TN(2)=P1(2)+P2(2)
      TN(3)=P1(3)+P2(3)
      PM12=1./SQRT(TN(1)**2+TN(2)**2+TN(3)**2)
      TN(1)=TN(1)*PM12
      TN(2)=TN(2)*PM12
      TN(3)=TN(3)*PM12
C
      SINL=TN(3)
      COSL=SQRT(ABS(1.-SINL**2))
      IF(COSL.EQ.0.) GO TO 904
      COSL1=1./COSL
      SINP=TN(2)*COSL1
      COSP=TN(1)*COSL1
C
C *** DEFINE TRANSFORMATION MATRIX BETWEEN X1 AND X2 FOR
C *** NEUTRAL PARTICLE OR FIELDFREE REGION
C
      DO 26 I=1,5
         DO 15 K=1,5
            A(I,K)=0.
   15    CONTINUE
         A(I,I)=1.
   26 CONTINUE
      A(4,3)=XL*COSL
      A(5,2)=XL
C
      IF(CH.EQ.0.) GO TO 45
      HA1=SQRT(H1(1)**2+H1(2)**2+H1(3)**2)
      HA2=SQRT(H2(1)**2+H2(2)**2+H2(3)**2)
      HAM1=HA1*PM1
      HAM2=HA2*PM2
      HAMX = MAX(HAM1,HAM2)
      IF(HAMX.EQ.0.) GO TO 45
C
C *** DEFINE AVERAGE MAGNETIC FIELD AND GRADIENT
C
      PM12=(PM1+PM2)*0.5
      P12=1./(2.*PM12)
      HN(1)=(H1(1)*PM1+H2(1)*PM2)*P12*CH*CFACT8
      HN(2)=(H1(2)*PM1+H2(2)*PM2)*P12*CH*CFACT8
      HN(3)=(H1(3)*PM1+H2(3)*PM2)*P12*CH*CFACT8
      HN(4)=(H1(4)*PM1+H2(4)*PM2)*P12*CH*CFACT8
      HN(5)=(H1(5)*PM1+H2(5)*PM2)*P12*CH*CFACT8
      HN(6)=(H1(6)*PM1+H2(6)*PM2)*P12*CH*CFACT8
      HN(7)=(H1(7)*PM1+H2(7)*PM2)*P12*CH*CFACT8
      HN(8)=(H1(8)*PM1+H2(8)*PM2)*P12*CH*CFACT8
      HN(9)=(H1(9)*PM1+H2(9)*PM2)*P12*CH*CFACT8
C
      B0=HN(1)*COSP+HN(2)*SINP
      B2=-HN(1)*SINP+HN(2)*COSP
      B3=-B0*SINL+HN(3)*COSL
      TGL=SINL*COSL1
C
C
C *** CHECK WHETHER H*ALFA/P IS TOO DIFFERENT AT X1 AND X2
C     AND WHETHER CHANGE OF TRACK DIRECTION DUE TO MAG.FIELD IS TOO LARGE
C
C
      IF(HA2.EQ.0.) GO TO 29
 
      GAM=(H2(1)*TN(1)+H2(2)*TN(2)+H2(3)*TN(3))/HA2
      GO TO 28
   29 GAM=(H1(1)*TN(1)+H1(2)*TN(2)+H1(3)*TN(3))/HA1
   28 CONTINUE
      ALFA=SQRT(ABS(1.-GAM**2))
C
      DH2=(H1(1)*PM1-H2(1)*PM2)**2+
     1    (H1(2)*PM1-H2(2)*PM2)**2+
     1    (H1(3)*PM1-H2(3)*PM2)**2
      IF(DH2*ALFA**2.GT.DELHP6**2) GO TO 903
      ALFAQ=-ALFA*CFACT8*(HAM1+HAM2)*0.5
      DFI=ABS(XL*ALFAQ)
      IF(DFI.GT.DELFI6) GO TO 903
C
C *** COMPLETE TRANSFORMATION MATRIX BETWEEN ERRORS AT X1 AND X2
C *** TAKING INTO ACCOUNT  FIELD GRADIENT PERPENDICULAR TO TRACK
C
   30 COSP2=COSP*COSP
      SINP2=SINP*SINP
      COSIP=COSP*SINP
C
      G22=SINP2*HN(9)+COSP2*HN(8)-2.0*COSIP*HN(7)
      G33=SINL*SINL*(COSP2*HN(9)+SINP2*HN(8)+2.0*COSIP*HN(7))
     ++COSL*(COSL*HN(6)-2.0*SINL*(COSP*HN(4)+SINP*HN(5)))
      G23=SINL*(COSIP*(HN(9)-HN(8))+(SINP2-COSP2)*HN(7))
     ++COSL*(COSP*HN(5)-SINP*HN(4))
C
      A(2,1)=XL*B2
      A(2,3)=-B0*XL*PM12
      A(2,4)=(B2*B3*PM12+G22)*XL*PM12
      A(2,5)=(-B2*B2*PM12+G23)*XL*PM12
C
      A(3,1)=-XL*B3*COSL1
      A(3,2)=B0*XL*PM12*COSL1**2
      A(3,3)=1.+TGL*B2*XL*PM12
      A(3,4)=(-B3*B3*PM12-G23)*XL*PM12*COSL1
      A(3,5)=(B3*B2*PM12-G33)*XL*PM12*COSL1
C
      A(4,5)=-B3*TGL*XL*PM12
      A(5,4)=B3*TGL*XL*PM12
C
   45 CONTINUE
C
C *** NEW = 0  TRANSFORMATION MATRIX IS UPDATED
C           1  TRANSFORMATION MATRIX IS INITIALIZED
C
      IF(NEW.EQ.0) GO TO 23
      NEW=0
      DO 25 I=1,5
         DO 24 K=1,5
            B(I,K)=A(I,K)
   24    CONTINUE
   25 CONTINUE
      GO TO 27
   23 CONTINUE
C
      CALL XMM55(A,B,B)
C
   27 CONTINUE
   80 IF(ITRAN.EQ.0) GO TO 90
C
C
      J=0
      DO 22 I=1,5
         DO 21 K=I,5
            J=J+1
            S(J)=R(J)
   21    CONTINUE
   22 CONTINUE
C
C
C *** TRANSFORM ERROR MATRIX
C
      CALL SSMT5T(B,S,S)
C
      NEW=1
 
      J=0
      DO 41 I=1,5
         DO 40 K=I,5
            J=J+1
            R(J)=S(J)
   40    CONTINUE
   41 CONTINUE
C
   90 IF(IFLAG.LE.0) GO TO 900
C
C
C *** TRANSFORM ERROR MATRIX FROM INTERNAL TO EXTERNAL VARIABLES;
C
C
      NEW=1
      IF(MVAR.NE.1) GO TO 91
      PC(1)=PM2
      PC(2)=ASIN(P2(3)*PC(1))
      IF (ABS (P2(1)) .LT. 1.E-30) P2(1) = 1.E-30
      PC(3) = ATAN2 (P2(2),P2(1))
      CALL TRSCSP(PC,R,PS,R,H2,CH,IERR,SPX)
      GO TO 900
C
   91 IF(MVAR.NE.0) GO TO 901
      GO TO 900
C
C *** ERROR EXITS
C
  901 IERR=1
      GO TO 999
  902 IERR=2
      GO TO 999
  903 IERR=3
      IF(INIT.NE.0) GO TO 30
      INIT=1
      GO TO 30
  904 IERR=4
  999 WRITE (LOUT, 1000) IERR
 1000 FORMAT(1H ,' *** S/R ERPROP   IERR =',I5)
C
  900 RETURN
      END
*
* $Id: trptsc.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trptsc.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE TRPTSC(PC,RC,PD,RD,IERR)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM   SC   VARIABLES (1/Pt,LAMBDA,PHI,YT,ZT)
C     FROM   SC   VARIABLES (1/P,LAMBDA,PHI,YT,ZT)
 
 
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  PC,PD,RC,RD
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC(3),PD(3),RC(15),RD(15)
*
*______________________________________________________________________
*
      IERR = 0
      COSL  = COS(PC(2))
      IF (ABS(COSL).EQ.0) GO TO 901
      SINL  =    SIN(PC(2))
*
      PD(1) = PC(1)*COSL
      PD(2) = PC(2)
      PD(3) = PC(3)
*
      J=0
*
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
*
      A(1,1) = COSL
      A(2,2) = 1.D0
      A(3,3) = 1.D0
      A(4,4) = 1.D0
      A(5,5) = 1.D0
*
      A(1,2) = -PC(1)*SINL
C
      CALL SSMT5T(A,S,S)
C
      DO 25 J=1,15
        RD(J)=S(J)
   25 CONTINUE
C
      RETURN
C
C *** ERROR EXITS
C
  901 IERR=1
  910 CONTINUE
C
      END
*
* $Id: trptsd.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trptsd.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE TRPTSD(PD,RD,PC,RC,H,CH,IERR,SPU,DJ,DK)
*
***************************************************************************
C
C *** TRANSFORMS ERROR MATRIX
C     FROM        VARIABLES (1/Pt,V',W',V,W)
C     FROM        VARIABLES (1/P, V',W',V,W)
C
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD,PC,H,RC,RD,CH,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD(3),PC(3),H(3),RC(15),RD(15),DJ(3),DK(3)
      DIMENSION UN(3),VN(3),DI(3),TVW(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
*
**_____________________________________________________________________
*
      IERR=0
      TVW(1)=1./SQRT(1.+PD(2)**2+PD(3)**2)
      IF(SPU.LT.0.) TVW(1)=-TVW(1)
      TVW(2)=PD(2)*TVW(1)
      TVW(3)=PD(3)*TVW(1)
C
      DI(1)=DJ(2)*DK(3)-DJ(3)*DK(2)
      DI(2)=DJ(3)*DK(1)-DJ(1)*DK(3)
      DI(3)=DJ(1)*DK(2)-DJ(2)*DK(1)
C
      DO 5 I=1,3
         TN(I)=TVW(1)*DI(I)+TVW(2)*DJ(I)+TVW(3)*DK(I)
    5 CONTINUE
C
      COSL=SQRT(ABS(1.-TN(3)**2))
      IF (COSL .LT. 1.E-30) COSL = 1.E-30
      COSL1=1./COSL
      SINL  = TN(3)
*
      PC(1)=PD(1)*COSL
      PC(2)=PD(2)
      PC(3)=PD(3)
      PM=PC(1)
*
      IF (ABS (TN(1)) .LT. 1.E-30) TN(1) = 1.E-30
C
      UN(1)=-TN(2)*COSL1
      UN(2)=TN(1)*COSL1
      UN(3)=0.
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
 
C
      UJ=UN(1)*DJ(1)+UN(2)*DJ(2)+UN(3)*DJ(3)
      UK=UN(1)*DK(1)+UN(2)*DK(2)+UN(3)*DK(3)
      VJ=VN(1)*DJ(1)+VN(2)*DJ(2)+VN(3)*DJ(3)
      VK=VN(1)*DK(1)+VN(2)*DK(2)+VN(3)*DK(3)
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD(J)
    4    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
C
      Q=-HAM*CFACT8
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)+H(3)*UN(3))*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
      A(1,4)=Q*TVW(2)*SINZ*(SINL*PD(1))
      A(1,5)=Q*TVW(3)*SINZ*(SINL*PD(1))
C
6     continue
      A(1,1) = COSL
      A(2,2) = 1.
      A(3,3) = 1.
      A(4,4) = 1.
      A(5,5) = 1.
*
      A(1,2)=-TVW(1)*VJ*(SINL*PD(1))
      A(1,3)=-TVW(1)*VK*(SINL*PD(1))
C
      CALL SSMT5T(A,S,S)
C
      DO J=1,15
        RC(J)=S(J)
      ENDDO
*
      END
*
* $Id: trs1s2.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trs1s2.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE TRS1S2 (PD1,RD1,PD2,RD2,H,CH,IERR,SP1,SP2
     1,                  DJ1,DK1,DJ2,DK2)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM        VARIABLES (1/P,V1',W1',V1,W1)
C      TO         VARIABLES (1/P,V2',W2',V2,W2)
C
C     Authors: A. Haas and W. Wittek
C
C
C *** PD1(3)    1/P,V1',W1'                             INPUT
C     PD2(3)    1/P,V2',W2'                            OUTPUT
C     H(3)      MAGNETIC FIELD                          INPUT
C     RD1(15)   ERROR MATRIX IN 1/P,V1',W1',V1,W1       INPUT      (TRIANGLE)
C     RD2(15)   ERROR MATRIX IN 1/P,V2',W2',V2,W2      OUTPUT      (TRIANGLE)
C     CH        CHARGE OF PARTICLE                      INPUT
C               CHARGE AND MAGNETIC FIELD ARE NEEDED
C               FOR CORRELATION TERMS (V2',V1),(V2',W1),(W2',V1),(W2',W1)
C               THESE CORRELATION TERMS APPEAR BECAUSE RD1 IS ASSUMED
C               TO BE THE ERROR MATRIX FOR FIXED U1
C               AND RD2 FOR FIXED U2
C     SP1       SIGN OF U1-COMPONENT OF PARTICLE MOMENTUM     INPUT
C     SP2       SIGN OF U2-COMPONENT OF PARTICLE MOMENTUM    OUTPUT
C     DJ1(3)    UNIT VECTOR IN V1-DIRECTION
C     DK1(3)    UNIT VECTOR IN W1-DIRECTION    OF SYSTEM 1
C     DJ2(3)    UNIT VECTOR IN V2-DIRECTION
C     DK2(3)    UNIT VECTOR IN W2-DIRECTION    OF SYSTEM 2
C
C     IERR      = 0    TRANSFORMATION OK
C               = 1    MOMENTUM PERPENDICULAR TO U2-DIRECTION (V2',W2' NOT DEFIN
C               = 2    MOMENTUM PERPENDICULAR TO X-AXIS
C
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD1,PD2,RD1,RD2,H,CH,SP1,SP2,DJ1,DK1,DJ2,DK2
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD1(3),PD2(3),RD1(15),RD2(15),H(3),DJ1(3),DK1(3)
     +,DJ2(3),DK2(3),UN(3),VN(3),DI1(3),DI2(3),TVW1(3),TVW2(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      IERR=0
      PM=PD1(1)
      TVW1(1)=1./SQRT(1.+PD1(2)**2+PD1(3)**2)
      IF(SP1.LT.0.) TVW1(1)=-TVW1(1)
      TVW1(2)=PD1(2)*TVW1(1)
      TVW1(3)=PD1(3)*TVW1(1)
C
      DI1(1)=DJ1(2)*DK1(3)-DJ1(3)*DK1(2)
      DI1(2)=DJ1(3)*DK1(1)-DJ1(1)*DK1(3)
      DI1(3)=DJ1(1)*DK1(2)-DJ1(2)*DK1(1)
C
      DO 5 I=1,3
         TN(I)=TVW1(1)*DI1(I)+TVW1(2)*DJ1(I)+TVW1(3)*DK1(I)
    5 CONTINUE
C
      DI2(1)=DJ2(2)*DK2(3)-DJ2(3)*DK2(2)
      DI2(2)=DJ2(3)*DK2(1)-DJ2(1)*DK2(3)
      DI2(3)=DJ2(1)*DK2(2)-DJ2(2)*DK2(1)
C
      TVW2(1)=TN(1)*DI2(1)+TN(2)*DI2(2)+TN(3)*DI2(3)
      TVW2(2)=TN(1)*DJ2(1)+TN(2)*DJ2(2)+TN(3)*DJ2(3)
      TVW2(3)=TN(1)*DK2(1)+TN(2)*DK2(2)+TN(3)*DK2(3)
C
      IF(TVW2(1).EQ.0.) GO TO 901
      TR=1./TVW2(1)
      SP2=1.
      IF(TVW2(1).LT.0.) SP2=-1.
      PD2(1)=PD1(1)
      PD2(2)=TVW2(2)*TR
      PD2(3)=TVW2(3)*TR
C
      COSL=SQRT(ABS(1.-TN(3)**2))
      IF(COSL.EQ.0.) GO TO 902
      COSL1=1./COSL
      UN(1)=-TN(2)*COSL1
      UN(2)=TN(1)*COSL1
      UN(3)=0.
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
C
      UJ1=UN(1)*DJ1(1)+UN(2)*DJ1(2)+UN(3)*DJ1(3)
      UK1=UN(1)*DK1(1)+UN(2)*DK1(2)+UN(3)*DK1(3)
      VJ1=VN(1)*DJ1(1)+VN(2)*DJ1(2)+VN(3)*DJ1(3)
      VK1=VN(1)*DK1(1)+VN(2)*DK1(2)+VN(3)*DK1(3)
C
      UJ2=UN(1)*DJ2(1)+UN(2)*DJ2(2)+UN(3)*DJ2(3)
      UK2=UN(1)*DK2(1)+UN(2)*DK2(2)+UN(3)*DK2(3)
      VJ2=VN(1)*DJ2(1)+VN(2)*DJ2(2)+VN(3)*DJ2(3)
      VK2=VN(1)*DK2(1)+VN(2)*DK2(2)+VN(3)*DK2(3)
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD1(J)
    4    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
C
      Q=-HAM*CFACT8
      TT=-Q*TR**3
      SJ1I2=DJ1(1)*DI2(1)+DJ1(2)*DI2(2)+DJ1(3)*DI2(3)
      SK1I2=DK1(1)*DI2(1)+DK1(2)*DI2(2)+DK1(3)*DI2(3)
      SK2U=DK2(1)*UN(1)+DK2(2)*UN(2)+DK2(3)*UN(3)
      SK2V=DK2(1)*VN(1)+DK2(2)*VN(2)+DK2(3)*VN(3)
      SJ2U=DJ2(1)*UN(1)+DJ2(2)*UN(2)+DJ2(3)*UN(3)
      SJ2V=DJ2(1)*VN(1)+DJ2(2)*VN(2)+DJ2(3)*VN(3)
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)+H(3)*UN(3))*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
      A(2,4)=-TT*SJ1I2*(SK2U*SINZ-SK2V*COSZ)
      A(2,5)=-TT*SK1I2*(SK2U*SINZ-SK2V*COSZ)
      A(3,4)= TT*SJ1I2*(SJ2U*SINZ-SJ2V*COSZ)
      A(3,5)= TT*SK1I2*(SJ2U*SINZ-SJ2V*COSZ)
C
    6 A(1,1)=1.
      A(4,4)=TR*(UJ1*VK2-VJ1*UK2)
      A(4,5)=TR*(UK1*VK2-VK1*UK2)
      A(5,4)=TR*(VJ1*UJ2-UJ1*VJ2)
      A(5,5)=TR*(VK1*UJ2-UK1*VJ2)
C
      TS=TR*TVW1(1)
      A(2,2)=A(4,4)*TS
      A(2,3)=A(4,5)*TS
      A(3,2)=A(5,4)*TS
      A(3,3)=A(5,5)*TS
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RD2(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C *** ERROR EXITS
C
  901 IERR=1
      GO TO 910
  902 IERR=2
  910 RETURN
      END
*
*
* $Id: trscpt.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trscpt.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE TRSCPT(PC,RC,PD,RD,IERR)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM   SC   VARIABLES (1/P,LAMBDA,PHI,YT,ZT)
C     FROM   SC   VARIABLES (1/Pt,LAMBDA,PHI,YT,ZT)
 
 
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  PC,PD,RC,RD
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC(3),PD(3),RC(15),RD(15)
*
*______________________________________________________________________
*
      IERR = 0
      COSL  = COS(PC(2))
      IF (ABS(COSL).EQ.0) GO TO 901
      COSL1 = 1./COSL
      TANL  =    TAN(PC(2))
*
      PD(1) = PC(1)*COSL1
      PD(2) = PC(2)
      PD(3) = PC(3)
*
      J=0
*
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
*
      A(1,1) = COSL1
      A(2,2) = 1.D0
      A(3,3) = 1.D0
      A(4,4) = 1.D0
      A(5,5) = 1.D0
*
      A(1,2) = PD(1)*TANL
C
      CALL SSMT5T(A,S,S)
C
      DO 25 J=1,15
        RD(J)=S(J)
   25 CONTINUE
C
      RETURN
C
C *** ERROR EXITS
C
  901 IERR=1
  910 CONTINUE
C
      END
*
* $Id: trscsd.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trscsd.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:35  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
C
      SUBROUTINE TRSCSD(PC,RC,PD,RD,H,CH,IERR,SPU,DJ,DK)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM   SC   VARIABLES (1/P,LAMBDA,PHI,YT,ZT)
C      TO         VARIABLES (1/P,V',W',V,W)
C
C     Authors: A. Haas and W. Wittek
C
C
C *** PC(3)     1/P,LAMBDA,PHI                          INPUT
C     PD(3)     1/P,V',W'                              OUTPUT
C     H(3)      MAGNETIC FIELD                          INPUT
C     RC(15)    ERROR MATRIX IN   SC   VARIABLES        INPUT     (TRIANGLE)
C     RD(15)    ERROR MATRIX IN 1/P,V',W',V,W          OUTPUT     (TRIANGLE)
C     CH        CHARGE OF PARTICLE                      INPUT
C               CHARGE AND MAGNETIC FIELD ARE NEEDED
C               FOR CORRELATION TERMS (V',YT),(V',ZT),(W',YT),(W',ZT)
C               THESE CORRELATION TERMS APPEAR BECAUSE RC IS ASSUMED
C               TO BE THE ERROR MATRIX FOR FIXED S (PATH LENGTH)
C               AND RD FOR FIXED U
C     DJ(3)     UNIT VECTOR IN V-DIRECTION
C     DK(3)     UNIT VECTOR IN W-DIRECTION    OF DETECTOR SYSTEM
C
C     IERR  =   1       PARTICLE MOVES PERPENDICULAR TO U-AXIS
C                      ( V',W' ARE NOT DEFINED )
C     SPU       SIGN OF U-COMPONENT OF PARTICLE MOMENTUM   OUTPUT
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  PC,PD,H,RC,RD,CH,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC(3),PD(3),H(3),RC(15),RD(15),DJ(3),DK(3)
      DIMENSION UN(3),VN(3),DI(3),TVW(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      IERR=0
      PM=PC(1)
      COSL=COS(PC(2))
      SINP=SIN(PC(3))
      COSP=COS(PC(3))
C
      TN(1)=COSL*COSP
      TN(2)=COSL*SINP
      TN(3)=SIN(PC(2))
C
      DI(1)=DJ(2)*DK(3)-DJ(3)*DK(2)
      DI(2)=DJ(3)*DK(1)-DJ(1)*DK(3)
      DI(3)=DJ(1)*DK(2)-DJ(2)*DK(1)
C
      TVW(1)=TN(1)*DI(1)+TN(2)*DI(2)+TN(3)*DI(3)
      SPU=1.
      IF(TVW(1).LT.0.) SPU=-1.
      TVW(2)=TN(1)*DJ(1)+TN(2)*DJ(2)+TN(3)*DJ(3)
      TVW(3)=TN(1)*DK(1)+TN(2)*DK(2)+TN(3)*DK(3)
      IF(TVW(1).EQ.0.) GO TO 901
C
      T1R=1./TVW(1)
      PD(1)=PC(1)
      PD(2)=TVW(2)*T1R
      PD(3)=TVW(3)*T1R
C
      UN(1)=-SINP
      UN(2)=COSP
      UN(3)=0.
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
C
      UJ=UN(1)*DJ(1)+UN(2)*DJ(2)+UN(3)*DJ(3)
      UK=UN(1)*DK(1)+UN(2)*DK(2)+UN(3)*DK(3)
      VJ=VN(1)*DJ(1)+VN(2)*DJ(2)+VN(3)*DJ(3)
      VK=VN(1)*DK(1)+VN(2)*DK(2)+VN(3)*DK(3)
C
C
      J=0
 
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
      Q=-HAM*CFACT8
C
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)+H(3)*UN(3))*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
      T3R=Q*T1R**3
      UI=UN(1)*DI(1)+UN(2)*DI(2)+UN(3)*DI(3)
      VI=VN(1)*DI(1)+VN(2)*DI(2)+VN(3)*DI(3)
      A(2,4)=-UI*(VK*COSZ-UK*SINZ)*T3R
      A(2,5)=-VI*(VK*COSZ-UK*SINZ)*T3R
      A(3,4)= UI*(VJ*COSZ-UJ*SINZ)*T3R
      A(3,5)= VI*(VJ*COSZ-UJ*SINZ)*T3R
C
    6 T2R=T1R**2
C
      A(1,1)=1.
      A(2,2)=-UK*T2R
      A(2,3)=VK*COSL*T2R
      A(3,2)=UJ*T2R
      A(3,3)=-VJ*COSL*T2R
      A(4,4)=VK*T1R
      A(4,5)=-UK*T1R
      A(5,4)=-VJ*T1R
      A(5,5)=UJ*T1R
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RD(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C *** ERROR EXITS
C
  901 IERR=1
  910 CONTINUE
C
      RETURN
      END
*
*
* $Id: trscsp.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trscsp.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:35  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
C
      SUBROUTINE TRSCSP(PC,RC,PS,RS,H,CH,IERR,SPX)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM   SC   VARIABLES (1/P,LAMBDA,PHI,YT,ZT)
C      TO  SPLINE VARIABLES (1/P,Y',Z',Y,Z)
C
C     Authors: A. Haas and W. Wittek
C
C
C *** PC(3)     1/P,LAMBDA,PHI                          INPUT
C     PS(3)     1/P,Y',Z'                              OUTPUT
C     H(3)      MAGNETIC FIELD                          INPUT
C     RC(15)    ERROR MATRIX IN   SC   VARIABLES        INPUT     (TRIANGLE)
C     RS(15)    ERROR MATRIX IN SPLINE VARIABLES       OUTPUT     (TRIANGLE)
C     CH        CHARGE OF PARTICLE                      INPUT
C               CHARGE AND MAGNETIC FIELD ARE NEEDED
C               FOR CORRELATION TERMS (Y',YT),(Y',ZT),(Z',YT),(Z',ZT)
C               THESE CORRELATION TERMS APPEAR BECAUSE RC IS ASSUMED
C               TO BE THE ERROR MATRIX FOR FIXED S (PATH LENGTH)
C               AND RS FOR FIXED X
C
C     IERR  =   1       PARTICLE MOVES PERPENDICULAR TO X-AXIS
C                      ( Y',Z' ARE NOT DEFINED )
C     SPX       SIGN OF X-COMPONENT OF PARTICLE MOMENTUM   OUTPUT
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PC,PS,H,RC,RS,CH,SPX
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC(3),PS(3),H(3),RC(15),RS(15)
      DIMENSION UN(3),VN(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      IERR=0
      PM=PC(1)
      COSL=COS(PC(2))
      SINP=SIN(PC(3))
      COSP=COS(PC(3))
C
      TN(1)=COSL*COSP
      SPX=1.
      IF(TN(1).LT.0.) SPX=-1.
      IF(TN(1).EQ.0.) GO TO 901
      TN(2)=COSL*SINP
      TN(3)=SIN(PC(2))
C
      T1R=1./TN(1)
      PS(1)=PC(1)
      PS(2)=SINP/COSP
      PS(3)=TN(3)*T1R
C
      UN(1)=-SINP
      UN(2)=COSP
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
C
      J=0
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
      Q=-HAM*CFACT8
C
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)           )*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
 
      T3R=Q*T1R**3
      A(2,4)=-UN(1)*(VN(3)*COSZ           )*T3R
      A(2,5)=-VN(1)*(VN(3)*COSZ           )*T3R
      A(3,4)=UN(1)*(VN(2)*COSZ-UN(2)*SINZ)*T3R
      A(3,5)=VN(1)*(VN(2)*COSZ-UN(2)*SINZ)*T3R
C
    6 T2R=T1R**2
C
      A(1,1)=1.
      A(2,3)=VN(3)*COSL*T2R
      A(3,2)=UN(2)*T2R
      A(3,3)=-VN(2)*COSL*T2R
      A(4,4)=VN(3)*T1R
      A(5,4)=-VN(2)*T1R
      A(5,5)=UN(2)*T1R
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RS(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C *** ERROR EXITS
C
  901 IERR=1
  910 CONTINUE
C
      RETURN
      END
*
*
* $Id: trsdpt.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trsdpt.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE TRSDPT(PD,RD,PC,RC,H,CH,IERR,SPU,DJ,DK)
*
***************************************************************************
C
C *** TRANSFORMS ERROR MATRIX
C     FROM        VARIABLES (1/P,V',W',V,W)
C     FROM        VARIABLES (1/Pt,V',W',V,W)
C
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD,PC,H,RC,RD,CH,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD(3),PC(3),H(3),RC(15),RD(15),DJ(3),DK(3)
      DIMENSION UN(3),VN(3),DI(3),TVW(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
*
**_____________________________________________________________________
*
      IERR=0
      PM=PD(1)
      TVW(1)=1./SQRT(1.+PD(2)**2+PD(3)**2)
      IF(SPU.LT.0.) TVW(1)=-TVW(1)
      TVW(2)=PD(2)*TVW(1)
      TVW(3)=PD(3)*TVW(1)
C
      DI(1)=DJ(2)*DK(3)-DJ(3)*DK(2)
      DI(2)=DJ(3)*DK(1)-DJ(1)*DK(3)
      DI(3)=DJ(1)*DK(2)-DJ(2)*DK(1)
C
      DO 5 I=1,3
         TN(I)=TVW(1)*DI(I)+TVW(2)*DJ(I)+TVW(3)*DK(I)
    5 CONTINUE
C
      COSL=SQRT(ABS(1.-TN(3)**2))
      IF (COSL .LT. 1.E-30) COSL = 1.E-30
      COSL1=1./COSL
      TANL  = TN(3)*COSL1
*
      PC(1)=PD(1)*COSL1
      PC(2)=PD(2)
      PC(3)=PD(3)
*
      IF (ABS (TN(1)) .LT. 1.E-30) TN(1) = 1.E-30
C
      UN(1)=-TN(2)*COSL1
      UN(2)=TN(1)*COSL1
      UN(3)=0.
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
 
C
      UJ=UN(1)*DJ(1)+UN(2)*DJ(2)+UN(3)*DJ(3)
      UK=UN(1)*DK(1)+UN(2)*DK(2)+UN(3)*DK(3)
      VJ=VN(1)*DJ(1)+VN(2)*DJ(2)+VN(3)*DJ(3)
      VK=VN(1)*DK(1)+VN(2)*DK(2)+VN(3)*DK(3)
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD(J)
    4    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
C
      Q=-HAM*CFACT8
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)+H(3)*UN(3))*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
      A(1,4)=-Q*TVW(2)*SINZ*(TANL*PC(1))
      A(1,5)=-Q*TVW(3)*SINZ*(TANL*PC(1))
C
6     continue
      A(1,1) = COSL1
      A(2,2) = 1.
      A(3,3) = 1.
      A(4,4) = 1.
      A(5,5) = 1.
*
      A(1,2)=TVW(1)*VJ*(TANL*PC(1))
      A(1,3)=TVW(1)*VK*(TANL*PC(1))
C
      CALL SSMT5T(A,S,S)
C
      DO J=1,15
        RC(J)=S(J)
      ENDDO
*
      END
*
* $Id: trsdsc.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trsdsc.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
C
      SUBROUTINE TRSDSC(PD,RD,PC,RC,H,CH,IERR,SPU,DJ,DK)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM        VARIABLES (1/P,V',W',V,W)
C      TO    SC   VARIABLES (1/P,LAMBDA,PHI,YT,ZT)
C
C     Authors: A. Haas and W. Wittek
C
C
C *** PD(3)     1/P,V',W'                               INPUT
C     PC(3)     1/P,LAMBDA,PHI                         OUTPUT
C     H(3)      MAGNETIC FIELD                          INPUT
C     RD(15)    ERROR MATRIX IN 1/P,V',W',V,W           INPUT      (TRIANGLE)
C     RC(15)    ERROR MATRIX IN   SC   VARIABLES       OUTPUT      (TRIANGLE)
C     CH        CHARGE OF PARTICLE                      INPUT
C               CHARGE AND MAGNETIC FIELD ARE NEEDED
C               FOR CORRELATION TERMS (LAMBDA,V),(LAMBDA,W),(PHI,V),(PHI,W)
C               THESE CORRELATION TERMS APPEAR BECAUSE RC IS ASSUMED
C               TO BE THE ERROR MATRIX FOR FIXED S (PATH LENGTH)
C               AND RD FOR FIXED U
C     DJ(3)     UNIT VECTOR IN V-DIRECTION
C     DK(3)     UNIT VECTOR IN W-DIRECTION    OF DETECTOR SYSTEM
C
C     IERR              NOT USED
C     SPU       SIGN OF U-COMPONENT OF PARTICLE MOMENTUM    INPUT
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD,PC,H,RC,RD,CH,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD(3),PC(3),H(3),RC(15),RD(15),DJ(3),DK(3)
      DIMENSION UN(3),VN(3),DI(3),TVW(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      IERR=0
      PM=PD(1)
      TVW(1)=1./SQRT(1.+PD(2)**2+PD(3)**2)
      IF(SPU.LT.0.) TVW(1)=-TVW(1)
      TVW(2)=PD(2)*TVW(1)
      TVW(3)=PD(3)*TVW(1)
C
      DI(1)=DJ(2)*DK(3)-DJ(3)*DK(2)
      DI(2)=DJ(3)*DK(1)-DJ(1)*DK(3)
      DI(3)=DJ(1)*DK(2)-DJ(2)*DK(1)
C
      DO 5 I=1,3
         TN(I)=TVW(1)*DI(I)+TVW(2)*DJ(I)+TVW(3)*DK(I)
    5 CONTINUE
C
      PC(1)=PD(1)
      PC(2)=ASIN(TN(3))
      IF (ABS (TN(1)) .LT. 1.E-30) TN(1) = 1.E-30
      PC(3) = ATAN2 (TN(2),TN(1))
C
      COSL=SQRT(ABS(1.-TN(3)**2))
      IF (COSL .LT. 1.E-30) COSL = 1.E-30
      COSL1=1./COSL
      UN(1)=-TN(2)*COSL1
      UN(2)=TN(1)*COSL1
      UN(3)=0.
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
 
C
      UJ=UN(1)*DJ(1)+UN(2)*DJ(2)+UN(3)*DJ(3)
      UK=UN(1)*DK(1)+UN(2)*DK(2)+UN(3)*DK(3)
      VJ=VN(1)*DJ(1)+VN(2)*DJ(2)+VN(3)*DJ(3)
      VK=VN(1)*DK(1)+VN(2)*DK(2)+VN(3)*DK(3)
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD(J)
    4    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
C
      Q=-HAM*CFACT8
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)+H(3)*UN(3))*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
      A(2,4)=-Q*TVW(2)*SINZ
      A(2,5)=-Q*TVW(3)*SINZ
      A(3,4)=-Q*TVW(2)*COSZ*COSL1
      A(3,5)=-Q*TVW(3)*COSZ*COSL1
C
    6 A(1,1)=1.
      A(2,2)=TVW(1)*VJ
      A(2,3)=TVW(1)*VK
      A(3,2)=TVW(1)*UJ*COSL1
      A(3,3)=TVW(1)*UK*COSL1
      A(4,4)=UJ
      A(4,5)=UK
      A(5,4)=VJ
      A(5,5)=VK
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RC(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C *** ERROR EXITS
C
      END
*
*
* $Id: trspsc.F,v 1.1.1.3 2009/02/18 20:32:19 fisyak Exp $
*
* $Log: trspsc.F,v $
* Revision 1.1.1.3  2009/02/18 20:32:19  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
C
      SUBROUTINE TRSPSC(PS,RS,PC,RC,H,CH,IERR,SPX)
C
C *** TRANSFORMS ERROR MATRIX
C     FROM SPLINE VARIABLES (1/P,Y',Z',Y,Z)
C      TO    SC   VARIABLES (1/P,LAMBDA,PHI,YT,ZT)
C
C     Authors: A. Haas and W. Wittek
C
C
C *** PS(3)     1/P,Y',Z'                               INPUT
C     PC(3)     1/P,LAMBDA,PHI                         OUTPUT
C     H(3)      MAGNETIC FIELD                          INPUT
C     RS(15)    ERROR MATRIX IN SPLINE VARIABLES        INPUT      (TRIANGLE)
C     RC(15)    ERROR MATRIX IN   SC   VARIABLES       OUTPUT      (TRIANGLE)
C     CH        CHARGE OF PARTICLE                      INPUT
C               CHARGE AND MAGNETIC FIELD ARE NEEDED
C               FOR CORRELATION TERMS (LAMBDA,Y),(LAMBDA,Z),(PHI,Y),(PHI,Z)
C               THESE CORRELATION TERMS APPEAR BECAUSE RC IS ASSUMED
C               TO BE THE ERROR MATRIX FOR FIXED S (PATH LENGTH)
C               AND RS FOR FIXED X
C
C     IERR              NOT USED
C     SPX       SIGN OF X-COMPONENT OF PARTICLE MOMENTUM    INPUT
C
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PS,PC,H,RC,RS,CH,SPX
#endif
#include "geant321/trcom3.inc"
      DIMENSION PS(3),PC(3),H(3),RC(15),RS(15)
      DIMENSION UN(3),VN(3)
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      IERR=0
      PM=PS(1)
      TN(1)=1./SQRT(1.+PS(2)**2+PS(3)**2)
      IF(SPX.LT.0.) TN(1)=-TN(1)
      TN(2)=PS(2)*TN(1)
      TN(3)=PS(3)*TN(1)
C
      PC(1)=PS(1)
      PC(2)=ASIN(TN(3))
      IF (ABS (TN(1)) .LT. 1.E-30) TN(1) = 1.E-30
      PC(3) = ATAN2 (TN(2),TN(1))
C
      COSL=SQRT(ABS(1.-TN(3)**2))
      IF (COSL .LT. 1.E-30) COSL = 1.E-30
      COSL1=1./COSL
      UN(1)=-TN(2)*COSL1
      UN(2)=TN(1)*COSL1
C
      VN(1)=-TN(3)*UN(2)
      VN(2)=TN(3)*UN(1)
      VN(3)=COSL
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RS(J)
    4    CONTINUE
   10 CONTINUE
C
      IF(CH.EQ.0.) GO TO 6
 
      HA=SQRT(H(1)**2+H(2)**2+H(3)**2)
      HAM=HA*PM
      IF(HAM.EQ.0.) GO TO 6
      HM=CH/HA
C
      Q=-HAM*CFACT8
C
      SINZ=-(H(1)*UN(1)+H(2)*UN(2)           )*HM
      COSZ= (H(1)*VN(1)+H(2)*VN(2)+H(3)*VN(3))*HM
      A(2,4)=-Q*TN(2)*SINZ
      A(2,5)=-Q*TN(3)*SINZ
      A(3,4)=-Q*TN(2)*COSZ*COSL1
      A(3,5)=-Q*TN(3)*COSZ*COSL1
C
    6 A(1,1)=1.
      A(2,2)=TN(1)*VN(2)
      A(2,3)=TN(1)*VN(3)
      A(3,2)=TN(1)*UN(2)*COSL1
      A(4,4)=UN(2)
      A(5,4)=VN(2)
      A(5,5)=VN(3)
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RC(J)=S(J)
   20    CONTINUE
   25 CONTINUE
*
* *** ERROR EXITS
*
      END
