* $Id: circov.F,v 1.2 1998/01/27 00:35:41 fisyak Exp $
* $Log: circov.F,v $
* Revision 1.2  1998/01/27 00:35:41  fisyak
* Split sources
*
      INTEGER FUNCTION CIRCOV(X,Y,W,N,A,B,R,H)
C>--------------------------------------------------------------------
C
C CIRCOV - a covariance matrix calculation program for circle fitting
C
C DESCRIPTION:
C Compute the covariance matrix of an effective circle fitting algorithm
C The circle equation is (X(I)-A)**2 + (Y(I)-B)**2 = R**2. 
C The functional minimum is W(I)*[(X(I)-A)**2+(Y(I)-B)**2-R*R]**2/(R*R)
C For details about the algorithm, see
C N.I. CHERNOV, G.A. OSOSKOV, COMPUT. PHYS. COMMUN. 33(1984) 329-333
C
C INPUT ARGUMENTS:                                             
C      X(N),Y(N),W(N) - Input values and weights                
C      N              - Number of pairs of data points,      INTEGER
C      X              - Array of data points in X-axis,      REAL    
C      Y              - Array of data points in Y-axis,      REAL
C      W              - Array of weights for data points,    REAL
C      A              - Best fitted circle center in X axis, REAL
C      B              - Best fitted circle center in Y axis, REAL
C      R              - Best fitted radius                   REAL
C
C OUTPUT ARGUMENTS:            
C     H(3,3)          - A 3x3 symmetric covariance matrix,   REAL
C                       H(1,1)=VAR(A)   H(1,2)=COV(A,B)  H(1,3)=COV(A,R)
C                       H(2,1)=COV(B,A) H(2,1)=VAR(B)    H(2,3)=COV(B,R)
C                       H(3,1)=COV(R,A) H(3,1)=COV(R,A)  H(3,3)=VAR(R)
C     CIRCOV          - -1 Got some errors
C                        0 Successfully return 
C
C  AUTHOR:
C  Jawluen Tang, Physics department , UT-Austin 
C                                                  
C  USAGE:                                                       
C     IRET=CIRCOV(X,Y,W,N,A,B,R,H)                   
C
C  CREATION DATE:
C  10-AUGUST-1993
C  Last modified,    Jawluen Tang, August 10,1993               
C<---------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Comment out unused variables.
C

        IMPLICIT NONE
C       REAL SUM,SX,SY,SXX,SXY,SYY,SR2,SR4,SRX,SRY
C       REAL GAMMA
        REAL TEMP
        REAL A,B,R
        REAL DETM
        REAL DMAX
        REAL X(*),Y(*),W(*)
        REAL H(3,3)
        REAL S1, C1, RATIO, HYP
        INTEGER I,J,K,N,L,IK(3),JK(3),NOD

        circov = 0

C-----------------------
C     N must be > 3
C----------------------
        if(n.lT.3) then
           write(6,*)' Only',n,' points input'
           write(6,*)' Not enough data to fit the CIRCLE !'
           CIRCOV=-1
           return
         endif  

C     initialization 

C        SUM=0.0
C        SX =0.0
C        SY =0.0
C        SXX=0.0
C        SXY=0.0
C        SYY=0.0
C        SR2=0.0
C        SR4=0.0
C        SRX=0.0
C        SRY=0.0
C
C     find sum , sumx ,sumy, sumxx, sumyy, sumxy, sumr2, sumr4, sumrx, sumry
C
C        DO 10 I=1,N
C            SUM=SUM + W(I)
C            SX =SX  + W(I)*X(I)
C            SY =SY  + W(I)*Y(I)
C            SXX=SXX + W(I)*X(I)*X(I)
C            SXY=SXY + W(I)*X(I)*Y(I)
C            SYY=SYY + W(I)*Y(I)*Y(I)
C            SR2=SR2 + W(I)*((X(I)-A)**2 + (Y(I)-B)**2)
C            SR4=SR4 + W(I)*(((X(I)-A)**2 + (Y(I)-B)**2)**2)
C            SRX=SRX + W(I)*X(I)*((X(I)-A)**2 + (Y(I)-B)**2)
C            SRY=SRY + W(I)*Y(I)*((X(I)-A)**2 + (Y(I)-B)**2)
C10      CONTINUE
C        IF(SUM.EQ.0.0) THEN
C           WRITE(6,*)'Got serious problem in WEIGHTS !'
C           WRITE(6,*)'Check your input data...........'
C           CIRCOV=-1
C           RETURN
C         ENDIF

C        SX = SX/SUM
C        SY = SY/SUM
C        SXX = SXX/SUM
C        SXY = SXY/SUM
C        SYY = SYY/SUM
C        SR2 = SR2/SUM
C        SR4 = SR4/SUM
C        SRX = SRX/SUM
C        SRY = SRY/SUM
C        GAMMA=R*R-A*A-B*B
        DO I=1,3
           DO J=1,3
              H(I,J)=0.
           ENDDO
        ENDDO
        IF(R.EQ.0.0)THEN
           WRITE(6,*)' FITTED RADIUS IS 0 !!'
           RETURN
        ENDIF
C
C     First, compute the inverse error matrix H(3,3)
C
C        H(1,1)=4.*(1.5*SXX+0.5*SYY-3.*A*SX-B*SY+
C     >         (A*A-0.5*GAMMA)*SUM)/(R*R)
C        H(1,2)=4.*(SXY-A*SY-B*SX+A*B*SUM)/(R*R)
C        H(1,3)=4.*(SRX-A*SR2)/(R**3)
C        H(2,1)=H(1,2)
C        H(2,2)=4.*(1.5*SYY+0.5*SXX-3.*B*SY-A*SX+
C     >         (B*B-0.5*GAMMA)*SUM)/(R*R)
C        H(2,3)=4.*(SRY-B*SR2)/(R**3)
C        H(3,1)=H(1,3)
C        H(3,2)=H(2,3)
C        H(3,3)=3.*SR4/(R**4) + SUM
        DO I = 1,N
           HYP = SQRT((X(I)-A)**2 + (Y(I)-B)**2)
           S1 = (X(I)-A)/HYP
           C1 = (Y(I)-B)/HYP
           RATIO = R/HYP

           H(1,1) = H(1,1) + (W(I)*(1+RATIO*(S1*S1-1)))
           H(1,2) = H(1,2) + (W(I)*RATIO*S1*C1)
           H(1,3) = H(1,3) + (W(I)*S1)
           H(2,1) = H(1,2)
           H(2,2) = H(2,2) + (W(I)*(1+RATIO*(C1*C1-1)))
           H(2,3) = H(2,3) + (W(I)*C1)
           H(3,1) = H(1,3)
           H(3,2) = H(2,3)
           H(3,3) = H(3,3) + W(I)
        ENDDO
C         write(6,*) 'circov'
C         write(6,*) 'h11 = ',h(1,1)
C         write(6,*) 'h12 = ',h(1,2)
C         write(6,*) 'h13 = ',h(1,3)
C         write(6,*) 'h21 = ',h(2,1)
C         write(6,*) 'h22 = ',h(2,2)
C         write(6,*) 'h23 = ',h(2,3)
C         write(6,*) 'h31 = ',h(3,1)
C         write(6,*) 'h32 = ',h(3,2)
C         write(6,*) 'h33 = ',h(3,3)

C
C     Second, inverse H matrix to obtain covariance matrix
C
C     The following routine is to invert a square symmetric matrix
C     (in our case,it is a 3x3 matrix,so NOD=3)and calculate its
C     determinant,substituting the inverse matrix into the same array
C     of the original matrix H.
C     See Philip R. Bevington,"Data reduction and error analysis for
C     the physical science",p302
C
C
        NOD=3
        DETM=1.00
        DO 100 K=1,NOD
           DMAX=0.D0
 21        DO 30 I=K,NOD
              DO 30 J=K,NOD
 23              IF(ABS(DMAX)-ABS(H(I,J)))24,24,30
 24              DMAX=H(I,J)
                 IK(K)=I
                 JK(K)=J
 30        CONTINUE
 31        IF(DMAX) 41,32,41
 32        DETM=0.D0
C          WRITE(6,*)'Determinant is ZERO!'
           RETURN
 41        I=IK(K)
           IF(I-K)21,51,43
 43        DO 50 J=1,NOD
                 TEMP=H(K,J)
                 H(K,J)=H(I,J)
 50              H(I,J)=-TEMP
 51              J=JK(K)
                 IF(J-K) 21,61,53
 53              DO 60 I=1,NOD
                    TEMP=H(I,K)
                    H(I,K)=H(I,J)
 60                 H(I,J)=-TEMP
 61                 DO 70 I=1,NOD
                       IF(I-K)63,70,63
 63                    H(I,K)=-H(I,K)/DMAX
 70                 CONTINUE
 71                 DO 80 I=1,NOD
                       DO 80 J=1,NOD
                          IF(I-K) 74,80,74
 74                       IF(J-K) 75,80,75
 75                       H(I,J)=H(I,J)+H(I,K)*H(K,J)
 80                 CONTINUE
 81                 DO 90 J=1,NOD
                       IF(J-K) 83,90,83
 83                    H(K,J)=H(K,J)/DMAX
 90                 CONTINUE
                    H(K,K)=1.D0/DMAX
 100                DETM=DETM*DMAX
 101   DO 130 L=1,NOD
          K=NOD-L+1
          J=IK(K)
          IF(J-K) 111,111,105
 105      DO 110 I=1,NOD
             TEMP=H(I,K)
             H(I,K)=-H(I,J)
             H(I,J)=TEMP
 110         CONTINUE
 111         I=JK(K)
             IF(I-K) 130,130,113
 113         DO 120 J=1,NOD
                TEMP=H(K,J)
                H(K,J)=-H(I,J)
 120            H(I,J)=TEMP
 130            CONTINUE

 
        RETURN
        END
