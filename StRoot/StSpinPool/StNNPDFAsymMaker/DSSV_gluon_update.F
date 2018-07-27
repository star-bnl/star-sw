c       Example 
c        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c        CALL DSSVINI
c
c        DO 10 i=0,49
c        X=10.d0**(-3.d0+3.d0*i/50)
c        Q2=10.0
c        CALL DSSVGUPDATE(X,Q2,DUV,DDV,DUBAR,DDBAR,DSTR,DGLU)
c       (remember it gives x times the pdf) 
c        WRITE(6,100) X,DUV,DDV,DUBAR,DGLU
c 10     continue
c 100    FORMAT(7(F11.7))
c
c        END
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                            
C       ------GLUON UPDATE FOR DSSV POLARIZED PARTON DISTRIBUTIONS ------                    
C                                                                            
C       REFERENCE: arXiv:1404.4293                                                          
C                                                                            
C       INPUT:                                                               
C           X     :  BJORKEN-X BETWEEN 10**(-5)< X < 1.0                     
C           Q2    :  SCALE**2 IN GeV**2 BETWEEN  1 < Q**2 < 10**5 GeV**2     
C                                                                            
C       OUTPUT:                                                              
C           DUV   :  X * U VALENCE DISTRIBUTION                              
C           DDV   :  X * D VALENCE DISTRIBUTION                              
C           DUBAR :  X * UBAR DISTRIBUTION                                   
C           DDBAR :  X * DBAR DISTRIBUTION                                   
C           DSTR  :  X * STRANGE DISTRIBUTION                                
C           DGLU  :  X * GLUON DISTRIBUTION                                  
                   
C                                                                            
C       IMPORTANT:                                                           
C           ALWAYS X*DISTRIBUTION IS RETURNED !!!                            
C           ALL PDFs ARE IN THE MSbar SCHEME                                 
C                                                                            
C           BEFORE CALLING THE SUBROUTINE 'DSSVFIT' FOR THE FIRST TIME,      
C           THE SUBROUTINE 'DSSVINI' MUST BE CALLED (ONLY ONCE) TO           
C           INITIALIZE THE GRIDS !!                                          
C                                                                            
C       IN CASE OF PROBLEMS, DOUBTS, ETC, PLEASE E-MAIL US:                  
C           D. de Florian  deflo@df.uba.ar                                   
C           R. Sassot      sassot@df.uba.ar                                  
C           M. Stratmann   marco.stratmann@uni-tuebingen.de                               
C           W. Vogelsang   werner.vogelsang@uni-tuebingen.de                  
C                                                                            
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C---------------------------------------------------------------------
      SUBROUTINE DSSVGUPDATE(X,Q2,DUV,DDV,DUBAR,DDBAR,DSTR,DGLU)
C---------------------------------------------------------------------
      IMPLICIT NONE
C...
      INTEGER NPART, NX, NQ, NARG
      PARAMETER (NPART=8, NX=47, NQ=30, NARG=2)
C...
      INTEGER IQ, IX
      INTEGER NA(NARG+3)
      DOUBLE PRECISION XUF(NX,NQ), XDF(NX,NQ), XUBF(NX,NQ), XDBF(NX,NQ), 
     1          XSF(NX,NQ), XGF(NX,NQ),
     2          PARTON (NPART,NQ,NX-1), QS(NQ), XB(NX), XT(NARG+3), 
     3          ARRF(NX+NQ) 
      DOUBLE PRECISION X, Q2
      DOUBLE PRECISION DUV, DDV, DUBAR, DDBAR, DSTR, DGLU, G1P, G1N
      DOUBLE PRECISION XB0, XB1
      DOUBLE PRECISION DFINT
C...      
      COMMON/ DSSVGRID/XUF, XDF, XUBF, XDBF, XSF, XGF,
     1                ARRF, NA
C
C...  CHECK IF X AND Q2 VALUES ARE WITHIN RANGE OF THE GRID: 
C
      IF ( (X.LT.1.0D-5) .OR. (X.GT.1.0D0) ) THEN
         WRITE(6,91) 
 91      FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
         STOP 
      ENDIF
      IF ( (Q2.LT.0.8D0) .OR. (Q2.GT.1.D6) ) THEN
         WRITE(6,92) 
 92      FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
         STOP
      ENDIF
C
C...  INTERPOLATION AND OUTPUT:
C
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      DUV   = DFINT(NARG,XT,NA,ARRF,XUF)  * (1.D0-X)**3 * X
      DDV   = DFINT(NARG,XT,NA,ARRF,XDF)  * (1.D0-X)**4 * X
      DUBAR = DFINT(NARG,XT,NA,ARRF,XUBF) * (1.D0-X)**8 * X**0.5
      DDBAR = DFINT(NARG,XT,NA,ARRF,XDBF) * (1.D0-X)**8 * X**0.5
      DSTR  = DFINT(NARG,XT,NA,ARRF,XSF)  * (1.D0-X)**8 * X**0.5
      DGLU  = DFINT(NARG,XT,NA,ARRF,XGF)  * (1.D0-X)**5 * X**1.0
C...
 60   RETURN
      END
C
C---------------------------
      SUBROUTINE DSSVINIG(FILENAME)
C---------------------------
      IMPLICIT NONE
C...
      CHARACTER FILENAME*60
      INTEGER NPART, NX, NQ, NARG
      PARAMETER (NPART=8, NX=47, NQ=30, NARG=2)
C...
      INTEGER NA(NARG+3)
      DOUBLE PRECISION PARTON (NPART,NQ,NX-1)
      DOUBLE PRECISION QS(NQ), XB(NX)
      DOUBLE PRECISION XUF(NX,NQ), XDF(NX,NQ), XUBF(NX,NQ), XDBF(NX,NQ), 
     1          XSF(NX,NQ), XGF(NX,NQ), XT(NARG+3), ARRF(NX+NQ)
      DOUBLE PRECISION XB0, XB1
      INTEGER M, N
      INTEGER IQ, IX
C...
      COMMON/ DSSVGRID/XUF, XDF, XUBF, XDBF, XSF, XGF,
     1                ARRF, NA
C...  BJORKEN-X AND Q**2 VALUES OF THE GRID :
C
      DATA QS / 0.8D0, 1.0D0, 1.25d0, 1.5D0, 2.d0, 2.5D0, 
     1     4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2     1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3     3.2D3, 5.8D3, 1.0D4, 1.8D4, 3.2D4, 5.8D4, 
     4     1.0D5, 1.8D5, 3.2D5, 5.8D5, 1.0D6  /
      DATA XB / 
     1           1.0D-5, 4.D-5, 6.7D-5, 1.0D-4, 1.4D-4, 2.0D-4,
     2           3.0D-4, 4.5D-4, 6.7D-4, 1.0D-3, 1.4D-3, 2.0D-3,
     3           3.0D-3, 4.5D-3, 6.7D-3, 1.0D-2, 1.4D-2, 2.0D-2,
     4           3.0D-2, 4.5D-2, 0.06, 0.08, 0.1, 0.125, 0.15,
     5           0.175, 0.2, 0.225, 0.25, 0.275, 0.3, 0.325,
     6           0.35, 0.375, 0.4, 0.45, 0.5, 0.55, 0.6,
     7           0.65, 0.7,  0.75, 0.8, 0.85, 0.9, 0.95, 1.0/ 

C...
C      imode= 0 best fit DSSV


c      if (imode.eq.0) then
       OPEN(UNIT=71,FILE=FILENAME,STATUS='OLD')
c       else
c            write(6,*) "wrong fit"
c      endif      

C...
      DO 15 M = 1, NX-1
         DO 15 N = 1, NQ
            READ(71,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1           PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M)
 90         FORMAT (8(1PE12.4))
 15   CONTINUE
C...
      CLOSE(71)
C
C.... ARRAYS FOR THE INTERPOLATION SUBROUTINE :
C
      DO 10 IQ = 1, NQ
         DO 20 IX = 1, NX-1
            XB0 = XB(IX) 
            XB1 = 1.D0-XB(IX)
            XUF(IX,IQ)  = PARTON(1,IQ,IX) / (XB1**3 * XB0)
            XDF(IX,IQ)  = PARTON(2,IQ,IX) / (XB1**4 * XB0)
            XUBF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**8 * XB0**0.5) 
            XDBF(IX,IQ) = PARTON(4,IQ,IX) / (XB1**8 * XB0**0.5) 
            XSF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**8 * XB0**0.5) 
            XGF(IX,IQ)  = PARTON(6,IQ,IX) / (XB1**5 * XB0**1.0)
  20      CONTINUE
         XUF(NX,IQ)  = 0.D0
         XDF(NX,IQ)  = 0.D0
         XUBF(NX,IQ) = 0.D0
         XDBF(NX,IQ) = 0.D0
         XSF(NX,IQ)  = 0.D0
         XGF(NX,IQ)  = 0.D0
 10   CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
         ARRF(IX) = DLOG(XB(IX))
 30   CONTINUE
      DO 40 IQ = 1, NQ
         ARRF(NX+IQ) = DLOG(QS(IQ))
 40   CONTINUE
 16   CONTINUE
C...
      RETURN
      END
*
*...CERN LIBRARY ROUTINE E104 (INTERPOLATION) :
*
      FUNCTION DFINT(NARG,ARG,NENT,ENT,TABLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ARG(5),NENT(5),ENT(73),TABLE(1200)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
      DO 5 I=1,NARG
         NCOMB(I)=1
         JB=JA-1+NENT(I)
         DO 2 J=JA,JB
            IF (ARG(I).LE.ENT(J)) GO TO 3
 2       CONTINUE
         J=JB
 3       IF (J.NE.JA) GO TO 4
         J=J+1
 4       JR=J-1
         D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
         IENT(I)=J-JA
         KD=KD+IENT(I)*M
         M=M*NENT(I)
 5    JA=JB+1
      DFINT=0.D0
 10   FAC=1.D0
      IADR=KD
      IFADR=1
      DO 15 I=1,NARG
         IF (NCOMB(I).EQ.0) GO TO 12
         FAC=FAC*(1.D0-D(I))
         GO TO 15
 12      FAC=FAC*D(I)
         IADR=IADR-IFADR
 15   IFADR=IFADR*NENT(I)
      DFINT=DFINT+FAC*TABLE(IADR)
      IL=NARG
 40   IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
      DO 50  K=IL,NARG
 50   NCOMB(K)=1
      GO TO 10
 80   IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END


