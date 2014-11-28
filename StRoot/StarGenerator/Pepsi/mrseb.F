*72*********************************************************************
      SUBROUTINE MRSEB
     &                  (X,SCALE,MODE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
C***************************************************************C
C								C
C     This is a package for the new MRS(A prime,G) parton       C
C     distributions. The minimum Q^2  value is 5 GeV^2 and the  C
C     x range is, as before 10^-5 < x < 1. MSbar factorization  C
C     is used. The package reads 2 grids, which are in separate C
C     files (A prime=for020.dat/ftn20, G=for021.dat/ftn21).     C  
C     Note that x times the parton distribution is returned,    C
C     Q is the scale in GeV,                                    C
C     and Lambda(MSbar,nf=4) = 231/255 MeV for A prime/G.       C
C								C
C	MODE=20 for MRS(A prime)                                C
C	MODE=21 for MRS(G)                                      C
C								C
C         The reference is :                                    C
C         A.D. Martin, R.G. Roberts and W.J. Stirling,          C
C         Phys. Lett. B354 (1995) 155-162                       C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C         THE FIRST NUMBER IN THE 20 GRID IS 0.00341            C
C         THE FIRST NUMBER IN THE 21 GRID IS 0.00269            C
C                                                               C
C         HI: changed SCALE to Q^2                              C
C***************************************************************C
      IMPLICIT REAL*8(A-H,O-Z)
      DATA ICOUNT /0/
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      Q2=SCALE
*     Q2STA= .5D0
      Q2STA = .625D0
      Q2FIN = 1310720.D0
      Xmin = 1D-5
      Xmax = 1D0

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to minimal value !',Q2
         ENDIF   
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to minimal value !',X
         ENDIF   
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to maximal value !',X
         ENDIF         
      endif

      IF(MODE.EQ.20) 
     .   CALL STRC20(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.21) 
     .   CALL STRC21(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.10) THEN 
         IF(Q2.LT.0.625D0.OR.Q2.GT.1310720.D0) PRINT 99
      IF(Q2.GT.5D0) CALL STRC10(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,
     X        GLU)
      IF(Q2.LE.5D0) CALL STRC11(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,
     X        GLU)
 99      FORMAT('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ')
      ENDIF
*  check for positive definiteness

      IF(UPV.LT.0.0) UPV=0.0
      IF(DNV.LT.0.0) DNV=0.0
      IF(USEA.LT.0.0) USEA=0.0
      IF(DSEA.LT.0.0) DSEA=0.0
      IF(GLU.LT.0.0) GLU=0.0
      IF(STR.LT.0.0) STR=0.0
      IF(CHM.LT.0.0) CHM=0.0
      IF(BOT.LT.0.0) BOT=0.0

      RETURN
      END
