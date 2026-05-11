
      SUBROUTINE PHO_SELSXR(Msoft,Msmin,Xpot1,Xpot2,Xmin,Xs1,Xs2,Xmax1,
     &                      Xmax2,Xsoft1,Xsoft2,Irej)
C***********************************************************************
C
C    select x values of soft string ends (rejection method)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , pot , revp , xldif , xlmax , xlmin , 
     &                 Xmax1 , Xmax2 , Xmin , xmin1 , xmin2 , xmink , 
     &                 Xpot1 , Xpot2 , xrest1 , xrest2 , Xs1 , Xs2 , 
     &                 Xsoft1 , Xsoft2
      DOUBLE PRECISION xwmax , xx , z1 , z2
      INTEGER i , Irej , itry0 , itry1 , Msmin , Msoft
      SAVE 
 
      DIMENSION Xpot1(*) , Xpot2(*) , Xmin(2,*) , Xsoft1(*) , Xsoft2(*)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
 
      DIMENSION xldif(2,50) , xlmin(2,50) , revp(2,50) , pot(2,50)
 
      IF ( IDEb(13).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 'PHO_SELSXR:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4,5E11.3)')
     &         'MSOFT,XS1,XS2,XMAX1,2' , Msoft , Xs1 , Xs2 , Xmax1 , 
     &        Xmax2
         DO i = 1 , Msoft
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4,2E12.3)') 'EXPONENTS' , 
     &           i , Xpot1(i) , Xpot2(i)
         END DO
      END IF
C
      Irej = 0
C
      xmink = MAX(PSOmin/ECM*2.D0,XSOmin)
      xmin1 = MAX(AS/Xmax1,xmink)
      xmin2 = MAX(AS/Xmax2,xmink)
C
      IF ( Msoft.EQ.1 ) THEN
         Xsoft1(2) = 0.D0
         Xsoft2(2) = 0.D0
         RETURN
      END IF
      xwmax = MAX(Xmax1**Xpot1(1),xmin1**Xpot1(1))
     &        *MAX(Xmax2**Xpot2(1),xmin2**Xpot2(1))
C
C
 100  DO i = 2 , Msoft
         pot(1,i) = Xpot1(i) + 1.D0
         pot(2,i) = Xpot2(i) + 1.D0
         revp(1,i) = 1.D0/pot(1,i)
         revp(2,i) = 1.D0/pot(2,i)
         xlmin(1,i) = Xmin(1,i)**pot(1,i)
         xlmax = Xmax1**pot(1,i)
         xldif(1,i) = xlmax - xlmin(1,i)
         xlmin(2,i) = Xmin(2,i)**pot(2,i)
         xlmax = Xmax2**pot(2,i)
         xldif(2,i) = xlmax - xlmin(2,i)
      END DO
C
      itry0 = 0
 200  itry0 = itry0 + 1
      IF ( itry0.GE.IPAmdl(181) ) THEN
         IF ( Msoft-Msmin.GE.2 ) THEN
            Msoft = Msmin
            GOTO 100
         END IF
         GOTO 300
      END IF
      xrest1 = 1.D0 - Xs1
      xrest2 = 1.D0 - Xs2
      DO i = 2 , Msoft
         itry1 = 0
 
 250     z1 = xldif(1,i)*DT_RNDM(Xs1) + xlmin(1,i)
         z2 = xldif(2,i)*DT_RNDM(Xs2) + xlmin(2,i)
         Xsoft1(i) = z1**revp(1,i)
         Xsoft2(i) = z2**revp(2,i)
         itry1 = itry1 + 1
         IF ( itry1.GE.50 ) GOTO 300
         IF ( (Xsoft1(i)*Xsoft2(i)).LT.AS ) GOTO 250
 
         xrest1 = xrest1 - Xsoft1(i)
         IF ( xrest1.LT.xmin1 ) GOTO 200
         IF ( xrest1.LT.Xmin(1,1) ) GOTO 200
         xrest2 = xrest2 - Xsoft2(i)
         IF ( xrest2.LT.xmin2 ) GOTO 200
         IF ( xrest2.LT.Xmin(2,1) ) GOTO 200
         IF ( xrest1*xrest2.LT.AS ) GOTO 200
 
      END DO
      Xsoft1(1) = xrest1
      Xsoft2(1) = xrest2
      Irej = 0
C     XX = 1.D0
C     DO 200 I=2,MSOFT
C       XX = XX*XSOFT1(I)**XPOT1(I)*XSOFT2(I)**XPOT2(I)
C200  CONTINUE
      xx = Xsoft1(1)**Xpot1(1)*Xsoft2(1)**Xpot2(1)
      IF ( (xx-DT_RNDM(xx)*xwmax).LT.0.D0 ) GOTO 200
 
      Xs1 = 1.D0 - xrest1
      Xs2 = 1.D0 - xrest2
      RETURN
 
 300  Irej = 1
      IF ( IDEb(13).GE.2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I4)')
     &         'PHO_SELSXR: REJECTION(ITRY0/1)' , itry0 , itry1
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,3E12.3)') 'XMAX1,2,AS:' , 
     &        Xmax1 , Xmax2 , AS
      END IF
 
      END SUBROUTINE
