
      SUBROUTINE PHO_SELSXS(Msoft,Msmin,Xpot1,Xpot2,Xmin,Xs1,Xs2,Xmax1,
     &                      Xmax2,Xsoft1,Xsoft2,Irej)
C***********************************************************************
C
C    select x values of soft string ends (rescaling method)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , fac1 , fac2 , pot , revp , xldif , 
     &                 xlmax , xlmin , Xmax1 , Xmax2 , Xmin , Xpot1 , 
     &                 Xpot2 , Xs1 , Xs2 , Xsoft1 , Xsoft2 , xsum1 , 
     &                 xsum2 , z1
      DOUBLE PRECISION z2
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
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
 
      DIMENSION xldif(2,50) , xlmin(2,50) , revp(2,50) , pot(2,50)
 
      Irej = 0
 
 
 100  IF ( Msoft.EQ.1 ) THEN
         Xsoft1(1) = 1.D0 - Xs1
         Xsoft1(2) = 0.D0
         Xsoft2(1) = 1.D0 - Xs2
         Xsoft2(2) = 0.D0
         RETURN
      END IF
 
      DO i = 1 , Msoft
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
 
      itry0 = 0
 200  itry0 = itry0 + 1
      IF ( itry0.GE.IPAmdl(180) ) THEN
         IF ( Msoft-Msmin.GE.2 ) THEN
            Msoft = Msmin
            GOTO 100
         END IF
         GOTO 300
      END IF
      xsum1 = 0.D0
      xsum2 = 0.D0
      DO i = 1 , Msoft
         itry1 = 0
 250     z1 = xldif(1,i)*DT_RNDM(Xs1) + xlmin(1,i)
         z2 = xldif(2,i)*DT_RNDM(Xs2) + xlmin(2,i)
         Xsoft1(i) = z1**revp(1,i)
         Xsoft2(i) = z2**revp(2,i)
         itry1 = itry1 + 1
         IF ( itry1.GE.50 ) GOTO 300
         IF ( (Xsoft1(i)*Xsoft2(i)).LT.AS ) GOTO 250
         xsum1 = xsum1 + Xsoft1(i)
         xsum2 = xsum2 + Xsoft2(i)
      END DO
      fac1 = (1.D0-Xs1)/xsum1
      fac2 = (1.D0-Xs2)/xsum2
      DO i = 1 , Msoft
         Xsoft1(i) = Xsoft1(i)*fac1
         Xsoft2(i) = Xsoft2(i)*fac2
         IF ( Xsoft1(i).LT.Xmin(1,i) ) GOTO 200
         IF ( Xsoft2(i).LT.Xmin(2,i) ) GOTO 200
         IF ( Xsoft1(i)*Xsoft2(i).LT.AS ) GOTO 200
      END DO
 
      Xs1 = 1.D0 - Xsoft1(1)
      Xs2 = 1.D0 - Xsoft2(1)
      RETURN
 
 300  Irej = 1
      IF ( IDEb(14).GE.2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,3I4)') 'PHO_SELSXS: ' , 
     &        'rejection (MSOFT,ITRY0/1)' , Msoft , itry0 , itry1
         DO i = 1 , Msoft
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I4,1P4E11.3)') i , Xmin(1,i)
     &           , Xmin(2,i) , Xmax1 , Xmax2
         END DO
      END IF
 
      END SUBROUTINE
