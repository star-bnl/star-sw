
      SUBROUTINE PHO_SELSX2(Xpot1,Xpot2,Xmin,Xsum1,Xsum2,Xmax1,Xmax2,
     &                      Xs1,Xs2,Irej)
C***********************************************************************
C
C    select x values of soft string ends using PHO_RNDBET
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION bet1 , bet2 , fac , fac1 , fac2 , gam1 , gam2 , 
     &                 PHO_RNDBET , x3 , x4 , Xmax1 , Xmax2 , Xmin , 
     &                 Xpot1 , Xpot2 , Xs1 , Xs2 , Xsum1 , Xsum2
      INTEGER i , Irej , itry0 , itry1 , itry2
      SAVE 
 
      DIMENSION Xpot1(*) , Xpot2(*) , Xmin(2,*) , Xs1(*) , Xs2(*)
 
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
 
      Irej = 0
 
      IF ( IDEb(32).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 'PHO_SELSX2:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,5E11.3)')
     &         'AS,XSUM1,2,XMAX1,2' , AS , Xsum1 , Xsum2 , Xmax1 , Xmax2
         DO i = 1 , 2
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4,2E12.3)') 'EXPONENTS' , 
     &           i , Xpot1(i) , Xpot2(i)
         END DO
      END IF
 
      fac1 = 1.D0 - Xsum1
      fac2 = 1.D0 - Xsum2
      fac = fac1*fac2
      gam1 = Xpot1(1) + 1.D0
      gam2 = Xpot2(1) + 1.D0
      bet1 = Xpot1(2) + 1.D0
      bet2 = Xpot2(2) + 1.D0
 
      itry0 = 0
      DO i = 1 , IPAmdl(182)
 
         itry1 = 0
 50      X1 = PHO_RNDBET(gam1,bet1)
         itry1 = itry1 + 1
         IF ( itry1.GE.50 ) GOTO 200
         IF ( (X1.LE.Xmin(1,1)) .OR. ((1.D0-X1).LE.Xmin(1,2)) ) GOTO 50
 
         itry2 = 0
 100     X2 = PHO_RNDBET(gam2,bet2)
         itry2 = itry2 + 1
         IF ( itry2.GE.50 ) GOTO 200
         IF ( (X2.LE.Xmin(2,1)) .OR. ((1.D0-X2).LE.Xmin(2,2)) ) GOTO 100
 
         x3 = 1.D0 - X1
         x4 = 1.D0 - X2
         IF ( X1*X2*fac.GT.AS ) THEN
            IF ( x3*x4*fac.GT.AS ) THEN
               Xs1(1) = X1*fac1
               Xs1(2) = x3*fac1
               Xs2(1) = X2*fac2
               Xs2(2) = x4*fac2
               IF ( Xs1(1).GT.Xmin(1,1) ) THEN
                  IF ( Xs2(1).GT.Xmin(2,1) ) THEN
                     IF ( Xs1(2).GT.Xmin(1,2) ) THEN
                        IF ( Xs2(2).GT.Xmin(2,2) ) THEN
                           Xsum1 = Xsum1 + Xs1(2)
                           Xsum2 = Xsum2 + Xs2(2)
                           GOTO 99999
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
         itry0 = itry0 + 1
 
      END DO
 
 200  Irej = 1
      IF ( IDEb(32).GE.2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I4)')
     &         'PHO_SELSX2: REJECTION(ITRY0/1/2)' , itry0 , itry1 , 
     &        itry2
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,3E12.3)') 'XMAX1,2,AS:' , 
     &        Xmax1 , Xmax2 , AS
      END IF
 
99999 END SUBROUTINE
