
      SUBROUTINE DT_SAPTRE(Idx1,Idx2)
 
C***********************************************************************
C p-t sampling for two-resonance systems. ("BAMJET-like" method)       *
C        IDX1,IDX2       indices of resonances ("chains") in DTEVT1    *
C Adopted from the original SAPTRE written by J. Ranft.                *
C This version dated 18.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION axexp , b3 , bexp , cfe , DT_RNDM , dum , es , 
     &                 esmax , esmax1 , esmax2 , exeb , hma , hps , 
     &                 hpx , hpy , p1 , p2 , pa1 , pa2 , pz1nsq
      DOUBLE PRECISION pz2nsq , sfe , TINY3 , TINY7 , wa , x , xab , 
     &                 xm1 , xm2 , y
      INTEGER idum , Idx1 , Idx2 , irej1 , k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY7=1.0D-7,TINY3=1.0D-3)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
      DIMENSION pa1(4) , pa2(4) , p1(4) , p2(4)
 
      DATA b3/4.0D0/
 
      esmax1 = PHKk(4,Idx1) - PHKk(5,Idx1)
      esmax2 = PHKk(4,Idx2) - PHKk(5,Idx2)
      esmax = MIN(esmax1,esmax2)
 
      IF ( esmax.LE.0.05D0 ) RETURN
      hma = PHKk(5,Idx1)
      DO k = 1 , 4
         pa1(k) = PHKk(k,Idx1)
         pa2(k) = PHKk(k,Idx2)
      END DO
 
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(pa1(1),pa1(2),pa1(3),pa1(4),1,idum,idum)
         CALL DT_EVTEMC(pa2(1),pa2(2),pa2(3),pa2(4),2,idum,idum)
      END IF
 
      exeb = 0.0D0
      IF ( b3*esmax.LE.60.0D0 ) exeb = EXP(-b3*esmax)
      bexp = hma*(1.0D0-exeb)/b3
      axexp = (1.0D0-(b3*esmax-1.0D0)*exeb)/b3**2
      wa = axexp/(bexp+axexp)
      xab = DT_RNDM(wa)
C ES is the transverse kinetic energy
 100  IF ( xab.LT.wa ) THEN
         x = DT_RNDM(wa)
         y = DT_RNDM(wa)
         es = -2.0D0/(b3**2)*LOG(x*y+TINY7)
      ELSE
         x = DT_RNDM(y)
         es = ABS(-LOG(x+TINY7)/b3)
      END IF
      IF ( es.GT.esmax ) GOTO 100
      es = es + hma
C transverse momentum
      hps = SQRT((es-hma)*(es+hma))
 
      CALL DT_DSFECF(sfe,cfe)
      hpx = hps*cfe
      hpy = hps*sfe
      pz1nsq = pa1(3)**2 - hps**2 - 2.0D0*pa1(1)*hpx - 2.0D0*pa1(2)*hpy
      pz2nsq = pa2(3)**2 - hps**2 + 2.0D0*pa2(1)*hpx + 2.0D0*pa2(2)*hpy
 
C     PA1(3) = SIGN(SQRT(PZ1NSQ),PA1(3))
C     PA2(3) = SIGN(SQRT(PZ2NSQ),PA2(3))
      IF ( (pz1nsq.LT.TINY3) .OR. (pz2nsq.LT.TINY3) ) RETURN
      pa1(1) = pa1(1) + hpx
      pa1(2) = pa1(2) + hpy
      pa2(1) = pa2(1) - hpx
      pa2(2) = pa2(2) - hpy
 
C put resonances on mass-shell again
      xm1 = PHKk(5,Idx1)
      xm2 = PHKk(5,Idx2)
      CALL DT_MASHEL(pa1,pa2,xm1,xm2,p1,p2,irej1)
 
      IF ( irej1.NE.0 ) RETURN
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(-p1(1),-p1(2),-p1(3),-p1(4),2,idum,idum)
         CALL DT_EVTEMC(-p2(1),-p2(2),-p2(3),-p2(4),2,idum,idum)
         CALL DT_EVTEMC(dum,dum,dum,dum,3,12,irej1)
         IF ( irej1.NE.0 ) RETURN
      END IF
 
      DO k = 1 , 4
         PHKk(k,Idx1) = p1(k)
         PHKk(k,Idx2) = p2(k)
      END DO
 
      END SUBROUTINE
