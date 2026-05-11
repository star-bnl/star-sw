
      SUBROUTINE PHO_HARWGX(Ptcut,Ecm)
C**********************************************************************
C
C     find maximum of remaining weight for MC sampling
C
C     input:   PTCUT  transverse momentum cutoff
C              ECM    cms energy
C
C     output:  HWgx(-1:Max_pro_2)  field for sampling hard processes
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION d , dmx , Ecm , f1 , f2 , f3 , fdis , ff , fold , 
     &                 pda , pdb , pds , Ptcut , ptm , TINY , xm1 , 
     &                 xm2 , z , zmx , zz
      INTEGER i , iftab , imx , ipo , ist , it , NKM , nkml , nkon
      SAVE 
 
      PARAMETER (NKM=10)
      PARAMETER (TINY=1.D-20)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  event debugging information
      INCLUDE 'inc/podebg'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
 
      DIMENSION z(3) , d(3) , ff(0:NKM) , pda(-6:6) , pdb(-6:6) , 
     &          xm1(NKM) , xm2(NKM) , ptm(NKM) , zmx(3,NKM) , dmx(3,NKM)
     &          , imx(NKM) , ipo(NKM)
      DIMENSION iftab(-1:MAX_PRO_2)
 
C  initial settings
      DATA iftab/4 , 0 , 1 , 2 , 4 , 1 , 2 , 2 , 3 , 5 , 0 , 6 , 7 , 8 , 
     &     9 , 10 , 0 , 10/
      AH = (2.D0*Ptcut/Ecm)**2
      ALNh = LOG(AH)
      ff(0) = 0.D0
      DO i = 1 , NKM
         ff(i) = 0.D0
         xm1(i) = 0.D0
         xm2(i) = 0.D0
         ptm(i) = 0.D0
         zmx(1,i) = 0.D0
         zmx(2,i) = 0.D0
         zmx(3,i) = 0.D0
         dmx(1,i) = 0.D0
         dmx(2,i) = 0.D0
         dmx(3,i) = 0.D0
         imx(i) = 0
         ipo(i) = 0
      END DO
 
      nkml = 10
      DO nkon = 1 , nkml
 
         DO ist = 1 , 3
C  start configuration
            IF ( ist.EQ.1 ) THEN
               z(1) = MIN(0.999D0,LOG(0.5D0)/LOG(AH))
               z(2) = 0.5
               z(3) = 0.1
               d(1) = -0.5
               d(2) = 0.5
               d(3) = 0.5
            ELSE IF ( ist.EQ.2 ) THEN
               z(1) = 0.999D0
               z(2) = 0.5
               z(3) = 0.0
               d(1) = -0.5
               d(2) = 0.5
               d(3) = 0.5
            ELSE IF ( ist.EQ.3 ) THEN
               z(1) = MIN(0.999D0,LOG(0.5D0)/LOG(AH))
               z(2) = 0.1
               z(3) = 0.1
               d(1) = -0.5
               d(2) = 0.5
               d(3) = 0.5
            ELSE IF ( ist.EQ.4 ) THEN
               z(1) = MIN(0.999D0,LOG(0.5D0)/LOG(AH))
               z(2) = 0.9
               z(3) = 0.1
               d(1) = -0.5
               d(2) = 0.5
               d(3) = 0.5
            END IF
            it = 0
            CALL PHO_HARWGI(Ecm,Ptcut,nkon,z,f2)
C  process possible?
            IF ( f2.LE.0.D0 ) GOTO 100
 
 20         it = it + 1
            fold = f2
            DO i = 1 , 3
               d(i) = d(i)/5.D0
               z(i) = z(i) + d(i)
               CALL PHO_HARWGI(Ecm,Ptcut,nkon,z,f3)
               IF ( f2.GT.f3 ) z(i) = z(i) - d(i)
               IF ( f2.GT.f3 ) d(i) = -d(i)
 30            f1 = MIN(f2,f3)
               f2 = MAX(f2,f3)
               z(i) = z(i) + d(i)
               CALL PHO_HARWGI(Ecm,Ptcut,nkon,z,f3)
               IF ( f3.GT.f2 ) GOTO 30
               zz = z(i) - d(i)
               z(i) = zz + 0.5*d(i)*(f3-f1)/MAX(TINY,f2+f2-f1-f3)
               IF ( ABS(zz-z(i)).GT.d(i)*0.1D0 )
     &              CALL PHO_HARWGI(Ecm,Ptcut,nkon,z,f1)
               IF ( f1.LE.f2 ) z(i) = zz
               f2 = MAX(f1,f2)
            END DO
            IF ( (ABS(fold-f2)/MAX(TINY,f2).GT.0.002D0) .OR. (it.LT.3) )
     &           GOTO 20
 
            IF ( f2.GT.ff(nkon) ) THEN
               ff(nkon) = MAX(f2,0.D0)
               xm1(nkon) = X1
               xm2(nkon) = X2
               ptm(nkon) = PT
               zmx(1,nkon) = z(1)
               zmx(2,nkon) = z(2)
               zmx(3,nkon) = z(3)
               dmx(1,nkon) = d(1)
               dmx(2,nkon) = d(2)
               dmx(3,nkon) = d(3)
               imx(nkon) = it
               ipo(nkon) = ist
            END IF
C
         END DO
 100  END DO
 
C  debug output
      IF ( IDEb(38).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 
     &        'PHO_HARWGX: maximum of weight (I,IT,IS,FF,Z(1-3),D(1-3))'
         DO i = 1 , NKM
            IF ( LPRi.GT.4 .AND. imx(i).NE.0 )
     &            WRITE (LO,'(1X,I2,I3,I2,7E10.3)') i , imx(i) , ipo(i)
     &           , ff(i) , zmx(1,i) , zmx(2,i) , zmx(3,i) , dmx(1,i) , 
     &           dmx(2,i) , dmx(3,i)
         END DO
      END IF
 
      DO i = -1 , MAX_PRO_2
         HWGx(i,IDXmpar) = MAX(ff(iftab(i))*HFAc(i,IDXmpar),0.D0)
      END DO
 
C  debug output
      IF ( IDEb(38).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &         'PHO_HARWGX: total weights'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &         'I    X1   X2   PT   HWgx(I)  FDIS'
         DO i = -1 , MAX_PRO_2
            IF ( (iftab(i).NE.0) .AND. (HWGx(i,IDXmpar).GT.0.D0) ) THEN
               MSPr = i
               X1 = MIN(xm1(iftab(i)),0.9999999999D0)
               X2 = MIN(xm2(iftab(i)),0.9999999999D0)
               PT = ptm(iftab(i))
               CALL PHO_HARWGH(pds,pda,pdb,fdis)
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,I3,5E12.3)') i , X1 , 
     &              X2 , PT , HWGx(i,IDXmpar) , fdis
            END IF
         END DO
      END IF
 
      END SUBROUTINE
