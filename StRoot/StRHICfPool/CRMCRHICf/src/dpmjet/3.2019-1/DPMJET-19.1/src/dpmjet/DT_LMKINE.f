
      SUBROUTINE DT_LMKINE(Ifp1,Ifp2,Kp,Ift1,Ift2,Kt,Irej)
 
C***********************************************************************
C Kinematical treatment of low-mass excitations.                       *
C This version dated 12.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION fac1 , fac2 , OHALF , ONE , p1 , p2 , pabs , 
     &                 poe , PYMASS , xm1 , xm2 , ZERO
      INTEGER Ifp1 , Ifp2 , Ift1 , Ift2 , Irej , irej1 , k , Kp , Kt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,OHALF=0.5D0,ONE=1.0D0)
 
C flags for input different options
      INCLUDE 'inc/dtflg1'
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
 
      DIMENSION p1(4) , p2(4)
 
      Irej = 0
 
      IF ( Kp.EQ.1 ) THEN
         pabs = SQRT(PPF(1)**2+PPF(2)**2+PPF(3)**2)
         poe = PPF(4)/pabs
         fac1 = OHALF*(poe+ONE)
         fac2 = -OHALF*(poe-ONE)
         DO k = 1 , 3
            PPLm1(k) = fac1*PPF(k)
            PPLm2(k) = fac2*PPF(k)
         END DO
         PPLm1(4) = fac1*pabs
         PPLm2(4) = -fac2*pabs
         IF ( IMShl.EQ.1 ) THEN
 
            xm1 = PYMASS(Ifp1)
            xm2 = PYMASS(Ifp2)
 
            CALL DT_MASHEL(PPLm1,PPLm2,xm1,xm2,p1,p2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            DO k = 1 , 4
               PPLm1(k) = p1(k)
               PPLm2(k) = p2(k)
            END DO
         END IF
      END IF
 
      IF ( Kt.EQ.1 ) THEN
         pabs = SQRT(PTF(1)**2+PTF(2)**2+PTF(3)**2)
         poe = PTF(4)/pabs
         fac1 = OHALF*(poe+ONE)
         fac2 = -OHALF*(poe-ONE)
         DO k = 1 , 3
            PTLm2(k) = fac1*PTF(k)
            PTLm1(k) = fac2*PTF(k)
         END DO
         PTLm2(4) = fac1*pabs
         PTLm1(4) = -fac2*pabs
         IF ( IMShl.EQ.1 ) THEN
 
            xm1 = PYMASS(Ift1)
            xm2 = PYMASS(Ift2)
 
            CALL DT_MASHEL(PTLm1,PTLm2,xm1,xm2,p1,p2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            DO k = 1 , 4
               PTLm1(k) = p1(k)
               PTLm2(k) = p2(k)
            END DO
         END IF
      END IF
 
      RETURN
 
 
 100  IF ( LPRi.GT.4 ) WRITE (LOUt,'(A)') 
     &                        'LMKINE:   kinematical treatment rejected'
      Irej = 1
      END SUBROUTINE
