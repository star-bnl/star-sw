
      SUBROUTINE DT_CRONIN(Incl)
 
C***********************************************************************
C Cronin-Effect. Multiple scattering of partons at chain ends.         *
C             INCL = 1     multiple sc. in projectile                  *
C                  = 2     multiple sc. in target                      *
C This version dated 05.01.96 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dev , e , etot , pin , pout , r , TINY3 , ZERO
      INTEGER i , idxnu , Incl , k , nval
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D+00,TINY3=1.0D-03)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
 
      DIMENSION r(3) , pin(4) , pout(4) , dev(4)
 
      DO k = 1 , 4
         dev(k) = ZERO
      END DO
 
      DO i = NPOint(2) , NHKk
         IF ( ISThkk(i).LT.0 ) THEN
C get z-position of the chain
            r(1) = VHKk(1,i)*1.0D12
            IF ( Incl.EQ.2 ) r(1) = VHKk(1,i)*1.0D12 - BIMpac
            r(2) = VHKk(2,i)*1.0D12
            idxnu = JMOhkk(1,i)
            IF ( (Incl.EQ.1) .AND. (ISThkk(idxnu).EQ.10) )
     &           idxnu = JMOhkk(1,i-1)
            IF ( (Incl.EQ.2) .AND. (ISThkk(idxnu).EQ.9) )
     &           idxnu = JMOhkk(1,i+1)
            r(3) = VHKk(3,idxnu)*1.0D12
C position of target parton the chain is connected to
            DO k = 1 , 4
               pin(k) = PHKk(k,i)
            END DO
C multiple scattering of parton with DTEVT1-index I
            CALL DT_CROMSC(pin,r,pout,Incl)
C*testprint
C           IF (NEVHKK.EQ.5) THEN
C              AMIN = PIN(4)**2-PIN(1)**2-PIN(2)**2-PIN(3)**2
C              AMOU = POUT(4)**2-POUT(1)**2-POUT(2)**2-POUT(3)**2
C              AMIN = SIGN(SQRT(ABS(AMIN)),AMIN)
C              AMOU = SIGN(SQRT(ABS(AMOU)),AMOU)
C              WRITE(6,'(A,I4,2E15.5)')'I,AMIN,AMOU: ',I,AMIN,AMOU
C              WRITE(6,'(A,4E15.5)')'PIN:       ',PIN
C              WRITE(6,'(A,4E15.5)')'POUT:      ',POUT
C           ENDIF
C*
C increase accumulator by energy-momentum difference
            DO k = 1 , 4
               dev(k) = dev(k) + pout(k) - pin(k)
               PHKk(k,i) = pout(k)
            END DO
            PHKk(5,i) = SQRT(ABS(PHKk(4,i)**2-PHKk(1,i)**2-PHKk(2,i)**2-
     &                  PHKk(3,i)**2))
         END IF
      END DO
 
C dump accumulator to momenta of valence partons
      nval = 0
      etot = 0.0D0
      DO i = NPOint(2) , NHKk
         IF ( (ISThkk(i).EQ.-21) .OR. (ISThkk(i).EQ.-22) ) THEN
            nval = nval + 1
            etot = etot + PHKk(4,i)
         END IF
      END DO
C     WRITE(LOUT,1000) NVAL,(DEV(K)/DBLE(NVAL),K=1,4)
C1000 FORMAT(1X,'CRONIN :  number of val. partons ',I4,/,
C    &       9X,4E12.4)
      DO i = NPOint(2) , NHKk
         IF ( (ISThkk(i).EQ.-21) .OR. (ISThkk(i).EQ.-22) ) THEN
            e = PHKk(4,i)
            DO k = 1 , 4
C              PHKK(K,I) = PHKK(K,I)-DEV(K)/DBLE(NVAL)
               PHKk(k,i) = PHKk(k,i) - dev(k)*e/etot
            END DO
            PHKk(5,i) = SQRT(ABS(PHKk(4,i)**2-PHKk(1,i)**2-PHKk(2,i)**2-
     &                  PHKk(3,i)**2))
         END IF
      END DO
 
      END SUBROUTINE
