
      SUBROUTINE DT_RJSEAC(Mop1,Mop2,Mot1,Mot2,Irej)
 
C***********************************************************************
C ReJection of SEA-sea Chains.                                         *
C         MOP1/2       entries of projectile sea-partons in DTEVT1     *
C         MOT1/2       entries of projectile sea-partons in DTEVT1     *
C This version dated 16.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , idone , idxnuc , idxsea , Irej , istval , j , k , 
     &        Mop1 , Mop2 , Mot1 , Mot2 , n
      DOUBLE PRECISION TINY10 , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ZERO=0.0D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C statistics
      INCLUDE 'inc/dtsta1'
 
      DIMENSION idxsea(2,2) , idxnuc(2) , istval(2)
 
      Irej = 0
 
C projectile sea q-aq-pair
C    indices of sea-pair
      idxsea(1,1) = Mop1
      idxsea(1,2) = Mop2
C    index of mother-nucleon
      idxnuc(1) = JMOhkk(1,Mop1)
C    status of valence quarks to be corrected
      istval(1) = -21
 
C target sea q-aq-pair
C    indices of sea-pair
      idxsea(2,1) = Mot1
      idxsea(2,2) = Mot2
C    index of mother-nucleon
      idxnuc(2) = JMOhkk(1,Mot1)
C    status of valence quarks to be corrected
      istval(2) = -22
 
      DO n = 1 , 2
         idone = 0
         DO i = NPOint(2) , NHKk
            IF ( (ISThkk(i).EQ.istval(n)) .AND. 
     &           (JMOhkk(1,i).EQ.idxnuc(n)) ) THEN
C valence parton found
C    inrease 4-momentum by sea 4-momentum
               DO k = 1 , 4
                  PHKk(k,i) = PHKk(k,i) + PHKk(k,idxsea(n,1))
     &                        + PHKk(k,idxsea(n,2))
               END DO
               PHKk(5,i) = SQRT(ABS(PHKk(4,i)**2-PHKk(1,i)**2-PHKk(2,i)
     &                     **2-PHKk(3,i)**2))
C    "cancel" sea-pair
               DO j = 1 , 2
                  ISThkk(idxsea(n,j)) = 100
                  IDHkk(idxsea(n,j)) = 0
                  JMOhkk(1,idxsea(n,j)) = 0
                  JMOhkk(2,idxsea(n,j)) = 0
                  JDAhkk(1,idxsea(n,j)) = 0
                  JDAhkk(2,idxsea(n,j)) = 0
                  DO k = 1 , 4
                     PHKk(k,idxsea(n,j)) = ZERO
                     VHKk(k,idxsea(n,j)) = ZERO
                     WHKk(k,idxsea(n,j)) = ZERO
                  END DO
                  PHKk(5,idxsea(n,j)) = ZERO
               END DO
               idone = 1
            END IF
         END DO
         IF ( idone.NE.1 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) NEVhkk , Mop1 , Mop2 , 
     &           Mot1 , Mot2
99010       FORMAT (1X,'RJSEAC: event ',I8,': inconsistent event',
     &              '-record!',/,1X,'        sea-quark pairs   ',2I5,4X,
     &              2I5,'   could not be canceled!')
            GOTO 100
         END IF
      END DO
      ICRjss = ICRjss + 1
      RETURN
 
 100  Irej = 1
      END SUBROUTINE
