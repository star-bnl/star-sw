
      SUBROUTINE DT_GETEMU(It,Itz,Kkmat,Mode)
 
C***********************************************************************
C Sampling of emulsion component to be considered as target-nucleus.   *
C This version dated 6.5.95   is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , ONE , rr , sumfra , TINY10 , TINY3 , 
     &                 ZERO
      INTEGER i , icomp , idiff , It , Itz , Kkmat , Mode , ndiff
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY3=1.0D-3,TINY10=1.0D-10)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
 
      IF ( Mode.EQ.0 ) THEN
         sumfra = ZERO
         rr = DT_RNDM(sumfra)
         It = 0
         Itz = 0
         DO icomp = 1 , NCOmpo
            sumfra = sumfra + EMUfra(icomp)
            IF ( sumfra.GT.rr ) THEN
               It = IEMuma(icomp)
               Itz = IEMuch(icomp)
               Kkmat = icomp
               GOTO 50
            END IF
         END DO
 50      IF ( It.LE.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,E12.3)')
     &            'Warning!  norm. failure within emulsion fractions' , 
     &           sumfra
            STOP
         END IF
      ELSE IF ( Mode.EQ.1 ) THEN
         ndiff = 10000
         DO i = 1 , NCOmpo
            idiff = ABS(It-IEMuma(i))
            IF ( idiff.LT.ndiff ) THEN
               Kkmat = i
               ndiff = idiff
            END IF
         END DO
      ELSE
         STOP 'DT_GETEMU'
      END IF
 
C bypass for variable projectile/target/energy runs: the correct
C Glauber data will be always loaded on kkmat=1
      IF ( IOGlb.EQ.100 ) Kkmat = 1
 
      END SUBROUTINE
