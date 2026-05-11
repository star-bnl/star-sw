
      INTEGER FUNCTION IDT_IEFUND(Pl,Ire)
 
      IMPLICIT NONE
      INTEGER i , ipla , iple , Ire , j
      DOUBLE PRECISION Pl
      SAVE 
 
C*****IEFUN CALCULATES A MOMENTUM INDEX
 
      INCLUDE 'inc/dtflka'
 
      INCLUDE 'inc/hndrun'
      INCLUDE 'inc/hnredv'
      INCLUDE 'inc/hnreac'
 
      ipla = IEIi(Ire) + 1 + 1
      iple = IEIi(Ire+1)
      IF ( Pl.LT.0. ) THEN
         DO i = ipla , iple
            j = i - ipla + 1
            IF ( -Pl.LE.UMO(i) ) GOTO 100
         END DO
         i = iple
         IF ( EFTes.LE.40.D0 ) THEN
            EFTes = EFTes + 1.0D0
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Pl , i
         END IF
      ELSE
         DO i = ipla , iple
            j = i - ipla + 1
            IF ( Pl.LE.PLAbf(i) ) GOTO 100
         END DO
         i = iple
         IF ( EFTes.LE.40.D0 ) THEN
            EFTes = EFTes + 1.0D0
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Pl , j
         END IF
      END IF
 100  IDT_IEFUND = i
      RETURN
99010 FORMAT (' PLAB OR -ECM=',E12.4,' IS OUT OF CONSIDERED RANGE',
     &        ' IEFUN=',I5)
      END FUNCTION
