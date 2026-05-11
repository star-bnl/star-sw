
      SUBROUTINE DT_DBKLAS(I,J,K,I8,I10)
 
      IMPLICIT NONE
      INTEGER I , I10 , I8 , ii , ind , J , jj , K , kk
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C quark-content to particle index conversion (DTUNUC 1.x)
      INCLUDE 'inc/dtq2id'
 
C baryons
      IF ( I.LE.0 ) THEN
C antibaryons
         ii = IABS(I)
         jj = IABS(J)
         kk = IABS(K)
         CALL DT_INDEXD(jj,kk,ind)
         I8 = IA08(ii,ind)
         I10 = IA10(ii,ind)
         IF ( I8.LE.0 ) I8 = I10
         GOTO 99999
      END IF
      CALL DT_INDEXD(J,K,ind)
      I8 = IB08(I,ind)
      I10 = IB10(I,ind)
      IF ( I8.LE.0 ) I8 = I10
 
99999 END SUBROUTINE
