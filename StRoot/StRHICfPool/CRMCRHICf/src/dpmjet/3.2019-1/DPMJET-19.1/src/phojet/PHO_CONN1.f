
      DOUBLE PRECISION FUNCTION PHO_CONN1(Beta)
C***********************************************************************
C
C    auxiliary function to determine parameters of soft
C    pt distribution  dNs/dP_t = P_t * AAS * EXP(-BETA*P_t)
C
C    internal factors: FS  number of soft partons in soft Pomeron
C                      FH  number of soft partons in hard Pomeron
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  average number of cut soft and hard ladders (obsolete)
      INCLUDE 'inc/point2'
C  data needed for soft-pt calculation
      INCLUDE 'inc/point3'
 
      DOUBLE PRECISION Beta , xx , ff
 
      xx = Beta*PTCon
      IF ( ABS(xx).LT.1.D-3 ) THEN
         ff = FS*SIGs + FH*SIGh - DSIghp*(PTCon/2.D0+PTCon**2*Beta/6.D0)
      ELSE
         ff = FS*SIGs + FH*SIGh - DSIghp/(PTCon*Beta**2)
     &        *(EXP(xx)-1.D0-Beta*PTCon)
      END IF
      PHO_CONN1 = ff
 
C     WRITE(LO,'(1X,A,3E12.3)') 'PHO_CONN1:BETA,AAS,FF',BETA,AAS,FF
C     WRITE(LO,'(1X,A,3E12.3)') 'PHO_CONN1:SIGS,SIGH,DSIGH',SIGS,SIGH,DSIGHP
 
      END FUNCTION
