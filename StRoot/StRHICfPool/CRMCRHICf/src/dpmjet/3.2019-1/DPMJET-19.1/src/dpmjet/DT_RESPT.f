
      SUBROUTINE DT_RESPT
 
C***********************************************************************
C Check DTEVT1 for two-resonance systems and sample intrinsic p_t.     *
C This version dated 18.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , nc
      DOUBLE PRECISION TINY3 , TINY7
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY7=1.0D-7,TINY3=1.0D-3)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
C get index of first chain
      DO i = NPOint(3) , NHKk
         IF ( IDHkk(i).EQ.88888 ) THEN
            nc = i
            GOTO 100
         END IF
      END DO
 
 100  DO WHILE ( (IDHkk(nc).EQ.88888) .AND. (IDHkk(nc+3).EQ.88888) )
C        WRITE(LOUT,*)NC,NC+3,IDRES(NC),IDRES(NC+3)
C skip VV-,SS- systems
         IF ( (IDCh(nc).NE.1) .AND. (IDCh(nc).NE.8) .AND. 
     &        (IDCh(nc+3).NE.1) .AND. (IDCh(nc+3).NE.8) ) THEN
C check if both "chains" are resonances
            IF ( (IDRes(nc).NE.0) .AND. (IDRes(nc+3).NE.0) )
     &           CALL DT_SAPTRE(nc,nc+3)
         END IF
         nc = nc + 6
      END DO
 
 
      END SUBROUTINE
