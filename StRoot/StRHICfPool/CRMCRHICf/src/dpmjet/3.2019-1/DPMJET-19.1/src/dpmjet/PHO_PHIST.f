#ifndef FOR_CORSIKA

      SUBROUTINE PHO_PHIST(Imode,Weight)
 
      IMPLICIT NONE
      INTEGER ilab , Imode , mode
      DOUBLE PRECISION Weight
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      ilab = 0
      IF ( Imode.EQ.10 ) THEN
         Imode = 1
         ilab = 1
      END IF
      IF ( ABS(Imode).LT.1000 ) THEN
C PHOJET-statistics
C        CALL POHISX(IMODE,WEIGHT)
         IF ( Imode.EQ.-1 ) THEN
            mode = 1
            XSTot(1,1,1) = Weight
         END IF
         IF ( Imode.EQ.1 ) mode = 2
         IF ( Imode.EQ.-2 ) mode = 3
C        IF (MODE.EQ.3) WRITE(LOUT,*)
C    &      ' Sigma = ',XSPRO(1,1,1),' mb   used for normalization'
         IF ( mode.EQ.2 ) CALL DT_SWPPHO(ilab)
         CALL DT_HISTOG(mode)
         CALL DT_USRHIS(mode)
      ELSE
C DTUNUC-statistics
         mode = Imode/1000
C        IF (MODE.EQ.3) WRITE(LOUT,*)
C    &      ' Sigma = ',XSPRO(1,1,1),' mb   used for normalization'
         CALL DT_HISTOG(mode)
         CALL DT_USRHIS(mode)
      END IF
 
      END SUBROUTINE
#endif
