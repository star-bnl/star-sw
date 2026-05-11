
      DOUBLE PRECISION FUNCTION PHO_DFMASS(Xmin,Xmax,Pref2,Pvirt2,Imode)
C**********************************************************************
C
C     sampling of Mx diffractive mass distribution within
C              limits XMIN, XMAX
C
C     input:    XMIN,XMAX     mass limitations (GeV)
C               PREF2         original particle mass/ reference mass
C                             (squared, GeV**2)
C               PVIRT2        particle virtuality
C               IMODE         M**2 mass distribution
C                             1      1/(M**2+Q**2)
C                             2      1/(M**2+Q**2)**alpha
C                            -1      1/(M**2-Mref**2+Q**2)
C                            -2      1/(M**2-Mref**2+Q**2)**alpha
C
C     output:   diffractive mass (GeV)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ddelta , DT_RNDM , EPS , pm2 , Pref2 , Pvirt2 , 
     &                 xi , xma2 , Xmax , xmax2 , Xmin , xmin2
      INTEGER Imode
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
 
      IF ( (Xmin.GE.Xmax) .OR. (Xmin.LE.0.3D0) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,3E12.4)')
     &         'PHO_DFMASS:ERROR: ' , 'invalid mass limits' , Xmin , 
     &        Xmax , Pref2
         CALL PHO_PREVNT(-1)
         PHO_DFMASS = 0.135D0
         RETURN
      END IF
 
      IF ( Imode.GT.0 ) THEN
         pm2 = -Pvirt2
      ELSE
         pm2 = Pref2 - Pvirt2
      END IF
 
C  critical pomeron
      IF ( ABS(Imode).EQ.1 ) THEN
         xmin2 = LOG(Xmin**2-pm2)
         xmax2 = LOG(Xmax**2-pm2)
         xi = (xmax2-xmin2)*DT_RNDM(pm2) + xmin2
         xma2 = EXP(xi) + pm2
 
C  supercritical pomeron
      ELSE IF ( ABS(Imode).EQ.2 ) THEN
         ddelta = 1.D0 - PARmdl(48)
         xmin2 = (Xmin**2-pm2)**ddelta
         xmax2 = (Xmax**2-pm2)**ddelta
         xi = (xmax2-xmin2)*DT_RNDM(pm2) + xmin2
         xma2 = xi**(1.D0/ddelta) + pm2
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I3)')
     &         'PHO_DFMASS:ERROR: unsupported mode' , Imode
         CALL PHO_ABORT
      END IF
 
      PHO_DFMASS = SQRT(xma2)
C  debug output
      IF ( IDEb(43).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4E12.3)')
     &         'PHO_DFMASS:Mmin,Mmax,Mref,Mass' , Xmin , Xmax , Pref2 , 
     &        SQRT(xma2)
      END IF
 
      END FUNCTION
