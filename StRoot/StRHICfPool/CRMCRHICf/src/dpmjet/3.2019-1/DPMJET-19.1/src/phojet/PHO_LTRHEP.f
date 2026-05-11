
      SUBROUTINE PHO_LTRHEP(I1,I2,Cod,Sid,Cof,Sif,Gam,Bgx,Bgy,Bgz)
C*******************************************************************
C
C     Lorentz transformation of entries I1 to I2 in /POEVT1/
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Bgx , Bgy , Bgz , Cod , Cof , DIFF , ee , EPS , 
     &                 Gam , pmass , ptot , Sid , Sif , xx , yy , zz
      INTEGER i , I1 , I2
      SAVE 
 
      PARAMETER (DIFF=0.001D0,EPS=1.D-5)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DO i = I1 , MIN(I2,NHEp)
         IF ( (ABS(ISThep(i)).LE.10) .OR. (ISThep(i).EQ.21) ) THEN
            CALL PHO_TRANS(PHEp(1,i),PHEp(2,i),PHEp(3,i),Cod,Sid,Cof,
     &                     Sif,xx,yy,zz)
            ee = PHEp(4,i)
            CALL PHO_ALTRA(Gam,Bgx,Bgy,Bgz,xx,yy,zz,ee,ptot,PHEp(1,i),
     &                     PHEp(2,i),PHEp(3,i),PHEp(4,i))
         ELSE IF ( ISThep(i).EQ.20 ) THEN
            ee = SQRT(PHEp(1,i)**2+PHEp(2,i)**2+PHEp(3,i)**2)
            CALL PHO_TRANS(PHEp(1,i),PHEp(2,i),PHEp(3,i),Cod,Sid,Cof,
     &                     Sif,xx,yy,zz)
            CALL PHO_ALTRA(Gam,Bgx,Bgy,Bgz,xx,yy,zz,ee,ptot,PHEp(1,i),
     &                     PHEp(2,i),PHEp(3,i),pmass)
         END IF
      END DO
 
C  debug precision
      IF ( IDEb(70).LT.1 ) RETURN
      DO i = I1 , MIN(NHEp,I2)
         IF ( ABS(ISThep(i)).LE.10 ) THEN
            pmass = PHEp(4,i)**2 - PHEp(1,i)**2 - PHEp(2,i)
     &              **2 - PHEp(3,i)**2
            pmass = SIGN(SQRT(ABS(pmass)),pmass)
            IF ( (ABS(pmass-PHEp(5,i))/MAX(PHEp(5,i),1.D0)).GT.DIFF )
     &           THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5,2E13.4)')
     &               'PHO_LTRHEP: inconsistent masses:' , i , pmass , 
     &              PHEp(5,i)
            END IF
         END IF
      END DO
 
      END SUBROUTINE
