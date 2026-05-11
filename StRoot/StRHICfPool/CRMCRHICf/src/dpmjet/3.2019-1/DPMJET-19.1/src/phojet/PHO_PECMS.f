
      SUBROUTINE PHO_PECMS(Id,Pmass1,Pmass2,Ecm,Pp,Ee)
C*******************************************************************
C
C     calculation of cms momentum and energy of massive particle
C     (ID=  1 using PMASS1,  2 using PMASS2)
C
C     output:  PP    cms momentum
C              EE    energy in CMS of particle ID
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Ecm , Ee , pm1 , pm2 , Pmass1 , Pmass2 , Pp , s
      INTEGER Id
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  some constants
      INCLUDE 'inc/pocons'
 
      s = Ecm**2
      pm1 = SIGN(Pmass1**2,Pmass1)
      pm2 = SIGN(Pmass2**2,Pmass2)
      Pp = SQRT(s**2-2.D0*pm1*s-2.D0*pm2*s-2.D0*pm1*pm2+pm1**2+pm2**2)
     &     /(2.D0*Ecm)
 
      IF ( Id.EQ.1 ) THEN
         Ee = SQRT(pm1+Pp**2)
      ELSE IF ( Id.EQ.2 ) THEN
         Ee = SQRT(pm2+Pp**2)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3,/)')
     &         'PHO_PECMS:ERROR: invalid ID number:' , Id
         Ee = Pp
      END IF
 
      END SUBROUTINE
