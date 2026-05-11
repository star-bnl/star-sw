
      INTEGER FUNCTION IPHO_FINDIDX(Iside,Value,Ndim)
C********************************************************************
 
C     Find index of
 
C********************************************************************
      IMPLICIT NONE
 
      INCLUDE 'inc/pobeam'
 
      INTEGER Iside , Value , Ndim , idx
 
      IPHO_FINDIDX = -1
 
      DO idx = 1 , Ndim
         IF ( MPMapp(Iside,idx).EQ.Value ) THEN
            IPHO_FINDIDX = idx
            GOTO 99999
         END IF
      END DO
 
99999 END FUNCTION
