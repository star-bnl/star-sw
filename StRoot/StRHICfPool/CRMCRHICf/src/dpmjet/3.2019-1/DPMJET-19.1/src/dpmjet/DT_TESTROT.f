
      SUBROUTINE DT_TESTROT(Pi,Po,Phi,Mode)
 
      IMPLICIT NONE
      INTEGER j , Mode
      DOUBLE PRECISION Phi , Pi , Po , rot
      SAVE 
 
      DIMENSION rot(3,3) , Pi(3) , Po(3)
 
      IF ( Mode.EQ.1 ) THEN
         rot(1,1) = 1.D0
         rot(1,2) = 0.D0
         rot(1,3) = 0.D0
         rot(2,1) = 0.D0
         rot(2,2) = COS(Phi)
         rot(2,3) = -SIN(Phi)
         rot(3,1) = 0.D0
         rot(3,2) = SIN(Phi)
         rot(3,3) = COS(Phi)
      ELSE IF ( Mode.EQ.2 ) THEN
         rot(1,1) = 0.D0
         rot(1,2) = 1.D0
         rot(1,3) = 0.D0
         rot(2,1) = COS(Phi)
         rot(2,2) = 0.D0
         rot(2,3) = -SIN(Phi)
         rot(3,1) = SIN(Phi)
         rot(3,2) = 0.D0
         rot(3,3) = COS(Phi)
      ELSE IF ( Mode.EQ.3 ) THEN
         rot(1,1) = 0.D0
         rot(2,1) = 1.D0
         rot(3,1) = 0.D0
         rot(1,2) = COS(Phi)
         rot(2,2) = 0.D0
         rot(3,2) = -SIN(Phi)
         rot(1,3) = SIN(Phi)
         rot(2,3) = 0.D0
         rot(3,3) = COS(Phi)
      ELSE IF ( Mode.EQ.4 ) THEN
         rot(1,1) = 1.D0
         rot(2,1) = 0.D0
         rot(3,1) = 0.D0
         rot(1,2) = 0.D0
         rot(2,2) = COS(Phi)
         rot(3,2) = -SIN(Phi)
         rot(1,3) = 0.D0
         rot(2,3) = SIN(Phi)
         rot(3,3) = COS(Phi)
      ELSE
         STOP ' TESTROT: mode not supported!'
      END IF
      DO j = 1 , 3
         Po(j) = rot(j,1)*Pi(1) + rot(j,2)*Pi(2) + rot(j,3)*Pi(3)
      END DO
 
      END SUBROUTINE
