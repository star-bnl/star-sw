
      DOUBLE PRECISION FUNCTION PHO_BESSJ0(Dx)
      IMPLICIT NONE
      INTEGER i
C**********************************************************************
C
C     CERN (KERN) LIB function C312
C
C     modified by R. Engel (03/02/93)
C
C**********************************************************************
      DOUBLE PRECISION Dx
      DOUBLE PRECISION x , y , v , h , alfa , eight
      DOUBLE PRECISION pi1 , pi2 , c1(0:14) , c2(0:9) , c3(0:10) , b0 , 
     &                 b1 , b2 , p , q , r
      SAVE 
 
      DATA eight/8.0D0/
      DATA pi1/0.79788456080287D0/ , pi2/0.78539816339745D0/
 
      DATA c1(0)/ + 0.15772797147489D0/
      DATA c1(1)/ - 0.00872344235285D0/
      DATA c1(2)/ + 0.26517861320334D0/
      DATA c1(3)/ - 0.37009499387265D0/
      DATA c1(4)/ + 0.15806710233210D0/
      DATA c1(5)/ - 0.03489376941141D0/
      DATA c1(6)/ + 0.00481918006947D0/
      DATA c1(7)/ - 0.00046062616621D0/
      DATA c1(8)/ + 0.00003246032882D0/
      DATA c1(9)/ - 0.00000176194691D0/
      DATA c1(10)/ + 0.00000007608164D0/
      DATA c1(11)/ - 0.00000000267925D0/
      DATA c1(12)/ + 0.00000000007849D0/
      DATA c1(13)/ - 0.00000000000194D0/
      DATA c1(14)/ + 0.00000000000004D0/
 
      DATA c2(0)/ + 0.99946034934752D0/
      DATA c2(1)/ - 0.00053652204681D0/
      DATA c2(2)/ + 0.00000307518479D0/
      DATA c2(3)/ - 0.00000005170595D0/
      DATA c2(4)/ + 0.00000000163065D0/
      DATA c2(5)/ - 0.00000000007864D0/
      DATA c2(6)/ + 0.00000000000517D0/
      DATA c2(7)/ - 0.00000000000043D0/
      DATA c2(8)/ + 0.00000000000004D0/
      DATA c2(9)/ - 0.00000000000001D0/
 
      DATA c3(0)/ - 0.015555854605337D0/
      DATA c3(1)/ + 0.000068385199426D0/
      DATA c3(2)/ - 0.000000741449841D0/
      DATA c3(3)/ + 0.000000017972457D0/
      DATA c3(4)/ - 0.000000000727192D0/
      DATA c3(5)/ + 0.000000000042201D0/
      DATA c3(6)/ - 0.000000000003207D0/
      DATA c3(7)/ + 0.000000000000301D0/
      DATA c3(8)/ - 0.000000000000033D0/
      DATA c3(9)/ + 0.000000000000004D0/
      DATA c3(10)/ - 0.000000000000001D0/
 
      x = Dx
      v = ABS(x)
      IF ( v.LT.eight ) THEN
         y = v/eight
         h = 2.D0*y**2 - 1.D0
         alfa = -2.D0*h
         b1 = 0.D0
         b2 = 0.D0
         DO i = 14 , 0 , -1
            b0 = c1(i) - alfa*b1 - b2
            b2 = b1
            b1 = b0
         END DO
         b1 = b0 - h*b2
      ELSE
         r = 1.D0/v
         y = eight*r
         h = 2.D0*y**2 - 1.D0
         alfa = -2.D0*h
         b1 = 0.D0
         b2 = 0.D0
         DO i = 9 , 0 , -1
            b0 = c2(i) - alfa*b1 - b2
            b2 = b1
            b1 = b0
         END DO
         p = b0 - h*b2
         b1 = 0.D0
         b2 = 0.D0
         DO i = 10 , 0 , -1
            b0 = c3(i) - alfa*b1 - b2
            b2 = b1
            b1 = b0
         END DO
         q = y*(b0-h*b2)
         b0 = v - pi2
         b1 = pi1*SQRT(r)*(p*COS(b0)-q*SIN(b0))
      END IF
      PHO_BESSJ0 = b1
      END FUNCTION
