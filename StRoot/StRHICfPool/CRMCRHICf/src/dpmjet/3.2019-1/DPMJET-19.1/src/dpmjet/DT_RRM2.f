
      DOUBLE PRECISION FUNCTION DT_RRM2(X,Q2)
 
      IMPLICIT NONE
      DOUBLE PRECISION am1c2 , am2c2 , amhi2 , amhi20 , amlo2 , DT_RM2 , 
     &                 ecm , GEV2MB , ONE , PI , Q2 , s , TINY10 , TWO , 
     &                 TWOPI , X , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,GEV2MB=0.38938D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
 
      s = Q2*(ONE-X)/X + AAM(1)**2
      ecm = SQRT(s)
 
      IF ( INTrge(1).EQ.1 ) THEN
         amlo2 = (3.0D0*AAM(13))**2
      ELSE IF ( INTrge(1).EQ.2 ) THEN
         amlo2 = AAM(33)**2
      ELSE
         amlo2 = AAM(96)**2
      END IF
      IF ( INTrge(2).EQ.1 ) THEN
         amhi2 = s/TWO
      ELSE IF ( INTrge(2).EQ.2 ) THEN
         amhi2 = s/4.0D0
      ELSE
         amhi2 = s
      END IF
      amhi20 = (ecm-AAM(1))**2
      IF ( amhi2.GE.amhi20 ) amhi2 = amhi20
 
      am1c2 = 16.0D0
      am2c2 = 121.0D0
      IF ( amhi2.LE.am1c2 ) THEN
         DT_RRM2 = TWO*DT_RM2(amlo2,amhi2,Q2)
      ELSE IF ( (amhi2.GT.am1c2) .AND. (amhi2.LE.am2c2) ) THEN
         DT_RRM2 = TWO*DT_RM2(amlo2,am1c2,Q2)
     &             + 10.0D0/3.0D0*DT_RM2(am1c2,amhi2,Q2)
      ELSE
         DT_RRM2 = TWO*DT_RM2(amlo2,am1c2,Q2)
     &             + 10.0D0/3.0D0*DT_RM2(am1c2,am2c2,Q2)
     &             + 11.0D0/3.0D0*DT_RM2(am2c2,amhi2,Q2)
      END IF
 
      END FUNCTION
