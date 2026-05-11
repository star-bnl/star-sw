
      DOUBLE PRECISION FUNCTION DT_RM2(Amlo2,Amhi2,Q2)
 
      IMPLICIT NONE
      DOUBLE PRECISION Amhi2 , Amlo2 , GEV2MB , ONE , PI , Q2 , TINY10 , 
     &                 tmpmhi , tmpmlo , TWO , TWOPI , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,GEV2MB=0.38938D0)
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
 
      IF ( RL2.LE.ZERO ) THEN
         DT_RM2 = -ONE/(Amhi2+Q2) + Q2/(TWO*(Amhi2+Q2)**2)
     &            - (-ONE/(Amlo2+Q2)+Q2/(TWO*(Amlo2+Q2)**2))
     &            + EPSpol*(-Q2/(TWO*(Amhi2+Q2)**2)
     &            +Q2/(TWO*(Amlo2+Q2)**2))
      ELSE
         tmpmlo = LOG(ONE+RL2/(Amlo2+Q2))
         tmpmhi = LOG(ONE+RL2/(Amhi2+Q2))
         DT_RM2 = Q2/(RL2*(Amhi2+Q2)) - (Q2+RL2)/RL2**2*tmpmhi - 
     &            (Q2/(RL2*(Amlo2+Q2))-(Q2+RL2)/RL2**2*tmpmlo)
     &            + EPSpol*(-Q2/(RL2*(Amhi2+Q2))+Q2/RL2**2*tmpmhi-
     &            (-Q2/(RL2*(Amlo2+Q2))+Q2/RL2**2*tmpmlo))
      END IF
 
      END FUNCTION
