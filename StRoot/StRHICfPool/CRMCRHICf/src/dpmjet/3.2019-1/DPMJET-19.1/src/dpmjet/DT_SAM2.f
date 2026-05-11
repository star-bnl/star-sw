
      DOUBLE PRECISION FUNCTION DT_SAM2(Q2,Ecm)
 
      IMPLICIT NONE
      DOUBLE PRECISION am1c2 , am2c2 , amhi2 , amhi20 , amlo2 , 
     &                 DT_RNDM , Ecm , ELVTRD , fac , fachi , GEV2MB , 
     &                 ONE , PI , Q2 , s , TENTRD , TINY10 , TWO , 
     &                 TWOPI , weigmx
      DOUBLE PRECISION xsam2 , yc1 , yc2 , yhi , ylo , ysam2 , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0,
     &           TENTRD=10.0D0/3.0D0,ELVTRD=11.0D0/3.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,GEV2MB=0.38938D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
 
      s = Ecm**2
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
      amhi20 = (Ecm-AAM(1))**2
      IF ( amhi2.GE.amhi20 ) amhi2 = amhi20
 
      am1c2 = 16.0D0
      am2c2 = 121.0D0
      ylo = LOG(amlo2+Q2)
      yc1 = LOG(am1c2+Q2)
      yc2 = LOG(am2c2+Q2)
      yhi = LOG(amhi2+Q2)
      IF ( amhi2.LE.am1c2 ) THEN
         fachi = TWO
      ELSE IF ( (amhi2.GT.am1c2) .AND. (amhi2.LE.am2c2) ) THEN
         fachi = TENTRD
      ELSE
         fachi = ELVTRD
      END IF
 
 100  ysam2 = ylo + (yhi-ylo)*DT_RNDM(am1c2)
      IF ( ysam2.LE.yc1 ) THEN
         fac = TWO
      ELSE IF ( (ysam2.GT.yc1) .AND. (ysam2.LE.yc2) ) THEN
         fac = TENTRD
      ELSE
         fac = ELVTRD
      END IF
      weigmx = fachi*(ONE-Q2*EXP(-yhi))
      xsam2 = fac*(ONE-Q2*EXP(-ysam2))
      IF ( DT_RNDM(ysam2)*weigmx.GT.xsam2 ) GOTO 100
 
      DT_SAM2 = EXP(ysam2) - Q2
 
      END FUNCTION
