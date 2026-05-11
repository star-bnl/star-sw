
      SUBROUTINE PHO_GET4VECTORS(Ipdgs1,Ipdgs2,Sqs,P1,P2)
C*********************************************************************
 
C     Generates two 4-vectors p1/2 in center of mass frame
 
C     input:   ipdgs1   PDG particle code side 1
C              ipdgs2   PDG particle code side 2
C              sqs      center of mass energy
C
C     output:  p1       particle 4-vector side 1
C              p2       particle 4-vector side 2
 
C*********************************************************************
      IMPLICIT NONE
 
      DOUBLE PRECISION Sqs , s , ee , pcm , P1(4) , P2(4) , pm1 , pm2 , 
     &                 PHO_PMASS
      INTEGER Ipdgs1 , Ipdgs2
 
      s = Sqs**2
      pm1 = PHO_PMASS(Ipdgs1,1)**2
      pm2 = PHO_PMASS(Ipdgs2,1)**2
      pcm = SQRT(s**2-2.D0*pm1*s-2.D0*pm2*s-2.D0*pm1*pm2+pm1**2+pm2**2)
     &      /(2.D0*Sqs)
      ee = SQRT(pm1+pcm**2)
      P1(1) = 0.0D0
      P1(2) = 0.0D0
      P1(3) = pcm
      P1(4) = ee
      P2(1) = 0.0D0
      P2(2) = 0.0D0
      P2(3) = -pcm
      P2(4) = Sqs - ee
 
 
      END SUBROUTINE
