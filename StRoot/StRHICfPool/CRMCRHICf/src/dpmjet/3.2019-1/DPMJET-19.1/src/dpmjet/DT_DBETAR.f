
      DOUBLE PRECISION FUNCTION DT_DBETAR(Gam,Eta)
 
C***********************************************************************
C Sampling from Beta -distribution between 0.0 and 1.0                 *
C  F(X)=X**(GAM-1.)*(1.-X)**(ETA-1)*GAMM(ETA+GAM)/(GAMM(GAM)*GAMM(ETA))*
C Processed by S. Roesler, 6.5.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_DGAMRN , Eta , Gam , y , z
      SAVE 
 
      y = DT_DGAMRN(1.0D0,Gam)
      z = DT_DGAMRN(1.0D0,Eta)
      DT_DBETAR = y/(y+z)
 
      END FUNCTION
