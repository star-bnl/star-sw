
      DOUBLE PRECISION FUNCTION DT_SIGVP(Xi,Q2i)
 
C***********************************************************************
C sigma_Vp                                                             *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ALPHEM , AMPROT , bot , chm , dnv , dsea , 
     &                 DT_RRM2 , dum1 , dum2 , dum3 , ecm , f2 , 
     &                 GEV2MB , gl , ONE , PI , q2 , Q2i , scale , stot
      DOUBLE PRECISION str , TINY10 , top , TWO , TWOPI , upv , usea , 
     &                 x , Xi , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,GEV2MB=0.38938D0,AMPROT=0.938D0,
     &           ALPHEM=ONE/137.0D0)
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
 
      x = Xi
      q2 = Q2i
      IF ( Xi.LE.ZERO ) x = 0.0001D0
      IF ( Q2i.LE.ZERO ) q2 = 0.0001D0
 
      ecm = SQRT(q2*(ONE-x)/x+AMPROT**2)
 
      scale = SQRT(q2)
      IF ( MODega.EQ.1 ) THEN
         CALL DT_CKMT(x,scale,upv,dnv,usea,dsea,str,chm,bot,top,gl,f2,
     &                IDPdf)
C        W = ECM
 
C        ALLMF2 = PHO_ALLM97(Q2,W)
 
C        write(*,*) 'X,Q2,W,F2,ALLMF2',X,Q2,W,F2,ALLMF2
C        STOT = TWOPI**2*ALPHEM/(Q2*(ONE-X)) * F2 *GEV2MB
C        DT_SIGVP = 12.0D0*PI**3.0D0*F2/(Q2*DT_RRM2(X,Q2))
         DT_SIGVP = 12.0D0*PI**3.0D0*f2/(q2*DT_RRM2(x,q2))*GEV2MB
      ELSE IF ( MODega.EQ.4 ) THEN
         CALL DT_SIGGP(x,q2,ecm,dum1,stot,dum2,dum3)
C        F2 = Q2*(ONE-X)/(TWOPI**2*ALPHEM*GEV2MB) * STOT
         DT_SIGVP = 3.0D0*PI/(ALPHEM*DT_RRM2(x,q2))*stot
      ELSE
         STOP ' DT_SIGVP: F2 not defined for this MODEGA !'
      END IF
 
 
      END FUNCTION
