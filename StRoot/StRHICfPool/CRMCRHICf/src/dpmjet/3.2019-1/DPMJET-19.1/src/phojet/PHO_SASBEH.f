
      SUBROUTINE PHO_SASBEH(Kf,X,Q2,P2,Pm2,Xpbh)
      IMPLICIT NONE
      REAL aem2pi , beta , beta2 , chsq , P2 , Pm2 , Q2 , rmq , rpbe , 
     &     rpbesn , rpq , sigbh , w2 , X , xbi , xbl , Xpbh
      INTEGER Kf
C...Purpose: to evaluate the Bethe-Heitler cross section for
C...heavy flavour production.
      SAVE 
      DATA aem2pi/0.0011614/
 
C...Reset output.
      Xpbh = 0.
      sigbh = 0.
 
C...Check kinematics limits.
      IF ( X.GE.Q2/(4.*Pm2+Q2+P2) ) RETURN
      w2 = Q2*(1.-X)/X - P2
      beta2 = 1. - 4.*Pm2/w2
      IF ( beta2.LT.1E-10 ) RETURN
      beta = SQRT(beta2)
      rmq = 4.*Pm2/Q2
 
C...Simple case: P2 = 0.
      IF ( P2.LT.1E-4 ) THEN
         IF ( beta.LT.0.99 ) THEN
            xbl = LOG((1.+beta)/(1.-beta))
         ELSE
            xbl = LOG((1.+beta)**2*w2/(4.*Pm2))
         END IF
         sigbh = beta*(8.*X*(1.-X)-1.-rmq*X*(1.-X))
     &           + xbl*(X**2+(1.-X)**2+rmq*X*(1.-3.*X)-0.5*rmq**2*X**2)
 
C...Complicated case: P2 > 0, based on approximation of
C...C.T. Hill and G.G. Ross, Nucl. Phys. B148 (1979) 373
      ELSE
         rpq = 1. - 4.*X**2*P2/Q2
         IF ( rpq.GT.1E-10 ) THEN
            rpbe = SQRT(rpq*beta2)
            IF ( rpbe.LT.0.99 ) THEN
               xbl = LOG((1.+rpbe)/(1.-rpbe))
               xbi = 2.*rpbe/(1.-rpbe**2)
            ELSE
               rpbesn = 4.*Pm2/w2 + (4.*X**2*P2/Q2)*beta2
               xbl = LOG((1.+rpbe)**2/rpbesn)
               xbi = 2.*rpbe/rpbesn
            END IF
            sigbh = beta*(6.*X*(1.-X)-1.)
     &              + xbl*(X**2+(1.-X)**2+rmq*X*(1.-3.*X)
     &              -0.5*rmq**2*X**2) + xbi*(2.*X/Q2)
     &              *(Pm2*X*(2.-rmq)-P2*X)
         END IF
      END IF
 
C...Multiply by charge-squared etc. to get parton distribution.
      chsq = 1./9.
      IF ( ABS(Kf).EQ.2 .OR. ABS(Kf).EQ.4 ) chsq = 4./9.
      Xpbh = 3.*chsq*aem2pi*X*sigbh
 
      END SUBROUTINE
