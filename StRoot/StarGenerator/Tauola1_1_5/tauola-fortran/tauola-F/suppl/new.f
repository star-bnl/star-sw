************************************************************************
* default Tauola resonance form factor (taken from tauola function
* FORM1):
************************************************************************
         XM2   = 1.402
         GAM2  = 0.174
         FORM1 = FPIKM(SQRT(S1),AMPI,AMPI)
         FORM1 = FORM1*WIGFOR(QQ,XM2,GAM2)
************************************************************************
* new form factor taking into account phase space limitations
* for decay K1(1270)->rho(770) K :
************************************************************************
         XM2   = 1.270
         GAM2  = 0.090 
         AMRES  = AMRO
         GAMRES = GAMRO
         AM3 = AMPI
         AM2 = AMPI
         AM1 = AMK
         L1 = 0
         L2 = 1
         FORM1 = BWIGK1(QQ,XM2,GAM2,AMRES,GAMRES,AM3,AM2,AM1,L1,L2)
         FORM1 = FORM1*BWIGKST(S1,AMRES,GAMRES,AM2,AM3,L2)

************************************************************************
* L-DEPENDENT BREIT WIGNER FOR K1. TAKES INTO ACCOUNT CASES WHERE
* ONLY A PORTION OF THE BW OF THE SECONDARY RESONANCE IS KINEMATICALLY
* ACCESSIBLE (IE; K1(1270)->RHOK).
*
* SQRT(S) = SAMPLED K1 MASS
* M       = MASS OF K1 (1270 or 1410)
* G       = WIDTH OF K1
* XMRES0  = MASS OF SECONDARY RESONANCE 
* GAM0    = WIDTH OF SECONDARY RESONANCE
* XMRES1  = MASS OF FIRST DAUGHTER OF SECONDARY RESONANCE
* XMRES2  = MASS OF SECOND DAUGHTER OF SECONDARY RESONANCE
* XMLEFT  = MASS OF SECOND DAUGHTER OF PRIMARY RESONANCE
*
* FORM TAKEN FROM MANLEY ET AL, 'Multichannel resonance parameterisation
* of piN scattering amplitudes', Phys Rev D, vol 45, 4002-4033 (1992).
*
* L1 IS THE ANG-MOM OF PRIMARY DECAY
* L2 IS ANG-MOM OF DECAY OF SECONDARY RESONANCE (RHO OR K*)
*
* Created: 1997
* Author: Sherry Towers
************************************************************************
+DECK,BWIGK1.
      COMPLEX FUNCTION BWIGK1(S,M,G,XMRES0,XGAM0,XMRES1,XMRES2,XMLEFT
     *                        ,L1,L2)
      IMPLICIT NONE
      REAL
     *         S,M,G
     *        ,XMRES1,XMRES2,XMLEFT
     *        ,MRES1,MRES2,MLEFT
     *        ,GAM
     *        ,EPS,W
     *        ,XMRES0,XGAM0
     *        ,MRES0,GAMRES
     *        ,RHOK1,RHOS
     *        ,A,B,C

      INTEGER  L1,L2,N1,N2
      REAL     GAUSS  ! CERNLIB GAUSSIAN INTEGRATION ROUTINE
      EXTERNAL GAUSS
      REAL     FUNC
      EXTERNAL FUNC

      COMMON /K1MASS/ W,MRES0,GAM0,MRES1,MRES2,MLEFT,N1,N2

      COMPLEX WIGNER
      WIGNER(A,B,C)= CMPLX(1.0,0.0)/CMPLX(A-B**2,B*C)

      N1 = L1
      N2 = L2
      MRES0  = XMRES0
      GAM0   = XGAM0
      MRES1 = XMRES1
      MRES2 = XMRES2
      MLEFT = XMLEFT

      EPS = 0.00001
      W = M
      RHOK1 = GAUSS(FUNC,XMRES1+XMRES2,M-XMLEFT,EPS)
      W = SQRT(S)
      RHOS  = GAUSS(FUNC,XMRES1+XMRES2,SQRT(S)-XMLEFT,EPS)
      GAM = G*(RHOS/RHOK1)
      BWIGK1 = (M**2)*WIGNER(S,M,GAM)

      RETURN
      END

************************************************************************
************************************************************************
* Created: 1997
* Author: Sherry Towers
************************************************************************
+DECK,FUNC.
      REAL FUNCTION FUNC(X)
      IMPLICIT NONE
      REAL
     *         X,W
     *        ,MRES0,MRES1,MRES2,MLEFT
     *        ,GAM,GAM0
     *        ,Q,Q0
     *        ,PIVAL
     *        ,BWIG
      INTEGER  L1,L2
      COMMON /K1MASS/ W,MRES0,GAM0,MRES1,MRES2,MLEFT,L1,L2

      PIVAL = ACOS(-1.0)

      L2 = 1
      IF (X.GT.(MRES1+MRES2)) THEN
         Q0 = (MRES0**2-(MRES1+MRES2)**2)*(MRES0**2-(MRES1-MRES2)**2)
         Q0 = SQRT(Q0)/(2.0*MRES0)

         Q = (X**2-(MRES1+MRES2)**2)*(X**2-(MRES1-MRES2)**2)
         Q = SQRT(Q)/(2.0*X)

         GAM = GAM0*(Q/Q0)**(2*L2+1)
         BWIG = (X-MRES0)**2 + (GAM/2.0)**2
         BWIG = (GAM/(2.0*PIVAL))/BWIG
      ELSE
         BWIG = 0.0
      END IF

      L1 = 0
      Q = (W**2-(MLEFT+X)**2)*(W**2-(MLEFT-X)**2)
      Q = SQRT(Q)/(2.0*W)

      FUNC = BWIG*(Q)**(2*L1+1)/W

      RETURN
      END
************************************************************************
************************************************************************
* L-DEPENDENT BREIT WIGNER FOR KSTAR(1430),K*(892) or RHO(770)
* Created: 1997
* Author: Sherry Towers
************************************************************************
+DECK,BWIGKST.
      COMPLEX FUNCTION BWIGKST(S,M,G,XM1,XM2,L)
      IMPLICIT NONE
      REAL
     *         S,M,G
     *        ,XM1,XM2
     *        ,GAM,Q,Q0
     *        ,A,B,C
      INTEGER  L
      COMPLEX WIGNER
      WIGNER(A,B,C)= CMPLX(1.0,0.0)/CMPLX(A-B**2,B*C)

      IF (S.GT.(XM1+XM2)**2.AND.M.GT.(XM1+XM2)) THEN
         Q0 = (M**2-(XM1+XM2)**2)*(M**2-(XM1-XM2)**2)
         Q0 = SQRT(Q0)/(2.0*M)

         Q = (S-(XM1+XM2)**2)*(S-(XM1-XM2)**2)
         Q = SQRT(Q)/(2.0*SQRT(S))

         GAM = G*(Q/Q0)**(2*L+1)
         BWIGKST = (M**2)*WIGNER(S,M,GAM)
      ELSE
         BWIGKST = CMPLX(0.0,0.0)
      END IF

      RETURN
      END


