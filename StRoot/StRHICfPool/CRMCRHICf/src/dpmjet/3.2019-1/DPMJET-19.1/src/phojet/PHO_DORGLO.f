
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C                                                                 *
C      G R V - P H O T O N - P A R A M E T R I Z A T I O N S      *
C                                                                 *
C                 FOR A DETAILED EXPLANATION SEE :                *
C              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/31             *
C                                                                 *
C    THE OUTPUT IS ALWAYS   1./ ALPHA(EM) * X * PARTON DENSITY    *
C                                                                 *
C   THE PARAMETRIZATIONS ARE FITTED TO THE PARTON DISTRIBUTIONS   *
C   FOR Q ** 2 BETWEEN MU ** 2 (=  0.25 / 0.30  GEV ** 2  IN LO   *
C   / HO) AND  1.E6 GEV ** 2  AND FOR X BETWEEN  1.E-5  AND  1.   *
C                                                                 *
C              HEAVY QUARK THRESHOLDS  Q(H) = M(H) :              *
C         M(C)  =  1.5,  M(B)  =  4.5,  M(T)  =  100  GEV         *
C                                                                 *
C      CORRESPONDING LAMBDA(F) VALUES FOR F ACTIVE FLAVOURS :     *
C      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.153,   LAMBDA(6)  =  0.082  GEV     *
C      HO :   LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.131,   LAMBDA(6)  =  0.053  GEV     *
C                                                                 *
C      HO DISTRIBUTIONS REFER TO THE DIS(GAMMA) SCHEME, SEE :     *
C              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/26             *
C                                                                 *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      SUBROUTINE PHO_DORGLO(X,Q2,Ul,Dl,Sl,Cl,Bl,Gl)
      IMPLICIT NONE
      DOUBLE PRECISION ag , ak , al , be , bg , bk , Bl , c , Cl , d , 
     &                 Dl , e , es , Gl , lam2 , mu2 , PHO_DORGF , 
     &                 PHO_DORGFS , Q2 , s
      DOUBLE PRECISION s2 , sf , Sl , ss , Ul , X
      SAVE 
 
      mu2 = 0.25
      lam2 = 0.232*0.232
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ss = SQRT(s)
      s2 = s*s
C...X * U = X * UBAR :
      al = 1.717
      be = 0.641
      ak = 0.500 - 0.176*s
      bk = 15.00 - 5.687*ss - 0.552*s2
      ag = 0.235 + 0.046*ss
      bg = 0.082 - 0.051*s + 0.168*s2
      c = 0.0 + 0.459*s
      d = 0.354 - 0.061*s
      e = 4.899 + 1.678*s
      es = 2.046 + 1.389*s
      Ul = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * D = X * DBAR :
      al = 1.549
      be = 0.782
      ak = 0.496 + 0.026*s
      bk = 0.685 - 0.580*ss + 0.608*s2
      ag = 0.233 + 0.302*s
      bg = 0.0 - 0.818*s + 0.198*s2
      c = 0.114 + 0.154*s
      d = 0.405 - 0.195*s + 0.046*s2
      e = 4.807 + 1.226*s
      es = 2.166 + 0.664*s
      Dl = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * G :
      al = 0.676
      be = 1.089
      ak = 0.462 - 0.524*ss
      bk = 5.451 - 0.804*s2
      ag = 0.535 - 0.504*ss + 0.288*s2
      bg = 0.364 - 0.520*s
      c = -0.323 + 0.115*s2
      d = 0.233 + 0.790*s - 0.139*s2
      e = 0.893 + 1.968*s
      es = 3.432 + 0.392*s
      Gl = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * S = X * SBAR :
      sf = 0.0
      al = 1.609
      be = 0.962
      ak = 0.470 - 0.099*s2
      bk = 3.246
      ag = 0.121 - 0.068*ss
      bg = -0.090 + 0.074*s
      c = 0.062 + 0.034*s
      d = 0.0 + 0.226*s - 0.060*s2
      e = 4.288 + 1.707*s
      es = 2.122 + 0.656*s
      Sl = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * C = X * CBAR :
      sf = 0.888
      al = 0.970
      be = 0.545
      ak = 1.254 - 0.251*s
      bk = 3.932 - 0.327*s2
      ag = 0.658 + 0.202*s
      bg = -0.699
      c = 0.965
      d = 0.0 + 0.141*s - 0.027*s2
      e = 4.911 + 0.969*s
      es = 2.796 + 0.952*s
      Cl = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * B = X * BBAR :
      sf = 1.351
      al = 1.016
      be = 0.338
      ak = 1.961 - 0.370*s
      bk = 0.923 + 0.119*s
      ag = 0.815 + 0.207*s
      bg = -2.275
      c = 1.480
      d = -0.223 + 0.173*s
      e = 5.426 + 0.623*s
      es = 3.819 + 0.901*s
      Bl = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
 
      END SUBROUTINE
