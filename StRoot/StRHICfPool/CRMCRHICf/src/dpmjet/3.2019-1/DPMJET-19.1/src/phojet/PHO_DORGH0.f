
      SUBROUTINE PHO_DORGH0(X,Q2,U0,D0,S0,C0,B0,G0)
      IMPLICIT NONE
      DOUBLE PRECISION ag , ak , al , B0 , be , bg , bk , c , C0 , d , 
     &                 D0 , e , es , G0 , lam2 , mu2 , PHO_DORGF , 
     &                 PHO_DORGFS , Q2 , s
      DOUBLE PRECISION S0 , s2 , sf , ss , U0 , X
      SAVE 
 
      mu2 = 0.3
      lam2 = 0.248*0.248
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ss = SQRT(s)
      s2 = s*s
C...X * U = X * UBAR :
      al = 1.447
      be = 0.848
      ak = 0.527 + 0.200*s - 0.107*s2
      bk = 7.106 - 0.310*ss - 0.786*s2
      ag = 0.197 + 0.533*s
      bg = 0.062 - 0.398*s + 0.109*s2
      c = 0.755*s - 0.112*s2
      d = 0.318 - 0.059*s
      e = 4.225 + 1.708*s
      es = 1.752 + 0.866*s
      U0 = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * D = X * DBAR :
      al = 1.424
      be = 0.770
      ak = 0.500 + 0.067*ss - 0.055*s2
      bk = 0.376 - 0.453*ss + 0.405*s2
      ag = 0.156 + 0.184*s
      bg = 0.0 - 0.528*s + 0.146*s2
      c = 0.121 + 0.092*s
      d = 0.379 - 0.301*s + 0.081*s2
      e = 4.346 + 1.638*s
      es = 1.645 + 1.016*s
      D0 = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * G :
      al = 0.661
      be = 0.793
      ak = 0.537 - 0.600*ss
      bk = 6.389 - 0.953*s2
      ag = 0.558 - 0.383*ss + 0.261*s2
      bg = 0.0 - 0.305*s
      c = -0.222 + 0.078*s2
      d = 0.153 + 0.978*s - 0.209*s2
      e = 1.429 + 1.772*s
      es = 3.331 + 0.806*s
      G0 = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * S = X * SBAR :
      sf = 0.0
      al = 1.578
      be = 0.863
      ak = 0.622 + 0.332*s - 0.300*s2
      bk = 2.469
      ag = 0.211 - 0.064*ss - 0.018*s2
      bg = -0.215 + 0.122*s
      c = 0.153
      d = 0.0 + 0.253*s - 0.081*s2
      e = 3.990 + 2.014*s
      es = 1.720 + 0.986*s
      S0 = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * C = X * CBAR :
      sf = 0.820
      al = 0.929
      be = 0.381
      ak = 1.228 - 0.231*s
      bk = 3.806 - 0.337*s2
      ag = 0.932 + 0.150*s
      bg = -0.906
      c = 1.133
      d = 0.0 + 0.138*s - 0.028*s2
      e = 5.588 + 0.628*s
      es = 2.665 + 1.054*s
      C0 = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * B = X * BBAR :
      sf = 1.297
      al = 0.970
      be = 0.207
      ak = 1.719 - 0.292*s
      bk = 0.928 + 0.096*s
      ag = 0.845 + 0.178*s
      bg = -2.310
      c = 1.558
      d = -0.191 + 0.151*s
      e = 6.089 + 0.282*s
      es = 3.379 + 1.062*s
      B0 = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
 
      END SUBROUTINE
