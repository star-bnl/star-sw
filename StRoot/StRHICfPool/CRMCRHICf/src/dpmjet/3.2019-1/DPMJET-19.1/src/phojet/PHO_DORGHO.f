
      SUBROUTINE PHO_DORGHO(X,Q2,Uh,Dh,Sh,Ch,Bh,Gh)
      IMPLICIT NONE
      DOUBLE PRECISION ag , ak , al , be , bg , Bh , bk , c , Ch , d , 
     &                 Dh , e , es , Gh , lam2 , mu2 , PHO_DORGF , 
     &                 PHO_DORGFS , Q2 , s
      DOUBLE PRECISION s2 , sf , Sh , ss , Uh , X
      SAVE 
 
      mu2 = 0.3
      lam2 = 0.248*0.248
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ss = SQRT(s)
      s2 = s*s
C...X * U = X * UBAR :
      al = 0.583
      be = 0.688
      ak = 0.449 - 0.025*s - 0.071*s2
      bk = 5.060 - 1.116*ss
      ag = 0.103
      bg = 0.319 + 0.422*s
      c = 1.508 + 4.792*s - 1.963*s2
      d = 1.075 + 0.222*ss - 0.193*s2
      e = 4.147 + 1.131*s
      es = 1.661 + 0.874*s
      Uh = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * D = X * DBAR :
      al = 0.591
      be = 0.698
      ak = 0.442 - 0.132*s - 0.058*s2
      bk = 5.437 - 1.916*ss
      ag = 0.099
      bg = 0.311 - 0.059*s
      c = 0.800 + 0.078*s - 0.100*s2
      d = 0.862 + 0.294*ss - 0.184*s2
      e = 4.202 + 1.352*s
      es = 1.841 + 0.990*s
      Dh = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * G :
      al = 1.161
      be = 1.591
      ak = 0.530 - 0.742*ss + 0.025*s2
      bk = 5.662
      ag = 0.533 - 0.281*ss + 0.218*s2
      bg = 0.025 - 0.518*s + 0.156*s2
      c = -0.282 + 0.209*s2
      d = 0.107 + 1.058*s - 0.218*s2
      e = 0.0 + 2.704*s
      es = 3.071 - 0.378*s
      Gh = PHO_DORGF(X,s,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * S = X * SBAR :
      sf = 0.0
      al = 0.635
      be = 0.456
      ak = 1.770 - 0.735*ss - 0.079*s2
      bk = 3.832
      ag = 0.084 - 0.023*s
      bg = 0.136
      c = 2.119 - 0.942*s + 0.063*s2
      d = 1.271 + 0.076*s - 0.190*s2
      e = 4.604 + 0.737*s
      es = 1.641 + 0.976*s
      Sh = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * C = X * CBAR :
      sf = 0.820
      al = 0.926
      be = 0.152
      ak = 1.142 - 0.175*s
      bk = 3.276
      ag = 0.504 + 0.317*s
      bg = -0.433
      c = 3.334
      d = 0.398 + 0.326*s - 0.107*s2
      e = 5.493 + 0.408*s
      es = 2.426 + 1.277*s
      Ch = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
C...X * B = X * BBAR :
      sf = 1.297
      al = 0.969
      be = 0.266
      ak = 1.953 - 0.391*s
      bk = 1.657 - 0.161*s
      ag = 1.076 + 0.034*s
      bg = -2.015
      c = 1.662
      d = 0.353 + 0.016*s
      e = 5.713 + 0.249*s
      es = 3.456 + 0.673*s
      Bh = PHO_DORGFS(X,s,sf,al,be,ak,bk,ag,bg,c,d,e,es)
 
      END SUBROUTINE
