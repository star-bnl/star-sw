
C
C...NLO PARAMETRIZATION (DIS) :
C
      SUBROUTINE PHO_DOR94DI(X,Q2,Uv,Dv,Del,Udb,Sb,Gl)
      IMPLICIT NONE
      DOUBLE PRECISION ad , ae , ag , agx , akd , ake , akg , aks , 
     &                 aku , akx , alg , als , alx , as , au , bd , be , 
     &                 beg , bes , bex
      DOUBLE PRECISION bg , bgx , bkd , bke , bkg , bku , bkx , bs , 
     &                 bu , cd , ce , cg , cu , cx , dd , de , Del , 
     &                 dg , ds , dst
      DOUBLE PRECISION du , Dv , dx , eg , esg , ess , est , esx , ex , 
     &                 Gl , lam2 , mu2 , nd , ne , nu , PHO_DOR94FS , 
     &                 PHO_DOR94FV , PHO_DOR94FW , Q2 , s
      DOUBLE PRECISION s2 , s3 , Sb , Udb , Uv , X
      SAVE 
 
      mu2 = 0.34
      lam2 = 0.248*0.248
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ds = SQRT(s)
      s2 = s*s
      s3 = s2*s
C...UV :
      nu = 2.484 + 0.116*s + 0.093*s2
      aku = 0.563 - 0.025*s
      bku = 0.054 + 0.154*s
      au = -0.326 - 0.058*s - 0.135*s2
      bu = -3.322 + 8.259*s - 3.119*s2 + 0.291*s3
      cu = 11.52 - 12.99*s + 3.161*s2
      du = 2.808 + 1.400*s - 0.557*s2 + 0.119*s3
      Uv = PHO_DOR94FV(X,nu,aku,bku,au,bu,cu,du)
C...DV :
      nd = 0.156 - 0.017*s
      akd = 0.299 - 0.022*s
      bkd = 0.259 - 0.015*s
      ad = 3.445 + 1.278*s + 0.326*s2
      bd = -6.934 + 37.45*s - 18.95*s2 + 1.463*s3
      cd = 55.45 - 69.92*s + 20.78*s2
      dd = 3.577 + 1.441*s - 0.683*s2 + 0.179*s3
      Dv = PHO_DOR94FV(X,nd,akd,bkd,ad,bd,cd,dd)
C...DEL :
      ne = 0.099 + 0.019*s + 0.002*s2
      ake = 0.419 - 0.013*s
      bke = 1.064 - 0.038*s
      ae = -44.00 + 98.70*s - 14.79*s2
      be = 28.59 - 40.94*s - 13.66*s2 + 2.523*s3
      ce = 84.57 - 108.8*s + 31.52*s2
      de = 7.469 + 2.480*s - 0.866*s2
      Del = PHO_DOR94FV(X,ne,ake,bke,ae,be,ce,de)
C...UDB :
      alx = 1.215
      bex = 0.466
      akx = 0.326 + 0.150*s
      bkx = 0.956 + 0.405*s
      agx = 0.272
      bgx = 3.794 - 2.359*ds
      cx = 2.014
      dx = 7.941 + 0.534*ds - 0.940*s + 0.410*s2
      ex = 3.049 + 1.597*s
      esx = 4.396 - 4.594*ds + 3.268*s
      Udb = PHO_DOR94FW(X,s,alx,bex,akx,bkx,agx,bgx,cx,dx,ex,esx)
C...SB :
      als = 0.175
      bes = 0.344
      aks = 1.415 - 0.641*ds
      as = 0.580 - 9.763*ds + 6.795*s - 0.558*s2
      bs = 5.617 + 5.709*ds - 3.972*s
      dst = 13.78 - 9.581*s + 5.370*s2 - 0.996*s3
      est = 4.546 + 0.372*s2
      ess = 5.053 - 1.070*s + 0.805*s2
      Sb = PHO_DOR94FS(X,s,als,bes,aks,as,bs,dst,est,ess)
C...GL :
      alg = 1.258
      beg = 1.846
      akg = 2.423
      bkg = 2.427 + 1.311*s - 0.153*s2
      ag = 25.09 - 7.935*s
      bg = -14.84 - 124.3*ds + 72.18*s
      cg = 590.3 - 173.8*s
      dg = 5.196 + 1.857*s
      eg = -1.648 + 3.988*s - 0.432*s2
      esg = 3.232 - 0.542*s
      Gl = PHO_DOR94FW(X,s,alg,beg,akg,bkg,ag,bg,cg,dg,eg,esg)
 
      END SUBROUTINE
