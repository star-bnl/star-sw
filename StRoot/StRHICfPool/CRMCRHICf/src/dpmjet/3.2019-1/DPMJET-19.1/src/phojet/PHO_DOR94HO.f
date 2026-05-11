
      SUBROUTINE PHO_DOR94HO(X,Q2,Uv,Dv,Del,Udb,Sb,Gl)
C
C...NLO PARAMETRIZATION (MS(BAR)) :
C

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
      nu = 1.304 + 0.863*s
      aku = 0.558 - 0.020*s
      bku = 0.183*s
      au = -0.113 + 0.283*s - 0.321*s2
      bu = 6.843 - 5.089*s + 2.647*s2 - 0.527*s3
      cu = 7.771 - 10.09*s + 2.630*s2
      du = 3.315 + 1.145*s - 0.583*s2 + 0.154*s3
      Uv = PHO_DOR94FV(X,nu,aku,bku,au,bu,cu,du)
C...DV :
      nd = 0.102 - 0.017*s + 0.005*s2
      akd = 0.270 - 0.019*s
      bkd = 0.260
      ad = 2.393 + 6.228*s - 0.881*s2
      bd = 46.06 + 4.673*s - 14.98*s2 + 1.331*s3
      cd = 17.83 - 53.47*s + 21.24*s2
      dd = 4.081 + 0.976*s - 0.485*s2 + 0.152*s3
      Dv = PHO_DOR94FV(X,nd,akd,bkd,ad,bd,cd,dd)
C...DEL :
      ne = 0.070 + 0.042*s - 0.011*s2 + 0.004*s3
      ake = 0.409 - 0.007*s
      bke = 0.782 + 0.082*s
      ae = -29.65 + 26.49*s + 5.429*s2
      be = 90.20 - 74.97*s + 4.526*s2
      ce = 0.0
      de = 8.122 + 2.120*s - 1.088*s2 + 0.231*s3
      Del = PHO_DOR94FV(X,ne,ake,bke,ae,be,ce,de)
C...UDB :
      alx = 0.877
      bex = 0.561
      akx = 0.275
      bkx = 0.0
      agx = 0.997
      bgx = 3.210 - 1.866*s
      cx = 7.300
      dx = 9.010 + 0.896*ds + 0.222*s2
      ex = 3.077 + 1.446*s
      esx = 3.173 - 2.445*ds + 2.207*s
      Udb = PHO_DOR94FW(X,s,alx,bex,akx,bkx,agx,bgx,cx,dx,ex,esx)
C...SB :
      als = 0.756
      bes = 0.216
      aks = 1.690 + 0.650*ds - 0.922*s
      as = -4.329 + 1.131*s
      bs = 9.568 - 1.744*s
      dst = 9.377 + 1.088*ds - 1.320*s + 0.130*s2
      est = 3.031 + 1.639*s
      ess = 5.837 + 0.815*s
      Sb = PHO_DOR94FS(X,s,als,bes,aks,as,bs,dst,est,ess)
C...GL :
      alg = 1.014
      beg = 1.738
      akg = 1.724 + 0.157*s
      bkg = 0.800 + 1.016*s
      ag = 7.517 - 2.547*s
      bg = 34.09 - 52.21*ds + 17.47*s
      cg = 4.039 + 1.491*s
      dg = 3.404 + 0.830*s
      eg = -1.112 + 3.438*s - 0.302*s2
      esg = 3.256 - 0.436*s
      Gl = PHO_DOR94FW(X,s,alg,beg,akg,bkg,ag,bg,cg,dg,eg,esg)
 
      END SUBROUTINE
