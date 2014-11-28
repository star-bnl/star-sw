      real radgen_xmin, radgen_xmax, radgen_ymin, radgen_ymax

      parameter (radgen_xmin=1.0e-09)
      parameter (radgen_xmax=0.99)
      parameter (radgen_ymin=0.05)
      parameter (radgen_ymax=0.95)

      double precision sigrad,tine,tnuc,tpro,tbor,demin,phipoi
     +     ,sig1g,sigcor,vac,vertex,small,redfac
     +     ,dsts,dcts
     +     ,taout,rrout,dsitkm,dcvtkm,ddetkm,dsigmr,drcurr,ddeler

      integer ntk,nrr,itkcur,iphi,ndxtkm

      real sigradu, sigradp, sig1gu, sig1gp

      common /rgencom/sigrad,tine,tnuc,tpro,tbor,demin
     +     ,sig1g,sigcor,vac,vertex,small,redfac
     +     ,dsts,dcts
     +     ,phipoi,taout,rrout
     +     ,dsitkm(400,3),dcvtkm(400,3),ddetkm(400,3)
     +     ,dsigmr(200,400),drcurr(200,400),ddeler(200,400)
     +     ,ntk,nrr ,itkcur,iphi,ndxtkm(3)
     +     ,sigradu, sigradp, sig1gu, sig1gp

      
