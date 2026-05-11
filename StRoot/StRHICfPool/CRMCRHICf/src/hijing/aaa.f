Command for complie:
c    f77 -o hijing aaa.f hijing.f hipyset1.35.f

      program main    
      CHARACTER FRAME*8, PROJ*8, TARG*8 
      DIMENSION DNDPT(50),DNDY(50)
      COMMON/HIPARNT/HIPR1(100), IHPR2(50), HINT1(100), IHNT2(50) 
C....information of produced particles:    
      COMMON/HIMAIN1/NATT, EATT, JATT, NT, NP, N0, N01, N10, N11 
      COMMON/HIMAIN2/KATT(130000,4), PATT(130000,4) 
C....information of produced partons:     
      COMMON/HIJJET1/NPJ(300), KFPJ(300,500), PJPX(300,500), 
     &    PJPY(300,500), PJPZ(300,500), PJPE(300,500), PJPM(300,500),
     &    NTJ(300), KFTJ(300,500), PJTX(300,500), PJTY(300,500),
     &    PJTZ(300,500), PJTE(300,500), PJTM(300,500)
      COMMON/HIJJET2/NSG, NJSG(900), IASG(900,3), K1SG(900,100),
     & K2SG(900,100), PXSG(900,100), PYSG(900,100), PZSG(900,100), 
     & PESG(900,100), PMSG(900,100) 
      COMMON/HISTRNG/NFP(300,15), PP(300,15), NFT(300,15), PT(300,15) 
C....initialize HIJING for PROJ+TARG collisions at c.m. energy of 20 GeV:    
      EFRM=20.0 
      FRAME='CMS' 
      PROJ='P' 
      TARG='A' 
      IAP=1 
      IZP=1 
      IAT=197 
      IZT=79 
      CALL HIJSET (EFRM, FRAME, PROJ, TARG, IAP, IZP, IAT, IZT) 
C....generating 10 central events:   
      N_EVENT=2 
      BMIN=0.0 
      print*,"HIPR1(34),HIPR1(35)",HIPR1(34),HIPR1(35)      
      BMAX=HIPR1(34)+HIPR1(35)
      DO 2000 J=1,N_EVENT
          CALL HIJING (FRAME, BMIN, BMAX) 
C....calculate rapidity and transverse momentum distributions of     
C....produced charged particles:    
          DO 1000 I=1,NATT 
C........  exclude beam nucleons as produced particles: 
            IF(KATT(I,2).EQ.0 .OR. KATT(I,2).EQ.10) GO TO 1000 
C........  select charged particles only: 
            IF (LUCHGE(KATT(I,1)) .EQ. 0) GO TO 1000 
            PTR=SQRT(PATT(I,1)**2+PATT(I,2)**2)
            IF (PTR .GT. 10.0) GO TO 100
            IPT=PTR/0.2
            DNDPT(IPT)=DNDPT(IPT)+1.0/FLOAT(N_EVENT)/0.2/2.0/PTR
100         Y=0.5*LOG((PATT(I,4)+PATT(I,3))/(PATT(I,4)+PATT(I,3)))
            IF(ABS(Y) .GT. 10.0) GO TO 1000
            IY=ABS(Y)/0.2
            DNDY(IY)=DNDY(IY)+1.0/FLOAT(N_EVENT)/0.2/2.0
1000      CONTINUE 
2000  CONTINUE 
C....print out the rapidity and transverse momentum distributions:  
      WRITE(*,*) (0.2*(K-1),DNDPT(K),DNDY(K),K=1,50)
      STOP 
      END



c-----------------------------------------------------------------------
      function rangen()
c-----------------------------------------------------------------------
c     generates a random number
c-----------------------------------------------------------------------
      common/files/ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp
      common/prnt1/iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi
1     rangen=ranf()
      if(rangen.le.0.)goto1
      if(rangen.ge.1.)goto1
      if(irandm.eq.1)write(ifch,*)'rangen()= ',rangen

      return
      end
 
c-----------------------------------------------------------------------
      real function ranf()
c-----------------------------------------------------------------------
c     uniform random number generator from cern library
c-----------------------------------------------------------------------
      double precision    dranf,    g900gt,   g900st
      double precision    dsranf(2),    dmranf(2),    dseed
      double precision    dx24,     dx48
      double precision    dl,       dc,       du,       dr
      logical             single
      data      dsranf     /  1665 1885.d0, 286 8876.d0  /
      data      dmranf     /  1518 4245.d0, 265 1554.d0  /
      data      dx24   /  1677 7216.d0  /
      data      dx48   /  281 4749 7671 0656.d0  /
      save
      single  =  .true.
      goto 10
      entry dranf()
      single  =  .false.
 10   dl  =  dsranf(1) * dmranf(1)
      dc  =  dint(dl/dx24)
      dl  =  dl - dc*dx24
      du  =  dsranf(1)*dmranf(2) + dsranf(2)*dmranf(1) + dc
      dsranf(2)  =  du - dint(du/dx24)*dx24
      dsranf(1)  =  dl
      dr     =  (dsranf(2)*dx24 + dsranf(1)) / dx48
      if(single)  then
         ranf  =  sngl(dr)
      else
         dranf  =  dr
      endif
      return
      entry g900gt()
      g900gt  =  dsranf(2)*dx24 + dsranf(1)
      return
      entry g900st(dseed)
      dsranf(2)  =  dint(dseed/dx24)
      dsranf(1)  =  dseed - dsranf(2)*dx24
      g900st =  dsranf(1)
      return
      end





