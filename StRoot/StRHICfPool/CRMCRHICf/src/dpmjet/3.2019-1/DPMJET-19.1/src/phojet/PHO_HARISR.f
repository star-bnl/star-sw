
      SUBROUTINE PHO_HARISR(Ihpom,P1,P2,Ipf1,Ipf2,Ipa1,Ipa2,Iv1,Iv2,Q2h,
     &                      Xh1,Xh2,Xhmax1,Xhmax2,Ipb1,Ipb2,Ivo1,Ivo2,
     &                      Xisr1,Xisr2,Irej)
C********************************************************************
C
C     initial state radiation according to DGLAP evolution equations
C     (backward evolution, no spin effects)
C
C     input:    IHPOM     index of hard Pomeron
C                         negative: delete all previous entries
C               P1,P2     4 momenta of hard scattered final partons
C                         (in CMS of hard scattering)
C               IPF1,2    flavours of final partons
C               IPA1,2    flavours of initial partons
C               IV1,2     valence quark labels (0/1)
C               Q2H       momentum transfer (squared, positive)
C               XH1,XH2   x values of initial partons
C               XHMAX1,2  max. x values allowed
C
C     output:   all emitted partons in /POPISR/, final state
C               partons are the first two entries
C               shower evolution traced in /PODGL1/
C               IPB1,2    flavours of new initial partons
C               XISR1,2   x values of new initial partons
C               IVO1,2    valence quark labels (0/1)
C
C     attention: quark numbering according to PDG convention,
C                but 0 for gluons
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION anorf , b0qcd , beg , cf , cfe , codd , cofd , 
     &                 cog , coh , DEPS , DT_RNDM , ee , ee3 , ee4 , 
     &                 fxp , gam , gb , P1 , P2 , pc
      DOUBLE PRECISION pd1 , pd2 , PHO_PMASS , pje , pjx , pjy , pjz , 
     &                 pm , pn , pp4 , pt2 , pt2new , pt3 , pta1 , 
     &                 ptot1 , px , px3 , py , py3 , pz
      DOUBLE PRECISION pz3 , pz4 , q2 , Q2h , q2new , q2p , qmax , r1 , 
     &                 r3 , RHOMAS , s1 , s3 , scale2 , sfe , sh , 
     &                 shat1 , shz , sidd , sifd , sig
      DOUBLE PRECISION sih , ssh , sshz , theta , TINY , tt , wgdir , 
     &                 wgf , wggap , wgpdf , wgtot , Xh1 , Xh2 , xhma , 
     &                 Xhmax1 , Xhmax2 , xhmi , xi , Xisr1 , Xisr2
      DOUBLE PRECISION xmip , xms4 , xms4m , xnew , xp , zmax , zmin , 
     &                 zz
      INTEGER i , idmo , ifla , iflb , ifsum , ihidx , Ihpom , ii , il , 
     &        indx , ip , ipa , Ipa1 , Ipa2 , ipal , ipb , Ipb1 , Ipb2 , 
     &        ipdfc , Ipf1
      INTEGER Ipf2 , Irej , Iv1 , Iv2 , ival , Ivo1 , Ivo2 , k , kf , 
     &        kk , l , next , ngen , niter , ntry
      SAVE 
 
      PARAMETER (RHOMAS=0.766D0,DEPS=1.D-10,TINY=1.D-10)
 
      DIMENSION P1(4) , P2(4)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  parameters for DGLAP backward evolution in ISR
      INCLUDE 'inc/podgl1'
C  initial state parton radiation (internal part)
      INCLUDE 'inc/point6'
C  some constants
      INCLUDE 'inc/pocons'
C  particles created by initial state evolution
      INCLUDE 'inc/popisr'
 
      DOUBLE PRECISION PYP , eer , ther , qmaxr
      INTEGER PYK
 
      DIMENSION xhma(2) , next(2) , pd1(-6:6) , pd2(-6:6) , wggap(-6:6)
     &          , wgpdf(-6:6) , xhmi(2) , gb(4) , pm(4) , pn(4) , 
     &          pc(2,4) , q2(2) , ival(2) , ipal(2) , il(2) , ifsum(2) , 
     &          idmo(2)
 
      Irej = 0
      ntry = 1000
      niter = 0
C  debug output
      IF ( IDEb(79).GE.10 ) THEN
         IF ( LPRi.GT.4 )
     &         WRITE (LO,'(1X,A,/1X,I10,3I3,5E11.3,2(/5X,4E12.3))')
     &         'PHO_HARISR: KEV,IHPOM,IP1,IP2,Q2H,XH1,XH2,XHM1,XHM2:' , 
     &        KEVent , Ihpom , Ipa1 , Ipa2 , Q2h , Xh1 , Xh2 , Xhmax1 , 
     &        Xhmax2 , P1 , P2
      END IF
      IF ( Ihpom.EQ.0 ) RETURN
C
 100  NACc = 0
      idmo(1) = IDPdg1
      idmo(2) = IDPdg2
C
C  copy final state partons to local fields
      ihidx = ABS(Ihpom)
 
      IF ( ihidx.GT.MXISR2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)')
     &         'PHO_HARISR: no space left in ' , 
     &        '/POPISR/ for hard scattering labels (IHIDX,MXISR2):' , 
     &        ihidx , MXISR2
         Irej = 1
      END IF
 
      DO k = 1 , 2
         IF ( Ihpom.LT.0 ) IMXisr(k) = 0
         IPOisr(k,1,ihidx) = IMXisr(k) + 1
         ipal(k) = IPOisr(k,1,ihidx)
      END DO
      DO i = 1 , 4
         PHIsr(1,i,ipal(1)) = P1(i)
         PHIsr(2,i,ipal(2)) = P2(i)
      END DO
      IFLisr(1,ipal(1)) = Ipf1
      IFLisr(2,ipal(2)) = Ipf2
C
C  check limitations, initialize /PODGL1/
      IF ( (Q2h.GT.Q2Misr(1)) .AND. (Xh1.LT.Xhmax1) ) THEN
         next(1) = 1
         Q2Sh(1,1) = Q2h
      ELSE
         next(1) = 0
         Q2Sh(1,1) = 0.D0
      END IF
      IF ( (Q2h.GT.Q2Misr(2)) .AND. (Xh2.LT.Xhmax2) ) THEN
         next(2) = 1
         Q2Sh(2,1) = Q2h
      ELSE
         next(2) = 0
         Q2Sh(2,1) = 0.D0
      END IF
C
      ISH(1) = 1
      ISH(2) = 1
      XPSh(1,1) = Xh1
      XPSh(2,1) = Xh2
C
      IFL1(1,1) = Ipa1
      ival(1) = Iv1
      IF ( (Ipa1.EQ.22) .OR. (Ipa1.EQ.990) ) next(1) = 0
      IFL1(2,1) = Ipa2
      ival(2) = Iv2
      IF ( (Ipa2.EQ.22) .OR. (Ipa2.EQ.990) ) next(2) = 0
C
      IF ( LPRi.GT.4 .AND. IDEb(79).GE.17 )
     &      WRITE (LO,'(1X,A,/5X,2I2,3E12.3)')
     &      'PHO_HARISR:INITIAL TESTS (NEXT1,2 Q2H Q21,2)' , next , 
     &     Q2h , Q2Misr
      IF ( next(1)+next(2).EQ.0 ) GOTO 600
C
C  initialize parton shower loop
      b0qcd = (33.D0-2.D0*NFSisr)/6.D0
      AL2isr(1) = PDFlam(1)
      AL2isr(2) = PDFlam(2)
      xhma(1) = Xhmax1
      xhma(2) = Xhmax2
      xhmi(1) = PMIsr(1)/PCMp
      xhmi(2) = PMIsr(2)/PCMp
      ZPSh(1,1) = 1.D0
      ZPSh(2,1) = 1.D0
      shat1 = Xh1*Xh2*ECMp**2
      IF ( IPAmdl(109).EQ.1 ) THEN
         PT2sh(1,1) = Q2h
      ELSE
         PT2sh(1,1) = Q2h*(1.D0-Q2h/shat1)
      END IF
      PT2sh(2,1) = PT2sh(1,1)
      IF ( PT2sh(1,1).LT.Q2Misr(1) ) next(1) = 0
      IF ( PT2sh(2,1).LT.Q2Misr(2) ) next(2) = 0
      THSh(1,1) = 2.D0*SQRT(Q2h/shat1)
      THSh(2,1) = THSh(1,1)
      IFAno(1) = 0
      IFAno(2) = 0
      zz = 1.D0
      IF ( Irej.NE.0 ) GOTO 600
C
C  main generation loop
C -------------------------------------------------
C  choose parton side to become solved
 200  IF ( (next(1)+next(2)).EQ.2 ) THEN
         IF ( Q2Sh(1,ISH(1)).GT.Q2Sh(2,ISH(2)) ) THEN
            ip = 1
         ELSE IF ( Q2Sh(2,ISH(2)).GT.Q2Sh(1,ISH(1)) ) THEN
            ip = 2
         ELSE
            ip = MAX(INT(DT_RNDM(shat1)*2.D0+0.999999D0),1)
         END IF
      ELSE IF ( next(1).EQ.1 ) THEN
         ip = 1
      ELSE IF ( next(2).EQ.1 ) THEN
         ip = 2
      ELSE
         GOTO 600
      END IF
      indx = ISH(ip)
C  INDX now parton position of parton to become solved
C  IP   now side to be treated
      xp = XPSh(ip,indx)
      q2p = Q2Sh(ip,indx)
      pt2 = PT2sh(ip,indx)
      iflb = IFL1(ip,indx)
C  check available x
      xmip = xhmi(ip)
C  cutoff by x limitation: no further development
      IF ( (xhma(ip)-xp).LT.xmip*2.D0 ) THEN
         next(ip) = 0
         Q2Sh(ip,indx) = 0.D0
         IF ( IDEb(79).GE.17 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/5X,3E12.4,2I3)')
     &            'PHO_HARISR: evolution x-stop (XP,XMIP,XHMA,IP,INDX)'
     &           , xp , xmip , xhma(ip) , ip , indx
         END IF
         GOTO 200
      END IF
C  initial value of evolution variable t
      tt = LOG(AQQali*q2p/AL2isr(ip))
      DO i = -NFSisr , NFSisr
         wggap(i) = 0.D0
         wgpdf(i) = 0.D0
      END DO
C  DGLAP weights
      zmin = xp/xhma(ip)
      zmax = xp/(xp+xmip)
      cf = 4./3.
C  q --> q g, g --> g g
      IF ( iflb.EQ.0 ) THEN
         wggap(0) = cf*((zmax**2-zmin**2)/2.D0-2.D0*(zmax-zmin)
     &              +2.D0*LOG(zmax/zmin))
         DO i = 1 , NFSisr
            wggap(i) = wggap(0)
            wggap(-i) = wggap(0)
         END DO
         wggap(0) = 6.D0*((zmin**3-zmax**3)/3.D0+(zmax**2-zmin**2)
     &              /2.D0-2.D0*(zmax-zmin)
     &              +LOG(zmax/zmin*(1.D0-zmin)/(1.D0-zmax)))
C  q --> g q, g --> q qb
      ELSE IF ( ABS(iflb).LE.6 ) THEN
         wggap(iflb) = cf*((zmin**2-zmax**2)/2.D0-zmax+zmin-2.D0*LOG((
     &                 1.D0-zmax)/(1.D0-zmin)))
         IF ( ival(ip).EQ.0 ) wggap(0)
     &        = 0.5D0*(2./3.*(zmax**3-zmin**3)-zmax**2+zmin**2+zmax-
     &        zmin)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I7)')
     &         'PHO_HARISR:ERROR: unsupported particle ID' , iflb
         CALL PHO_ABORT
      END IF
C  anomalous/resolved evolution
      ipdfc = 0
      IF ( IPAmdl(110).GE.1 ) THEN
         IF ( (idmo(ip).EQ.22) .AND. (iflb.NE.0) .AND. (iflb.NE.21) )
     &        THEN
            wgdir = 0.D0
            IF ( NQQali.EQ.1 ) THEN
               scale2 = pt2*AQQpd
            ELSE
               scale2 = q2p*AQQpd
            END IF
            CALL PHO_PDF(ip,xp,scale2,0.D0,pd1)
            ipdfc = 1
            CALL PHO_QPMPDF(iflb,xp,scale2,pta1,PVIrtp(ip),wgdir)
            xi = DT_RNDM(xp)*pd1(iflb)
            IF ( wgdir.GT.xi ) THEN
C  debug output
               IF ( LPRi.GT.4 .AND. IDEb(79).GE.17 )
     &               WRITE (LO,'(1X,2A,/5X,4E12.5,I2,I3)')
     &               'PHO_HARISR: ' , 
     &              'direct splitting (WGDIR,WGPDF,X,SCALE2,IP,IFLB)' , 
     &              wgdir , pd1(iflb) , xp , scale2 , ip , iflb
               Q2Sh(ip,indx) = 0.D0
               next(ip) = 0
               IFAno(ip) = indx
               GOTO 200
            END IF
         END IF
      END IF
C
C  rejection loop for z,t sampling
C ------------------------------------
 300  niter = niter + 1
      IF ( niter.GE.ntry ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I6)')
     &         'PHO_HARISR: too many rejections' , niter , ntry
         CALL PHO_PREVNT(-1)
C  clean up event
         Irej = 1
         GOTO 100
      END IF
C  PDF weights
      IF ( ipdfc.EQ.0 ) THEN
         IF ( NQQali.EQ.1 ) THEN
            scale2 = pt2*AQQpd
         ELSE
            scale2 = q2p*AQQpd
         END IF
         CALL PHO_PDF(ip,xp,scale2,0.D0,pd1)
      END IF
      ipdfc = 0
C
      wgtot = 0.D0
      DO i = -NFSisr , NFSisr
         wgpdf(i) = pd1(i)/(pd1(iflb)+1.D-12)*5.D0
         wgtot = wgtot + wgpdf(i)*wggap(i)
      END DO
C
C  sample new t value
 400  tt = tt*EXP(MAX(-10.D0,LOG(DT_RNDM(shat1))*b0qcd/wgtot))
      q2new = AL2isr(ip)*EXP(tt)/AQQali
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(79).GE.20 ) WRITE (LO,'(1X,A,E12.5)')
     &      'PHO_HARISR: pre-selected Q2:' , q2new
C  compare to limits
      IF ( q2new.LT.Q2Misr(ip) ) THEN
         Q2Sh(ip,indx) = 0.D0
         next(ip) = 0
         IF ( LPRi.GT.4 .AND. IDEb(79).GE.17 )
     &         WRITE (LO,'(1X,A,2E10.3,2I3)')
     &         'PHO_HARISR: evolution Q2-stop (Q2,Q2MIN,IP,INDX):' , 
     &        q2new , Q2Misr(ip) , ip , indx
         GOTO 200
      END IF
      Q2Sh(ip,indx) = q2new
      tt = LOG(AQQali*q2new/AL2isr(ip))
C  selection of flavours
      xi = wgtot*DT_RNDM(tt)
      ifla = -NFSisr - 1
 500  ifla = ifla + 1
      xi = xi - wgpdf(ifla)*wggap(ifla)
      IF ( (xi.GT.0.D0) .AND. (ifla.LT.NFSisr) ) GOTO 500
C  debug output
C  selection of z
      IF ( LPRi.GT.4 .AND. IDEb(79).GE.20 ) WRITE (LO,'(1X,A,2I3)')
     &      'PHO_HARISR: pre-selected IFLA (IFLA,IFLB):' , ifla , iflb
      CALL PHO_HARZSP(ifla,iflb,NFSisr,zmin,zmax,zz)
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(79).GE.20 ) WRITE (LO,'(1X,A,E12.3)')
     &      'PHO_HARISR: pre-selected ZZ' , zz
C  angular ordering
      theta = 4.D0*zz**2*q2new/((ECMp*xp)**2*(1.D0-zz))
      IF ( theta.GT.THSh(ip,indx) ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(79).GE.20 )
     &        WRITE (LO,'(1X,A,2E12.3)')
     &         'PHO_HARISR: reject by angle (NEW/OLD)' , theta , 
     &        THSh(ip,indx)
         GOTO 400
      END IF
C  rejection weight given by new PDFs
      xnew = xp/zz
      pt2new = q2new*(1.D0-zz)
      IF ( NQQali.EQ.1 ) THEN
         scale2 = pt2new*AQQpd
      ELSE
         scale2 = q2new*AQQpd
      END IF
      IF ( scale2.LT.Q2Misr(ip) ) THEN
         Q2Sh(ip,indx) = 0.D0
         next(ip) = 0
         IF ( LPRi.GT.4 .AND. IDEb(79).GE.17 )
     &         WRITE (LO,'(1X,A,2E10.3,2I3)')
     &         'PHO_HARISR: evol.Q2-stop (SCALE2,Q2MIN,IP,INDX):' , 
     &        q2new , Q2Misr(ip) , ip , indx
         GOTO 200
      END IF
      CALL PHO_PDF(ip,xnew,scale2,0.D0,pd2)
      IF ( pd2(ifla).LT.1.D-10 ) GOTO 300
      CALL PHO_PDF(ip,xp,scale2,0.D0,pd1)
      pd1(iflb) = MAX(pd1(iflb),1.D-10)
      wgf = pd2(ifla)/pd1(iflb)/(wgpdf(ifla)+1.D-12)
      IF ( NQQali.EQ.1 ) wgf = wgf*LOG(q2new*AQQali/AL2isr(ip))
     &     /LOG(pt2new*AQQali/AL2isr(ip))
      IF ( (wgf.GT.1.D0) .AND. (IDEb(79).GE.2) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.3)')
     &         'PHO_HARISR: final weight:' , wgf
         IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,I7,2I3,3E11.3)')
     &         'EV,IFLA,IFLB,Q2,PT2,Z:' , KEVent , ifla , iflb , q2new , 
     &        pt2new , zz
      END IF
      IF ( wgf.LT.DT_RNDM(xnew) ) GOTO 300
 
      IF ( IDEb(79).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/3X,3I3,3E11.3)')
     &         'PHO_HARISR: accepted IP,IFLA,IFLB,PT2,Q2,Z:' , ip , 
     &        ifla , iflb , pt2new , q2new , zz
      END IF
 
      IF ( indx.GE.MXISR3 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)')
     &         'PHO_HARISR: no space left in ' , 
     &        '/POINT6/ for parton radiation (INDX,MXISR3):' , indx , 
     &        MXISR3
         Irej = 1
         RETURN
      END IF
 
C  branching accepted, registration
      Q2Sh(ip,indx) = q2new
      PT2sh(ip,indx) = pt2new
      ZPSh(ip,indx) = zz
      IFL2(ip,indx) = ifla - iflb
      Q2Sh(ip,indx+1) = q2new
      PT2sh(ip,indx+1) = PT2sh(ip,indx)
      XPSh(ip,indx+1) = xnew
      THSh(ip,indx+1) = theta
      IFL1(ip,indx+1) = ifla
      ISH(ip) = ISH(ip) + 1
 
      NACc = NACc + 1
 
      IF ( NACc.GT.MXISR4 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)')
     &         'PHO_HARISR: no space left in ' , 
     &        '/POINT6/ for parton radiation (NACC,MXISR4):' , NACc , 
     &        MXISR4
         Irej = 1
         RETURN
      END IF
 
      SHAt(NACc) = shat1
      IBRa(1,NACc) = ip
      IBRa(2,NACc) = indx
      shat1 = shat1/zz
 
C  generation of next branching
      IF ( next(1)+next(2).NE.0 ) GOTO 200
 
 
C  new initial flavours, x values
 600  Ipb1 = IFL1(1,ISH(1))
      Ipb2 = IFL1(2,ISH(2))
      Xisr1 = XPSh(1,ISH(1))
      Xisr2 = XPSh(2,ISH(2))
      Ivo1 = ival(1)
      Ivo2 = ival(2)
C  valence flavours
      IF ( Ipb1.NE.0 ) THEN
         IF ( ISH(1).GT.1 ) THEN
            CALL PHO_PDF(1,Xisr1,Q2Misr(1),0.D0,pd1)
            IF ( IDPdg1.EQ.22 ) THEN
               CALL PHO_QPMPDF(Ipb1,Xisr1,Q2Misr(1),0.D0,PVIrtp(1),fxp)
               IF ( DT_RNDM(Xisr1)*pd1(Ipb1).GT.pd1(Ipb1)-fxp ) ival(1)
     &              = 1
            ELSE
               CALL PHO_PDF(1,Xisr1,Q2Misr(1),0.D0,pd1)
               IF ( DT_RNDM(Xisr1)*pd1(Ipb1).GT.pd1(-Ipb1) ) ival(1) = 1
            END IF
         END IF
      END IF
      IF ( Ipb2.NE.0 ) THEN
         IF ( ISH(2).GT.1 ) THEN
            CALL PHO_PDF(2,Xisr2,Q2Misr(2),0.D0,pd1)
            IF ( IDPdg2.EQ.22 ) THEN
               CALL PHO_QPMPDF(Ipb2,Xisr2,Q2Misr(2),0.D0,PVIrtp(2),fxp)
               IF ( DT_RNDM(Xisr2)*pd1(Ipb2).GT.pd1(Ipb2)-fxp ) ival(2)
     &              = 1
            ELSE
               IF ( DT_RNDM(Xisr2)*pd1(Ipb2).GT.pd1(-Ipb2) ) ival(2) = 1
            END IF
         END IF
      END IF
 
C  parton kinematics
      IF ( NACc.GT.0 ) THEN
C  final partons in CMS
         pm(3) = (Xh1-Xh2)*ECMp/2.D0
         pm(4) = (Xh1+Xh2)*ECMp/2.D0
         sh = Xh1*Xh2*ECMp**2
         ssh = SQRT(sh)
         gb(3) = pm(3)/ssh
         gb(4) = pm(4)/ssh
         CALL PHO_ALTRA(gb(4),0.D0,0.D0,-gb(3),P1(1),P1(2),P1(3),P1(4),
     &                  ptot1,PHIsr(1,1,ipal(1)),PHIsr(1,2,ipal(1)),
     &                  PHIsr(1,3,ipal(1)),PHIsr(1,4,ipal(1)))
         CALL PHO_ALTRA(gb(4),0.D0,0.D0,-gb(3),P2(1),P2(2),P2(3),P2(4),
     &                  ptot1,PHIsr(2,1,ipal(2)),PHIsr(2,2,ipal(2)),
     &                  PHIsr(2,3,ipal(2)),PHIsr(2,4,ipal(2)))
         il(1) = 1
         il(2) = 1
         DO i = 1 , NACc
            ipa = IBRa(1,i)
            ipb = 3 - ipa
            il(ipa) = IBRa(2,i)
C  new initial partons in CMS
            sh = SHAt(i)
            ssh = SQRT(sh)
            shz = sh/ZPSh(ipa,il(ipa))
            sshz = SQRT(shz)
            q2(1) = Q2Sh(1,il(1))
            q2(2) = Q2Sh(2,il(2))
            pc(1,1) = 0.D0
            pc(1,2) = 0.D0
            pc(1,3) = SQRT((sh+q2(1)+q2(2))**2-4.D0*q2(1)*q2(2))
     &                /(2.D0*ssh)
            pc(1,4) = (sh-q2(1)+q2(2))/(2.D0*ssh)
            pc(2,1) = 0.D0
            pc(2,2) = 0.D0
            pc(2,3) = -pc(1,3)
            pc(2,4) = ssh - pc(1,4)
            xms4 = PHO_PMASS(IFL2(ipa,il(ipa)),1)**2
            ee3 = (shz-q2(ipa)+q2(ipb)-xms4)/(2.D0*ssh)
            s1 = sh + q2(ipa) + q2(ipb)
            s3 = shz + q2(ipb) + Q2Sh(ipa,il(ipa)+1)
            r1 = SQRT(s1**2-4.D0*q2(ipa)*q2(ipb))
            r3 = SQRT(s3**2-4.D0*q2(ipb)*Q2Sh(ipa,il(ipa)+1))
            IF ( q2(ipb).LT.0.1D0 ) THEN
               xms4m = (q2(ipa)/ZPSh(ipa,il(ipa))-Q2Sh(ipa,il(ipa)+1))
     &                 *(sh/(sh+q2(ipa))-sh/(shz+Q2Sh(ipa,il(ipa)+1)))
            ELSE
               xms4m = (s1*s3-r1*r3)/(2.D0*q2(ipb)) - q2(ipa)
     &                 - Q2Sh(ipa,il(ipa)+1)
            END IF
            ngen = 1
C  max. virtuality for time-like showers
            qmax = MIN(xms4m,PARmdl(95)*q2(ipa))
            IF ( (IPAmdl(111).GE.1) .AND. (qmax.GT.PARmdl(94)) ) THEN
C  generate time-like parton shower
               kf = IFL2(ipa,il(ipa))
               IF ( kf.EQ.0 ) kf = 21
               eer = MIN(ee3-pc(ipa,4),ECMp)
               ther = 0.
 
               CALL PY1ENT(1,kf,eer,ther,ther)
               qmaxr = SQRT(qmax)
               CALL PYSHOW(1,0,qmaxr)
C debug output
               IF ( IDEb(79).GE.25 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/,5X,1P,4E12.4)')
     &                  'PHO_HARISR: ' , 
     &                 'PYSHOW called for EE,QMAX,XMS4M,Q2(IPA)' , eer , 
     &                 qmax , xms4m , q2(ipa)
                  CALL PYLIST(1)
               END IF
               ngen = PYK(0,1)
 
               IF ( ngen.GT.1 ) THEN
                  pjx = 0.D0
                  pjy = 0.D0
                  pjz = 0.D0
                  pje = 0.D0
                  kk = ipal(ipa)
                  DO k = 3 , ngen
 
                     IF ( PYK(k,1).LE.4 ) THEN
                        kk = kk + 1
 
                        IF ( kk.GT.MXISR1 ) THEN
                           IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I5)')
     &                         'PHO_HARISR: no space ' , 
     &                        'left in /POPISR/ (KK,MXISR1):' , kk , 
     &                        MXISR1
                           Irej = 1
                           RETURN
                        END IF
 
                        PHIsr(ipa,1,kk) = PYP(k,1)
                        pjx = pjx + PHIsr(ipa,1,kk)
                        PHIsr(ipa,2,kk) = PYP(k,2)
                        pjy = pjy + PHIsr(ipa,2,kk)
                        PHIsr(ipa,3,kk) = PYP(k,3)
                        pjz = pjz + PHIsr(ipa,3,kk)
                        PHIsr(ipa,4,kk) = PYP(k,4)
                        pje = pje + PHIsr(ipa,4,kk)
                        IFLisr(ipa,kk) = PYK(k,2)
 
                        IF ( IFLisr(ipa,kk).EQ.21 ) IFLisr(ipa,kk) = 0
                        IF ( IFLisr(ipa,kk).EQ.5 ) IFLisr(ipa,kk) = 3
                        IF ( IFLisr(ipa,kk).EQ.-5 ) IFLisr(ipa,kk) = -3
                     END IF
                  END DO
                  ngen = kk - ipal(ipa)
                  xms4 = (pje+pjz)*(pje-pjz) - pjx**2 - pjy**2
                  pp4 = SQRT(pje**2-xms4)
                  ee3 = (shz-q2(ipa)+q2(ipb)-xms4)/(2.D0*ssh)
C debug output
                  IF ( LPRi.GT.4 .AND. IDEb(79).GE.20 )
     &                  WRITE (LO,'(1X,2A,/,5X,1P,6E12.4)')
     &                  'PHO_HARISR: ' , 
     &                 'time-like shower: PJE,PJX,PJY,PJZ,PP4,XMS4' , 
     &                 pje , pjx , pjy , pjz , pp4 , xms4
               END IF
            END IF
            pz3 = (2.D0*pc(ipa,4)*ee3+q2(ipa)+Q2Sh(ipa,il(ipa)+1)+xms4)
     &            /(2.D0*pc(ipa,3))
            pt3 = (ee3+pz3)*(ee3-pz3) + Q2Sh(ipa,il(ipa)+1)
            IF ( pt3.LT.0.D0 ) THEN
               IF ( LPRi.GT.4 .AND. IDEb(79).GE.5 )
     &               WRITE (LO,'(1X,A,E12.3)')
     &               'PHO_HARISR: rejection due to PT3' , pt3
               GOTO 100
            END IF
            pt3 = SQRT(pt3)
            CALL PHO_SFECFE(sfe,cfe)
            px3 = cfe*pt3
            py3 = sfe*pt3
C
            IF ( ngen.GT.1 ) THEN
C  time-like shower generated
               ee4 = ee3 - pc(ipa,4)
               pz4 = pz3 - pc(ipa,3)
               pp4 = SQRT(pt3**2+pz4**2)
C  Lorentz boost
               gam = (ee4*pje-pp4*pjz)/xms4
               beg = (pje*pp4-ee4*pjz)/xms4
C  rotation angles
               codd = pz4/pp4
               sidd = SQRT(px3**2+py3**2)/pp4
               cofd = 1.D0
               sifd = 0.D0
               IF ( pp4*sidd.GT.1.D-5 ) THEN
                  cofd = px3/(sidd*pp4)
                  sifd = py3/(sidd*pp4)
                  anorf = SQRT(cofd*cofd+sifd*sifd)
                  cofd = cofd/anorf
                  sifd = sifd/anorf
               END IF
C  copy partons back
               kk = ipal(ipa)
               DO k = 1 , ngen
                  kk = kk + 1
                  px = PHIsr(ipa,1,kk)
                  py = PHIsr(ipa,2,kk)
                  pz = PHIsr(ipa,3,kk)
                  coh = PHIsr(ipa,4,kk)
                  ee = gam*coh + beg*pz
                  pz = gam*pz + beg*coh
                  PHIsr(ipa,4,kk) = ee
                  CALL PHO_TRANS(px,py,pz,codd,sidd,cofd,sifd,
     &               PHIsr(ipa,1,kk),PHIsr(ipa,2,kk),PHIsr(ipa,3,kk))
               END DO
               ipal(ipa) = kk
            ELSE
C  no time-like shower generated
               ipal(ipa) = ipal(ipa) + 1
               PHIsr(ipa,1,ipal(ipa)) = px3
               PHIsr(ipa,2,ipal(ipa)) = py3
               PHIsr(ipa,3,ipal(ipa)) = pz3 - pc(ipa,3)
               PHIsr(ipa,4,ipal(ipa)) = ee3 - pc(ipa,4)
               IFLisr(ipa,ipal(ipa)) = IFL2(ipa,il(ipa))
            END IF
            pc(ipa,1) = px3
            pc(ipa,2) = py3
            pc(ipa,3) = pz3
            pc(ipa,4) = ee3
C  boost / rotate into new CMS
            DO k = 1 , 4
               gb(k) = (pc(1,k)+pc(2,k))/sshz
            END DO
            CALL PHO_ALTRA(gb(4),-gb(1),-gb(2),-gb(3),pc(1,1),pc(1,2),
     &                     pc(1,3),pc(1,4),ptot1,pm(1),pm(2),pm(3),
     &                     pm(4))
            cog = pm(3)/ptot1
            sig = SQRT(pm(1)**2+pm(2)**2)/ptot1
            coh = 1.D0
            sih = 0.D0
            IF ( ptot1*sig.GT.1.D-5 ) THEN
               coh = pm(1)/(sig*ptot1)
               sih = pm(2)/(sig*ptot1)
               anorf = SQRT(coh*coh+sih*sih)
               coh = coh/anorf
               sih = sih/anorf
            END IF
            DO k = 1 , 2
               DO l = IPOisr(k,1,ihidx) , ipal(k)
                  CALL PHO_ALTRA(gb(4),-gb(1),-gb(2),-gb(3),PHIsr(k,1,l)
     &               ,PHIsr(k,2,l),PHIsr(k,3,l),PHIsr(k,4,l),ptot1,
     &               pm(1),pm(2),pm(3),pm(4))
                  CALL PHO_TRANI(pm(1),pm(2),pm(3),cog,sig,coh,sih,
     &               pn(1),pn(2),pn(3))
                  CALL PHO_TRANS(pn(1),pn(2),pn(3),1.D0,0.D0,coh,sih,
     &               PHIsr(k,1,l),PHIsr(k,2,l),PHIsr(k,3,l))
                  PHIsr(k,4,l) = pm(4)
               END DO
            END DO
         END DO
C  boost back to global CMS
         pm(3) = (Xisr1-Xisr2)/2.D0
         pm(4) = (Xisr1+Xisr2)/2.D0
         ssh = SQRT(Xisr1*Xisr2)
         gb(3) = pm(3)/ssh
         gb(4) = pm(4)/ssh
         DO k = 1 , 2
            DO l = IPOisr(k,1,ihidx) , ipal(k)
               CALL PHO_ALTRA(gb(4),0.D0,0.D0,gb(3),PHIsr(k,1,l),
     &                        PHIsr(k,2,l),PHIsr(k,3,l),PHIsr(k,4,l),
     &                        ptot1,pm(1),pm(2),pm(3),pm(4))
               PHIsr(k,1,l) = pm(1)
               PHIsr(k,2,l) = pm(2)
               PHIsr(k,3,l) = pm(3)
               PHIsr(k,4,l) = pm(4)
            END DO
         END DO
      END IF
      IPOisr(1,2,ihidx) = ipal(1)
      IPOisr(2,2,ihidx) = ipal(2)
      IMXisr(1) = ipal(1)
      IMXisr(2) = ipal(2)
C
C  debug output
      IF ( IDEb(79).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I10/,6X,A,2E12.3,2I5)')
     &         'NUMBER OF EMISSIONS' , ISH(1) - 1 , ISH(2) - 1 , 
     &        'NEW X1,X2,IFL1,ILF2' , Xisr1 , Xisr2 , Ipb1 , Ipb2
         IF ( NACc.GT.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5,/6X,A)')
     &            'PHO_HARISR: ISR configuration (NITER,NACC)' , niter , 
     &           NACc , 
     &     ' SIDE   NO.   IFLB IFLC     Q2SH    PT2SH     XH         ZZ'
            DO ii = 1 , NACc
               k = IBRa(1,ii)
               i = IBRa(2,ii)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4I5,4E11.3)') k , i , 
     &              IFL1(k,i) , IFL2(k,i) , Q2Sh(k,i) , PT2sh(k,i) , 
     &              XPSh(k,i) , ZPSh(k,i)
            END DO
         END IF
C  check of final configuration
         px3 = 0.D0
         py3 = 0.D0
         pz3 = 0.D0
         ee3 = 0.D0
         ifsum(1) = 0
         ifsum(2) = 0
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_HARISR: outgoing partons'
         DO k = 1 , 2
            DO l = IPOisr(k,1,ihidx) , IPOisr(k,2,ihidx)
               IF ( LPRi.GT.4 ) WRITE (LO,'(6X,2I4,I6,4E11.3)') k , l , 
     &              IFLisr(k,l) , PHIsr(k,1,l) , PHIsr(k,2,l) , 
     &              PHIsr(k,3,l) , PHIsr(k,4,l)
               ifsum(k) = ifsum(k) + IFLisr(k,l)
               px3 = px3 + PHIsr(k,1,l)
               py3 = py3 + PHIsr(k,2,l)
               pz3 = pz3 + PHIsr(k,3,l)
               ee3 = ee3 + PHIsr(k,4,l)
            END DO
         END DO
         ifsum(1) = ifsum(1) - Ipb1
         ifsum(2) = ifsum(2) - Ipb2
         pz3 = pz3 - (Xisr1-Xisr2)*ECMp/2.D0
         ee3 = ee3 - (Xisr1+Xisr2)*ECMp/2.D0
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I4,4E11.3)')
     &         'CHECK:IFL1,2 PCM(1-4)' , ifsum , px3 , py3 , pz3 , ee3
      END IF
      END SUBROUTINE
