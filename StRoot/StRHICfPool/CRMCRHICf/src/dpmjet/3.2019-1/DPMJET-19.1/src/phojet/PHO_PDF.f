
      SUBROUTINE PHO_PDF(Npar,X,Scale2,P2vir,Pd)
C***************************************************************
C
C     call different PDF sets for different particle types
C
C     input:      NPAR     1     IGRP(1),ISET(1)
C                          2     IGRP(2),ISET(2)
C                 X        momentum fraction
C                 SCALE2   squared scale (GeV**2)
C                 P2VIR    particle virtuality (positive, GeV**2)
C
C     output      PD(-6:6) field containing the x*PDF fractions
C
C***************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION agb , agc , akb , akc , alam2 , alb , alc , 
     &                 amu2 , avval , bb , bbo , bc , beb , bec , cb , 
     &                 db , dc , del , dfval , ds
      DOUBLE PRECISION dum , dv , dval , eb , ec , esb , esc , gl , 
     &                 P2vir , Pd , PHO_CT14PDF , PHO_DOR92FS , qb , s , 
     &                 sb , sbo , sc , scale , Scale2 , ub
      DOUBLE PRECISION udb , udv , ufval , us , uv , va , vals , valu , 
     &                 value , wgx , X , xi
      INTEGER i , idcpc , ifl , ifmax , ip2 , ipabs , IPHO_PDG2ID , 
     &        ipsign , iret , iv , ival3 , mode , Npar
      SAVE 
 
      DIMENSION Pd(-6:6)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general particle data
      INCLUDE 'inc/popar2'
 
      DIMENSION param(20) , value(20)
      CHARACTER*20 param
 
      REAL xr , p2r , q2r , f2gm , xpdfgm
      DIMENSION xpdfgm(-6:6)
 
C  check of kinematic boundaries
      xi = X
      IF ( X.GT.1.D0 ) THEN
         IF ( IDEb(37).GE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,E15.8/)')
     &            'PHO_PDF: x>1 (corrected to x=1)' , X
            CALL PHO_PREVNT(-1)
         END IF
         xi = 0.99999999999D0
      ELSE IF ( X.LE.0.D0 ) THEN
         IF ( IDEb(37).GE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,E15.8/)')
     &            'PHO_PDF: X <= 0 ' , X
            CALL PHO_PREVNT(-1)
         END IF
         xi = 0.0001D0
      END IF
 
      DO i = -6 , 6
         Pd(i) = 0.D0
      END DO
      iret = 1
 
      scale = SQRT(Scale2)
 
      IF ( (Npar.EQ.1) .OR. (Npar.EQ.2) ) THEN
 
C  INTERNAL PDFS
 
         IF ( IEXt(Npar).EQ.0 ) THEN
            IF ( IGRp(Npar).EQ.2 ) THEN
C  CTEQ-TEA 14 LO PDFS + LO ALPHA_S
               Pd(-5) = xi*PHO_CT14PDF(-5,xi,scale)   !BBAR
               Pd(-4) = xi*PHO_CT14PDF(-4,xi,scale)   !CBAR
               Pd(-3) = xi*PHO_CT14PDF(-3,xi,scale)   !SBAR
               Pd(-2) = xi*PHO_CT14PDF(-1,xi,scale)   !DBAR
               Pd(-1) = xi*PHO_CT14PDF(-2,xi,scale)   !UBAR
               Pd(0) = xi*PHO_CT14PDF(0,xi,scale)   !G
               Pd(1) = xi*PHO_CT14PDF(2,xi,scale)   !D
               Pd(2) = xi*PHO_CT14PDF(1,xi,scale)   !U
               Pd(3) = xi*PHO_CT14PDF(3,xi,scale)   !S
               Pd(4) = xi*PHO_CT14PDF(4,xi,scale)   !C
               Pd(5) = xi*PHO_CT14PDF(5,xi,scale)   !B
 
               iret = 0
            ELSE IF ( ITYpe(Npar).EQ.1 ) THEN
C  proton PDFs
               IF ( IGRp(Npar).EQ.5 ) THEN
                  IF ( ISEt(Npar).EQ.3 ) THEN
                     CALL PHO_DOR92HO(xi,Scale2,udv,dv,gl,udb,sb,cb,bb)
                     uv = udv - dv
                     udb = 2.D0*udb
                     del = 0.D0
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.4 ) THEN
                     CALL PHO_DOR92LO(xi,Scale2,udv,dv,gl,udb,sb,cb,bb)
                     uv = udv - dv
                     udb = 2.D0*udb
                     del = 0.D0
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.5 ) THEN
                     CALL PHO_DOR94HO(xi,Scale2,uv,dv,del,udb,sb,gl)
C  heavy quarks from GRV92-HO
                     amu2 = 0.3
                     alam2 = 0.248*0.248
                     s = LOG(LOG(Scale2/alam2)/LOG(amu2/alam2))
                     sc = 0.820
                     alc = 0.98
                     bec = 0.0
                     akc = -0.625 - 0.523*s
                     agc = 0.0
                     bc = 1.896 + 1.616*s
                     dc = 4.12 + 0.683*s
                     ec = 4.36 + 1.328*s
                     esc = 0.677 + 0.679*s
                     cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,
     &                    esc)
                     sbo = 1.297
                     alb = 0.99
                     beb = 0.0
                     akb = 0.0 - 0.193*s
                     agb = 0.0
                     bbo = 0.0
                     db = 3.447 + 0.927*s
                     eb = 4.68 + 1.259*s
                     esb = 1.892 + 2.199*s
                     bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,
     &                    esb)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.6 ) THEN
                     CALL PHO_DOR94LO(xi,Scale2,uv,dv,del,udb,sb,gl)
C  heavy quarks from GRV92-LO
                     amu2 = 0.25
                     alam2 = 0.232D0**2
                     s = LOG(LOG(Scale2/alam2)/LOG(amu2/alam2))
                     sc = 0.888
                     alc = 1.01
                     bec = 0.37
                     akc = 0.0
                     agc = 0.0
                     bc = 4.24 - 0.804*s
                     dc = 3.46 + 1.076*s
                     ec = 4.61 + 1.490*s
                     esc = 2.555 + 1.961*s
                     cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,
     &                    esc)
                     sbo = 1.351
                     alb = 1.00
                     beb = 0.51
                     akb = 0.0
                     agb = 0.0
                     bbo = 1.848
                     db = 2.929 + 1.396*s
                     eb = 4.71 + 1.514*s
                     esb = 4.02 + 1.239*s
                     bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,
     &                    esb)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.7 ) THEN
                     CALL PHO_DOR94DI(xi,Scale2,uv,dv,del,udb,sb,gl)
C  heavy quarks from GRV92-HO
                     amu2 = 0.3
                     alam2 = 0.248*0.248
                     s = LOG(LOG(Scale2/alam2)/LOG(amu2/alam2))
                     sc = 0.820
                     alc = 0.98
                     bec = 0.0
                     akc = -0.625 - 0.523*s
                     agc = 0.0
                     bc = 1.896 + 1.616*s
                     dc = 4.12 + 0.683*s
                     ec = 4.36 + 1.328*s
                     esc = 0.677 + 0.679*s
                     cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,
     &                    esc)
                     sbo = 1.297
                     alb = 0.99
                     beb = 0.0
                     akb = 0.0 - 0.193*s
                     agb = 0.0
                     bbo = 0.0
                     db = 3.447 + 0.927*s
                     eb = 4.68 + 1.259*s
                     esb = 1.892 + 2.199*s
                     bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,
     &                    esb)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.8 ) THEN
                     CALL PHO_DOR98LO(xi,Scale2,uv,dv,us,ds,sb,gl)
                     del = ds - us
                     udb = ds + us
C  heavy quarks from GRV92-LO
                     amu2 = 0.25
                     alam2 = 0.232D0**2
                     s = LOG(LOG(Scale2/alam2)/LOG(amu2/alam2))
                     sc = 0.888
                     alc = 1.01
                     bec = 0.37
                     akc = 0.0
                     agc = 0.0
                     bc = 4.24 - 0.804*s
                     dc = 3.46 + 1.076*s
                     ec = 4.61 + 1.490*s
                     esc = 2.555 + 1.961*s
                     cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,
     &                    esc)
                     sbo = 1.351
                     alb = 1.00
                     beb = 0.51
                     akb = 0.0
                     agb = 0.0
                     bbo = 1.848
                     db = 2.929 + 1.396*s
                     eb = 4.71 + 1.514*s
                     esb = 4.02 + 1.239*s
                     bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,
     &                    esb)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.9 ) THEN
C               CALL PHO_DOR98SC(XI,SCALE2,UV,DV,US,DS,SB,GL)
                     del = ds - us
                     udb = ds + us
C  heavy quarks from GRV92-LO
                     amu2 = 0.25
                     alam2 = 0.232D0**2
                     s = LOG(LOG(Scale2/alam2)/LOG(amu2/alam2))
                     sc = 0.888
                     alc = 1.01
                     bec = 0.37
                     akc = 0.0
                     agc = 0.0
                     bc = 4.24 - 0.804*s
                     dc = 3.46 + 1.076*s
                     ec = 4.61 + 1.490*s
                     esc = 2.555 + 1.961*s
                     cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,
     &                    esc)
                     sbo = 1.351
                     alb = 1.00
                     beb = 0.51
                     akb = 0.0
                     agb = 0.0
                     bbo = 1.848
                     db = 2.929 + 1.396*s
                     eb = 4.71 + 1.514*s
                     esb = 4.02 + 1.239*s
                     bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,
     &                    esb)
                     iret = 0
                  END IF
                  Pd(-5) = bb
                  Pd(-4) = cb
                  Pd(-3) = sb
                  Pd(-2) = 0.5D0*(udb-del)
                  Pd(-1) = 0.5D0*(udb+del)
                  Pd(0) = gl
                  Pd(1) = dv + Pd(-1)
                  Pd(2) = uv + Pd(-2)
                  Pd(3) = Pd(-3)
                  Pd(4) = Pd(-4)
                  Pd(5) = Pd(-5)
               END IF
            ELSE IF ( ITYpe(Npar).EQ.2 ) THEN
C  pion PDFs (default for pi+)
               IF ( IGRp(Npar).EQ.5 ) THEN
                  IF ( ISEt(Npar).EQ.1 ) THEN
                     CALL PHO_DORPHO(xi,Scale2,va,gl,qb,cb,bb)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.2 ) THEN
                     CALL PHO_DORPLO(xi,Scale2,va,gl,qb,cb,bb)
                     iret = 0
                  END IF
                  Pd(-5) = bb
                  Pd(-4) = cb
                  Pd(-3) = qb
                  Pd(-2) = qb
                  Pd(-1) = qb + va
                  Pd(0) = gl
                  Pd(1) = qb
                  Pd(2) = va + qb
                  Pd(3) = qb
                  Pd(4) = cb
                  Pd(5) = bb
               END IF
            ELSE IF ( ITYpe(Npar).EQ.3 ) THEN
C  photon PDFs
               IF ( IGRp(Npar).EQ.5 ) THEN
                  IF ( ISEt(Npar).EQ.1 ) THEN
                     CALL PHO_DORGH0(xi,Scale2,ub,db,sb,cb,bb,gl)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.2 ) THEN
                     CALL PHO_DORGHO(xi,Scale2,ub,db,sb,cb,bb,gl)
                     iret = 0
                  ELSE IF ( ISEt(Npar).EQ.3 ) THEN
                     CALL PHO_DORGLO(xi,Scale2,ub,db,sb,cb,bb,gl)
                     iret = 0
                  END IF
C  reweight with Drees-Godbole factor
                  wgx = 1.D0
                  IF ( P2vir.GT.0.001D0 ) THEN
                     wgx = LOG(Scale2/(P2vir+PARmdl(144)))
     &                     /LOG(Scale2/PARmdl(144))
                     wgx = MAX(wgx,0.D0)
                  END IF
                  Pd(-5) = bb*wgx/137.D0
                  Pd(-4) = cb*wgx/137.D0
                  Pd(-3) = sb*wgx/137.D0
                  Pd(-2) = ub*wgx/137.D0
                  Pd(-1) = db*wgx/137.D0
                  Pd(0) = gl*wgx*wgx/137.D0
                  Pd(1) = Pd(-1)
                  Pd(2) = Pd(-2)
                  Pd(3) = Pd(-3)
                  Pd(4) = Pd(-4)
                  Pd(5) = Pd(-5)
               ELSE IF ( IGRp(Npar).EQ.8 ) THEN
                  IF ( ISEt(Npar).EQ.1 ) THEN
                     CALL PHO_PHGAL(xi,Scale2,Pd)
                     iret = 0
                  END IF
               END IF
            ELSE IF ( ITYpe(Npar).EQ.20 ) THEN
C  Pomeron PDFs
               mode = IGRp(Npar)
               IF ( mode.EQ.1 ) THEN
                  Pd(0) = 6.D0*(1.D0-xi)**5*PARmdl(26)*PARmdl(78)
                  iret = 0
               ELSE IF ( mode.EQ.2 ) THEN
                  Pd(0) = 6.D0*xi*(1.D0-xi)*PARmdl(26)*PARmdl(78)
                  iret = 0
               ELSE IF ( mode.EQ.3 ) THEN
                  Pd(0) = (0.18D0/xi+5.46D0)*(1.D0-xi)*PARmdl(26)
     &                    *PARmdl(78)
                  iret = 0
               ELSE IF ( mode.EQ.4 ) THEN
                  CALL PHO_CKMTPD(990,xi,Scale2,Pd)
                  DO i = -4 , 4
                     Pd(i) = Pd(i)*PARmdl(78)
                  END DO
                  iret = 0
               END IF
            END IF
 
C  external PDFs
 
         ELSE IF ( IEXt(Npar).EQ.2 ) THEN
C  PDFLIB call: new PDF numbering
            IF ( Npar.NE.NPAold ) THEN
               param(1) = 'NPTYPE'
               param(2) = 'NGROUP'
               param(3) = 'NSET'
               param(4) = ' '
               value(1) = ITYpe(Npar)
               value(2) = ABS(IGRp(Npar))
               value(3) = ISEt(Npar)
               CALL PDFSET(param,value)
            END IF
            IF ( ITYpe(Npar).EQ.3 ) THEN
               ip2 = 0
               CALL STRUCTP(xi,Scale2,P2vir,ip2,Pd(2),Pd(1),Pd(-2),
     &                      Pd(-1),Pd(-3),Pd(-4),Pd(-5),Pd(-6),Pd(0))
            ELSE
               scale = SQRT(Scale2)
               CALL STRUCTM(xi,scale,Pd(2),Pd(1),Pd(-2),Pd(-1),Pd(-3),
     &                      Pd(-4),Pd(-5),Pd(-6),Pd(0))
            END IF
            DO i = 3 , 6
               Pd(i) = Pd(-i)
            END DO
            IF ( ITYpe(Npar).EQ.1 ) THEN
C  proton valence quarks
               Pd(1) = Pd(1) + Pd(-1)
               Pd(2) = Pd(2) + Pd(-2)
            ELSE IF ( ITYpe(Npar).EQ.2 ) THEN
C  pi+ valences
               dval = Pd(1)
               Pd(1) = Pd(-1)
               Pd(-1) = dval + Pd(1)
               Pd(2) = Pd(2) + Pd(-2)
            ELSE IF ( ITYpe(Npar).EQ.3 ) THEN
C  photon conventions
               Pd(1) = Pd(-1)
               Pd(2) = Pd(-2)
            END IF
            iret = 0
 
         ELSE IF ( IEXt(Npar).EQ.3 ) THEN
C  PHOLIB call: version 2.0
            CALL PHVAL(IGRp(Npar),ISEt(Npar),xi,Scale2,Pd,iret)
            IF ( iret.LT.0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I2)')
     &               'PHO_PDF:ERROR: non-vanishing PHVAL return code' , 
     &              iret
               CALL PHO_ABORT
            END IF
            iret = 0
 
C  photon PDFs depending on photon virtuality
 
         ELSE IF ( IEXt(Npar).EQ.4 ) THEN
            IF ( IGRp(Npar).EQ.1 ) THEN
C  Schuler/Sjostrand PDF (interface to single precision)
               xr = SNGL(xi)
               q2r = SNGL(Scale2)
               p2r = SNGL(P2vir)
               ip2 = 0
               CALL PHO_SASGAM(ISEt(Npar),xr,q2r,p2r,ip2,f2gm,xpdfgm)
               DO i = -6 , 6
                  Pd(i) = DBLE(xpdfgm(i))
               END DO
               iret = 0
            ELSE IF ( IGRp(Npar).EQ.5 ) THEN
C  Gluck/Reya/Stratmann
               IF ( ISEt(Npar).EQ.4 ) THEN
                  CALL PHO_DORGLV(xi,Scale2,P2vir,ub,db,sb,gl)
                  CALL PHO_QPMPDF(4,xi,Scale2,0.D0,P2vir,cb)
                  iret = 0
                  Pd(-5) = 0.D0
                  Pd(-4) = cb
                  Pd(-3) = sb/137.D0
                  Pd(-2) = ub/137.D0
                  Pd(-1) = db/137.D0
                  Pd(0) = gl/137.D0
                  Pd(1) = Pd(-1)
                  Pd(2) = Pd(-2)
                  Pd(3) = Pd(-3)
                  Pd(4) = Pd(-4)
                  Pd(5) = Pd(-5)
               END IF
            END IF
         END IF
 
C  check for errors
 
         IF ( iret.NE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/10X,5I6)') 
     &        'PHO_PDF:ERROR:unsupported PDF(NPAR,IEXT,ITYPE,IGRP,ISET)'
     &        , Npar , IEXt(Npar) , ITYpe(Npar) , IGRp(Npar) , 
     &        ISEt(Npar)
            CALL PHO_ABORT
         END IF
C  error in NPAR
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &         'PHO_PDF:ERROR:invalid NPAR(1,2) ' , Npar
         CALL PHO_ABORT
      END IF
      NPAold = Npar
 
C  valence quark treatment
      ipabs = ABS(IPArid(Npar))
      ipsign = SIGN(1,IPArid(Npar))
      IF ( (ITYpe(Npar).EQ.2) .AND. (ipabs.NE.211) ) THEN
C  valence quarks for mesons but pi+
         idcpc = IPHO_PDG2ID(ipabs)
         IF ( IQ_list(3,idcpc).NE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,*)
     &            'PHO_PDF: Error determining meson valences.'
            CALL PHO_ABORT
         END IF
 
        !Ordinary meson with fixed valence quarks
         IF ( IQ_list(1,idcpc).NE.IQ_list(2,idcpc) ) THEN
          !down-flavor valence
            dfval = Pd(-1) - Pd(1)
          !up-flavor valence
            ufval = Pd(2) - Pd(-2)
          ! set valence density to sea density
            Pd(-1) = Pd(1)
            Pd(2) = Pd(-2)
            DO iv = 1 , 2
            ! Valence is up-type (u, c, t)
               IF ( MOD(IQ_list(iv,idcpc),2).EQ.0 ) THEN
                  Pd(IQ_list(iv,idcpc)) = Pd(IQ_list(iv,idcpc)) + ufval
               ELSE
            ! Valence is down-type (d,s,b)
                  Pd(IQ_list(iv,idcpc)) = Pd(IQ_list(iv,idcpc)) + dfval
               END IF
            END DO
 
        ! neutral kaon mixed state
         ELSE IF ( (ipabs.EQ.310) .OR. (ipabs.EQ.130) ) THEN
            vals = Pd(-1) - Pd(1)
            valu = Pd(2) - Pd(-2)
            Pd(-1) = Pd(1)
            Pd(2) = Pd(-2)
            Pd(2) = Pd(2) + valu/2.D0
            Pd(-2) = Pd(-2) + valu/2.D0
            Pd(3) = Pd(3) + vals/2.D0
            Pd(-3) = Pd(-3) + vals/2.D0
        ! unflavored meson: pi0, eta,...
         ELSE
            ifmax = MAX(2,ABS(IQ_list(1,idcpc)))
            dfval = Pd(-1) - Pd(1)
            ufval = Pd(2) - Pd(-2)
            avval = (dfval+ufval)/(2.D0*DBLE(ifmax))
          ! set valence density to sea density
            Pd(-1) = Pd(1)
            Pd(2) = Pd(-2)
          ! Add average valence density to all contributing flavors
            DO ifl = 1 , ifmax
               Pd(ifl) = Pd(ifl) + avval
               Pd(-ifl) = Pd(-ifl) + avval
            END DO
 
         END IF
 
      ELSE IF ( (ITYpe(Npar).EQ.1) .AND. (ipabs.NE.2212) ) THEN
C  valence quarks for nucleons but proton
         IF ( ipabs.EQ.2112 ) THEN
C  neutron
            CALL PHO_SWAPFLAV(Pd,1,2)
         ELSE
            idcpc = IPHO_PDG2ID(ipabs)
            IQ_list(1,idcpc) = IQ_list(1,idcpc)
            IQ_list(2,idcpc) = IQ_list(2,idcpc)
            ival3 = IQ_list(3,idcpc)
            IF ( IQ_list(3,idcpc).EQ.0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               'PHO_PDF: Error determining baryon valences.'
               WRITE (6,*) ipabs , idcpc , (IQ_list(i,idcpc),i=1,3)
               CALL PHO_ABORT
            END IF
 
          !Ordinary baryon
          !down-flavor valence
            dfval = Pd(1) - Pd(-1)
          !up-flavor valence
            ufval = (Pd(2)-Pd(-2))/2.D0
          ! set valence density to sea density
            Pd(1) = Pd(-1)
            Pd(2) = Pd(-2)
 
            DO iv = 1 , 3
               IF ( MOD(IQ_list(iv,idcpc),2).EQ.0 ) THEN
            ! Valence 1 is up-type (u, c, t)
                  Pd(IQ_list(iv,idcpc)) = Pd(IQ_list(iv,idcpc)) + ufval
               ELSE
            ! Valence 1 is down-type (d,s,b)
                  Pd(IQ_list(iv,idcpc)) = Pd(IQ_list(iv,idcpc)) + dfval
               END IF
            END DO
         END IF
      END IF
 
C  antiparticle
      IF ( IPArid(Npar).LT.0 ) THEN
         DO i = 1 , 4
            dum = Pd(i)
            Pd(i) = Pd(-i)
            Pd(-i) = dum
         END DO
      END IF
 
C  optionally remove valence quarks
      IF ( IPAva(Npar).EQ.0 ) THEN
         DO i = 1 , 4
            Pd(i) = MIN(Pd(-i),Pd(i))
            Pd(-i) = Pd(i)
         END DO
      END IF
 
 
C  debug information
      IF ( LPRi.GT.4 .AND. IDEb(37).GE.30 ) WRITE (LO,
     &     '(1X,A,I4,1P,3E12.4/,2X,A,6E10.3,/2X,A,E10.3,/2X,A,6E10.3)')
     &      'PHO_PDF: NPAR,X,SCALE**2,P2VIR' , Npar , X , Scale2 , 
     &     P2vir , 'PD(-6..-1)' , (Pd(i),i=-6,-1) , 'PD(0)     ' , 
     &     Pd(0) , 'PD(1..6)  ' , (Pd(i),i=1,6)
 
      END SUBROUTINE
