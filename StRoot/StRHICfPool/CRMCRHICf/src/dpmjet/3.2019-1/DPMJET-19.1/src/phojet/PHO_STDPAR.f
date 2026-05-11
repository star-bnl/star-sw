
      SUBROUTINE PHO_STDPAR(Ijm1,Ijm2,Igen,Mspom,Msreg,Mhpom,Mhdir,Irej)
C***********************************************************************
C
C     select the initial parton x-fractions and flavors and
C     the final parton momenta and flavours
C     for standard Pomeron/Reggeon cuts
C
C     input:   IJM1   index of mother particle 1 in /POEVT1/
C              IJM2   index of mother particle 2 in /POEVT1/
C              IGEN   production process of mother particles
C              MSPOM  soft cut Pomerons
C              MHPOM  hard or semihard cut Pomerons
C              MSREG  soft cut Reggeons
C              MHDIR  direct hard processes
C
C              IJM1   -1    initialization of statistics
C                     -2    output of statistics
C
C     output:  partons are directly written to /POEVT1/,/POEVT2/
C
C          structure of /POSOFT/
C               XS1(I),XS2(I):     x-values of initial partons
C               IJSI1(I),IJSI2(I): flavor of initial parton
C                                  0            gluon
C                                  1,2,3,4      quarks
C                                  negative     antiquarks
C               IJSF1(I),IJSF2(I): flavor of final state partons
C               PSOFT1(I,J),PSOFT2(I,J): final part. momentum and energy
C                                J=1   PX
C                                 =2   PY
C                                 =3   PZ
C                                 =4   ENERGY
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION anorf , cmass1 , delmas , DEPS , DT_RNDM , 
     &                 dummy , ecmh , pc , PHO_PMASS , PHO_XLAM , 
     &                 ptot1 , RHOMAS , ss , TINY , xm12 , xm22
      INTEGER i , i1 , ic1 , ic2 , ica1 , ica2 , icb1 , icb2 , ici , 
     &        ifl1 , ifl2 , ifla , ifla1 , iflb , iflb1 , ifli1 , 
     &        ifli2 , iflo1 , iflo2 , Igen
      INTEGER ii , iifl , Ijm1 , Ijm2 , ind , ind1 , ind2 , ipa , ipdf , 
     &        ipdf1 , ipdf2 , IPHO_CNV1 , ipois1 , ipois2 , ipos , 
     &        ipos1 , ipos2 , Irej , ist , iswap
      INTEGER iused , ival1 , ival2 , ivq , jm1 , jm2 , k , khdirs , 
     &        khpoms , kk , kspoms , ksregs , l , Mhdir , mhpar1 , 
     &        mhpar2 , Mhpom , mspar1 , mspar2 , Mspom
      INTEGER Msreg , nheps , nlor1 , ntry
      SAVE 
 
      PARAMETER (RHOMAS=0.766D0,DEPS=1.D-10,TINY=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  some constants
      INCLUDE 'inc/pocons'
C  general process information
      INCLUDE 'inc/poprcs'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  particles created by initial state evolution
      INCLUDE 'inc/popisr'
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
C  hard scattering data
      INCLUDE 'inc/pohslt'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  internal cross check information on hard scattering limits
      INCLUDE 'inc/pohlim'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
 
      DOUBLE PRECISION PHO_ALPHAS
 
      DIMENSION pc(4) , ifla(2) , ici(2,2)
 
      IF ( Ijm1.EQ.-1 ) THEN
         DO i = 1 , 15
            ETAmi(1,i) = 1.D10
            ETAma(1,i) = -1.D10
            ETAmi(2,i) = 1.D10
            ETAma(2,i) = -1.D10
            XXMi(1,i) = 1.D0
            XXMa(1,i) = 0.D0
            XXMi(2,i) = 1.D0
            XXMa(2,i) = 0.D0
         END DO
         CALL PHO_HARSCA(Ijm1,1)
         CALL PHO_HARCOL(Ijm1,0.D0,0,0,0,0,0,0,0,0,0,0,0,0)
 
         RETURN
 
      ELSE IF ( Ijm1.EQ.-2 ) THEN
 
C  output internal statistics
         IF ( IDEb(23).GE.1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 
     &           'kinematic limits particle c (ETAMIN,ETAMAX,XMIN,XMAX)'
            DO i = 1 , 15
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,4E13.5)') i , 
     &              ETAmi(1,i) , ETAma(1,i) , XXMi(1,i) , XXMa(1,i)
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &           'kinematic limits particle d (ETAMIN,ETAMAX,XMIN,XMAX)'
            DO i = 1 , 15
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,4E13.5)') i , 
     &              ETAmi(2,i) , ETAma(2,i) , XXMi(2,i) , XXMa(2,i)
            END DO
         END IF
         CALL PHO_HARSCA(Ijm1,1)
         CALL PHO_HARCOL(Ijm1,0.D0,0,0,0,0,0,0,0,0,0,0,0,0)
 
         RETURN
      END IF
 
      Irej = 0
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(23).GT.5 ) WRITE (LO,99010) Ijm1 , 
     &     Ijm2 , Mspom , Mhpom , Msreg , Mhdir
99010 FORMAT (' PHO_STDPAR: JM1/2,MSPOM,MHPOM,MSREG,MHDIR ',6I5)
 
C  get mother data (exchange if first particle is a pomeron)
      IF ( (IDHep(Ijm1).EQ.990) .AND. (IFPap(1).NE.990) ) THEN
         jm1 = Ijm2
         jm2 = Ijm1
      ELSE
         jm1 = Ijm1
         jm2 = Ijm2
      END IF
 
      NPOsp(1) = jm1
      NPOsp(2) = jm2
      IDPdg1 = IDHep(jm1)
      IDBam1 = IMPart(jm1)
      IDPdg2 = IDHep(jm2)
      IDBam2 = IMPart(jm2)
 
C  store current status of /POEVT1/
      khpoms = KHPom
      kspoms = KSPom
      ksregs = KSReg
      khdirs = KHDir
      nheps = NHEp
      ipois1 = IPOix1
      ipois2 = IPOix2
 
C  get nominal masses (photons: VDM assumption)
      delmas = 0.D0
      IF ( IDHep(jm1).EQ.22 ) THEN
         PMAssp(1) = RHOMAS + delmas
         PVIrtp(1) = PHEp(5,jm1)**2
      ELSE
         PMAssp(1) = PHO_PMASS(IDBam1,0) + delmas
         PVIrtp(1) = 0.D0
      END IF
      IF ( IDHep(jm2).EQ.22 ) THEN
         PMAssp(2) = RHOMAS + delmas
         PVIrtp(2) = PHEp(5,jm2)**2
      ELSE
         PMAssp(2) = PHO_PMASS(IDBam2,0) + delmas
         PVIrtp(2) = 0.D0
      END IF
 
C  calculate c.m. energy and check kinematics
      pc(1) = PHEp(1,jm1) + PHEp(1,jm2)
      pc(2) = PHEp(2,jm1) + PHEp(2,jm2)
      pc(3) = PHEp(3,jm1) + PHEp(3,jm2)
      pc(4) = PHEp(4,jm1) + PHEp(4,jm2)
      ss = (pc(4)+pc(3))*(pc(4)-pc(3)) - pc(1)**2 - pc(2)**2
 
      IF ( ss.LE.(PMAssp(1)+PMAssp(2)+DEPS)**2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,2A)') 'PHO_STDPAR: ' , 
     &     'energy smaller than two-particle threshold (event rejected)'
         CALL PHO_PREVNT(1)
         Irej = 5
         GOTO 100
      END IF
      ECMp = SQRT(ss)
 
      IF ( IDEb(23).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I7,E12.4)')
     &        'PHO_STDPAR: ' , 'particles, available energy:' , 
     &        IDHep(jm1) , IDHep(jm2) , ECMp
         IF ( IDEb(23).GE.25 ) CALL PHO_PREVNT(0)
      END IF
 
C  Lorentz transformation into c.m. system
      DO i = 1 , 4
         GAMbep(i) = pc(i)/ECMp
      END DO
      CALL PHO_ALTRA(GAMbep(4),-GAMbep(1),-GAMbep(2),-GAMbep(3),
     &               PHEp(1,jm1),PHEp(2,jm1),PHEp(3,jm1),PHEp(4,jm1),
     &               ptot1,pc(1),pc(2),pc(3),pc(4))
C  rotation angle: particle 1 moves along +z
      CODp = pc(3)/ptot1
      SIDp = SQRT(pc(1)**2+pc(2)**2)/ptot1
      COFp = 1.D0
      SIFp = 0.D0
      IF ( ptot1*SIDp.GT.1.D-5 ) THEN
         COFp = pc(1)/(SIDp*ptot1)
         SIFp = pc(2)/(SIDp*ptot1)
         anorf = SQRT(COFp*COFp+SIFp*SIFp)
         COFp = COFp/anorf
         SIFp = SIFp/anorf
      END IF
C  get CM momentum
      xm12 = PMAssp(1)**2
      xm22 = PMAssp(2)**2
      PCMp = PHO_XLAM(ss,xm12,xm22)/(2.D0*ECMp)
 
C  find particle combination
      ii = 0
      IF ( IDPdg2.EQ.IFPap(2) ) THEN
         IF ( IDPdg1.EQ.IFPap(1) ) ii = 1
      ELSE IF ( IDPdg2.EQ.990 ) THEN
         IF ( IDPdg1.EQ.IFPap(1) ) THEN
            ii = 2
         ELSE IF ( IDPdg1.EQ.IFPap(2) ) THEN
            ii = 3
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            ii = 4
         END IF
      END IF
      IF ( ii.EQ.0 ) THEN
         IF ( ISWmdl(14).GT.0 ) THEN
            ii = 1
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2I8)')
     &            'PHO_STDPAR:ERROR: ' , 
     &           'invalid particle combination:' , IDPdg1 , IDPdg2
            CALL PHO_ABORT
         END IF
      END IF
 
C  select parton distribution functions from tables
      IF ( (Mhpom+Mhdir).GT.0 ) THEN
         CALL PHO_ACTPDF(IDPdg1,1)
         CALL PHO_ACTPDF(IDPdg2,2)
C  initialize alpha_s calculation
         dummy = PHO_ALPHAS(0.D0,-4)
      END IF
 
C  interpolate hard cross sections and rejection weights
      CALL PHO_HARINT(ii,ECMp,PVIrtp(1),PVIrtp(2),-1,MAX_PRO_2,1,4,
     &                Mspom+Mhpom+Mhdir)
 
      ntry = 10
 
C  position of first particle added to /POEVT2/
      nlor1 = NHEp + 1
 
C  ---------------- direct processes -----------------
 
      IF ( Mhdir.EQ.1 ) THEN
         CALL PHO_HARDIR(ii,ival1,ival2,mspar1,mspar2,mhpar1,mhpar2,
     &                   Irej)
         IF ( Irej.EQ.50 ) RETURN
         IF ( Irej.NE.0 ) GOTO 100
 
C  soft spectator partons
         ica1 = 0
         ica2 = 0
         icb1 = 0
         icb2 = 0
         ipdf1 = 0
         ipdf2 = 0
 
C  write comments to /POEVT1/
         CALL PHO_REGPAR(25,ii,NPRohd(1),IDPdg1,IDPdg2,X0Hd(1,1),
     &                   X0Hd(1,2),PTHd(1),VHD(1),N0Inhd(1,1),
     &                   N0Inhd(1,2),IPHO_CNV1(NOUthd(1,1)),
     &                   IPHO_CNV1(NOUthd(1,2)),ipos,1)
         CALL PHO_REGPAR(20,IPHO_CNV1(N0Inhd(1,1)),ipdf1,jm1,jm2,
     &                   PPH(1,1),PPH(2,1),PPH(3,1),Q2Sca(1,1),100,
     &                   NBRahd(1,1),ica1,ica2,ipos,1)
         CALL PHO_REGPAR(20,IPHO_CNV1(N0Inhd(1,2)),ipdf2,jm2,jm1,
     &                   PPH(1,2),PPH(2,2),PPH(3,2),Q2Sca(1,2),100,
     &                   NBRahd(1,2),ica1,ica2,ipos,1)
         CALL PHO_REGPAR(21,NOUthd(1,1),ipdf1,jm1,jm2,PPH(5,1),PPH(6,1),
     &                   PPH(7,1),PPH(8,1),100,NBRahd(1,1),ica1,ica2,
     &                   ipos1,1)
         CALL PHO_REGPAR(21,NOUthd(1,2),ipdf2,jm2,jm1,PPH(5,2),PPH(6,2),
     &                   PPH(7,2),PPH(8,2),100,NBRahd(1,2),ica1,ica2,
     &                   ipos2,1)
 
 
C  single resolved: QCD compton scattering
C ------------------------------
         IF ( NPRohd(1).EQ.10 ) THEN
C  register hadron remnant
            CALL PHO_HARREM(jm2,jm1,Igen,-1,ival2,1,icb1,icb2,iused,
     &                      Irej)
            ipdf2 = 1000*IGRp(2) + ISEt(2)
         ELSE IF ( NPRohd(1).EQ.12 ) THEN
C  register hadron remnant
            CALL PHO_HARREM(jm1,jm2,Igen,1,ival1,1,ica1,ica2,iused,Irej)
            ipdf1 = 1000*IGRp(1) + ISEt(1)
 
C  single resolved: photon gluon fusion
C ---------------------------
         ELSE IF ( NPRohd(1).EQ.11 ) THEN
C  register hadron remnant
            CALL PHO_HARREM(jm2,jm1,Igen,-1,ival2,1,icb1,icb2,iused,
     &                      Irej)
            ipdf2 = 1000*IGRp(2) + ISEt(2)
         ELSE IF ( NPRohd(1).EQ.13 ) THEN
C  register hadron remnant
            CALL PHO_HARREM(jm1,jm2,Igen,1,ival1,1,ica1,ica2,iused,Irej)
            ipdf1 = 1000*IGRp(1) + ISEt(1)
 
C  direct process (no remnant)
C ----------------------------
         ELSE IF ( NPRohd(1).EQ.14 ) THEN
 
         END IF
 
C  write final high-pt partons to POEVT1
         IF ( (ISWmdl(8).GE.2) .AND. (NPRohd(1).NE.14) ) THEN
            ici(1,1) = ica1
            ici(1,2) = ica2
            ici(2,1) = icb1
            ici(2,2) = icb2
            i = 1
            ifla(1) = NINhd(i,1)
            ifla(2) = NINhd(i,2)
C  initial state radiation
            DO k = 1 , 2
               DO ipa = IPOisr(k,2,i) , IPOisr(k,1,i) + 1 , -1
                  kk = 1
 5                iflb = IFLisr(k,ipa)
                  IF ( ABS(iflb).LE.6 ) THEN
C  partons
                     IF ( ici(k,1)*ici(k,2).EQ.0 ) THEN
                        IF ( (ici(k,1)+ici(k,2))*iflb.LT.0 ) THEN
                           IF ( ipa-kk.GT.IPOisr(k,1,i) ) THEN
                              CALL PHO_SWAPI(IFLisr(k,ipa),
     &                           IFLisr(k,ipa-kk))
                              kk = kk + 1
                              GOTO 5
                           END IF
                        END IF
                        IF ( iflb.EQ.0 ) THEN
                           CALL PHO_SELCOL(ici(k,1),ici(k,2),ici(k,1),
     &                        ici(k,2),ic1,ic2,2)
                        ELSE
                           CALL PHO_SELCOL(ici(k,1),ici(k,2),ic1,ic2,
     &                        ici(k,1),ici(k,2),2)
                        END IF
                     ELSE IF ( iflb.EQ.0 ) THEN
                        CALL PHO_SELCOL(ici(k,1),ici(k,2),ic1,ic2,
     &                     ici(k,1),ici(k,2),3)
                     ELSE IF ( iflb.GT.0 ) THEN
                        CALL PHO_SELCOL(ici(k,1),ici(k,2),ic1,ic2,
     &                     ici(k,1),ici(k,2),4)
                     ELSE
                        CALL PHO_SELCOL(ici(k,1),ici(k,2),ici(k,1),
     &                     ici(k,2),ic1,ic2,4)
                     END IF
                     iifl = IPHO_CNV1(iflb)
 
                     ifla(k) = ifla(k) - iflb
                     ist = -1
                  ELSE
C  other particle
                     iifl = iflb
                     ic1 = 0
                     ic2 = 0
                     ist = 1
                  END IF
                  CALL PHO_REGPAR(ist,iifl,0,jm1,jm2,PHIsr(k,1,ipa),
     &               PHIsr(k,2,ipa),PHIsr(k,3,ipa),PHIsr(k,4,ipa),
     &               i*100+k,Igen,ic1,ic2,ipos,1)
               END DO
            END DO
            ICOlor(1,ipos1-2) = ici(1,1)
            ICOlor(2,ipos1-2) = ici(1,2)
            ICOlor(1,ipos1-1) = ici(2,1)
            ICOlor(2,ipos1-1) = ici(2,2)
            CALL PHO_HARCOL(NPRohd(i),VHD(i),ifla(1),ici(1,1),ici(1,2),
     &                      ifla(2),ici(2,1),ici(2,2),NOUthd(i,1),
     &                      ici(1,1),ici(1,2),NOUthd(i,2),ici(2,1),
     &                      ici(2,2))
            ICOlor(1,ipos1) = ici(1,1)
            ICOlor(2,ipos1) = ici(1,2)
            ICOlor(1,ipos2) = ici(2,1)
            ICOlor(2,ipos2) = ici(2,2)
            DO k = 1 , 2
               ipa = IPOisr(k,1,i)
               CALL PHO_REGPAR(-1,IPHO_CNV1(IFLisr(k,ipa)),0,jm1,jm2,
     &            PHIsr(k,1,ipa),PHIsr(k,2,ipa),PHIsr(k,3,ipa),
     &            PHIsr(k,4,ipa),-i*100,Igen,ici(k,1),ici(k,2),ipos,1)
            END DO
         ELSE
            ICOlor(1,ipos1-2) = ica1
            ICOlor(2,ipos1-2) = ica2
            ICOlor(1,ipos1-1) = icb1
            ICOlor(2,ipos1-1) = icb2
            CALL PHO_HARCOL(NPRohd(1),VHD(1),NINhd(1,1),ica1,ica2,
     &                      NINhd(1,2),icb1,icb2,NOUthd(1,1),ica1,ica2,
     &                      NOUthd(1,2),icb1,icb2)
            ICOlor(1,ipos1) = ica1
            ICOlor(2,ipos1) = ica2
            ICOlor(1,ipos2) = icb1
            ICOlor(2,ipos2) = icb2
            i = -1
            IF ( ABS(NOUthd(1,1)).GT.12 ) i = 1
            CALL PHO_REGPAR(i,IPHO_CNV1(NOUthd(1,1)),0,jm1,jm2,PPH(5,1),
     &                      PPH(6,1),PPH(7,1),PPH(8,1),-100,Igen,ica1,
     &                      ica2,ipos,1)
            CALL PHO_REGPAR(i,IPHO_CNV1(NOUthd(1,2)),0,jm1,jm2,PPH(5,2),
     &                      PPH(6,2),PPH(7,2),PPH(8,2),-100,Igen,icb1,
     &                      icb2,ipos,1)
         END IF
 
C  assign soft pt to spectators
         IF ( ISWmdl(18).EQ.0 ) THEN
            ipos2 = ipos2 - 1
            CALL PHO_PARTPT(0,nlor1,ipos2,PTCut(ii),Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(26) = IFAil(26) + 1
               GOTO 100
            END IF
 
         END IF
 
C  ----------------- resolved processes -------------------
 
C  single Reggeon exchange
C ----------------------------
      ELSE IF ( (Msreg.EQ.1) .AND. (Mhpom+Mspom.EQ.0) ) THEN
C  flavours
         CALL PHO_REGFLA(jm1,jm2,ifl1,ifl2,Irej)
         IF ( Irej.NE.0 ) THEN
            IFAil(24) = IFAil(24) + 1
            GOTO 100
         END IF
 
C  colors
         CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
         IF ( ((ABS(ifl1).GT.6) .AND. (ifl1.GT.0)) .OR. 
     &        ((ABS(ifl1).LE.6) .AND. (ifl1.LT.0)) )
     &        CALL PHO_SWAPI(ica1,icb1)
         ecmh = ECMp/2.D0
 
C  registration
 
C  DPMJET call with special projectile / target
         IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) .AND. (IPRoce.EQ.1)
     &        ) THEN
            CALL PHO_REGPAR(-1,ifl1,0,jm1,jm2,0.D0,0.D0,ecmh*XPSub,
     &                      ecmh*XPSub,-1,Igen,ica1,0,ipos1,1)
            CALL PHO_REGPAR(-1,ifl2,0,jm1,jm2,0.D0,0.D0,-ecmh*XTSub,
     &                      ecmh*XTSub,-1,Igen,icb1,0,ipos2,1)
C  default treatment
         ELSE
            CALL PHO_REGPAR(-1,ifl1,0,jm1,jm2,0.D0,0.D0,ecmh,ecmh,-1,
     &                      Igen,ica1,0,ipos1,1)
            CALL PHO_REGPAR(-1,ifl2,0,jm1,jm2,0.D0,0.D0,-ecmh,ecmh,-1,
     &                      Igen,icb1,0,ipos2,1)
         END IF
 
C  soft pt assignment
         IF ( ISWmdl(18).EQ.0 ) THEN
            CALL PHO_PARTPT(0,ipos1,ipos2,PTCut(ii),Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(25) = IFAil(25) + 1
               GOTO 100
            END IF
         END IF
C
C  multi Reggeon / Pomeron exchange
C----------------------------------------
      ELSE
C  parton configuration
 
         CALL PHO_POMSCA(ii,Mspom,Mhpom,Msreg,ival1,ival2,mspar1,mspar2,
     &                   mhpar1,mhpar2,Irej)
 
         IF ( Irej.EQ.50 ) RETURN
         IF ( Irej.NE.0 ) GOTO 100
 
C  register particles
         IF ( LPRi.GT.4 .AND. IDEb(23).GE.15 )
     &         WRITE (LO,'(1X,A,/15X,7I5)')
     &         'PHO_STDPAR: MSPOM,MHPOM,MSREG,MSPAR1/2,IVAL1/2' , 
     &        Mspom , Mhpom , Msreg , mspar1 , mspar2 , ival1 , ival2
 
C  register soft partons
         IF ( ival1.NE.0 ) THEN
            IF ( ival1.LT.0 ) THEN
               ind1 = 3
               ival1 = -ival1
            ELSE
               ind1 = 2
            END IF
         ELSE IF ( Mspom.EQ.0 ) THEN
            ind1 = 4
         ELSE
            ind1 = 1
         END IF
         IF ( ival2.NE.0 ) THEN
            IF ( ival2.LT.0 ) THEN
               ind2 = 3
               ival2 = -ival2
            ELSE
               ind2 = 2
            END IF
         ELSE IF ( Mspom.EQ.0 ) THEN
            ind2 = 4
         ELSE
            ind2 = 1
         END IF
 
         IF ( LPRi.GT.4 .AND. IDEb(23).GE.20 )
     &         WRITE (LO,'(1X,A,2I3,2X,2I3)')
     &         'PHO_STDPAR: IND1/2,IVAL1/2' , ind1 , ind2 , ival1 , 
     &        ival2
 
C  soft Pomeron final states
C -----------------------------------
         k = Mspom + Mhpom + Msreg
         DO i = 1 , Mspom
 
            CALL PHO_POSPOM(ii,ind1,ind2,Igen,i,k,iswap,Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(8) = IFAil(8) + 1
               GOTO 100
            END IF
C
         END DO
 
C  soft Reggeon final states
C -----------------------------------------
         DO i = 1 , Msreg
C  flavours
            cmass1 = MIN(PSOft1(4,ind1),PSOft2(4,ind2))
            IF ( DT_RNDM(cmass1).LT.0.5D0 ) THEN
               CALL PHO_SEAFLA(jm1,ifla1,iflb1,cmass1)
            ELSE
               CALL PHO_SEAFLA(jm2,ifla1,iflb1,cmass1)
            END IF
 
C  colors
            CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
            IF ( ((ABS(ifla1).GT.6) .AND. (ifla1.GT.0)) .OR. 
     &           ((ABS(ifla1).LE.6) .AND. (ifla1.LT.0)) )
     &           CALL PHO_SWAPI(ica1,icb1)
C  registration
            CALL PHO_REGPAR(-1,ifla1,0,jm1,jm2,PSOft1(1,ind1),
     &                      PSOft1(2,ind1),PSOft1(3,ind1),PSOft1(4,ind1)
     &                      ,i,Igen,ica1,ica2,ipos1,1)
            ind1 = ind1 + 1
            CALL PHO_REGPAR(-1,iflb1,0,jm2,jm1,PSOft2(1,ind2),
     &                      PSOft2(2,ind2),PSOft2(3,ind2),PSOft2(4,ind2)
     &                      ,i,Igen,icb1,icb2,ipos2,1)
            ind2 = ind2 + 1
 
            IF ( LPRi.GT.4 .AND. IDEb(23).GE.20 )
     &            WRITE (LO,'(1X,A,/15X,6I4)')
     &            'PHO_STDPAR: reg.cut: IND1,IND2,IFLA,IFLB,IPOS1,IPOS2'
     &           , ind1 - 1 , ind2 - 1 , ifla1 , iflb1 , ipos1 , ipos2
 
C  soft pt assignment
            IF ( ISWmdl(18).EQ.0 ) THEN
               CALL PHO_PARTPT(0,ipos1,ipos2,PTCut(ii),Irej)
               IF ( Irej.NE.0 ) THEN
                  IFAil(25) = IFAil(25) + 1
                  GOTO 100
               END IF
            END IF
 
         END DO
 
C  hard Pomeron final states
C ------------------------------------
         ind1 = mspar1
         ind2 = mspar2
         ica1 = 0
         ica2 = 0
         icb1 = 0
         icb2 = 0
 
         DO l = 1 , Mhpom
            i = LSIdx(l)
 
            ifli1 = IPHO_CNV1(N0Inhd(i,1))
            ifli2 = IPHO_CNV1(N0Inhd(i,2))
            iflo1 = IPHO_CNV1(NOUthd(i,1))
            iflo2 = IPHO_CNV1(NOUthd(i,2))
 
C  write comments to /POEVT1/
            CALL PHO_REGPAR(25,ii,NPRohd(i),IDPdg1,IDPdg2,X0Hd(i,1),
     &                      X0Hd(i,2),PTHd(i),VHD(i),N0Inhd(i,1),
     &                      N0Inhd(i,2),iflo1,iflo2,ipos,1)
            i1 = 8*i - 7
            ipdf = 1000*IGRp(1) + ISEt(1)
            CALL PHO_REGPAR(20,ifli1,ipdf,jm1,jm2,PPH(i1,1),PPH(i1+1,1),
     &                      PPH(i1+2,1),Q2Sca(i,1),l*100,NBRahd(i,1),
     &                      ica1,ica2,ipos,1)
            ipdf = 1000*IGRp(2) + ISEt(2)
            CALL PHO_REGPAR(20,ifli2,ipdf,jm2,jm1,PPH(i1,2),PPH(i1+1,2),
     &                      PPH(i1+2,2),Q2Sca(i,2),l*100,NBRahd(i,2),
     &                      icb1,icb2,ipos,1)
            i1 = 8*i - 3
            ipdf = 1000*IGRp(1) + ISEt(1)
            CALL PHO_REGPAR(21,iflo1,ipdf,jm1,jm2,PPH(i1,1),PPH(i1+1,1),
     &                      PPH(i1+2,1),PPH(i,1),l*100,NBRahd(i,1),ica1,
     &                      ica2,ipos1,1)
            ipdf = 1000*IGRp(2) + ISEt(2)
            CALL PHO_REGPAR(21,iflo2,ipdf,jm2,jm1,PPH(i1,2),PPH(i1+1,2),
     &                      PPH(i1+2,2),PPH(i,2),l*100,NBRahd(i,2),icb1,
     &                      icb2,ipos2,1)
 
C  spectator partons belonging to hard interaction
            IF ( ival1.EQ.i ) THEN
               ivq = 1
               ind = 1
            ELSE IF ( (Mspom.EQ.0) .AND. (l.EQ.1) .AND. (ival1.EQ.0) )
     &                THEN
               ivq = 0
               ind = 1
            ELSE
               ivq = -1
               ind = ind1
            END IF
            CALL PHO_HARREM(jm1,jm2,Igen,l,ivq,ind,ica1,ica2,iused,Irej)
            IF ( ivq.LT.0 ) ind1 = ind1 - iused
            IF ( ival2.EQ.i ) THEN
               ivq = 1
               ind = 1
            ELSE IF ( (Mspom.EQ.0) .AND. (l.EQ.1) .AND. (ival2.EQ.0) )
     &                THEN
               ivq = 0
               ind = 1
            ELSE
               ivq = -1
               ind = ind2
            END IF
            CALL PHO_HARREM(jm2,jm1,Igen,-l,ivq,ind,icb1,icb2,iused,
     &                      Irej)
            IF ( ivq.LT.0 ) ind2 = ind2 - iused
C
C  register hard scattered partons
            IF ( (ISWmdl(8).GE.2) .AND. 
     &           ((IPAmdl(101).NE.1) .OR. (l.EQ.1)) ) THEN
               ici(1,1) = ica1
               ici(1,2) = ica2
               ici(2,1) = icb1
               ici(2,2) = icb2
               ifla(1) = NINhd(i,1)
               ifla(2) = NINhd(i,2)
C  initial state radiation
               DO k = 1 , 2
                  DO ipa = IPOisr(k,2,i) , IPOisr(k,1,i) + 1 , -1
                     kk = 1
 6                   iflb = IFLisr(k,ipa)
                     IF ( ABS(iflb).LE.6 ) THEN
C  partons
                        IF ( ici(k,1)*ici(k,2).EQ.0 ) THEN
                           IF ( (ici(k,1)+ici(k,2))*iflb.LT.0 ) THEN
                              IF ( ipa-kk.GT.IPOisr(k,1,i) ) THEN
                               CALL PHO_SWAPI(IFLisr(k,ipa),
     &                            IFLisr(k,ipa-kk))
                               kk = kk + 1
                               GOTO 6
                              END IF
                           END IF
                           IF ( iflb.EQ.0 ) THEN
                              CALL PHO_SELCOL(ici(k,1),ici(k,2),ici(k,1)
     &                           ,ici(k,2),ic1,ic2,2)
                           ELSE
                              CALL PHO_SELCOL(ici(k,1),ici(k,2),ic1,ic2,
     &                           ici(k,1),ici(k,2),2)
                           END IF
                        ELSE IF ( iflb.EQ.0 ) THEN
                           CALL PHO_SELCOL(ici(k,1),ici(k,2),ic1,ic2,
     &                        ici(k,1),ici(k,2),3)
                        ELSE IF ( iflb.GT.0 ) THEN
                           CALL PHO_SELCOL(ici(k,1),ici(k,2),ic1,ic2,
     &                        ici(k,1),ici(k,2),4)
                        ELSE
                           CALL PHO_SELCOL(ici(k,1),ici(k,2),ici(k,1),
     &                        ici(k,2),ic1,ic2,4)
                        END IF
                        iifl = IPHO_CNV1(iflb)
 
                        ifla(k) = ifla(k) - iflb
                        ist = -1
                     ELSE
C  other particles
                        iifl = iflb
                        ic1 = 0
                        ic2 = 0
                        ist = 1
                     END IF
                     CALL PHO_REGPAR(ist,iifl,0,jm1,jm2,PHIsr(k,1,ipa),
     &                  PHIsr(k,2,ipa),PHIsr(k,3,ipa),PHIsr(k,4,ipa),
     &                  l*100+k,Igen,ic1,ic2,ipos,1)
                  END DO
               END DO
               ICOlor(1,ipos1-2) = ici(1,1)
               ICOlor(2,ipos1-2) = ici(1,2)
               ICOlor(1,ipos1-1) = ici(2,1)
               ICOlor(2,ipos1-1) = ici(2,2)
               CALL PHO_HARCOL(NPRohd(i),VHD(i),ifla(1),ici(1,1),
     &            ici(1,2),ifla(2),ici(2,1),ici(2,2),NOUthd(i,1),
     &            ici(1,1),ici(1,2),NOUthd(i,2),ici(2,1),ici(2,2))
               ICOlor(1,ipos1) = ici(1,1)
               ICOlor(2,ipos1) = ici(1,2)
               ICOlor(1,ipos2) = ici(2,1)
               ICOlor(2,ipos2) = ici(2,2)
               DO k = 1 , 2
                  ipa = IPOisr(k,1,i)
                  CALL PHO_REGPAR(-1,IPHO_CNV1(IFLisr(k,ipa)),0,jm1,jm2,
     &               PHIsr(k,1,ipa),PHIsr(k,2,ipa),PHIsr(k,3,ipa),
     &               PHIsr(k,4,ipa),-l*100,Igen,ici(k,1),ici(k,2),ipos,
     &               1)
               END DO
            ELSE
               ICOlor(1,ipos1-2) = ica1
               ICOlor(2,ipos1-2) = ica2
               ICOlor(1,ipos1-1) = icb1
               ICOlor(2,ipos1-1) = icb2
               CALL PHO_HARCOL(NPRohd(i),VHD(i),NINhd(i,1),ica1,ica2,
     &            NINhd(i,2),icb1,icb2,NOUthd(i,1),ica1,ica2,NOUthd(i,2)
     &            ,icb1,icb2)
               ICOlor(1,ipos1) = ica1
               ICOlor(2,ipos1) = ica2
               ICOlor(1,ipos2) = icb1
               ICOlor(2,ipos2) = icb2
               i1 = 8*i - 3
               CALL PHO_REGPAR(-1,IPHO_CNV1(NOUthd(i,1)),0,jm1,jm2,
     &            PPH(i1,1),PPH(i1+1,1),PPH(i1+2,1),PPH(i1+3,1),-l*100,
     &            Igen,ica1,ica2,ipos,1)
               CALL PHO_REGPAR(-1,IPHO_CNV1(NOUthd(i,2)),0,jm1,jm2,
     &            PPH(i1,2),PPH(i1+1,2),PPH(i1+2,2),PPH(i1+3,2),-l*100,
     &            Igen,icb1,icb2,ipos,1)
            END IF
         END DO
C  end of resolved parton registration
      END IF
 
      IF ( Mhdir+Mhpom.GT.0 ) THEN
 
         IF ( ISWmdl(29).GE.1 ) THEN
C  primordial kt of hard scattering
            CALL PHO_PRIMKT(1,nlor1,NHEp,PTCut(ii),Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(27) = IFAil(27) + 1
               GOTO 100
            END IF
         ELSE IF ( ISWmdl(24).GE.0 ) THEN
C  give "soft" pt only to soft (spectator) partons in hard processes
            CALL PHO_PARTPT(1,nlor1,NHEp,PTCut(ii),Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(26) = IFAil(26) + 1
               GOTO 100
            END IF
         END IF
 
      END IF
 
C  give "soft" pt to partons in soft Pomerons
      IF ( (Mhdir.EQ.0) .AND. (ISWmdl(18).EQ.1) ) THEN
         CALL PHO_PARTPT(0,nlor1,NHEp,PTCut(ii),Irej)
         IF ( Irej.NE.0 ) THEN
            IFAil(25) = IFAil(25) + 1
            GOTO 100
         END IF
      END IF
 
C  boost back to lab frame
      CALL PHO_LTRHEP(nlor1,NHEp,CODp,SIDp,COFp,SIFp,GAMbep(4),GAMbep(1)
     &                ,GAMbep(2),GAMbep(3))
      RETURN
 
C  rejection treatment
 100  IFAil(2) = IFAil(2) + 1
C  reset counters
      KSPom = kspoms
      KHPom = khpoms
      KHDir = khdirs
      KSReg = ksregs
C  reset mother-daugther relations
      JDAhep(1,jm1) = 0
      JDAhep(2,jm1) = 0
      JDAhep(1,jm2) = 0
      JDAhep(2,jm2) = 0
      ISThep(jm1) = 1
      ISThep(jm2) = 1
      IPOix1 = ipois1
      IPOix2 = ipois2
      NHEp = nheps
C  debug
      IF ( LPRi.GT.4 .AND. IDEb(23).GT.2 ) WRITE (LO,'(/1X,A,4I6)')
     &      'PHO_STDPAR: rejection (MSPOM,MHPOM,MSREG,MHDIR)' , Mspom , 
     &     Mhpom , Msreg , Mhdir
 
      END SUBROUTINE
