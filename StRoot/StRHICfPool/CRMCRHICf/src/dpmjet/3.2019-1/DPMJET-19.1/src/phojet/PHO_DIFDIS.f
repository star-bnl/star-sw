
      SUBROUTINE PHO_DIFDIS(Idif1,Idif2,Imoth1,Imoth2,Sprob,Imode,Msoft,
     &                      Mhard,Irej)
C***********************************************************************
C
C     sampling of diffractive events of different kinds,
C                            (produced particles stored in /POEVT1/)
C
C     input:   IDIF1/2   diffractive process particle 1/2
C                          0   elastic/quasi-elastic scattering
C                          1   diffraction dissociation
C              IMOTH1/2  index of mother particles in /POEVT1/
C              SPROB     suppression factor (survival probability) for
C                        resolved diffraction dissociation
C              IMODE     mode of operation
C                          0  sampling of diffractive cut
C                          1  sampling of enhanced cut
C                          2  sampling of diffractive cut without
C                             scattering (needed for double-pomeron)
C                         -1  initialization
C                         -2  output of statistics
C
C     output:   MSOFT    number of generated soft strings
C               MHARD    number of generated hard strings
C               IDIF1/2  diffraction label for particle 1/2 in /PROCES/
C                          0   quasi elastic scattering
C                          1   low-mass diffractive dissociation
C                          2   soft high-mass diffractive dissociation
C                          3   hard resolved diffractive dissociation
C                          4   hard direct diffractive dissociation
C               IREJ     rejection label
C                          0  successful generation of partons
C                          1  failure
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION am12 , am22 , anorf , DEPS , DT_RNDM , EPS , 
     &                 PHO_DFMASS , PHO_PMASS , PHO_XLAM , pimass , 
     &                 pref2 , ptot1 , slwght , Sprob , ss , thrm1 , 
     &                 thrm2 , tt , xmmin , xmnew
      INTEGER i , i1 , i2 , icpos , idbam , Idif1 , Idif2 , idir , 
     &        idpdg , ifl1p , ifl2p , igen , Imode , Imoth1 , Imoth2 , 
     &        ip , ipar , ipars1 , ipars2 , ipois1
      INTEGER ipois2 , ipois3 , ipos , iposp , iproc , irbam , Irej , 
     &        irpdg , isam , isp , isth1 , isth2 , itry , itrym , ivec , 
     &        jda11 , jda12 , jda21 , jda22 , jsam
      INTEGER k , khdirs , khpoms , ksam , kspoms , ksregs , Mhard , 
     &        Msoft , ncor , nheps , nslp
      SAVE 
 
      PARAMETER (EPS=1.D-7,DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general process information
      INCLUDE 'inc/poprcs'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  c.m. kinematics of diffraction
      INCLUDE 'inc/podcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  some constants
      INCLUDE 'inc/pocons'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      DOUBLE PRECISION p1 , p2 , xmass , amp , pp , pd1 , pd2
      DIMENSION p1(5) , p2(5) , xmass(2) , amp(2) , pp(7,2) , pd1(7) , 
     &          pd2(7)
      DIMENSION idpdg(2) , idbam(2) , ipar(2) , iposp(2,2) , irpdg(2) , 
     &          ivec(2) , irbam(2) , ifl1p(2) , ifl2p(2) , isam(2) , 
     &          jsam(2) , ksam(2) , idir(2) , iproc(2)
 
      IF ( Imode.EQ.-1 ) THEN
C  initialization
         RETURN
      ELSE IF ( Imode.EQ.-2 ) THEN
C  output of statistics
         RETURN
      END IF
 
      Irej = 0
C  mass cuts
      pimass = 0.140D0
C  debug output
      IF ( IDEb(45).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/16X,7I6)')
     &         'PHO_DIFDIS: (1) ' , 
     &        'IDIF1,IDIF2,IMOTH1,IMOTH2,MSOFT,MHARD,IMODE' , Idif1 , 
     &        Idif2 , Imoth1 , Imoth2 , Msoft , Mhard , Imode
      END IF
      ipar(1) = Idif1
      ipar(2) = Idif2
C  save current status
      Msoft = 0
      Mhard = 0
      khpoms = KHPom
      kspoms = KSPom
      ksregs = KSReg
      khdirs = KHDir
      ipois1 = IPOix1
      ipois2 = IPOix2
      ipois3 = IPOix3
      jda11 = JDAhep(1,Imoth1)
      jda21 = JDAhep(2,Imoth1)
      jda12 = JDAhep(1,Imoth2)
      jda22 = JDAhep(2,Imoth2)
      isth1 = ISThep(Imoth1)
      isth2 = ISThep(Imoth2)
      nheps = NHEp
C  get mother data
      NPOsd(1) = Imoth1
      NPOsd(2) = Imoth2
      DO i = 1 , 2
         idpdg(i) = IDHep(NPOsd(i))
         idbam(i) = IMPart(NPOsd(i))
         iposp(1,i) = ICOlor(1,NPOsd(i))
         iposp(2,i) = ICOlor(2,NPOsd(i))
         amp(i) = PHO_PMASS(idbam(i),0)
         IF ( idpdg(i).EQ.22 ) THEN
            PMAssd(i) = 0.765D0
            PVIrtd(i) = PHEp(5,NPOsd(i))**2
         ELSE
            PMAssd(i) = PHO_PMASS(idbam(i),0)
            PVIrtd(i) = 0.D0
         END IF
      END DO
C  get CM system
      p1(1) = PHEp(1,Imoth1) + PHEp(1,Imoth2)
      p1(2) = PHEp(2,Imoth1) + PHEp(2,Imoth2)
      p1(3) = PHEp(3,Imoth1) + PHEp(3,Imoth2)
      p1(4) = PHEp(4,Imoth1) + PHEp(4,Imoth2)
      ss = (p1(4)+p1(3))*(p1(4)-p1(3)) - p1(1)**2 - p1(2)**2
      ECMd = SQRT(ss)
      IF ( LPRi.GT.4 .AND. IDEb(45).GE.15 ) WRITE (LO,'(1X,A,E12.4)')
     &      'PHO_DIFDIS: availabe energy' , ECMd
C  check total available energy
      IF ( (amp(1)+amp(2)+0.1D0).GE.ECMd ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(45).GE.2 )
     &         WRITE (LO,'(1X,2A,/5X,A,1P,3E11.3)') 'PHO_DIFDIS: ' , 
     &        'not enough energy for inelastic diffraction' , 
     &        'ECM, particle masses:' , ECMd , amp
         IFAil(7) = IFAil(7) + 1
         Irej = 1
         RETURN
      END IF
C  boost into CMS
      DO i = 1 , 4
         GAMbed(i) = p1(i)/ECMd
      END DO
      CALL PHO_ALTRA(GAMbed(4),-GAMbed(1),-GAMbed(2),-GAMbed(3),
     &               PHEp(1,Imoth1),PHEp(2,Imoth1),PHEp(3,Imoth1),
     &               PHEp(4,Imoth1),ptot1,p1(1),p1(2),p1(3),p1(4))
C  rotation angles
      CODd = p1(3)/ptot1
      SIDd = SQRT(p1(1)**2+p1(2)**2)/ptot1
      COFd = 1.D0
      SIFd = 0.D0
      IF ( ptot1*SIDd.GT.1.D-5 ) THEN
         COFd = p1(1)/(SIDd*ptot1)
         SIFd = p1(2)/(SIDd*ptot1)
         anorf = SQRT(COFd*COFd+SIFd*SIFd)
         COFd = COFd/anorf
         SIFd = SIFd/anorf
      END IF
C  initial particles in CMS
      PDCms(1,1) = 0.D0
      PDCms(2,1) = 0.D0
      PDCms(3,1) = ptot1
      PDCms(4,1) = p1(4)
      PDCms(1,2) = 0.D0
      PDCms(2,2) = 0.D0
      PDCms(3,2) = -ptot1
      PDCms(4,2) = ECMd - p1(4)
C  get new CM momentum
      am12 = PMAssd(1)**2
      am22 = PMAssd(2)**2
      PCMd = PHO_XLAM(ss,am12,am22)/(2.D0*ECMd)
 
C  coherence constraint (min/max diffractive mass allowed)
      IF ( Imode.EQ.2 ) THEN
         thrm1 = PARmdl(71)/SQRT(1-PARmdl(72))
         thrm1 = MAX(thrm1,PARmdl(70)*PARmdl(71))
         thrm2 = SQRT(1-PARmdl(72))*ECMd
         thrm2 = MIN(thrm2,ECMd/PARmdl(70))
      ELSE
         thrm1 = PARmdl(46)
         thrm2 = PARmdl(45)*ECMd
C  check kinematic limits
         IF ( thrm2.LE.(4.D0*PARmdl(162)) ) ipar(1) = MIN(ipar(1),1)
         IF ( thrm2.LE.(4.D0*PARmdl(163)) ) ipar(2) = MIN(ipar(2),1)
      END IF
 
C  check energy vs. coherence constraints
      IF ( MAX(PARmdl(162),PMAssd(1)+thrm1).GE.thrm2 ) ipar(1) = 0
      IF ( MAX(PARmdl(163),PMAssd(2)+thrm1).GE.thrm2 ) ipar(2) = 0
 
C  no phase space available
      IF ( ipar(1)+ipar(2).EQ.0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(45).GE.2 )
     &         WRITE (LO,'(1X,2A,1P,E11.3,2(/5X,A,2E11.3))')
     &         'PHO_DIFDIS: ' , 
     &        'not enough phase space for ine. diffraction (Ecm)' , 
     &        ECMd , 'side 1: min. mass, upper mass limit:' , 
     &        MAX(PARmdl(162),PMAssd(1)+thrm1) , thrm2 , 
     &        'side 2: min. mass, upper mass limit:' , 
     &        MAX(PARmdl(163),PMAssd(2)+thrm1) , thrm2
         IFAil(7) = IFAil(7) + 1
         Irej = 1
         RETURN
      END IF
 
      itry = 0
      itrym = 10
      ipars1 = ipar(1)
      ipars2 = ipar(2)
 
C  main rejection loop
C -------------------------------
 100  itry = itry + 1
      IF ( itry.GT.1 ) THEN
         IFAil(13) = IFAil(13) + 1
         IF ( itry.GE.itrym ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(45).GE.2 )
     &            WRITE (LO,'(1X,A,I10,2I3)')
     &            'PHO_DIFDIS: rejection (KEVE,IPAR1/2) ' , KEVent , 
     &           ipar
            IFAil(7) = IFAil(7) + 1
            Irej = 1
            RETURN
         END IF
      END IF
      KSPom = kspoms
      KHPom = khpoms
      KHDir = khdirs
      KSReg = ksregs
      ipar(1) = ipars1
      ipar(2) = ipars2
C  reset mother-daugther relations
      NHEp = nheps
      JDAhep(1,Imoth1) = jda11
      JDAhep(2,Imoth1) = jda21
      JDAhep(1,Imoth2) = jda12
      JDAhep(2,Imoth2) = jda22
      ISThep(Imoth1) = isth1
      ISThep(Imoth2) = isth2
      IPOix1 = ipois1
      IPOix2 = ipois2
      IPOix3 = ipois3
C
      nslp = 0
      ncor = 0
 
C  calculation of kinematics
 200  DO i = 1 , 2
C  sampling of masses
         irpdg(i) = 0
         irbam(i) = 0
         ifl1p(i) = idpdg(i)
         ifl2p(i) = idbam(i)
         ivec(i) = 0
         idir(i) = 0
         isam(i) = 0
         jsam(i) = 0
         ksam(i) = 0
         IF ( ipar(i).EQ.0 ) THEN
C  vector meson dominance assumed
            xmass(i) = amp(i)
            CALL PHO_VECRES(ivec(i),xmass(i),ifl1p(i),ifl2p(i))
C  diffraction dissociation
         ELSE IF ( ipar(i).EQ.1 ) THEN
            xmmin = MAX(PARmdl(161+i),PMAssd(i)+thrm1)
            pref2 = PMAssd(i)**2
            xmass(i) = PHO_DFMASS(xmmin,thrm2,pref2,PVIrtd(i),ISWmdl(22)
     &                 )
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I3)')
     &            'PHO_DIFDIS:ERROR:invalid IPAR1,IPAR2' , ipar(1) , 
     &           ipar(2)
            CALL PHO_ABORT
         END IF
      END DO
 
C  sampling of momentum transfer
      CALL PHO_DIFSLP(ipar(1),ipar(2),ivec(1),ivec(2),xmass(1),xmass(2),
     &                thrm2,tt,slwght,Irej)
      IF ( Irej.NE.0 ) THEN
         nslp = nslp + 1
         IF ( nslp.LT.100 ) GOTO 200
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/10X,2I3,2E12.3)')
     &         'PHO_DIFDIS: ' , 
     &        'too many slope rejections:IPAR1,IPAR2,M1,M2' , ipar , 
     &        xmass
         Irej = 5
         RETURN
      END IF
 
C  correct for t-M^2 correlation in diffraction
      IF ( DT_RNDM(tt).GT.slwght ) THEN
         ncor = ncor + 1
         IF ( ncor.LT.100 ) GOTO 200
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I10)') 'PHO_DIFDIS: ' , 
     &        'too many rejections due to t-M**2 correlation (EVE)' , 
     &        KEVent
         Irej = 5
         RETURN
      END IF
 
C  debug output
      IF ( IDEb(45).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/5X,2I3,3E12.3)')
     &         'PHO_DIFDIS: IPAR1,IPAR2,XMASS1,XMASS2,TT' , ipar , 
     &        xmass , tt
      END IF
C  not double pomeron scattering
      IF ( Imode.NE.2 ) THEN
C  sample diffractive interaction processes
         DO i = 1 , 2
            IF ( ipar(i).NE.0 ) THEN
C  find particle combination
               IF ( idpdg(i).EQ.IFPap(1) ) THEN
                  ip = 2
               ELSE IF ( idpdg(i).EQ.IFPap(2) ) THEN
                  ip = 3
               ELSE IF ( idpdg(i).EQ.990 ) THEN
                  ip = 4
               ELSE
                  ip = i + 1
               END IF
C  sample dissociation process
               CALL PHO_DIFPRO(ip,ISWmdl(16),idpdg(i),990,xmass(i),
     &            PVIrtd(i),0.D0,Sprob,iproc(i),isam(i),jsam(i),ksam(i),
     &            idir(i))
               IF ( (iproc(i).NE.1) .AND. (iproc(i).NE.8) ) THEN
C  diffractive pomeron-hadron interaction
                  ipar(i) = 10 + iproc(i)
C  store process label
               ELSE IF ( idir(i).GT.0 ) THEN
                  ipar(i) = 4
               ELSE IF ( ksam(i).GT.0 ) THEN
                  ipar(i) = 3
               ELSE IF ( isam(i).GT.0 ) THEN
                  ipar(i) = 2
               ELSE
                  ipar(i) = 1
C  mass fine correction
                  CALL PHO_MASSAD(idpdg(i),ifl1p(i),ifl2p(i),PMAssd(i),
     &               xmass(i),xmnew,irpdg(i),irbam(i))
                  xmass(i) = xmnew
               END IF
C  debug output
               IF ( LPRi.GT.4 .AND. IDEb(45).GE.15 )
     &               WRITE (LO,'(1X,A,/10X,I3,E12.4,5I3)')
     &               'PHO_DIFDIS: IP,XMASS,IPROC,ISAM,JSAM,KSAM,IDIR' , 
     &              ip , xmass(i) , iproc(i) , isam(i) , jsam(i) , 
     &              ksam(i) , idir(i)
            END IF
         END DO
      END IF
C  actualize debug information
      IF ( Imode.EQ.1 ) THEN
         IDIfr1 = ipar(1)
         IDIfr2 = ipar(2)
      END IF
C  calculate new momenta in CMS
      CALL PHO_DIFKIN(xmass(1),xmass(2),tt,p1,p2,Irej)
      IF ( Irej.NE.0 ) GOTO 100
      DO i = 1 , 4
         pp(i,1) = p1(i)
         pp(i,2) = p2(i)
      END DO
 
C  comment line for diffraction
      icpos = 0
      CALL PHO_REGPAR(30,IPRoce,Imode,NPOsd(1),NPOsd(2),xmass(1),
     &                xmass(2),tt,ECMd,ipar(1),ipar(2),idpdg(1),idpdg(2)
     &                ,icpos,1)
C  write diffractive strings/particles
      DO i = 1 , 2
         i1 = i
         i2 = 3 - i1
         DO k = 1 , 4
            pd1(k) = pp(k,i1)
            pd2(k) = pp(k,i2)
         END DO
         pp(6,i1) = SIGN(PHEp(5,NPOsd(i1))**2,PHEp(5,NPOsd(i1)))
         pp(7,i1) = tt
         igen = IPHist(2,NPOsd(i1))
         IF ( igen.EQ.0 ) igen = -i1*10
         CALL PHO_DIFPAR(NPOsd(i1),NPOsd(i2),igen,ifl1p(i1),ifl2p(i1),
     &                   ipar(i1),pd1,pd2,iposp(1,i1),iposp(2,i1),Imode,
     &                   Irej)
         IF ( Irej.NE.0 ) THEN
            IFAil(7+i) = IFAil(7+i) + 1
            IF ( LPRi.GT.4 .AND. IDEb(45).GE.3 )
     &            WRITE (LO,'(1X,A,2I3,E11.3)')
     &            'PHO_DIFDIS: rejection by PHO_DIFPAR (I,IPAR,XM)' , 
     &           i , ipar(i) , xmass(i)
            GOTO 100
         END IF
         ICOlor(i1,icpos) = iposp(1,i1)
      END DO
C  double-pomeron scattering?
      IF ( Imode.NE.2 ) THEN
 
C  diffractive final states
         DO i = 1 , 2
 220        IF ( ipar(i).NE.0 ) THEN
               IF ( (iproc(i).EQ.1) .OR. (iproc(i).EQ.8) ) THEN
                  IF ( isam(i)+jsam(i)+ksam(i).EQ.0 ) jsam(i) = 1
                  IF ( idir(i).GT.0 ) THEN
                     ipar(i) = 4
                  ELSE IF ( ksam(i).GT.0 ) THEN
                     ipar(i) = 3
                  ELSE IF ( isam(i).GT.0 ) THEN
                     ipar(i) = 2
                  ELSE
                     ipar(i) = 1
                  END IF
               ELSE
                  ipar(i) = 10 + iproc(i)
               END IF
               IPHist(i,icpos) = ipar(i)
C  update debug informantion
               KSPom = isam(i)
               KSReg = jsam(i)
               KHPom = ksam(i)
               KHDir = idir(i)
               IDIfr1 = ipar(1)
               IDIfr2 = ipar(2)
               IF ( (irpdg(i).NE.0) .AND. (ISWmdl(23).GT.0) ) THEN
 
C  resonance decay, pi+pi- background
                  p1(1) = PHEp(1,iposp(1,i)) + PHEp(1,iposp(2,i))
                  p1(2) = PHEp(2,iposp(1,i)) + PHEp(2,iposp(2,i))
                  p1(3) = PHEp(3,iposp(1,i)) + PHEp(3,iposp(2,i))
                  p1(4) = PHEp(4,iposp(1,i)) + PHEp(4,iposp(2,i))
                  CALL PHO_REGPAR(1,irpdg(i),irbam(i),iposp(1,i),
     &               iposp(2,i),p1(1),p1(2),p1(3),p1(4),0,2,0,0,ipos,1)
C  decay
                  IF ( idpdg(i).EQ.22 ) THEN
                     IPHist(2,ipos) = 3
                     IF ( ISWmdl(21).GE.0 ) THEN
                        isp = IPAmdl(3)
                        IF ( ISWmdl(21).GE.2 ) isp = IPAmdl(4)
                        CALL PHO_SDECAY(ipos,isp,2)
                     END IF
                  ELSE
                     CALL PHO_SDECAY(ipos,IPAmdl(3),2)
                  END IF
                  Irej = 0
 
C  particle-pomeron scattering
               ELSE IF ( ipar(i).LE.4 ) THEN
C  non-diffractive particle-pomeron scattering
                  igen = IPHist(2,NPOsd(i))
                  IF ( igen.EQ.0 ) THEN
                     IF ( i.EQ.1 ) THEN
                        igen = 5
                     ELSE
                        igen = 6
                     END IF
                  END IF
                  CALL PHO_STDPAR(iposp(1,i),iposp(2,i),igen,isam(i),
     &               jsam(i),ksam(i),idir(i),Irej)
               ELSE
C  diffractive particle-pomeron scattering
                  IPOix2 = IPOix2 + 1
                  IPOres(IPOix2) = iproc(i)
                  IPOpos(1,IPOix2) = iposp(1,i)
                  IPOpos(2,IPOix2) = iposp(2,i)
               END IF
C  vector meson production
            ELSE IF ( idpdg(i).EQ.22 ) THEN
               IF ( ISWmdl(21).GE.0 ) THEN
                  isp = IPAmdl(3)
                  IF ( ISWmdl(21).GE.1 ) isp = IPAmdl(4)
                  CALL PHO_SDECAY(iposp(1,i),isp,2)
               END IF
C  hadronic state of multi-pomeron coupling
            ELSE IF ( idpdg(i).EQ.990 ) THEN
               CALL PHO_SDECAY(iposp(1,i),0,2)
            END IF
 
C  rejection?
            IF ( Irej.NE.0 ) THEN
               IFAil(20+i) = IFAil(20+i) + 1
               IF ( ipar(i).GT.1 ) THEN
                  IF ( ipar(i).EQ.3 ) IFAil(7+2*i) = IFAil(7+2*i) + 1
                  IF ( ipar(i).EQ.4 ) IFAil(8+2*i) = IFAil(8+2*i) + 1
                  IF ( idir(i).GT.0 ) THEN
                     idir(i) = 0
                  ELSE IF ( ksam(i).GT.0 ) THEN
                     ksam(i) = ksam(i) - 1
                  ELSE IF ( isam(i).GT.0 ) THEN
                     isam(i) = isam(i) - 1
                  END IF
                  GOTO 220
               ELSE
                  IF ( LPRi.GT.4 .AND. IDEb(45).GE.2 )
     &                  WRITE (LO,'(1X,A,2I3,E11.3)')
     &                  'PHO_DIFDIS: rejection PHO_STDPAR (I,IPAR,XM)' , 
     &                 i , ipar(i) , xmass(i)
                  GOTO 100
               END IF
            END IF
         END DO
 
         Idif1 = ipar(1)
         Idif2 = ipar(2)
C  update debug information
         KSPom = kspoms + isam(1) + isam(2)
         KSReg = ksregs + jsam(1) + jsam(2)
         KHPom = khpoms + ksam(1) + ksam(2)
         KHDir = khdirs + idir(1) + idir(2)
      END IF
 
 
C  debug output
      IF ( IDEb(45).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/18X,7I6)')
     &         'PHO_DIFDIS: (2) ' , 
     &        'IPAR1,IPAR2,IMOTH1,IMOTH2,MSOFT,MHARD,IMODE' , ipar , 
     &        NPOsd , Msoft , Mhard , Imode
      END IF
      IF ( IDEb(45).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &         'PHO_DIFDIS: output of /POEVT1/' , 
     &        '------------------------------'
         CALL PHO_PREVNT(0)
      END IF
 
      END SUBROUTINE
