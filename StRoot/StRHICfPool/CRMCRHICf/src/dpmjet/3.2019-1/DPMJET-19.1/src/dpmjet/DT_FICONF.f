
      SUBROUTINE DT_FICONF(Ijproj,Ip,Ipz,It,Itz,Nloop,Irej)
 
C***********************************************************************
C Treatment of FInal CONFiguration including evaporation, fission and  *
C Fermi-break-up (for light nuclei only).                              *
C Adopted from the original routine FINALE and extended to residual    *
C projectile nuclei.                                                   *
C This version dated 12.12.95 is written by S. Roesler.                *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DOUBLE PRECISION aif , aizf , akprho , AMELCT , amelec , AMNAMA , 
     &                 AMNTRN , AMPRTN , amrcl , AMUC12 , AMUGEV , 
     &                 AMUNMU , ANGLGB , BEXC12 , COUGFM , dlkprh , 
     &                 DT_RNDM , ELCCGS , ELCMKS , EMVGEV
      DOUBLE PRECISION etaeta , exc , excitf , EXMSAZ , expnuc , 
     &                 FERTHO , frcfll , frmrdc , HLFHLF , ONE , p1in , 
     &                 p1out , p2in , p2out , PFRMAV , prcl , prcl0 , 
     &                 prfrmi , prskin , ptold
      DOUBLE PRECISION ptorcl , rdcore , redctn , redori , skinrh , 
     &                 TINY10 , TINY3 , vrcl , we , wrcl , xm1 , xm2 , 
     &                 ZERO
      INTEGER i , icor , idfsp , idpar , idrcl , idtmp , idxpar , 
     &        idxtmp , Ijproj , ilcopt , inorcl , inuc , Ip , Ipz , 
     &        Irej , irej1 , It , Itz , izdum , j
      INTEGER jchar , jmass , k , kf , m , mo , mo1 , mo2 , nch , nexc , 
     &        nfsp , Nloop , nnchit
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY3=1.0D-3,TINY10=1.0D-10)
      PARAMETER (ANGLGB=5.0D-16)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
C treatment of residual nuclei: 4-momenta
      INCLUDE 'inc/dtrnu1'
C treatment of residual nuclei: properties of residual nuclei
      INCLUDE 'inc/dtrnu2'
C statistics: residual nuclei
      INCLUDE 'inc/dtsta2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
      
#ifdef FOR_FLUKA
      INCLUDE '(DIMPAR)'
      INCLUDE '(GENSTK)'
      INCLUDE '(RESNUC)'
#else
      INCLUDE 'DIMPAR'
      INCLUDE 'GENSTK'
      INCLUDE 'RESNUC'
#endif

      PARAMETER (EMVGEV=1.0D-03)
      PARAMETER (AMUGEV=0.93149432D+00)
      PARAMETER (AMPRTN=0.93827231D+00)
      PARAMETER (AMNTRN=0.93956563D+00)
      PARAMETER (AMELCT=0.51099906D-03)
      PARAMETER (ELCCGS=4.8032068D-10)
      PARAMETER (ELCMKS=1.60217733D-19)
      PARAMETER (COUGFM=ELCCGS*ELCCGS/ELCMKS*1.D-07*1.D+13*1.D-09)
      PARAMETER (HLFHLF=0.5D+00)
      PARAMETER (FERTHO=14.33D-09)
      PARAMETER (BEXC12=FERTHO*72.40715579499394D+00)
      PARAMETER (AMUNMU=HLFHLF*AMELCT-BEXC12/12.D+00)
      PARAMETER (AMUC12=AMUGEV-AMUNMU)
#ifdef FOR_FLUKA
      INCLUDE '(NUCDAT)'
      INCLUDE '(PAREVT)'
      INCLUDE '(FHEAVY)'
#else
      INCLUDE 'NUCDAT'
      INCLUDE 'PAREVT'
      INCLUDE 'FHEAVY'
#endif
 
C event flag
      INCLUDE 'inc/dtevno'
 
      DIMENSION inuc(2) , idxpar(2) , idpar(2) , aif(2) , aizf(2) , 
     &          amrcl(2) , prcl(2,4) , mo1(2) , mo2(2) , vrcl(2,4) , 
     &          wrcl(2,4) , p1in(4) , p2in(4) , p1out(4) , p2out(4)
 
      DIMENSION expnuc(2) , exc(2,260) , nexc(2,260)
      LOGICAL llcpot
      DATA exc , nexc/520*ZERO , 520*0/
      DATA expnuc/4.0D-3 , 4.0D-3/
 
      Irej = 0
      LRClpr = .FALSE.
      LRClta = .FALSE.
 
C skip residual nucleus treatment if not requested or in case
C of central collisions
 
      IF ( (.NOT.LEVprt) .OR. (ICEntr.GT.0) .OR. (ICEntr.EQ.-100) .OR. 
     &     (ICEntr.EQ.-1) ) RETURN
      DO k = 1 , 2
         idpar(k) = 0
         idxpar(k) = 0
         NTOt(k) = 0
         NTOtfi(k) = 0
         NPRo(k) = 0
         NPRofi(k) = 0
         NN(k) = 0
         NH(k) = 0
         NHPos(k) = 0
         NQ(k) = 0
         EEXc(k) = ZERO
         mo1(k) = 0
         mo2(k) = 0
         DO i = 1 , 4
            vrcl(k,i) = ZERO
            wrcl(k,i) = ZERO
         END DO
      END DO
      nfsp = 0
      inuc(1) = Ip
      inuc(2) = It
 
      DO i = 1 , NHKk
 
C number of final state particles
         IF ( ABS(ISThkk(i)).EQ.1 ) THEN
            nfsp = nfsp + 1
            idfsp = IDBam(i)
         END IF
 
C properties of remaining nucleon configurations
         kf = 0
         IF ( (ISThkk(i).EQ.13) .OR. (ISThkk(i).EQ.15) ) kf = 1
         IF ( (ISThkk(i).EQ.14) .OR. (ISThkk(i).EQ.16) ) kf = 2
         IF ( kf.GT.0 ) THEN
            IF ( mo1(kf).EQ.0 ) mo1(kf) = i
            mo2(kf) = i
C   position of residual nucleus = average position of nucleons
            DO k = 1 , 4
               vrcl(kf,k) = vrcl(kf,k) + VHKk(k,i)
               wrcl(kf,k) = wrcl(kf,k) + WHKk(k,i)
            END DO
C   total number of particles contributing to each residual nucleus
            NTOt(kf) = NTOt(kf) + 1
            idtmp = IDBam(i)
            idxtmp = i
C   total charge of residual nuclei
            NQ(kf) = NQ(kf) + IICh(idtmp)
C   number of protons
            IF ( IDHkk(i).EQ.2212 ) THEN
               NPRo(kf) = NPRo(kf) + 1
C   number of neutrons
            ELSE IF ( IDHkk(i).EQ.2112 ) THEN
               NN(kf) = NN(kf) + 1
C   number of baryons other than n, p
            ELSE IF ( IIBar(idtmp).EQ.1 ) THEN
               NH(kf) = NH(kf) + 1
               IF ( IICh(idtmp).EQ.1 ) NHPos(kf) = NHPos(kf) + 1
            ELSE
C   any other mesons (status set to 1)
C                 WRITE(LOUT,1002) KF,IDTMP
C1002             FORMAT(1X,'FICONF:   residual nucleus ',I2,
C    &                   ' containing meson ',I4,', status set to 1')
               ISThkk(i) = 1
               idtmp = idpar(kf)
               idxtmp = idxpar(kf)
               NTOt(kf) = NTOt(kf) - 1
            END IF
            idpar(kf) = idtmp
            idxpar(kf) = idxtmp
         END IF
      END DO
 
C reject elastic events (def: one final state particle = projectile)
      IF ( (Ip.EQ.1) .AND. (nfsp.EQ.1) .AND. (idfsp.EQ.Ijproj) ) THEN
         IRExci(3) = IRExci(3) + 1
         GOTO 200
C        RETURN
      END IF
 
C check if one nucleus disappeared..
C     IF ((IP.GT.1).AND.(NTOT(1).EQ.0).AND.(NTOT(2).NE.0)) THEN
C        DO 5 K=1,4
C           PRCLTA(K) = PRCLTA(K)+PRCLPR(K)
C           PRCLPR(K) = ZERO
C   5    CONTINUE
C     ELSEIF ((IT.GT.1).AND.(NTOT(2).EQ.0).AND.(NTOT(1).NE.0)) THEN
C        DO 6 K=1,4
C           PRCLPR(K) = PRCLPR(K)+PRCLTA(K)
C           PRCLTA(K) = ZERO
C   6    CONTINUE
C     ENDIF
 
      icor = 0
      inorcl = 0
      DO i = 1 , 2
         DO k = 1 , 4
C get the average of the nucleon positions
            vrcl(i,k) = vrcl(i,k)/MAX(NTOt(i),1)
            wrcl(i,k) = wrcl(i,k)/MAX(NTOt(i),1)
            IF ( i.EQ.1 ) prcl(1,k) = PRClpr(k)
            IF ( i.EQ.2 ) prcl(2,k) = PRClta(k)
         END DO
C mass number and charge of residual nuclei
         aif(i) = DBLE(NTOt(i))
         aizf(i) = DBLE(NPRo(i)+NHPos(i))
         IF ( NTOt(i).GT.1 ) THEN
C masses of residual nuclei in ground state
 
C           AMRCL0(I) = AIF(I)*AMUAMU+1.0D-3*ENERGY(AIF(I),AIZF(I))
            AMRcl0(i) = aif(i)*AMUC12 + EMVGEV*EXMSAZ(aif(i),aizf(i),
     &                  .TRUE.,izdum)
 
C masses of residual nuclei
            ptorcl = SQRT(prcl(i,1)**2+prcl(i,2)**2+prcl(i,3)**2)
            amrcl(i) = (prcl(i,4)-ptorcl)*(prcl(i,4)+ptorcl)
            IF ( amrcl(i).GT.ZERO ) amrcl(i) = SQRT(amrcl(i))
            IF ( amrcl(i).LE.ZERO ) THEN
 
               IF ( IOUlev(3).GT.0 .AND. LPRi.GT.4 ) WRITE (LOUt,99010)
     &              i , prcl(i,1) , prcl(i,2) , prcl(i,3) , prcl(i,4) , 
     &              NTOt
99010          FORMAT (1X,'warning! negative excitation energy',/,I4,
     &                 4E15.4,2I4)
               amrcl(i) = ZERO
               EEXc(i) = ZERO
               IF ( Nloop.LE.500 ) GOTO 100
               IRExci(2) = IRExci(2) + 1
               GOTO 200
            ELSE IF ( (amrcl(i).GT.ZERO) .AND. (amrcl(i).LT.AMRcl0(i)) )
     &                THEN
C*sr
C              WRITE(6,*) NEVHKK,I,NTOT(1),NTOT(2),AMRCL(I),AMRCL0(I)
C*
C*sr 3.3
C              AMRCL(I) = AMRCL0(I)+EXPNUC(I)*DBLE(NTOT(I))
               m = MIN(NTOt(i),260)
               IF ( nexc(i,m).GT.0 ) THEN
                  amrcl(i) = AMRcl0(i) + exc(i,m)/DBLE(nexc(i,m))
C                 WRITE(77,*)' EXC(I,M),NEXC(I,M),M,I',
C    &                         EXC(I,M),NEXC(I,M),M,I
               ELSE
 5                m = m + 1
                  IF ( m.LT.inuc(i) ) THEN
C                       WRITE(77,*)' 2:EXC(I,M),NEXC(I,M),M,I',
C    &                                 EXC(I,M),NEXC(I,M),M,I
                     IF ( nexc(i,m).LE.0 ) GOTO 5
                     amrcl(i) = AMRcl0(i) + exc(i,m)/DBLE(nexc(i,m))
C  A.F.
C                    AMRCL(I) = AMRCL0(I)+EXPNUC(I)*DBLE(NTOT(I))
                  ELSE IF ( inuc(i).GT.NTOt(i) ) THEN
                     amrcl(i) = AMRcl0(i) + expnuc(i)
     &                  *DBLE(MAX(inuc(i)-NTOt(i),0))
C                       WRITE(77,*)' EXPNUC(I),NTOT(I),M,INUC(I)',
C    &                               EXPNUC(I),NTOT(I),M,INUC(I)
                  ELSE
                     amrcl(i) = AMRcl0(i) + 0.5D+00*expnuc(i)
                  END IF
               END IF
C*
               EEXc(i) = amrcl(i) - AMRcl0(i)
C              WRITE(77,*)' ICOR,EEXC(I),AMRCL0(I)',
C    &                      ICOR,EEXC(I),AMRCL0(I)
               icor = icor + i
            ELSE IF ( amrcl(i).GE.2.5D0*AMRcl0(i) ) THEN
 
               IF ( IOUlev(3).GT.0 .AND. LPRi.GT.4 ) WRITE (LOUt,99020)
     &              i , amrcl(i) , AMRcl0(i) , NTOt , NEVhkk
99020          FORMAT (1X,'warning! too high excitation energy',/,I4,1P,
     &                 2E15.4,3I5)
               amrcl(i) = ZERO
               EEXc(i) = ZERO
               IF ( Nloop.LE.500 ) GOTO 100
               IRExci(2) = IRExci(2) + 1
               GOTO 200
            ELSE
C excitation energies of residual nuclei
               EEXc(i) = amrcl(i) - AMRcl0(i)
C  === A.F. === *
               llcpot = .TRUE.
               ilcopt = 3
               IF ( llcpot ) THEN
                  nnchit = MAX(inuc(i)-NTOt(i),0)
                  IF ( ilcopt.LE.2 ) THEN
C Patch for Fermi momentum reduction correlated with impact parameter:
                     frmrdc = MIN((PFRMAV(inuc(i))/APFrmx)**3,ONE)
                     dlkprh = 0.1D+00 + 0.5D+00/SQRT(DBLE(inuc(i)))
                     akprho = ONE - dlkprh
C f x K rho_cen + (1-f) x 0.5 x K rho_cen = frmrdc x rho_cen
                     frcfll = MAX(2.D+00*frmrdc/akprho-ONE,0.05D+00)
C                    REDORI = 0.75D+00
C                    REDORI = ONE
                     redori = ONE/(frmrdc)**(2.D+00/3.D+00)
                  ELSE
                     dlkprh = ZERO
                     rdcore = 1.14D+00*DBLE(inuc(i))**(ONE/3.D+00)
C  Take out roughly one/half of the skin:
                     rdcore = rdcore - 0.5D+00
                     frcfll = rdcore**3
                     prskin = (rdcore+2.4D+00)**3 - frcfll
                     prskin = 0.5D+00*prskin/(prskin+frcfll)
                     frcfll = ONE - prskin
                     frmrdc = frcfll + 0.5D+00*prskin
CD                    WRITE(77,*)' PRSKIN,FRCFLL',PRSKIN,FRCFLL
                     redori = ONE/(frmrdc)**(2.D+00/3.D+00)
                  END IF
                  IF ( nnchit.GT.0 ) THEN
                     IF ( ilcopt.EQ.1 ) THEN
                        skinrh = ONE - frcfll/(DBLE(inuc(i))-ONE)
                        DO nch = 1 , 10
                           etaeta = (ONE-skinrh**inuc(i)-DBLE(inuc(i))
     &                        *(ONE-frcfll)*(ONE-skinrh))
     &                        /(skinrh**inuc(i)-DBLE(inuc(i))
     &                        *(ONE-frcfll)*skinrh)
                           skinrh = skinrh*(ONE+etaeta)
C                          WRITE(77,*)
C    &               ' SKINRH,NCH,INUC(I),FRCFLL,FRMRDC,APFRMX',
C    &                 SKINRH,NCH,INUC(I),FRCFLL,FRMRDC,APFRMX
                        END DO
                        prskin = skinrh**(nnchit-1)
                     ELSE IF ( ilcopt.EQ.2 ) THEN
                        prskin = ONE - frcfll
                     END IF
                     redctn = ZERO
                     DO nch = 1 , nnchit
                        IF ( DT_RNDM(prfrmi).LT.prskin ) THEN
                           prfrmi = ((ONE-2.D+00*dlkprh)*DT_RNDM(prfrmi)
     &                        )**0.333333333333D+00
                        ELSE
                           prfrmi = (ONE-2.D+00*dlkprh*DT_RNDM(prfrmi))
     &                        **0.333333333333D+00
                        END IF
                        redctn = redctn + prfrmi**2
                     END DO
                     redctn = redctn/DBLE(nnchit)
                  ELSE
                     redctn = 0.5D+00
                  END IF
                  EEXc(i) = EEXc(i)*redctn/redori
                  amrcl(i) = AMRcl0(i) + EEXc(i)
                  prcl(i,4) = SQRT(ptorcl**2+amrcl(i)**2)
               END IF
C  === End A.F. === *
               IF ( ICAsca.EQ.0 ) THEN
C*sr 15.1.
C                 EXPNUC(I) = EEXC(I)/DBLE(NTOT(I))
                  expnuc(i) = EEXc(i)/MAX(1,inuc(i)-NTOt(i))
                  m = MIN(NTOt(i),260)
                  exc(i,m) = exc(i,m) + EEXc(i)
                  nexc(i,m) = nexc(i,m) + 1
               END IF
            END IF
         ELSE IF ( NTOt(i).EQ.1 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) i
99030       FORMAT (1X,'FICONF:   warning! NTOT(I)=1? (I=',I3,')')
            GOTO 200
         ELSE
            AMRcl0(i) = ZERO
            amrcl(i) = ZERO
            EEXc(i) = ZERO
            inorcl = inorcl + i
         END IF
      END DO
 
      PRClpr(5) = amrcl(1)
      PRClta(5) = amrcl(2)
 
      IF ( icor.GT.0 ) THEN
         IF ( inorcl.EQ.0 ) THEN
C one or both residual nuclei consist of one nucleon only, transform
C this nucleon on mass shell
            DO k = 1 , 4
               p1in(k) = prcl(1,k)
               p2in(k) = prcl(2,k)
            END DO
            xm1 = amrcl(1)
            xm2 = amrcl(2)
            CALL DT_MASHEL(p1in,p2in,xm1,xm2,p1out,p2out,irej1)
            IF ( irej1.GT.0 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'ficonf-mashel rejection'
               GOTO 200
            END IF
            DO k = 1 , 4
               prcl(1,k) = p1out(k)
               prcl(2,k) = p2out(k)
               PRClpr(k) = p1out(k)
               PRClta(k) = p2out(k)
            END DO
            PRClpr(5) = amrcl(1)
            PRClta(5) = amrcl(2)
         ELSE
 
            IF ( IOUlev(3).GT.0 .AND. LPRi.GT.4 ) WRITE (LOUt,99040)
     &           NEVhkk , INT(aif(1)) , INT(aizf(1)) , INT(aif(2)) , 
     &           INT(aizf(2)) , AMRcl0(1) , amrcl(1) , amrcl(1)
     &           - AMRcl0(1) , AMRcl0(2) , amrcl(2) , amrcl(2)
     &           - AMRcl0(2)
99040       FORMAT (1X,'FICONF:   warning! no residual nucleus for',
     &              ' correction',/,11X,'at event',I8,
     &              ',  nucleon config. 1:',2I4,' 2:',2I4,
     &              2(/,11X,3E12.3))
            IF ( Nloop.LE.500 ) GOTO 100
            IRExci(1) = IRExci(1) + 1
         END IF
      END IF
 
C update counter
C     IF (NRESEV(1).NE.NEVHKK) THEN
C        NRESEV(1) = NEVHKK
C        NRESEV(2) = NRESEV(2)+1
C     ENDIF
      NREsev(2) = NREsev(2) + 1
      DO i = 1 , 2
         EXCdpm(i) = EXCdpm(i) + EEXc(i)
         EXCdpm(i+2) = EXCdpm(i+2) + (EEXc(i)/MAX(NTOt(i),1))
         NREsto(i) = NREsto(i) + NTOt(i)
         NREspr(i) = NREspr(i) + NPRo(i)
         NREsnu(i) = NREsnu(i) + NN(i)
         NREsba(i) = NREsba(i) + NH(i)
         NREspb(i) = NREspb(i) + NHPos(i)
         NREsch(i) = NREsch(i) + NQ(i)
      END DO
 
C evaporation
      IF ( LEVprt ) THEN
         DO i = 1 , 2
C initialize evaporation counter
C !!!!!!!! Aarghh !!!!!!!! This is a major crime, it spoils FLUKA!!!
C           NP = 0
            EEXcfi(i) = ZERO
            IF ( (inuc(i).GT.1) .AND. (aif(i).GT.ONE) .AND. 
     &           (EEXc(i).GT.ZERO) ) THEN
C put residual nuclei into DTEVT1
               idrcl = 80000
               jmass = INT(aif(i))
               jchar = INT(aizf(i))
C  the following patch is required to transmit the correct excitation
C   energy to Eventd
               IF ( ITRspt.EQ.1 ) THEN
                 IF ( ABS(amrcl(i)-AMRcl0(i)-EEXc(i)).GT.1.D-04 )
#ifndef FOR_CORSIKA
     &                WRITE (77,*)
#else
     &                WRITE (LOUT,*)
#endif
     &                         ' DT_FICONF:AMRCL(I),AMRCL0(I),EEXC(I)' , 
     &                        amrcl(i) , AMRcl0(i) , EEXc(i)
                  prcl0 = prcl(i,4)
                  prcl(i,4) = SQRT(amrcl(i)**2+prcl(i,1)**2+prcl(i,2)
     &                        **2+prcl(i,3)**2)
                  IF ( ABS(prcl0-prcl(i,4)).GT.0.1D0 ) THEN
 
                     IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                     ' PRCL(I,4) recalculated :' , prcl0 , 
     &                    prcl(i,4)
                  END IF
               END IF
               CALL DT_EVTPUT(1000,idrcl,mo1(i),mo2(i),prcl(i,1),
     &                        prcl(i,2),prcl(i,3),prcl(i,4),jmass,jchar,
     &                        0)
C*sr 22.6.97
               NOBam(NHKk) = i
C*
               DO j = 1 , 4
                  VHKk(j,NHKk) = vrcl(i,j)
                  WHKk(j,NHKk) = wrcl(i,j)
               END DO
C  interface to evaporation module - fill final residual nucleus into
C  common FKRESN
C   fill resnuc only if code is not used as event generator in Fluka
               IF ( ITRspt.NE.1 ) THEN
                  PXRes = prcl(i,1)
                  PYRes = prcl(i,2)
                  PZRes = prcl(i,3)
                  IBRes = NPRo(i) + NN(i) + NH(i)
                  ICRes = NPRo(i) + NHPos(i)
                  ANOw = DBLE(IBRes)
                  ZNOw = DBLE(ICRes)
                  PTRes = SQRT(PXRes**2+PYRes**2+PZRes**2)
C   ground state mass of the residual nucleus (should be equal to AM0T)
 
                  AMNres = AMRcl0(i)
#ifdef FOR_FLUKA
                  AMMres = AMNAMA(AMNres,IBRes,ICRes)
#else
C**af added to replace amnama from original DPMJET
                  AMNres = AMMres - ZNOw*amelec + ELBnde(ICRes)
#endif
 
C  common FKFINU
                  TV = ZERO
C   kinetic energy of residual nucleus
                  TVRecl = prcl(i,4) - amrcl(i)
C   excitation energy of residual nucleus
                  TVCms = EEXc(i)
                  ptold = PTRes
                  PTRes = SQRT(ABS(TVRecl*(TVRecl+2.0D0*(AMMres+TVCms)))
     &                    )
                  IF ( ptold.LT.ANGLGB ) THEN
                     CALL DT_RACO(PXRes,PYRes,PZRes)
                     ptold = ONE
                  END IF
                  PXRes = PXRes*PTRes/ptold
                  PYRes = PYRes*PTRes/ptold
                  PZRes = PZRes*PTRes/ptold
C evaporation
                  we = ONE
C movd from above
                  NP = 0
                  NPHeav = 0
                  LRNfss = .FALSE.
                  LFRagm = .FALSE.
                  CALL EVEVAP(we)
 
C put evaporated particles and residual nuclei to DTEVT1
                  mo = NHKk
                  CALL DT_EVA2HE(mo,excitf,i,irej1)
               END IF
               EEXcfi(i) = excitf
               EXCeva(i) = EXCeva(i) + excitf
            END IF
         END DO
      END IF
 
      RETURN
 
C9998 IREXCI(1) = IREXCI(1)+1
 100  Irej = Irej + 1
 200  LRClpr = .TRUE.
      LRClta = .TRUE.
      Irej = Irej + 1
      END SUBROUTINE
