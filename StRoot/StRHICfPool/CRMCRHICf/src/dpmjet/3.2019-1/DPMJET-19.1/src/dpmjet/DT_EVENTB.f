
      SUBROUTINE DT_EVENTB(Ncsy,Irej)
 
C***********************************************************************
C Treatment of nucleon-nucleon interactions with full two-component    *
C Dual Parton Model.                                                   *
C          NCSY     number of nucleon-nucleon interactions             *
C          IREJ     rejection flag                                     *
C This version dated 14.01.2000 is written by S. Roesler               *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amnn , amp2 , amtot , amtot2 , diff1 , diff2 , 
     &                 dum , ONE , parrj21 , parrj23 , parrj24 , 
     &                 parrj41 , parrj42 , pp , pp1 , pp2 , ppcms , 
     &                 ppnn , ppsub
      DOUBLE PRECISION ppsuto , pptcms , pptmp , pptot , pptotn , 
     &                 pptsub , psutot , pt , pt1 , pt2 , ptcms , ptnn , 
     &                 ptot , ptot1 , ptot2 , ptotnn , ptsub , ptsuto , 
     &                 pttcms , pttmp
      DOUBLE PRECISION pttsub , TINY10 , xmp , xmt , ZERO
      INTEGER i , iback , iesss1 , iesss2 , ifp1 , ifp2 , ift1 , ift2 , 
     &        ir1 , Irej , irej1 , irej2 , irej3 , iremn1 , iremn2 , 
     &        irjano
      INTEGER isingl , iswsav , jfrg , k , kpron , mo1 , mo2 , mop , 
     &        mop1 , mop2 , mot , mot1 , mot2 , mxdtfr , mxleft , 
     &        mxphfr , nc , Ncsy , ndtusc
      INTEGER nfrg , nleft , nphosc , npje , npymax , npymem
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ZERO=0.0D0,ONE=1.0D0)
 
C PYTHIA parameters
 
      INCLUDE 'inc/pydat1'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C! uncomment this line for internal phojet-fragmentation
C #include "dtu_dtevtp.inc"
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C rejection counter
      INCLUDE 'inc/dtrejc'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
C statistics
      INCLUDE 'inc/dtsta1'
C DTUNUC-PHOJET interface, Lorentz-param. of n-n subsystem
      INCLUDE 'inc/dtltsu'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C statistics: double-Pomeron exchange
      INCLUDE 'inc/dtflg2'
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  initial state parton radiation (internal part)
      INCLUDE 'inc/point6'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general process information
      INCLUDE 'inc/poprcs'
 
      DIMENSION pp(4) , pt(4) , ptot(4) , pp1(4) , pp2(4) , pt1(4) , 
     &          pt2(4) , ppnn(4) , ptnn(4) , ptotnn(4) , ppsub(4) , 
     &          ptsub(4) , pptcms(4) , pttcms(4) , pptmp(4) , pttmp(4) , 
     &          kpron(15) , isingl(10000)
 
C initial values for max. number of phojet scatterings and dtunuc chains
C to be fragmented with one pyexec call
      DATA mxphfr , mxdtfr/10 , 100/
#ifdef FOR_FLUKA
C     COMMON / DBGPRE / LDBGPR
C     LOGICAL LDBGPR
#endif
      Irej = 0
C pointer to first parton of the first chain in dtevt common
      NPOint(3) = NHKk + 1
C special flag for double-Pomeron statistics
      IPOpo = 1
C counter for low-mass (DTUNUC) interactions
      ndtusc = 0
C counter for interactions treated by PHOJET
      nphosc = 0
 
C scan interactions for single nucleon-nucleon interactions
C (this has to be checked here because Cronin modifies parton momenta)
      nc = NPOint(2)
      IF ( Ncsy.GT.10000 ) THEN
         WRITE (LOUt,*) ' DT_EVENTB: NCSY > 10000 ! '
         GOTO 400
      END IF
      DO i = 1 , Ncsy
         isingl(i) = 0
         mop = JMOhkk(1,nc)
         mot = JMOhkk(1,nc+1)
         diff1 = ABS(PHKk(4,mop)-PHKk(4,nc)-PHKk(4,nc+2))
         diff2 = ABS(PHKk(4,mot)-PHKk(4,nc+1)-PHKk(4,nc+3))
         IF ( (diff1.LT.TINY10) .AND. (diff2.LT.TINY10) ) isingl(i) = 1
         nc = nc + 4
      END DO
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,I6)')
C    &    ' EVENTB IN:',NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
C multiple scattering of chain ends
      IF ( (IP.GT.1) .AND. (MKCron.NE.0) ) CALL DT_CRONIN(1)
 
C switch to PHOJET-settings for JETSET parameter
      IF ( (IT.GT.1) .AND. (MKCron.NE.0) ) CALL DT_CRONIN(2)
      CALL DT_INITJS(1)
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,2I6)')
C    &    ' EVENTB INITJS:',MKCRON,NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
C loop over nucleon-nucleon interaction
      nc = NPOint(2)
      DO i = 1 , Ncsy
C
C   pick up one nucleon-nucleon interaction from DTEVT1
C     ppnn  / ptnn   - momenta of the interacting nucleons (cms)
C     ptotnn         - total momentum of the interacting nucleons (cms)
C     pp1,2 / pt1,2  - momenta of the four partons
C     pp    / pt     - total momenta of the proj / targ partons
C     ptot           - total momentum of the four partons
         mop = JMOhkk(1,nc)
         mot = JMOhkk(1,nc+1)
         DO k = 1 , 4
            ppnn(k) = PHKk(k,mop)
            ptnn(k) = PHKk(k,mot)
            ptotnn(k) = ppnn(k) + ptnn(k)
            pp1(k) = PHKk(k,nc)
            pt1(k) = PHKk(k,nc+1)
            pp2(k) = PHKk(k,nc+2)
            pt2(k) = PHKk(k,nc+3)
            pp(k) = pp1(k) + pp2(k)
            pt(k) = pt1(k) + pt2(k)
            ptot(k) = pp(k) + pt(k)
         END DO
C
C-----------------------------------------------------------------------
C   this is a complete nucleon-nucleon interaction
C
         IF ( isingl(i).EQ.1 ) THEN
C
C     initialize PHOJET-variables for remnant/valence-partons
            IHFld(1,1) = 0
            IHFld(1,2) = 0
            IHFld(2,1) = 0
            IHFld(2,2) = 0
            IHFls(1) = 1
            IHFls(2) = 1
            XPSub = 1.D0
            XTSub = 1.D0
C     save current settings of PHOJET process and min. bias flags
            DO k = 1 , 11
               kpron(k) = IPRon(k,1)
            END DO
            iswsav = ISWmdl(2)
C
C     check if forced sampling of diffractive interaction requested
            IF ( ISIngd.LT.-1 ) THEN
               DO k = 1 , 11
                  IPRon(k,1) = 0
               END DO
               IF ( (ISIngd.EQ.-2) .OR. (ISIngd.EQ.-3) ) IPRon(5,1) = 1
               IF ( (ISIngd.EQ.-2) .OR. (ISIngd.EQ.-4) ) IPRon(6,1) = 1
               IF ( ISIngd.EQ.-5 ) IPRon(4,1) = 1
            END IF
C
C     for photons: a direct/anomalous interaction is not sampled
C     in PHOJET but already in Glauber-formalism. Here we check if such
C     an interaction is requested
            IF ( IJProj.EQ.7 ) THEN
C       first switch off direct interactions
               IPRon(8,1) = 0
C       this is a direct interactions
               IF ( IDIrec.EQ.1 ) THEN
                  DO k = 1 , 11
                     IPRon(k,1) = 0
                  END DO
                  IPRon(8,1) = 1
C       this is an anomalous interactions
C         (iswmdl(2) = 0 only hard int. generated ( = 1 min. bias) )
               ELSE IF ( IDIrec.EQ.2 ) THEN
                  ISWmdl(2) = 0
               END IF
            ELSE IF ( IDIrec.NE.0 ) THEN
               STOP ' DT_EVENTB: IDIREC > 0 ! '
            END IF
C
C     make sure that total momenta of partons, pp and pt, are on mass
C     shell (Cronin may have srewed this up..)
            CALL DT_MASHEL(pp,pt,PHKk(5,mop),PHKk(5,mot),ppnn,ptnn,ir1)
#ifdef FOR_FLUKA
C           IF ( LDBGPR ) THEN
C              WRITE (77,'(A,4I6,(/,1P,4G23.15))')
C    &         ' EVENTB MASHEL:',I,MOP,MOT,NHKK,PP,PT
C              CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C              WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C           END IF
#endif
            IF ( ir1.NE.0 ) THEN
 
               IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 )
     &               WRITE (LOUt,'(1X,A)')
     &               'EVENTB:  mass shell correction rejected'
               GOTO 400
            END IF
C
C     initialize the incoming particles in PHOJET
            IF ( (IP.EQ.1) .AND. (IJProj.EQ.7) ) THEN
 
               CALL PHO_SETPAR(1,22,0,VIRt)
 
            ELSE
 
               CALL PHO_SETPAR(1,IDHkk(mop),0,ZERO)
 
            END IF
#ifdef FOR_FLUKA
C           IF ( LDBGPR ) THEN
C              WRITE (77,'(A,I6)')
C    &         ' EVENTB SETPAR-1:',NHKK
C              CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C              WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C           END IF
#endif
            CALL PHO_SETPAR(2,IDHkk(mot),0,ZERO)
#ifdef FOR_FLUKA
C           IF ( LDBGPR ) THEN
C              WRITE (77,'(A,I6)')
C    &         ' EVENTB SETPAR-2:',NHKK
C              CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C              WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C           END IF
#endif
 
C
C     initialize rejection loop counter for anomalous processes
            irjano = 0
 20         irjano = irjano + 1
C
C     temporary fix for ifano problem
            IFAno(1) = 0
            IFAno(2) = 0
C
C     generate complete hadron/nucleon/photon-nucleon event with PHOJET
 
 
            CALL PHO_EVENT(2,ppnn,ptnn,dum,irej1)
#ifdef FOR_FLUKA
C           IF ( LDBGPR ) THEN
C              WRITE (77,'(A,2I6,(/,1P,4G23.15))')
C    &         ' EVENTB EVENT:',IREJ1,NHKK,PPNN,PTNN
C              CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C              WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C           END IF
#endif
 
C
C     for photons: special consistency check for anomalous interactions
            IF ( IJProj.EQ.7 ) THEN
               IF ( irjano.GE.30 ) THEN
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                  ' DT_EVENTB: Warning! IRJANO > 30 ' , irjano , 
     &                 IDIrec , NEVhkk
               ELSE IF ( IFAno(1).NE.0 ) THEN
C       here, an anomalous interaction was generated. Check if it
C       was also requested. Otherwise reject this event.
                  IF ( IDIrec.EQ.0 ) GOTO 20
C       here, an anomalous interaction was not generated. Check if it
C       was requested in which case we need to reject this event.
               ELSE IF ( IDIrec.EQ.2 ) THEN
                  GOTO 20
               END IF
            END IF
C
C     copy back original settings of PHOJET process and min. bias flags
            DO k = 1 , 11
               IPRon(k,1) = kpron(k)
            END DO
            ISWmdl(2) = iswsav
C
C     check if PHOJET has rejected this event
            IF ( irej1.NE.0 ) THEN
C              IF (IOULEV(1).GT.0) WRITE(LOUT,'(1X,A,I4)')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,I4)')
     &               'EVENTB:  chain system rejected' , IDIrec
 
               CALL PHO_PREVNT(0)
 
               GOTO 400
            END IF
C
C     copy partons and strings from PHOJET common back into DTEVT for
C     external fragmentation
            mo1 = nc
            mo2 = nc + 3
C!      uncomment this line for internal phojet-fragmentation
C           CALL DT_GETFSP(MO1,MO2,PPNN,PTNN,-1)
            nphosc = nphosc + 1
            CALL DT_GETPJE(mo1,mo2,ppnn,ptnn,-1,nphosc,irej1)
#ifdef FOR_FLUKA
C           IF ( LDBGPR ) THEN
C              WRITE (77,'(A,2I6,(/,1P,4G23.15))')
C    &         ' EVENTB GETPJE:',NPHOSC,NHKK,PPNN,PTNN
C              CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C              WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C           END IF
#endif
            IF ( irej1.NE.0 ) THEN
 
               IF ( IOUlev(1).GT.0 .AND. LPRi.GT.4 )
     &               WRITE (LOUt,'(1X,A,I4)')
     &               'EVENTB: chain system rejected 1'
               GOTO 400
            END IF
C
C     update statistics counter
            ICEvtg(IDCh(nc),29) = ICEvtg(IDCh(nc),29) + 1
C
C-----------------------------------------------------------------------
C   this interaction involves "remnants"
C
         ELSE
C
C     total mass of this system
            pptot = SQRT(ptot(1)**2+ptot(2)**2+ptot(3)**2)
            amtot2 = (ptot(4)-pptot)*(ptot(4)+pptot)
            IF ( amtot2.LT.ZERO ) THEN
               amtot = ZERO
            ELSE
               amtot = SQRT(amtot2)
            END IF
C
C     systems with masses larger than elojet are treated with PHOJET
            IF ( amtot.GT.ELOjet ) THEN
C
C     initialize PHOJET-variables for remnant/valence-partons
C       projectile parton flavors and valence flag
               IHFld(1,1) = IDHkk(nc)
               IHFld(1,2) = IDHkk(nc+2)
               IHFls(1) = 0
               IF ( (IDCh(nc).EQ.6) .OR. (IDCh(nc).EQ.7) .OR. 
     &              (IDCh(nc).EQ.8) ) IHFls(1) = 1
C       target parton flavors and valence flag
               IHFld(2,1) = IDHkk(nc+1)
               IHFld(2,2) = IDHkk(nc+3)
               IHFls(2) = 0
               IF ( (IDCh(nc).EQ.4) .OR. (IDCh(nc).EQ.5) .OR. 
     &              (IDCh(nc).EQ.8) ) IHFls(2) = 1
C       flag signalizing PHOJET how to treat the remnant:
C         iremn = -1 sea-quark remnant: PHOJET takes flavors from ihfld
C         iremn > -1 valence remnant: PHOJET assumes flavors according
C                    to mother particle
               iremn1 = IHFls(1) - 1
               iremn2 = IHFls(2) - 1
C
C     initialize the incoming particles in PHOJET
               IF ( (IP.EQ.1) .AND. (IJProj.EQ.7) ) THEN
 
                  CALL PHO_SETPAR(1,22,iremn1,VIRt)
 
               ELSE
 
                  CALL PHO_SETPAR(1,IDHkk(mop),iremn1,ZERO)
 
               END IF
#ifdef FOR_FLUKA
C              IF ( LDBGPR ) THEN
C                 WRITE (77,'(A,I6)')
C    &            ' EVENTB SETPAR-12:',NHKK
C                 CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C                 WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C              END IF
#endif
               CALL PHO_SETPAR(2,IDHkk(mot),iremn2,ZERO)
#ifdef FOR_FLUKA
C              IF ( LDBGPR ) THEN
C                 WRITE (77,'(A,I6)')
C    &            ' EVENTB SETPAR-22:',NHKK
C                 CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C                 WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C              END IF
#endif
C
C     calculate Lorentz parameter of the nucleon-nucleon cm-system
               pptotn = SQRT(ptotnn(1)**2+ptotnn(2)**2+ptotnn(3)**2)
               amnn = SQRT((ptotnn(4)-pptotn)*(ptotnn(4)+pptotn))
               BGX = ptotnn(1)/amnn
               BGY = ptotnn(2)/amnn
               BGZ = ptotnn(3)/amnn
               GAM = ptotnn(4)/amnn
C     transform interacting nucleons into nucleon-nucleon cm-system
               CALL DT_DALTRA(GAM,-BGX,-BGY,-BGZ,ppnn(1),ppnn(2),ppnn(3)
     &                        ,ppnn(4),ppcms,pptcms(1),pptcms(2),
     &                        pptcms(3),pptcms(4))
               CALL DT_DALTRA(GAM,-BGX,-BGY,-BGZ,ptnn(1),ptnn(2),ptnn(3)
     &                        ,ptnn(4),ptcms,pttcms(1),pttcms(2),
     &                        pttcms(3),pttcms(4))
C     transform (total) momenta of the proj and targ partons into
C     nucleon-nucleon cm-system
               CALL DT_DALTRA(GAM,-BGX,-BGY,-BGZ,pp(1),pp(2),pp(3),
     &                        pp(4),pptsub,ppsub(1),ppsub(2),ppsub(3),
     &                        ppsub(4))
               CALL DT_DALTRA(GAM,-BGX,-BGY,-BGZ,pt(1),pt(2),pt(3),
     &                        pt(4),pttsub,ptsub(1),ptsub(2),ptsub(3),
     &                        ptsub(4))
C     energy fractions of the proj and targ partons
               XPSub = MIN(ppsub(4)/pptcms(4),ONE)
               XTSub = MIN(ptsub(4)/pttcms(4),ONE)
C**
C testprint
C              PTOTCM = SQRT( (PPTCMS(1)+PTTCMS(1))**2 +
C    &                        (PPTCMS(2)+PTTCMS(2))**2 +
C    &                        (PPTCMS(3)+PTTCMS(3))**2 )
C              EOLDCM = SQRT( (PPTCMS(4)+PTTCMS(4)-PTOTCM) *
C    &                        (PPTCMS(4)+PTTCMS(4)+PTOTCM) )
C              PTOTSU = SQRT( (PPSUB(1)+PTSUB(1))**2 +
C    &                        (PPSUB(2)+PTSUB(2))**2 +
C    &                        (PPSUB(3)+PTSUB(3))**2 )
C              EOLDSU = SQRT( (PPSUB(4)+PTSUB(4)-PTOTSU) *
C    &                        (PPSUB(4)+PTSUB(4)+PTOTSU) )
C**
C
C     save current settings of PHOJET process and min. bias flags
               DO k = 1 , 11
                  kpron(k) = IPRon(k,1)
               END DO
C     disallow direct photon int. (does not make sense here anyway)
               IPRon(8,1) = 0
C     disallow double pomeron processes (due to technical problems
C     in PHOJET, needs to be solved sometime)
               IPRon(4,1) = 0
C     disallow diffraction for sea-diquarks
               IF ( (IABS(IHFld(1,1)).GT.1100) .AND. 
     &              (IABS(IHFld(1,2)).GT.1100) ) THEN
                  IPRon(3,1) = 0
                  IPRon(6,1) = 0
               END IF
               IF ( (IABS(IHFld(2,1)).GT.1100) .AND. 
     &              (IABS(IHFld(2,2)).GT.1100) ) THEN
                  IPRon(3,1) = 0
                  IPRon(5,1) = 0
               END IF
C     switch off qelast. vectormeson production for photons,
C     electrons and positrons - implemented to avoid final
C     state particles/resonances from Phojet with Id=81
               IF ( IJProj.EQ.7 ) IPRon(3,1) = 0
C
C     we need massless partons: transform them on mass shell
               xmp = ZERO
               xmt = ZERO
               DO k = 1 , 4
                  pptmp(k) = ppsub(k)
                  pttmp(k) = ptsub(k)
               END DO
               CALL DT_MASHEL(pptmp,pttmp,xmp,xmt,ppsub,ptsub,irej1)
#ifdef FOR_FLUKA
C              IF ( LDBGPR ) THEN
C                 WRITE (77,'(A,4I6,(/,1P,4G23.15))')
C    &            ' EVENTB MASHEL:',I,MOP,MOT,NHKK,PPTMP,PTTMP
C                 CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C                 WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C              END IF
#endif
               ppsuto = SQRT(ppsub(1)**2+ppsub(2)**2+ppsub(3)**2)
               ptsuto = SQRT(ptsub(1)**2+ptsub(2)**2+ptsub(3)**2)
               psutot = SQRT((ppsub(1)+ptsub(1))**2+(ppsub(2)+ptsub(2))
     &                  **2+(ppsub(3)+ptsub(3))**2)
C     total energy of the subsysten after mass transformation
C      (should be the same as before..)
               SECm = SQRT((ppsub(4)+ptsub(4)-psutot)
     &                *(ppsub(4)+ptsub(4)+psutot))
C
C     after mass shell transformation the x_sub - relation has to be
C     corrected. We therefore create "pseudo-momenta" of mother-nucleons.
C
C     The old version was to scale based on the original x_sub and the
C     4-momenta of the subsystem. At very high energy this could lead to
C     "pseudo-cm energies" of the parent system considerably exceeding
C     the true cm energy. Now we keep the true cm energy and calculate
C     new x_sub instead.
C old version  PPTCMS(4) = PPSUB(4)/XPSUB
               pptcms(4) = MAX(pptcms(4),ppsub(4))
               XPSub = ppsub(4)/pptcms(4)
               IF ( IJProj.EQ.7 ) THEN
                  amp2 = PHKk(5,mot)**2
                  ptot1 = SQRT(pptcms(4)**2-amp2)
               ELSE
C???????
                  ptot1 = SQRT((pptcms(4)-PHKk(5,mop))
     &                    *(pptcms(4)+PHKk(5,mop)))
C                 PTOT1 = SQRT((PPTCMS(4)-PHKK(5,MOT))
C    &                        *(PPTCMS(4)+PHKK(5,MOT)))
               END IF
C old version  PTTCMS(4) = PTSUB(4)/XTSUB
               pttcms(4) = MAX(pttcms(4),ptsub(4))
               XTSub = ptsub(4)/pttcms(4)
               ptot2 = SQRT((pttcms(4)-PHKk(5,mot))
     &                 *(pttcms(4)+PHKk(5,mot)))
               DO k = 1 , 3
                  pptcms(k) = ptot1*ppsub(k)/ppsuto
                  pttcms(k) = ptot2*ptsub(k)/ptsuto
               END DO
C**
C testprint
C
C     ppnn  / ptnn   - momenta of the int. nucleons (cms, negl. Fermi)
C     ptotnn         - total momentum of the int. nucleons (cms, negl. Fermi)
C     pptcms/ pttcms - momenta of the interacting nucleons (cms)
C     pp1,2 / pt1,2  - momenta of the four partons
C
C     pp    / pt     - total momenta of the pr/ta partons (cms, negl. Fermi)
C     ptot           - total momentum of the four partons (cms, negl. Fermi)
C     ppsub / ptsub  - total momenta of the proj / targ partons (cms)
C
C              PTOTCM = SQRT( (PPTCMS(1)+PTTCMS(1))**2 +
C    &                        (PPTCMS(2)+PTTCMS(2))**2 +
C    &                        (PPTCMS(3)+PTTCMS(3))**2 )
C              ENEWCM = SQRT( (PPTCMS(4)+PTTCMS(4)-PTOTCM) *
C    &                        (PPTCMS(4)+PTTCMS(4)+PTOTCM) )
C              PTOTSU = SQRT( (PPSUB(1)+PTSUB(1))**2 +
C    &                        (PPSUB(2)+PTSUB(2))**2 +
C    &                        (PPSUB(3)+PTSUB(3))**2 )
C              ENEWSU = SQRT( (PPSUB(4)+PTSUB(4)-PTOTSU) *
C    &                        (PPSUB(4)+PTSUB(4)+PTOTSU) )
C              IF (ENEWCM/EOLDCM.GT.1.1D0) THEN
C                 WRITE(*,*) ' EOLDCM, ENEWCM : ',EOLDCM,ENEWCM
C                 WRITE(*,*) ' EOLDSU, ENEWSU : ',EOLDSU,ENEWSU
C                 WRITE(*,*) ' XPSUB,  XTSUB  : ',XPSUB,XTSUB
C              ENDIF
C              BBGX = (PPTCMS(1)+PTTCMS(1))/ENEWCM
C              BBGY = (PPTCMS(2)+PTTCMS(2))/ENEWCM
C              BBGZ = (PPTCMS(3)+PTTCMS(3))/ENEWCM
C              BGAM = (PPTCMS(4)+PTTCMS(4))/ENEWCM
C     transform interacting nucleons into nucleon-nucleon cm-system
C              CALL DT_DALTRA(BGAM,-BBGX,-BBGY,-BBGZ,
C    &                    PPTCMS(1),PPTCMS(2),PPTCMS(3),PPTCMS(4),PPTOT,
C    &                     PPNEW1,PPNEW2,PPNEW3,PPNEW4)
C              CALL DT_DALTRA(BGAM,-BBGX,-BBGY,-BBGZ,
C    &                    PTTCMS(1),PTTCMS(2),PTTCMS(3),PTTCMS(4),PTTOT,
C    &                     PTNEW1,PTNEW2,PTNEW3,PTNEW4)
C              CALL DT_DALTRA(BGAM,-BBGX,-BBGY,-BBGZ,
C    &                     PPSUB(1),PPSUB(2),PPSUB(3),PPSUB(4),PPTOT,
C    &                     PPSUB1,PPSUB2,PPSUB3,PPSUB4)
C              CALL DT_DALTRA(BGAM,-BBGX,-BBGY,-BBGZ,
C    &                     PTSUB(1),PTSUB(2),PTSUB(3),PTSUB(4),PTTOT,
C    &                     PTSUB1,PTSUB2,PTSUB3,PTSUB4)
C              PTSTCM = SQRT( (PPNEW1+PTNEW1)**2 +
C    &                        (PPNEW2+PTNEW2)**2 +
C    &                        (PPNEW3+PTNEW3)**2 )
C              ETSTCM = SQRT( (PPNEW4+PTNEW4-PTSTCM) *
C    &                        (PPNEW4+PTNEW4+PTSTCM) )
C              PTSTSU = SQRT( (PPSUB1+PTSUB1)**2 +
C    &                        (PPSUB2+PTSUB2)**2 +
C    &                        (PPSUB3+PTSUB3)**2 )
C              ETSTSU = SQRT( (PPSUB4+PTSUB4-PTSTSU) *
C    &                        (PPSUB4+PTSUB4+PTSTSU) )
C              WRITE(*,*) ' mother cmE :'
C              WRITE(*,*) ETSTCM,ENEWCM
C              WRITE(*,*) ' subsystem cmE :'
C              WRITE(*,*) ETSTSU,ENEWSU
C              WRITE(*,*) ' projectile mother :'
C              WRITE(*,*) PPNEW1,PPNEW2,PPNEW3,PPNEW4
C              WRITE(*,*) ' target mother :'
C              WRITE(*,*) PTNEW1,PTNEW2,PTNEW3,PTNEW4
C              WRITE(*,*) ' projectile subsystem:'
C              WRITE(*,*) PPSUB1,PPSUB2,PPSUB3,PPSUB4
C              WRITE(*,*) ' target subsystem:'
C              WRITE(*,*) PTSUB1,PTSUB2,PTSUB3,PTSUB4
C              WRITE(*,*) ' projectile subsystem should be:'
C              WRITE(*,*) ZERO,ZERO,XPSUB*ETSTCM/2.0D0,
C    &                    XPSUB*ETSTCM/2.0D0
C              WRITE(*,*) ' target subsystem should be:'
C              WRITE(*,*) ZERO,ZERO,-XTSUB*ETSTCM/2.0D0,
C    &                    XTSUB*ETSTCM/2.0D0
C              WRITE(*,*) ' subsystem cmE should be: '
C              WRITE(*,*) SQRT(XPSUB*XTSUB)*ETSTCM,XPSUB,XTSUB
C**
C
C     generate complete remnant - nucleon/remnant event with PHOJET
 
               CALL PHO_EVENT(3,pptcms,pttcms,dum,irej1)
#ifdef FOR_FLUKA
C              IF ( LDBGPR ) THEN
C                 WRITE (77,'(A,2I6,(/,1P,4G23.15))')
C    &            ' EVENTB EVENT-2:',IREJ1,NHKK,PPTCMS,PTTCMS
C                 CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C                 WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C              END IF
#endif
C
C     copy back original settings of PHOJET process flags
               DO k = 1 , 11
                  IPRon(k,1) = kpron(k)
               END DO
C
C     check if PHOJET has rejected this event
               IF ( irej1.NE.0 ) THEN
 
                  IF ( IOUlev(1).GT.0 .AND. LPRi.GT.4 )
     &                  WRITE (LOUt,'(1X,A)')
     &                  'EVENTB:  chain system rejected'
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'XPSUB,XTSUB,SECM ' , 
     &                 XPSub , XTSub , SECm , amtot
 
                  CALL PHO_PREVNT(0)
 
                  GOTO 400
               END IF
C
C     copy partons and strings from PHOJET common back into DTEVT for
C     external fragmentation
               mo1 = nc
               mo2 = nc + 3
C!      uncomment this line for internal phojet-fragmentation
C              CALL DT_GETFSP(MO1,MO2,PP,PT,1)
               nphosc = nphosc + 1
               CALL DT_GETPJE(mo1,mo2,pp,pt,1,nphosc,irej1)
#ifdef FOR_FLUKA
C              IF ( LDBGPR ) THEN
C                 WRITE (77,'(A,2I6,(/,1P,4G23.15))')
C    &            ' EVENTB GETPJE-2:',NPHOSC,NHKK,PP,PT
C                 CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C                 WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C              END IF
#endif
               IF ( irej1.NE.0 ) THEN
 
                  IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 )
     &                  WRITE (LOUt,'(1X,A,I4)')
     &                  'EVENTB: chain system rejected 2'
                  GOTO 400
               END IF
C
C     update statistics counter
               ICEvtg(IDCh(nc),2) = ICEvtg(IDCh(nc),2) + 1
C
C-----------------------------------------------------------------------
C two-chain approx. for smaller systems
C
            ELSE
C
               ndtusc = ndtusc + 1
C   special flag for double-Pomeron statistics
               IPOpo = 0
C
C   pick up flavors at the ends of the two chains
               ifp1 = IDHkk(nc)
               ift1 = IDHkk(nc+1)
               ifp2 = IDHkk(nc+2)
               ift2 = IDHkk(nc+3)
C   ..and the indices of the mothers
               mop1 = nc
               mot1 = nc + 1
               mop2 = nc + 2
               mot2 = nc + 3
               CALL DT_GETCSY(ifp1,pp1,mop1,ifp2,pp2,mop2,ift1,pt1,mot1,
     &                        ift2,pt2,mot2,irej1)
#ifdef FOR_FLUKA
C              IF ( LDBGPR ) THEN
C                 WRITE (77,'(A,2I6)')
C    &            ' EVENTB GETCSY:',IREJ1,NHKK
C                 CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C                 WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C              END IF
#endif
C
C   check if this chain system was rejected
               IF ( irej1.GT.0 ) THEN
                  IF ( IOUlev(1).GT.0 ) THEN
 
                     IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                     'rejected 1 in EVENTB'
 
                     IF ( LPRi.GT.4 )
     &                     WRITE (LOUt,'(1X,4(I6,4E12.3,/),E12.3)')
     &                    ifp1 , pp1 , ift1 , pt1 , ifp2 , pp2 , ift2 , 
     &                    pt2 , amtot
                  END IF
                  IRHha = IRHha + 1
                  GOTO 400
               END IF
C   the following lines are for sea-sea chains rejected in GETCSY
               IF ( irej1.EQ.-1 ) ndtusc = ndtusc - 1
               ICEvtg(IDCh(nc),1) = ICEvtg(IDCh(nc),1) + 1
            END IF
C
         END IF
C
C     update statistics counter
         ICEvtg(IDCh(nc),0) = ICEvtg(IDCh(nc),0) + 1
C
         nc = nc + 4
C
      END DO
C
C-----------------------------------------------------------------------
C treatment of low-mass chains (if there are any)
C
      IF ( ndtusc.LE.0 ) THEN
C! uncomment this line for internal phojet-fragmentation
C        NPOINT(4) = NHKK+1
         IF ( NPOint(4).LE.NPOint(3) ) NPOint(4) = NHKk + 1
C
C   correct chains of very low masses for possible resonances
      ELSE IF ( IREsco.EQ.1 ) THEN
         CALL DT_EVTRES(irej1)
#ifdef FOR_FLUKA
C           IF ( LDBGPR ) THEN
C              WRITE (77,'(A,2I6)')
C    &         ' EVENTB EVTRES:',IREJ1,NHKK
C              CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C              WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C           END IF
#endif
         IF ( irej1.GT.0 ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 2a in EVENTB'
            IRRes(1) = IRRes(1) + 1
            GOTO 400
         END IF
C   fragmentation of low-mass chains
C!  uncomment this line for internal phojet-fragmentation
C   (of course it will still be fragmented by DPMJET-routines but it
C    has to be done here instead of further below)
C        CALL DT_EVTFRA(IREJ1)
C        IF (IREJ1.GT.0) THEN
C           IF (IOULEV(1).GT.0) WRITE(LOUT,*) 'rejected 2b in EVENTB'
C           IRFRAG = IRFRAG+1
C           GOTO 9999
C        ENDIF
      END IF
C
C-----------------------------------------------------------------------
C new di-quark breaking mechanisms
C
      mxleft = 2
      CALL DT_CHASTA(0)
      IF ( (PDBsea(1).GT.0.0D0) .OR. (PDBsea(2).GT.0.0D0) .OR. 
     &     (PDBsea(3).GT.0.0D0) ) THEN
         CALL DT_DIQBRK
#ifdef FOR_FLUKA
C        IF ( LDBGPR ) THEN
C           WRITE (77,'(A,I6)')
C    &       ' EVENTB DIQBRK:',NHKK
C           CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C           WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C        END IF
#endif
         mxleft = 4
      END IF
 
C     chain fusion
C     WRITE(6,*)' EVENTB: IFUSION before DENSITY,IAPROJ,IATARG',
C    * IFUSION,IP,IT
      IF ( (IFUsion.EQ.1) .AND. (IP.GT.12) .AND. (IT.GT.12) )
     &     CALL DT_DENSITY
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,2I6)')
C    &    ' EVENTB DENSITY:',IFUSION,NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
C
C
C
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,I6)')
C    &    ' EVENTB BEF HADRONIZATION:',NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
C-----------------------------------------------------------------------
C hadronize this event
C
C   hadronize PHOJET chain systems
      npymax = 0
      npje = nphosc/mxphfr
      IF ( mxphfr.LT.mxleft ) mxleft = 2
      IF ( npje.GT.1 ) THEN
         nleft = nphosc - npje*mxphfr
         DO jfrg = 1 , npje
            nfrg = jfrg*mxphfr
            IF ( (jfrg.EQ.npje) .AND. (nleft.LE.mxleft) ) THEN
               CALL DT_EVTFRG(1,nphosc,npymem,irej1)
               IF ( irej1.GT.0 ) GOTO 200
               nleft = 0
            ELSE
               CALL DT_EVTFRG(1,nfrg,npymem,irej1)
               IF ( irej1.GT.0 ) GOTO 200
            END IF
            IF ( npymem.GT.npymax ) npymax = npymem
         END DO
         IF ( nleft.GT.0 ) THEN
            CALL DT_EVTFRG(1,nphosc,npymem,irej1)
            IF ( irej1.GT.0 ) GOTO 200
            IF ( npymem.GT.npymax ) npymax = npymem
         END IF
      ELSE
         CALL DT_EVTFRG(1,nphosc,npymem,irej1)
         IF ( irej1.GT.0 ) GOTO 200
         IF ( npymem.GT.npymax ) npymax = npymem
      END IF
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,5I6)')
C    &   ' EVENTB AFTER PHOJET HADR.:',
C    &   NPJE,NPHOSC,NPYMEM,NPYMAX,NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
C
C   check max. filling level of jetset common and
C   reduce mxphfr if necessary
      IF ( npymax.GT.3000 ) THEN
         IF ( npymax.GT.3500 ) THEN
            mxphfr = MAX(1,mxphfr-2)
         ELSE
            mxphfr = MAX(1,mxphfr-1)
         END IF
C        WRITE(LOUT,*) ' EVENTB: Mxphfr reduced to ',MXPHFR
      END IF
C
C   hadronize DTUNUC chain systems
 100  iback = mxdtfr
      CALL DT_EVTFRG(2,iback,npymem,irej2)
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,4I6)')
C    &   ' EVENTB AFTER DTUNUC HADR.:',
C    &   IBACK,NPYMEM,IREJ2,NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
      IF ( irej2.LE.0 ) THEN
C
C   check max. filling level of jetset common and
C   reduce mxdtfr if necessary
         IF ( npymem.GT.3000 ) THEN
            IF ( npymem.GT.3500 ) THEN
               mxdtfr = MAX(1,mxdtfr-20)
            ELSE
               mxdtfr = MAX(1,mxdtfr-10)
            END IF
C        WRITE(LOUT,*) ' EVENTB: Mxdtfr reduced to ',MXDTFR
         END IF
C
         IF ( iback.EQ.-1 ) GOTO 100
      END IF
C
C     CALL DT_EVTFRG(1,IREJ1)
C     CALL DT_EVTFRG(2,IREJ2)
 200  IF ( (irej1.GT.0) .OR. (irej2.GT.0) ) THEN
 
         IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &         'rejected 1 in EVENTB'
         IRFrag = IRFrag + 1
         GOTO 400
      END IF
 
C    from dpm3304per 24.5.06 (j.r.)
 
 
C   j.r.12/01            Fragmentation of fused chains
 
C     1 change PYTHIA Parameters
      parrj21 = PARj(21)
      parrj23 = PARj(23)
      parrj24 = PARj(24)
      parrj41 = PARj(41)
      parrj42 = PARj(42)
C temporarily standard parameters j.r.5/02
C     PARJ(21)=1.08D0
C     PARJ(23)=0.2D0
C     PARJ(24)=2.D0
      PARj(41) = 0.15D0
C    old before 7.11.6: to be kept if pyptdi is kept the old one:
      PARj(42) = 1.3D0
C    new: only if pyptdi is the new one
C     PARJ(42)=0.40D0
      iesss1 = iesss1 + 1
#ifdef FOR_FLUKA
 
      IF ( iesss1.LT.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      '1 PARJ(21),PARJ(23),PARJ(24),PARJ(41),PARJ(42)' , PARj(21)
     &     , PARj(23) , PARj(24) , PARj(41) , PARj(42)
 
      IF ( iesss1.LT.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      '1 PARJ(11),PARJ(18),PARJ(21),PARJ(1),PARJ(2),PARJ(3)' , 
     &     PARj(11) , PARj(18) , PARj(21) , PARj(1) , PARj(2) , PARj(3)
 
      IF ( iesss1.LT.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      '1 PARJ(5),PARJ(19)' , PARj(5) , PARj(19)
#endif
 300  iback = mxdtfr
C     IBACK = 10
C     IBACK=1
      iback = 50
      CALL DT_EVTFRG2(2,iback,npymem,irej2)
C     WRITE(6,*)'PARJ(21),PARJ(23),PARJ(24),PARJ(41),PARJ(42)',
C    *      PARJ(21),PARJ(23),PARJ(24),PARJ(41),PARJ(42)
C     WRITE(6,*)'DT_EVTFRG2(2,IBACK,NPYMEM,IREJ2) ',IBACK,NPYMEM,IREJ2
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,4I6)')
C    &   ' EVENTB AFTER FUSION HADR.:',
C    &   IBACK,NPYMEM,IREJ2,NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
      IF ( irej2.LE.0 ) THEN
 
C   check max. filling level of jetset common and
C   reduce mxdtfr if necessary
         IF ( npymem.GT.3000 ) THEN
            IF ( npymem.GT.3500 ) THEN
               mxdtfr = MAX(1,mxdtfr-20)
            ELSE
               mxdtfr = MAX(1,mxdtfr-10)
            END IF
C        WRITE(LOUT,*) ' EVENTB: Mxdtfr reduced to ',MXDTFR
         END IF
         IF ( iback.EQ.-1 ) GOTO 300
      END IF
C     2  Return to PYTHIA default parameters
      PARj(21) = parrj21
      PARj(23) = parrj23
      PARj(24) = parrj24
      PARj(41) = parrj41
      PARj(42) = parrj42
      iesss2 = iesss2 + 1
#ifdef FOR_FLUKA
      IF ( iesss2.LT.3 ) WRITE (6,*)
     &      ' 2 PARJ(21),PARJ(23),PARJ(24),PARJ(41),PARJ(42)' , PARj(21)
     &     , PARj(23) , PARj(24) , PARj(41) , PARj(42)
      IF ( iesss2.LT.3 ) WRITE (6,*)
     &      '2 PARJ(11),PARJ(18),PARJ(21),PARJ(1),PARJ(2),PARJ(3)' , 
     &     PARj(11) , PARj(18) , PARj(21) , PARj(1) , PARj(2) , PARj(3)
      IF ( iesss2.LT.3 ) WRITE (6,*) '2 PARJ(5),PARJ(19)' , PARj(5) , 
     &     PARj(19)
#endif
C
 
C    from dpm3304per 24.5.06 (j.r.)
C
C get final state particles from /DTEVTP/
C! uncomment this line for internal phojet-fragmentation
C     CALL DT_GETFSP(IDUM,IDUM,PP,PT,2)
      IF ( IJProj.NE.7 ) CALL DT_EMC2(9,10,0,0,0,3,1,0,0,0,0,3,4,88,
     &     irej3)
C     IF (IREJ3.NE.0) GOTO 9999
 
      RETURN
 
 400  IREvt = IREvt + 1
      Irej = 1
      END SUBROUTINE
