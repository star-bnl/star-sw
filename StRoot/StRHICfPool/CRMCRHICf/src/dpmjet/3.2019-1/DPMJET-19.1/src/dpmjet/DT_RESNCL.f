
      SUBROUTINE DT_RESNCL(Epn,Nloop,Mode)

#ifdef FOR_FLUKA
      INCLUDE '(DBLPRC)'
#else
      INCLUDE 'DBLPRC'
#endif

      SAVE

 
      DOUBLE PRECISION aip , aipz , ait , aitz , amsec , AMUAMU , 
     &                 chklev , dum , dum1 , dum2 , ebipot , Epn , 
     &                 epni , EXMSAZ , FM2MM , ONE , pei , pfsp , pm1 , 
     &                 pm2
      DOUBLE PRECISION pmass1 , pmass2 , pmomb , pmomm , psec , psec0 , 
     &                 psecn , pseco , pzi , RNUCLE , scpot , THREE , 
     &                 thresh , TINY1 , TINY10 , TINY2 , TINY3 , TINY4 , 
     &                 TWO , ZERO
      INTEGER i , icor , idsec , idum , idxb , idxcor , idxm , idxoth , 
     &        iflg , imode , iother , ipot , irej1 , izdum , j , jpcw , 
     &        jpmod , jpw , jtcw , jtw
      INTEGER k , MAXNCL , MAXINT , MAXSQU , MAXVQU , Mode , ncor , 
     &        Nloop , nob , nom
 
C***********************************************************************
C Treatment of residual nuclei and nuclear effects.                    *
C         MODE = 1     initializations                                 *
C              = 2     treatment of final state                        *
C This version dated 16.11.95 is written by S. Roesler.                *
C***********************************************************************
 
C
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.D0,ONE=1.D0,TWO=2.D0,THREE=3.D0,TINY3=1.0D-3,
     &           TINY2=1.0D-2,TINY1=1.0D-1,TINY4=1.0D-4,TINY10=1.0D-10)
      PARAMETER (AMUAMU=AMUGEV,FM2MM=1.0D-12,RNUCLE=1.12D0)
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
 
C event history
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C treatment of residual nuclei: wounded nucleons
      INCLUDE 'inc/dtwoun'
C treatment of residual nuclei: 4-momenta
      INCLUDE 'inc/dtrnu1'
 
      DIMENSION pfsp(4) , psec(4) , psec0(4)
      DIMENSION pmomb(5000) , idxb(5000) , pmomm(10000) , idxm(10000) , 
     &          idxcor(15000) , idxoth(NMXHKK)
 
      IF ( Mode.EQ.2 ) THEN
 
C------- treatment of final state
 
         Nloop = Nloop + 1
         IF ( Nloop.GT.1 ) scpot = 0.10D0
 
         jpw = NPW
         jpcw = NPCw
         jtw = NTW
         jtcw = NTCw
         DO k = 1 , 4
            pfsp(k) = ZERO
         END DO
 
         nob = 0
         nom = 0
         DO i = NPOint(4) , NHKk
            idxoth(i) = -1
            IF ( ISThkk(i).EQ.1 ) THEN
               IF ( IDBam(i).NE.7 ) THEN
                  ipot = 0
                  iother = 0
C particle moving into forward direction
                  IF ( PHKk(3,i).GE.ZERO ) THEN
C   most likely to be effected by projectile potential
                     ipot = 1
C     there is no projectile nucleus, try target
                     IF ( (IP.LE.1) .OR. ((IP-NPW).LE.1) ) THEN
                        ipot = 2
                        IF ( IP.GT.1 ) iother = 1
C       there is no target nucleus --> skip
                        IF ( (IT.LE.1) .OR. ((IT-NTW).LE.1) ) GOTO 50
                     END IF
C particle moving into backward direction
                  ELSE
C   most likely to be effected by target potential
                     ipot = 2
C     there is no target nucleus, try projectile
                     IF ( (IT.LE.1) .OR. ((IT-NTW).LE.1) ) THEN
                        ipot = 1
                        IF ( IT.GT.1 ) iother = 1
C       there is no projectile nucleus --> skip
                        IF ( (IP.LE.1) .OR. ((IP-NPW).LE.1) ) GOTO 50
                     END IF
                  END IF
                  iflg = -ipot
C nobam=3: particle is in overlap-region or neither inside proj. nor target
C      =1: particle is not in overlap-region AND is inside target (2)
C      =2: particle is not in overlap-region AND is inside projectile (1)
C flag particles which are inside the nucleus ipot but not in its
C overlap region
                  IF ( (NOBam(i).NE.ipot) .AND. (NOBam(i).LT.3) )
     &                 iflg = ipot
                  IF ( IDBam(i).EQ.0 ) THEN
C baryons: keep all nucleons and all others where flag is set
                  ELSE IF ( IIBar(IDBam(i)).NE.0 ) THEN
                     IF ( (IDBam(i).EQ.1) .OR. (IDBam(i).EQ.8) .OR. 
     &                    (iflg.GT.0) ) THEN
                        nob = nob + 1
                        pmomb(nob) = PHKk(3,i)
                        idxb(nob)
     &                     = SIGN(10000000*IABS(iflg)+1000000*iother+i,
     &                     iflg)
                     END IF
C mesons: keep only those mesons where flag is set
                  ELSE IF ( iflg.GT.0 ) THEN
                     nom = nom + 1
                     pmomm(nom) = PHKk(3,i)
                     idxm(nom) = 10000000*iflg + 1000000*iother + i
                  END IF
               END IF
            END IF
 50      END DO
C
C sort particles in the arrays according to increasing long. momentum
         CALL DT_SORT1(pmomb,idxb,nob,1,nob,1)
         CALL DT_SORT1(pmomm,idxm,nom,1,nom,1)
C
C shuffle indices into one and the same array according to the later
C sequence of correction
         ncor = 0
         IF ( IT.GT.1 ) THEN
            DO i = 1 , nob
               IF ( pmomb(i).GT.ZERO ) GOTO 60
               ncor = ncor + 1
               idxcor(ncor) = idxb(i)
            END DO
 60         IF ( IP.GT.1 ) THEN
               DO j = 1 , nob
                  i = nob + 1 - j
                  IF ( pmomb(i).LT.ZERO ) GOTO 100
                  ncor = ncor + 1
                  idxcor(ncor) = idxb(i)
               END DO
            ELSE
               DO i = 1 , nob
                  IF ( pmomb(i).GT.ZERO ) THEN
                     ncor = ncor + 1
                     idxcor(ncor) = idxb(i)
                  END IF
               END DO
            END IF
         ELSE
            DO j = 1 , nob
               i = nob + 1 - j
               ncor = ncor + 1
               idxcor(ncor) = idxb(i)
            END DO
         END IF
 100     DO i = 1 , nom
            IF ( pmomm(i).GT.ZERO ) GOTO 150
            ncor = ncor + 1
            idxcor(ncor) = idxm(i)
         END DO
 150     DO j = 1 , nom
            i = nom + 1 - j
            IF ( pmomm(i).LT.ZERO ) GOTO 200
            ncor = ncor + 1
            idxcor(ncor) = idxm(i)
         END DO
C
C      IF (NEVHKK.EQ.484) THEN
C         WRITE(LOUT,9000) JPCW,JPW-JPCW,JTCW,JTW-JTCW
C 9000    FORMAT(1X,'wounded nucleons (proj.-p,n  targ.-p,n)',/,4I10)
C         WRITE(LOUT,9001) NOB,NOM,NCOR
C 9001    FORMAT(1X,'produced particles (baryons,mesons,all)',3I10)
C         WRITE(LOUT,'(/,A)') ' baryons '
C         DO 950 I=1,NOB
CC           J     = IABS(IDXB(I))
CC           INDEX = J-IABS(J/10000000)*10000000
C            IPOT   = IABS(IDXB(I))/10000000
C            IOTHER = IABS(IDXB(I))/1000000-IPOT*10
C            INDEX = IABS(IDXB(I))-IPOT*10000000-IOTHER*1000000
C            WRITE(LOUT,9002) I,INDEX,IDXB(I),IDBAM(INDEX),PMOMB(I)
C  950    CONTINUE
C         WRITE(LOUT,'(/,A)') ' mesons '
C         DO 951 I=1,NOM
CC           INDEX = IDXM(I)-IABS(IDXM(I)/10000000)*10000000
C            IPOT   = IABS(IDXM(I))/10000000
C            IOTHER = IABS(IDXM(I))/1000000-IPOT*10
C            INDEX = IABS(IDXM(I))-IPOT*10000000-IOTHER*1000000
C            WRITE(LOUT,9002) I,INDEX,IDXM(I),IDBAM(INDEX),PMOMM(I)
C  951    CONTINUE
C 9002    FORMAT(1X,4I14,E14.5)
C         WRITE(LOUT,'(/,A)') ' all '
C         DO 952 I=1,NCOR
CC           J     = IABS(IDXCOR(I))
CC           INDEX = J-IABS(J/10000000)*10000000
CC            IPOT   = IABS(IDXCOR(I))/10000000
C            IOTHER = IABS(IDXCOR(I))/1000000-IPOT*10
C            INDEX = IABS(IDXCOR(I))-IPOT*10000000-IOTHER*1000000
C            WRITE(LOUT,9003) I,INDEX,IDXCOR(I),IDBAM(INDEX)
C  952    CONTINUE
C 9003    FORMAT(1X,4I14)
C      ENDIF
C
 200     DO icor = 1 , ncor
            ipot = IABS(idxcor(icor))/10000000
            iother = IABS(idxcor(icor))/1000000 - ipot*10
            i = IABS(idxcor(icor)) - ipot*10000000 - iother*1000000
            idxoth(i) = 1
 
            idsec = IDBam(i)
 
C reduction of particle momentum by corresponding nuclear potential
C (this applies only if Fermi-momenta are requested)
 
            IF ( LFErmi ) THEN
 
C   Lorentz-transformation into the rest system of the selected nucleus
               imode = -ipot - 1
               CALL DT_LTRANS(PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i),
     &                        psec(1),psec(2),psec(3),psec(4),idsec,
     &                        imode)
               pseco = SQRT(psec(1)**2+psec(2)**2+psec(3)**2)
               amsec = SQRT(ABS((psec(4)-pseco)*(psec(4)+pseco)))
               jpmod = 0
 
               chklev = TINY3
               IF ( (EPRoj.GE.1.0D4) .AND. (idsec.EQ.7) ) chklev = TINY1
               IF ( EPRoj.GE.2.0D6 ) chklev = 1.0D0
               IF ( ABS(amsec-AAM(idsec)).GT.chklev ) THEN
 
                  IF ( IOUlev(3).GT.0 .AND. LPRi.GT.4 )
     &                 WRITE (LOUt,99010) i , NEVhkk , idsec , amsec , 
     &                 AAM(idsec)
99010             FORMAT (1X,'RESNCL: inconsistent mass of particle',
     &                    ' at entry ',I5,' (evt.',I8,')',/,' IDSEC: ',
     &                    I4,'   AMSEC: ',E12.3,'  AAM(IDSEC): ',E12.3,
     &                    /)
                  GOTO 220
               END IF
 
               DO k = 1 , 4
                  psec0(k) = psec(k)
               END DO
 
C   the correction for nuclear potential effects is applied to as many
C   p/n as many nucleons were wounded; the momenta of other final state
C   particles are corrected only if they materialize inside the corresp.
C   nucleus (here: NOBAM = 1 part. outside proj., = 2 part. outside targ
C   = 3 part. outside proj. and targ., >=10 in overlapping region)
               IF ( (idsec.EQ.1) .OR. (idsec.EQ.8) ) THEN
                  IF ( ipot.EQ.1 ) THEN
                     IF ( (jpw.GT.0) .AND. (iother.EQ.0) ) THEN
C      this is most likely a wounded nucleon
C*test
C                    RDIST = SQRT((VHKK(1,IPW(JPW))/FM2MM)**2
C    &                           +(VHKK(2,IPW(JPW))/FM2MM)**2
C    &                           +(VHKK(3,IPW(JPW))/FM2MM)**2)
C                    RAD   = RNUCLE*DBLE(IP)**ONETHI
C                    FDEN  = 1.4D0*DT_DENSIT(IP,RDIST,RAD)
C                    PSEC(4) = PSEC(4)-SCPOT*FDEN*EPOT(IPOT,IDSEC)
C*
                        psec(4) = psec(4) - scpot*EPOt(ipot,idsec)
                        jpw = jpw - 1
                        jpmod = 1
C      correct only if part. was materialized inside nucleus
C      and if it is ouside the overlapping region
                     ELSE IF ( (NOBam(i).NE.1) .AND. (NOBam(i).LT.3) )
     &                  THEN
                        psec(4) = psec(4) - scpot*EPOt(ipot,idsec)
                        jpmod = 1
                     END IF
                  ELSE IF ( ipot.EQ.2 ) THEN
                     IF ( (jtw.GT.0) .AND. (iother.EQ.0) ) THEN
C      this is most likely a wounded nucleon
C*test
C                    RDIST = SQRT((VHKK(1,ITW(JTW))/FM2MM)**2
C    &                           +(VHKK(2,ITW(JTW))/FM2MM)**2
C    &                           +(VHKK(3,ITW(JTW))/FM2MM)**2)
C                    RAD   = RNUCLE*DBLE(IT)**ONETHI
C                    FDEN  = 1.4D0*DT_DENSIT(IT,RDIST,RAD)
C                    PSEC(4) = PSEC(4)-SCPOT*FDEN*EPOT(IPOT,IDSEC)
C*
                        psec(4) = psec(4) - scpot*EPOt(ipot,idsec)
                        jtw = jtw - 1
                        jpmod = 1
C      correct only if part. was materialized inside nucleus
                     ELSE IF ( (NOBam(i).NE.2) .AND. (NOBam(i).LT.3) )
     &                  THEN
                        psec(4) = psec(4) - scpot*EPOt(ipot,idsec)
                        jpmod = 1
                     END IF
                  END IF
               ELSE IF ( (NOBam(i).NE.ipot) .AND. (NOBam(i).LT.3) ) THEN
                  psec(4) = psec(4) - scpot*EPOt(ipot,idsec)
                  jpmod = 1
               END IF
 
               IF ( Nloop.EQ.1 ) THEN
C Coulomb energy correction:
C the treatment of Coulomb potential correction is similar to the
C one for nuclear potential
                  IF ( idsec.EQ.1 ) THEN
                     IF ( (ipot.EQ.1) .AND. (jpcw.GT.0) ) THEN
                        jpcw = jpcw - 1
                     ELSE IF ( (ipot.EQ.2) .AND. (jtcw.GT.0) ) THEN
                        jtcw = jtcw - 1
                     ELSE IF ( (NOBam(i).EQ.ipot) .OR. (NOBam(i).EQ.3) )
     &                  THEN
                        GOTO 210
                     END IF
                  ELSE IF ( (NOBam(i).EQ.ipot) .OR. (NOBam(i).EQ.3) )
     &                      THEN
                     GOTO 210
                  END IF
                  IF ( IICh(idsec).EQ.1 ) THEN
C    pos. particles: check if they are able to escape Coulomb potential
                     IF ( psec(4).LT.amsec+ETAcou(ipot) ) THEN
                        ISThkk(i) = 14 + ipot
                        IF ( ISThkk(i).EQ.15 ) THEN
                           DO k = 1 , 4
                              PHKk(k,i) = psec0(k)
                              TRClpr(k) = TRClpr(k) + psec0(k)
                           END DO
                           IF ( (idsec.EQ.1) .OR. (idsec.EQ.8) )
     &                        NPW = NPW - 1
                           IF ( idsec.EQ.1 ) NPCw = NPCw - 1
                        ELSE IF ( ISThkk(i).EQ.16 ) THEN
                           DO k = 1 , 4
                              PHKk(k,i) = psec0(k)
                              TRClta(k) = TRClta(k) + psec0(k)
                           END DO
                           IF ( (idsec.EQ.1) .OR. (idsec.EQ.8) )
     &                        NTW = NTW - 1
                           IF ( idsec.EQ.1 ) NTCw = NTCw - 1
                        END IF
                        GOTO 250
                     END IF
                  ELSE IF ( IICh(idsec).EQ.-1 ) THEN
C    neg. particles: decrease energy by Coulomb-potential
                     psec(4) = psec(4) - ETAcou(ipot)
                     jpmod = 1
                  END IF
               END IF
 
 
 210           IF ( psec(4).LT.amsec ) THEN
 
                  IF ( IOUlev(6).GT.0 .AND. LPRi.GT.4 )
     &                 WRITE (LOUt,99020) i , idsec , psec(4) , amsec
99020             FORMAT (1X,'KKINC: particle at DTEVT1-pos. ',I5,
     &                    ' is not allowed to escape nucleus',/,8X,
     &                    'id : ',I3,'   reduced energy: ',E15.4,
     &                    '   mass: ',E12.3)
                  ISThkk(i) = 14 + ipot
                  IF ( ISThkk(i).EQ.15 ) THEN
                     DO k = 1 , 4
                        PHKk(k,i) = psec0(k)
                        TRClpr(k) = TRClpr(k) + psec0(k)
                     END DO
                     IF ( (idsec.EQ.1) .OR. (idsec.EQ.8) ) NPW = NPW - 1
                     IF ( idsec.EQ.1 ) NPCw = NPCw - 1
                  ELSE IF ( ISThkk(i).EQ.16 ) THEN
                     DO k = 1 , 4
                        PHKk(k,i) = psec0(k)
                        TRClta(k) = TRClta(k) + psec0(k)
                     END DO
                     IF ( (idsec.EQ.1) .OR. (idsec.EQ.8) ) NTW = NTW - 1
                     IF ( idsec.EQ.1 ) NTCw = NTCw - 1
                  END IF
                  GOTO 250
               END IF
 
               IF ( jpmod.EQ.1 ) THEN
                  psecn = SQRT((psec(4)-amsec)*(psec(4)+amsec))
C 4-momentum after correction for nuclear potential
                  DO k = 1 , 3
                     psec(k) = psec(k)*psecn/pseco
                  END DO
 
C store recoil momentum from particles escaping the nuclear potentials
                  DO k = 1 , 4
                     IF ( ipot.EQ.1 ) THEN
                        TRClpr(k) = TRClpr(k) + psec0(k) - psec(k)
                     ELSE IF ( ipot.EQ.2 ) THEN
                        TRClta(k) = TRClta(k) + psec0(k) - psec(k)
                     END IF
                  END DO
 
C transform momentum back into n-n cms
                  imode = ipot + 1
                  CALL DT_LTRANS(psec(1),psec(2),psec(3),psec(4),
     &               PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i),idsec,
     &               imode)
               END IF
 
            END IF
 
 220        DO k = 1 , 4
               pfsp(k) = pfsp(k) + PHKk(k,i)
            END DO
 
 250     END DO
 
         DO i = NPOint(4) , NHKk
            IF ( (ISThkk(i).EQ.1) .AND. (idxoth(i).LT.0) ) THEN
               pfsp(1) = pfsp(1) + PHKk(1,i)
               pfsp(2) = pfsp(2) + PHKk(2,i)
               pfsp(3) = pfsp(3) + PHKk(3,i)
               pfsp(4) = pfsp(4) + PHKk(4,i)
            END IF
         END DO
 
         DO k = 1 , 5
            PRClpr(k) = TRClpr(k)
            PRClta(k) = TRClta(k)
         END DO
 
         IF ( (IP.EQ.1) .AND. (IT.GT.1) .AND. LFErmi ) THEN
C hadron-nucleus interactions: get residual momentum from energy-
C momentum conservation
            DO k = 1 , 4
               PRClpr(k) = ZERO
               PRClta(k) = PINipr(k) + PINita(k) - pfsp(k)
            END DO
         ELSE
C nucleus-hadron, nucleus-nucleus: get residual momentum from
C accumulated recoil momenta of particles leaving the spectators
C   transform accumulated recoil momenta of residual nuclei into
C   n-n cms
            pzi = PRClpr(3)
            pei = PRClpr(4)
            CALL DT_LTNUC(pzi,pei,PRClpr(3),PRClpr(4),2)
            pzi = PRClta(3)
            pei = PRClta(4)
            CALL DT_LTNUC(pzi,pei,PRClta(3),PRClta(4),3)
C        IF (IP.GT.1) THEN
            PRClpr(3) = PRClpr(3) + PINipr(3)
            PRClpr(4) = PRClpr(4) + PINipr(4)
C        ENDIF
            IF ( IT.GT.1 ) THEN
               PRClta(3) = PRClta(3) + PINita(3)
               PRClta(4) = PRClta(4) + PINita(4)
            END IF
         END IF
 
C check momenta of residual nuclei
         IF ( LEMcck ) THEN
            CALL DT_EVTEMC(-PINipr(1),-PINipr(2),-PINipr(3),-PINipr(4),
     &                     1,idum,idum)
            CALL DT_EVTEMC(-PINita(1),-PINita(2),-PINita(3),-PINita(4),
     &                     2,idum,idum)
            CALL DT_EVTEMC(PRClpr(1),PRClpr(2),PRClpr(3),PRClpr(4),2,
     &                     idum,idum)
            CALL DT_EVTEMC(PRClta(1),PRClta(2),PRClta(3),PRClta(4),2,
     &                     idum,idum)
            CALL DT_EVTEMC(pfsp(1),pfsp(2),pfsp(3),pfsp(4),2,idum,idum)
C*sr 19.12. changed to avoid output when used with phojet
C        CHKLEV = TINY3
            chklev = TINY1
            CALL DT_EVTEMC(dum,dum,dum,chklev,-1,501,irej1)
C        IF ((NEVHKK.EQ.409).OR.(NEVHKK.EQ.460).OR.(NEVHKK.EQ.765))
C    &      CALL DT_EVTOUT(4)
            IF ( irej1.GT.0 ) RETURN
         END IF
         GOTO 99999
      END IF
 
C------- initializations
 
C initialize arrays for residual nuclei
      DO k = 1 , 5
         IF ( k.LE.4 ) pfsp(k) = ZERO
         PINipr(k) = ZERO
         PINita(k) = ZERO
         PRClpr(k) = ZERO
         PRClta(k) = ZERO
         TRClpr(k) = ZERO
         TRClta(k) = ZERO
      END DO
      scpot = ONE
      Nloop = 0
 
C correction of projectile 4-momentum for effective target pot.
C and Coulomb-energy (in case of hadron-nucleus interaction only)
      IF ( (IP.EQ.1) .AND. (IT.GT.1) .AND. LFErmi ) THEN
         epni = Epn
C   Coulomb-energy:
C     positively charged hadron - check energy for Coloumb pot.
         IF ( IICh(IJProj).EQ.1 ) THEN
            thresh = ETAcou(2) + AAM(IJProj)
            IF ( epni.LE.thresh ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99030)
99030          FORMAT (/,1X,'KKINC:  WARNING!  projectile energy',
     &                 ' below Coulomb threshold - event rejected',/)
               ISThkk(1) = 1
               RETURN
            END IF
C     negatively charged hadron - increase energy by Coulomb energy
         ELSE IF ( IICh(IJProj).EQ.-1 ) THEN
            epni = epni + ETAcou(2)
         END IF
         IF ( (IJProj.EQ.1) .OR. (IJProj.EQ.8) ) THEN
C   Effective target potential
Csr 6.6. binding energy only (to avoid negative exc. energies)
C           EPNI = EPNI+EPOT(2,IJPROJ)
            ebipot = EBIndp(2)
            IF ( (IJProj.NE.1) .AND. (ABS(EPOt(2,IJProj)).GT.5.0D-3) )
     &           ebipot = EBIndn(2)
            epni = epni + ABS(ebipot)
C re-initialization of DTLTRA
            dum1 = ZERO
            dum2 = ZERO
            CALL DT_LTINI(IJProj,IJTarg,epni,dum1,dum2,0)
         END IF
      END IF
 
C projectile in n-n cms
      IF ( (IP.LE.1) .AND. (IT.GT.1) ) THEN
         pmass1 = AAM(IJProj)
C* VDM assumption
C         IF (IJPROJ.EQ.7) PMASS1 = AAM(33)
         IF ( IJProj.EQ.7 ) pmass1 = AAM(IJProj) - SQRT(VIRt)
         pmass2 = AAM(1)
         pm1 = SIGN(pmass1**2,pmass1)
         pm2 = SIGN(pmass2**2,pmass2)
         PINipr(4) = (UMO**2-pm2+pm1)/(TWO*UMO)
         PINipr(5) = pmass1
         IF ( pmass1.GT.ZERO ) THEN
            PINipr(3) = SQRT((PINipr(4)-PINipr(5))*(PINipr(4)+PINipr(5))
     &                  )
         ELSE
            PINipr(3) = SQRT(PINipr(4)**2-pm1)
         END IF
         ait = DBLE(IT)
         aitz = DBLE(ITZ)
 
C  A.F.
C        PINITA(5) = AIT*AMUAMU+1.0D-3*ENERGY(AIT,AITZ)
         PINita(5) = ait*AMUC12 + EMVGEV*EXMSAZ(ait,aitz,.TRUE.,izdum)
 
         CALL DT_LTNUC(ZERO,PINita(5),PINita(3),PINita(4),3)
      ELSE IF ( (IP.GT.1) .AND. (IT.LE.1) ) THEN
         pmass1 = AAM(1)
         pmass2 = AAM(IJTarg)
         pm1 = SIGN(pmass1**2,pmass1)
         pm2 = SIGN(pmass2**2,pmass2)
         PINita(4) = (UMO**2-pm1+pm2)/(TWO*UMO)
         PINita(5) = pmass2
         PINita(3) = -SQRT((PINita(4)-PINita(5))*(PINita(4)+PINita(5)))
         aip = DBLE(IP)
         aipz = DBLE(IPZ)
C  A.F.
C        PINIPR(5) = AIP*AMUAMU+1.0D-3*ENERGY(AIP,AIPZ)
         PINipr(5) = aip*AMUC12 + EMVGEV*EXMSAZ(aip,aipz,.TRUE.,izdum)
 
         CALL DT_LTNUC(ZERO,PINipr(5),PINipr(3),PINipr(4),2)
      ELSE IF ( (IP.GT.1) .AND. (IT.GT.1) ) THEN
         aip = DBLE(IP)
         aipz = DBLE(IPZ)
C  A.F.
C        PINIPR(5) = AIP*AMUAMU+1.0D-3*ENERGY(AIP,AIPZ)
         PINipr(5) = aip*AMUC12 + EMVGEV*EXMSAZ(aip,aipz,.TRUE.,izdum)
 
         CALL DT_LTNUC(ZERO,PINipr(5),PINipr(3),PINipr(4),2)
         ait = DBLE(IT)
         aitz = DBLE(ITZ)
 
C  A.F.
C        PINITA(5) = AIT*AMUAMU+1.0D-3*ENERGY(AIT,AITZ)
         PINita(5) = ait*AMUC12 + EMVGEV*EXMSAZ(ait,aitz,.TRUE.,izdum)
 
         CALL DT_LTNUC(ZERO,PINita(5),PINita(3),PINita(4),3)
      END IF
 
C
C     Pinipr,Pinita projectile/target 4-momenta in the n-n cms system
C
 
 
99999 END SUBROUTINE
