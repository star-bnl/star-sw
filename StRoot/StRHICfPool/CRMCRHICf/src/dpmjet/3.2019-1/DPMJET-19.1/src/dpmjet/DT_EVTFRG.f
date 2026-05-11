
      SUBROUTINE DT_EVTFRG(Kmode,Nfrg,Npymem,Irej)
 
C***********************************************************************
C Hadronization of chains in DTEVT1.                                   *
C                                                                      *
C Input:                                                               *
C   KMODE = 1   hadronization of PHOJET-chains (id=77xxx)              *
C         = 2   hadronization of DTUNUC-chains (id=88xxx)              *
C   NFRG  if KMODE = 1 : upper index of PHOJET-scatterings to be       *
C                        hadronized with one PYEXEC call               *
C         if KMODE = 2 : max. number of DTUNUC-chains to be hadronized *
C                        with one PYEXEC call                          *
C Output:                                                              *
C   NPYMEM      number of entries in JETSET-common after hadronization *
C   IREJ        rejection flag                                         *
C                                                                      *
C This version dated 17.09.00 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION chklev , dum , ONE , pe , pt1 , pt2 , px , py , 
     &                 PYP , pz , rqlun , TINY1 , TINY10 , TINY3 , ZERO
      INTEGER i , id , idchk , IDT_IPDGHA , idum , idxmor , iflg , 
     &        ifrg , ih1 , ih2 , ihismo , ii , ij , ijk , ijoin , 
     &        iniemc , ip , ipje , Irej
#ifndef FOR_CORSIKA
      INTEGER isdrn1 , isdrn2 , iseed1 , iseed2
#endif
      INTEGER irej1 , irej3 , ish , 
     &        isjoin , ismor , istsrg , iststg , jdaug , k1 , k2 , 
     &        kfmor , kk , Kmode
      INTEGER mo , mode , MXJOIN , naccep , nend , Nfrg , nlines , 
     &        npje , Npymem
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY3=1.0D-3,TINY1=1.0D-1)
      PARAMETER (ONE=1.0D0,ZERO=0.0D0)
 
      LOGICAL laccep
 
      PARAMETER (MXJOIN=400)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C statistics
      INCLUDE 'inc/dtsta1'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C phojet
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C jetset
 
      INCLUDE 'inc/pydat1'
 
      INCLUDE 'inc/pyjets'
 
      INTEGER PYK
 
      DIMENSION ijoin(MXJOIN) , isjoin(MXJOIN) , ihismo(8000) , 
     &          iflg(4000)
      COMMON /DBGPRE/ LDBgpr
      LOGICAL LDBgpr
 
      IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
         WRITE (77,*)
         WRITE (77,'(A,5I6)') ' EVTFRG IN:' , Kmode , Nfrg , Npymem , 
     &                        Irej , NHKk
         CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
         WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
         WRITE (LOUT,*)
         WRITE (LOUT,'(A,5I6)')
     &    ' DT_EVTFRG IN:',KMODE,NFRG,NPYMEM,IREJ,NHKK
#endif
      END IF
      mode = Kmode
      iststg = 7
      IF ( mode.NE.1 ) iststg = 8
      Irej = 0
 
      ip = 0
      ish = 0
      iniemc = 1
      nend = NHKk
      naccep = 0
      ifrg = 0
      IF ( NPOint(4).LE.NPOint(3) ) NPOint(4) = NHKk + 1
      DO i = NPOint(3) , nend
C sr 14.02.00: seems to be not necessary anymore, commented
C        LACCEP = ((NOBAM(I).EQ.0).AND.(MODE.EQ.1)).OR.
C    &            ((NOBAM(I).NE.0).AND.(MODE.EQ.2))
         laccep = .TRUE.
C pick up chains from dtevt1
         idchk = IDHkk(i)/10000
         IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
            WRITE (77,'(A,6I6)') ' EVTFRG LACCEP:' , i , NPOint(3) , 
     &             NPOint(4) , idchk , istsrg , NHKk
            CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
            WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
            WRITE (LOUT,'(A,6I6)')
     &      ' DT_EVTFRG LACCEP:',I,NPOINT(3),NPOINT(4),IDCHK,ISTSRG,NHKK
#endif
         END IF
         IF ( (idchk.EQ.iststg) .AND. laccep ) THEN
            IF ( idchk.EQ.7 ) THEN
               ipje = IDHkk(i) - idchk*10000
               IF ( ipje.NE.ifrg ) THEN
                  ifrg = ipje
                  IF ( ifrg.GT.Nfrg ) GOTO 100
               END IF
            ELSE
               ipje = 1
               ifrg = ifrg + 1
               IF ( ifrg.GT.Nfrg ) THEN
                  Nfrg = -1
                  GOTO 100
               END IF
            END IF
C   statistics counter
C           IF (IDCH(I).LE.8)
C    &         ICCHAI(2,IDCH(I)) = ICCHAI(2,IDCH(I))+1
C           IF (IDRES(I).NE.0) ICRES(IDCH(I)) = ICRES(IDCH(I))+1
C special treatment for small chains already corrected to hadrons
            IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
               WRITE (77,'(A,4I6)') ' EVTFRG NO GOTO 16:' , IDRes(i) , 
     &                ifrg , Nfrg , NHKk
               CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
               WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
               WRITE (LOUT,'(A,4I6)')
     &         ' DT_EVTFRG NO GOTO 16:',IDRES(I),IFRG,NFRG,NHKK
#endif
            END IF
            IF ( IDRes(i).NE.0 ) THEN
               IF ( IDRes(i).EQ.11 ) THEN
                  id = IDXres(i)
               ELSE
                  id = IDT_IPDGHA(IDXres(i))
               END IF
               IF ( LEMcck ) THEN
                  CALL DT_EVTEMC(PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i)
     &               ,iniemc,idum,idum)
                  IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
                     WRITE (77,'(A,4I6,/,1P,4G23.15)')
     &                      ' EVTFRG EVTEMC:' , i , IDHkk(i) , iniemc , 
     &                      NHKk , (PHKk(ijk,i),ijk=1,4)
                     CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
                     WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
                     WRITE (LOUT,'(A,4I6,/,1P,4G23.15)')
     &               ' DT_EVTFRG EVTEMC:',I,IDHKK(I),INIEMC,NHKK,
     &                 (PHKK(IJK,I),IJK=1,4)
#endif
                  END IF
                  iniemc = 2
               END IF
               ip = ip + 1
               IF ( ip.GT.MSTu(4) ) STOP ' NEWFRA 1: IP.GT.MSTU(4) !'
               P(ip,1) = PHKk(1,i)
               P(ip,2) = PHKk(2,i)
               P(ip,3) = PHKk(3,i)
               P(ip,4) = PHKk(4,i)
               P(ip,5) = PHKk(5,i)
               K(ip,1) = 1
               K(ip,2) = id
               K(ip,3) = 0
               K(ip,4) = 0
               K(ip,5) = 0
               IHIst(2,i) = 10000*ipje + ip
               IF ( IHIst(1,i).LE.-100 ) THEN
                  ish = ish + 1
                  IF ( ish.GT.MXJOIN ) THEN
                     WRITE (6,*) 'ISH > MXJOIN !'
                     GOTO 200
                  END IF
                  isjoin(ish) = i
               END IF
               N = ip
               ihismo(ip) = i
            ELSE
               ij = 0
               DO kk = JMOhkk(1,i) , JMOhkk(2,i)
                  IF ( LEMcck ) THEN
                     CALL DT_EVTEMC(PHKk(1,kk),PHKk(2,kk),PHKk(3,kk),
     &                  PHKk(4,kk),iniemc,idum,idum)
                     IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
                        WRITE (77,'(A,4I6,/,1P,4G23.15)')
     &                      ' EVTFRG EVTEMC-2:' , kk , IDHkk(kk) , 
     &                     iniemc , NHKk , (PHKk(ijk,kk),ijk=1,4)
                        CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
                        WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
                        WRITE (LOUT,'(A,4I6,/,1P,4G23.15)')
     &                  ' DT_EVTFRG EVTEMC-2:',KK,IDHKK(KK),INIEMC,NHKK,
     &                    (PHKK(IJK,KK),IJK=1,4)
#endif
                     END IF
                     CALL DT_EVTFLC(IDHkk(kk),1,iniemc,idum,idum)
                     IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
                        WRITE (77,'(A,4I6)') ' EVTFRG EVTFLC:' , kk , 
     &                     IDHkk(kk) , iniemc , NHKk
                        CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
                        WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
                        WRITE (LOUT,'(A,4I6)')
     &                  ' DT_EVTFRG EVTFLC:',KK,IDHKK(KK),INIEMC,NHKK
#endif
                     END IF
                     iniemc = 2
                  END IF
                  id = IDHkk(kk)
                  IF ( id.EQ.0 ) id = 21
C                  PTOT = SQRT(PHKK(1,KK)**2+PHKK(2,KK)**2+PHKK(3,KK)**2)
C                  AM0  = SQRT(ABS((PHKK(4,KK)-PTOT)*(PHKK(4,KK)+PTOT)))
 
C                  AMRQ   = PYMASS(ID)
 
C                  AMDIF2 = (AM0-AMRQ)*(AM0+AMRQ)
C                  IF ((ABS(AMDIF2).GT.TINY3).AND.(PTOT.GT.ZERO).AND.
C     &                (ABS(IDIFF).EQ.0)) THEN
CC                    WRITE(LOUT,*)'here: ',NEVHKK,AM0,AMRQ
C                     DELTA      = -AMDIF2/(2.0D0*(PHKK(4,KK)+PTOT))
C                     PHKK(4,KK) = PHKK(4,KK)+DELTA
C                     PTOT1      = PTOT-DELTA
C                     PHKK(1,KK) = PHKK(1,KK)*PTOT1/PTOT
C                     PHKK(2,KK) = PHKK(2,KK)*PTOT1/PTOT
C                     PHKK(3,KK) = PHKK(3,KK)*PTOT1/PTOT
C                     PHKK(5,KK) = AMRQ
C                  ENDIF
                  ip = ip + 1
                  IF ( ip.GT.MSTu(4) ) STOP ' NEWFRA 2: IP.GT.MSTU(4) !'
                  P(ip,1) = PHKk(1,kk)
                  P(ip,2) = PHKk(2,kk)
                  P(ip,3) = PHKk(3,kk)
                  P(ip,4) = PHKk(4,kk)
                  P(ip,5) = PHKk(5,kk)
                  K(ip,1) = 1
                  K(ip,2) = id
                  K(ip,3) = 0
                  K(ip,4) = 0
                  K(ip,5) = 0
                  IHIst(2,kk) = 10000*ipje + ip
                  IF ( IHIst(1,kk).LE.-100 ) THEN
                     ish = ish + 1
                     IF ( ish.GT.MXJOIN ) THEN
                        WRITE (6,*) 'ISH > MXJOIN !'
                        GOTO 200
                     END IF
                     isjoin(ish) = kk
                  END IF
                  ij = ij + 1
                  IF ( ij.GT.MXJOIN ) THEN
                     WRITE (6,*) 'IJ > MXJOIN !'
                     GOTO 200
                  END IF
                  ijoin(ij) = ip
                  ihismo(ip) = i
               END DO
               N = ip
C join the two-parton system
 
               CALL PYJOIN(ij,ijoin)
               IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
                  WRITE (77,'(A,2I6)') ' EVTFRG PYJOIN:' , ij , NHKk
                  CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
                  WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
                  WRITE (LOUT,'(A,2I6)')
     &            ' EVTFRG PYJOIN:',IJ,NHKK
#endif
               END IF
            END IF
            IDHkk(i) = 99999
         END IF
      END DO
 100  N = ip
 
      IF ( ip.GT.0 ) THEN
 
C final state parton shower
         DO npje = 1 , ipje
            IF ( (MCGene.EQ.2) .AND. (ish.GE.2) ) THEN
               IF ( (ISWmdl(8).EQ.1) .OR. (ISWmdl(8).EQ.3) ) THEN
                  DO k1 = 1 , ish
                     IF ( isjoin(k1).NE.0 ) THEN
                        i = isjoin(k1)
                        IF ( (IPAmdl(102).NE.1) .OR. 
     &                       (IHIst(1,i).EQ.-100) ) THEN
                           ih1 = IHIst(2,i)/10000
                           IF ( ih1.EQ.npje ) THEN
                              ih1 = IHIst(2,i) - ih1*10000
                              DO k2 = k1 + 1 , ish
                               IF ( isjoin(k2).NE.0 ) THEN
                               ii = isjoin(k2)
                               ih2 = IHIst(2,ii)/10000
                               IF ( ih2.EQ.npje ) THEN
                               ih2 = IHIst(2,ii) - ih2*10000
                               IF ( IHIst(1,i).EQ.IHIst(1,ii) ) THEN
                               pt1 = SQRT(PHKk(1,ii)**2+PHKk(2,ii)**2)
                               pt2 = SQRT(PHKk(1,i)**2+PHKk(2,i)**2)
 
                               rqlun = MIN(pt1,pt2)
                               CALL PYSHOW(ih1,ih2,rqlun)
                               IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
                               WRITE (77,'(A,2I6,1P,G23.15)')
     &                             ' EVTFRG PYSHOW:' , ih1 , ih2 , rqlun
                               CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
                               WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
                               WRITE (LOUT,'(A,2I6,1P,G23.15)')
     &                        ' EVTFRG PYSHOW:',IH1,IH2,RQLUN
#endif
                               END IF
 
                               isjoin(k1) = 0
                               isjoin(k2) = 0
                               GOTO 105
                               END IF
                               END IF
                               END IF
                              END DO
                           END IF
                        END IF
                     END IF
 105              END DO
               END IF
            END IF
         END DO
 
         CALL DT_INITJS(mode)
         IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
            WRITE (77,'(A,2I6)') ' EVTFRG INITJS:' , mode , NHKk
            CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
            WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
            WRITE (LOUT,'(A,2I6)')
     &      ' EVTFRG INITJS:',MODE,NHKK
#endif
         END IF
C hadronization
 
         CALL PYEXEC
         IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
            WRITE (77,'(A,2I6)') ' EVTFRG PYEXEC:' , MSTu(24) , NHKk
            CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
            WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
            WRITE (LOUT,'(A,2I6)')
     &      ' EVTFRG PYEXEC:',MSTU(24),NHKK
#endif
         END IF
 
         IF ( MSTu(24).NE.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) ' JETSET-reject at event' , 
     &           NEVhkk , MSTu(24) , Kmode
C           CALL DT_EVTOUT(4)
 
C           CALL PYLIST(2)
 
            GOTO 200
         END IF
 
C   number of entries in LUJETS
 
         nlines = PYK(0,1)
 
         Npymem = nlines
 
         DO i = 1 , nlines
            iflg(i) = 0
         END DO
 
         DO ii = 1 , nlines
 
            IF ( (PYK(ii,7).EQ.1) .AND. (iflg(ii).NE.1) ) THEN
 
C  pick up mother resonance if possible and put it together with
C  their decay-products into the common
               idxmor = K(ii,3)
               IF ( (idxmor.GE.1) .AND. (idxmor.LE.MAXLND) ) THEN
                  kfmor = K(idxmor,2)
                  ismor = K(idxmor,1)
               ELSE
                  kfmor = 91
                  ismor = 1
               END IF
               IF ( (kfmor.NE.91) .AND. (kfmor.NE.92) .AND. 
     &              (kfmor.NE.94) .AND. (ismor.EQ.11) ) THEN
                  id = K(idxmor,2)
 
                  mo = ihismo(PYK(idxmor,15))
                  px = PYP(idxmor,1)
                  py = PYP(idxmor,2)
                  pz = PYP(idxmor,3)
                  pe = PYP(idxmor,4)
 
                  CALL DT_EVTPUT(2,id,mo,0,px,py,pz,pe,0,0,0)
                  iflg(idxmor) = 1
                  mo = NHKk
                  DO jdaug = K(idxmor,4) , K(idxmor,5)
 
                     IF ( PYK(jdaug,7).EQ.1 ) THEN
                        id = PYK(jdaug,8)
                        px = PYP(jdaug,1)
                        py = PYP(jdaug,2)
                        pz = PYP(jdaug,3)
                        pe = PYP(jdaug,4)
 
                        CALL DT_EVTPUT(1,id,mo,0,px,py,pz,pe,0,0,0)
                        IF ( LEMcck ) THEN
 
                           px = -PYP(jdaug,1)
                           py = -PYP(jdaug,2)
                           pz = -PYP(jdaug,3)
                           pe = -PYP(jdaug,4)
 
                           CALL DT_EVTEMC(px,py,pz,pe,2,idum,idum)
                        END IF
                        iflg(jdaug) = 1
                     END IF
                  END DO
               ELSE
C  there was no mother resonance
 
                  mo = ihismo(PYK(ii,15))
                  id = PYK(ii,8)
                  px = PYP(ii,1)
                  py = PYP(ii,2)
                  pz = PYP(ii,3)
                  pe = PYP(ii,4)
 
                  CALL DT_EVTPUT(1,id,mo,0,px,py,pz,pe,0,0,0)
                  IF ( LEMcck ) THEN
 
                     px = -PYP(ii,1)
                     py = -PYP(ii,2)
                     pz = -PYP(ii,3)
                     pe = -PYP(ii,4)
 
                     CALL DT_EVTEMC(px,py,pz,pe,2,idum,idum)
                  END IF
               END IF
            END IF
         END DO
         IF ( LDBgpr ) THEN
#ifndef FOR_CORSIKA
            WRITE (77,'(A,2I6)') ' EVTFRG 13:' , nlines , NHKk
            CALL FLRNOC(isdrn1,isdrn2,iseed1,iseed2)
            WRITE (77,'(2X,2Z8)') iseed1 , iseed2
#else
            WRITE (LOUT,'(A,2I6)')
     &      ' EVTFRG 13:',NLINES,NHKK
#endif
         END IF
         IF ( LEMcck ) THEN
            chklev = TINY1
            CALL DT_EVTEMC(dum,dum,dum,chklev,-1,6,irej1)
C           IF (IREJ1.NE.0) CALL DT_EVTOUT(4)
         END IF
 
C global energy-momentum & flavor conservation check
C*sr 16.5. this check is skipped in case of phojet-treatment
         IF ( MCGene.EQ.1 ) CALL DT_EMC2(9,10,0,0,0,3,1,0,0,0,0,3,4,12,
     &        irej3)
 
C update statistics-counter for diffraction
C        IF (IFLAGD.NE.0) THEN
C           ICDIFF(1) = ICDIFF(1)+1
C           IF (IFLAGD.EQ. 1) ICDIFF(2) = ICDIFF(2)+1
C           IF (IFLAGD.EQ. 2) ICDIFF(3) = ICDIFF(3)+1
C           IF (IFLAGD.EQ.-1) ICDIFF(4) = ICDIFF(4)+1
C           IF (IFLAGD.EQ.-2) ICDIFF(5) = ICDIFF(5)+1
C        ENDIF
 
      END IF
 
      RETURN
 
 200  Irej = 1
      END SUBROUTINE
