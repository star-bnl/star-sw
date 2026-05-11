
      SUBROUTINE DT_EVTFRG2(Kmode,Nfrg,Npymem,Irej)
 
C***********************************************************************
C Hadronization of chains in DTEVT1.                                   *
C                                                                      *
C Input:                                                               *
C   KMODE = 1   hadronization of PHOJET-chains (id=77xxx)              *
C         = 2   hadronization of DTUNUC-chains (id=66xxx)              *
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
      DOUBLE PRECISION chklev , dum , ONE , parrj21 , parrj23 , 
     &                 parrj24 , parrj41 , parrj42 , pe , pt1 , pt2 , 
     &                 px , py , PYP , pz , rqlun , TINY1 , TINY10
      DOUBLE PRECISION TINY3 , ZERO
      INTEGER i , id , idchk , IDT_IPDGHA , idum , idxmor , iesss1 , 
     &        iesss2 , iesss3 , iesss4 , iesss5 , iesss6 , iesss7 , 
     &        iesss8 , iesss9 , iflg , ifrg , ih1 , ih2 , ihismo
      INTEGER ii , ij , ijoin , iniemc , ip , ipje , Irej , irej1 , 
     &        irej3 , ish , isjoin , ismor , iststg , jdaug , k1 , k2 , 
     &        kfmor
      INTEGER kk , Kmode , mo , mode , MXJOIN , naccep , nend , Nfrg , 
     &        nlines , npje , Npymem
      SAVE 
      PARAMETER (TINY10=1.0D-10,TINY3=1.0D-3,TINY1=1.0D-1)
      PARAMETER (ONE=1.0D0,ZERO=0.0D0)
 
      LOGICAL laccep
 
      PARAMETER (MXJOIN=300)
 
      INCLUDE 'inc/dtflka'
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
      DATA iesss1 , iesss2 , iesss4 , iesss5 , iesss6 , iesss7 , 
     &     iesss8 , iesss9/0 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
 
      mode = Kmode
      iststg = 7
      IF ( mode.NE.1 ) iststg = 6
      Irej = 0
 
      ip = 0
      ish = 0
      iniemc = 1
      nend = NHKk
      naccep = 0
      ifrg = 0
      IF ( NPOint(4).LE.NPOint(3) ) NPOint(4) = NHKk + 1
C     WRITE(6,*)' NPOINT 3,4,NEND ',NPOINT(3),NPOINT(4),NEND
C     WRITE(6,*)'ISTSTG ',ISTSTG
      DO i = NPOint(3) , nend
C sr 14.02.00: seems to be not necessary anymore, commented
C        LACCEP = ((NOBAM(I).EQ.0).AND.(MODE.EQ.1)).OR.
C    &            ((NOBAM(I).NE.0).AND.(MODE.EQ.2))
         laccep = .TRUE.
C pick up chains from dtevt1
         idchk = IDHkk(i)/10000
         IF ( (idchk.EQ.iststg) .AND. laccep ) THEN
            IF ( idchk.EQ.7 ) THEN
               ipje = IDHkk(i) - idchk*10000
               IF ( ipje.NE.ifrg ) THEN
                  ifrg = ipje
                  IF ( ifrg.GT.Nfrg ) GOTO 100
               END IF
            ELSE IF ( idchk.EQ.6 ) THEN
               ipje = 1
               ifrg = ifrg + 1
C              WRITE(6,*)'I, IDCHK,IFRG,NFRG'
C    *                   ,I, IDCHK,IFRG,NFRG,IDHKK(I)
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
            IF ( IDRes(i).NE.0 ) THEN
               IF ( IDRes(i).EQ.11 ) THEN
                  id = IDXres(i)
               ELSE
                  id = IDT_IPDGHA(IDXres(i))
               END IF
               IF ( LEMcck ) THEN
                  CALL DT_EVTEMC(PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i)
     &               ,iniemc,idum,idum)
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
                     CALL DT_EVTFLC(IDHkk(kk),1,iniemc,idum,idum)
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
 
C           CALL PYLIST(2)
               CALL PYJOIN(ij,ijoin)
 
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
C   j.r.12/01            Fragmentation of fused chains
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
         PARj(42) = 1.3D0
         iesss3 = iesss3 + 1
C      IF(IESSS3.LT.3)
C     *WRITE(6,*)' 3 PARJ(21),PARJ(23),PARJ(24),PARJ(41),PARJ(42)',
C     *      PARJ(21),PARJ(23),PARJ(24),PARJ(41),PARJ(42)
CC      IF(IESSS3.LT.3)
C     * WRITE(6,*)'3 PARJ(5),PARJ(19)',
C     *      PARJ(5),PARJ(19)
C hadronization
 
         CALL PYEXEC
C           CALL PYLIST(2)
 
         IF ( MSTu(24).NE.0 ) THEN
            WRITE (LOUt,*) ' JETSET-reject at event' , NEVhkk , MSTu(24)
     &                     , Kmode
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
