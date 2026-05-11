
      SUBROUTINE DT_INUCAS(It,Ip,Idxcas,Lcas,Ncas,Irej)
 
C***********************************************************************
C Formation zone supressed IntraNUclear CAScade for one final state    *
C particle.                                                            *
C           IT, IP    mass numbers of target, projectile nuclei        *
C           IDXCAS    index of final state particle in DTEVT1          *
C           NCAS =  1 intranuclear cascade in projectile               *
C                = -1 intranuclear cascade in target                   *
C This version dated 18.11.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amt , becas , bgcas , bgta , bimmax , bimnu , 
     &                 bimnu2 , bimpc , bint , coscas , ddistl , del , 
     &                 del0 , del1 , dist , distn , distnu , distp , 
     &                 DLARGE , dstep
      DOUBLE PRECISION dstep1 , dtime , dtime1 , dtimel , DT_RNDM , 
     &                 dum , dumzer , ethr , FM2MM , gacas , OHALF , 
     &                 ONE , pcas , pcas1 , pdif , pe , PHIH , PLOWH , 
     &                 pnuc , posn
      DOUBLE PRECISION posnuc , posp , pot , potlow , pptot , ptocas , 
     &                 px , py , pz , rcaspr , rcasta , rdist , rdistl , 
     &                 rel , rel1 , rnclpr , rnclta , rnuc , RNUCLE , rr
      DOUBLE PRECISION rstep , rstep1 , rtime , rtime1 , sabs , sela , 
     &                 sigab , sigel , sigin , sigtot , stot , tausa1 , 
     &                 tausam , TINY10 , TINY2 , TWOPI , vtxca1 , 
     &                 vtxcas , vtxdst , xcas
      DOUBLE PRECISION xnclta , ycas , ZERO
      INTEGER i , i2 , icas , id , idcas , idcas1 , idnuc , idnuc1 , 
     &        idspe , IDT_ICIHAD , IDT_IPDGHA , IDT_MCHAD , idum , idx , 
     &        Idxcas , idxhkk , idxn , idxp , idxspe , igen
      INTEGER imode , Ip , iproc , Irej , irej1 , is , ist , It , ixr , 
     &        j , j1 , k , mode , Ncas , npauli , nspe , nwtmp
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0)
      PARAMETER (FM2MM=1.0D-12,RNUCLE=1.12D0)
      PARAMETER (TWOPI=6.283185307179586454D+00)
      PARAMETER (PLOWH=0.01D0,PHIH=9.0D0)
 
      LOGICAL labsor , Lcas
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C final state after inc step
      INCLUDE 'inc/dtcapa'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C parameter for intranuclear cascade
      INCLUDE 'inc/dtfoti'
C final state after intranuclear cascade step
      INCLUDE 'inc/dtpaul'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C statistics: residual nuclei
      INCLUDE 'inc/dtsta2'
 
      DIMENSION pcas(2,5) , ptocas(2) , coscas(2,3) , vtxcas(2,4) , 
     &          vtxca1(2,4) , pcas1(5) , pnuc(5) , bgta(4) , bgcas(2) , 
     &          gacas(2) , becas(2) , rnuc(2) , bimpc(2) , vtxdst(3) , 
     &          idxspe(2) , idspe(2) , nwtmp(2)
 
      DATA pdif/0.545D0/
 
      Irej = 0
 
C update counter
      IF ( NINcev(1).NE.NEVhkk ) THEN
         NINcev(1) = NEVhkk
         NINcev(2) = NINcev(2) + 1
      END IF
 
C "BAMJET-index" of this hadron
      idcas = IDBam(Idxcas)
 
C skip gammas, electrons, etc..
      IF ( IDT_MCHAD(idcas).EQ.-1 ) RETURN
 
C Lorentz-trsf. into projectile rest system
      IF ( AAM(idcas).LT.TINY2 ) RETURN
      IF ( Ip.GT.1 ) THEN
         CALL DT_LTRANS(PHKk(1,Idxcas),PHKk(2,Idxcas),PHKk(3,Idxcas),
     &                  PHKk(4,Idxcas),pcas(1,1),pcas(1,2),pcas(1,3),
     &                  pcas(1,4),idcas,-2)
         ptocas(1) = SQRT(pcas(1,1)**2+pcas(1,2)**2+pcas(1,3)**2)
         pcas(1,5) = (pcas(1,4)-ptocas(1))*(pcas(1,4)+ptocas(1))
         IF ( pcas(1,5).GT.ZERO ) THEN
            pcas(1,5) = SQRT(pcas(1,5))
         ELSE
            pcas(1,5) = AAM(idcas)
         END IF
         DO k = 1 , 3
            coscas(1,k) = pcas(1,k)/MAX(ptocas(1),TINY10)
         END DO
C Lorentz-parameters
C   particle rest system --> projectile rest system
         bgcas(1) = ptocas(1)/MAX(pcas(1,5),TINY10)
         gacas(1) = pcas(1,4)/MAX(pcas(1,5),TINY10)
         becas(1) = bgcas(1)/gacas(1)
      ELSE
         DO k = 1 , 5
            pcas(1,k) = ZERO
            IF ( k.LE.3 ) coscas(1,k) = ZERO
         END DO
         ptocas(1) = ZERO
         bgcas(1) = ZERO
         gacas(1) = ZERO
         becas(1) = ZERO
      END IF
C Lorentz-trsf. into target rest system
      IF ( It.GT.1 ) THEN
C LEPTO: final state particles are already in target rest frame
C        IF (MCGENE.EQ.3) THEN
C           PCAS(2,1) = PHKK(1,IDXCAS)
C           PCAS(2,2) = PHKK(2,IDXCAS)
C           PCAS(2,3) = PHKK(3,IDXCAS)
C           PCAS(2,4) = PHKK(4,IDXCAS)
C        ELSE
         CALL DT_LTRANS(PHKk(1,Idxcas),PHKk(2,Idxcas),PHKk(3,Idxcas),
     &                  PHKk(4,Idxcas),pcas(2,1),pcas(2,2),pcas(2,3),
     &                  pcas(2,4),idcas,-3)
C        ENDIF
         ptocas(2) = SQRT(pcas(2,1)**2+pcas(2,2)**2+pcas(2,3)**2)
         pcas(2,5) = (pcas(2,4)-ptocas(2))*(pcas(2,4)+ptocas(2))
         IF ( pcas(2,5).GT.ZERO ) THEN
            pcas(2,5) = SQRT(pcas(2,5))
         ELSE
            pcas(2,5) = AAM(idcas)
         END IF
         DO k = 1 , 3
            coscas(2,k) = pcas(2,k)/MAX(ptocas(2),TINY10)
         END DO
C Lorentz-parameters
C   particle rest system --> target rest system
         bgcas(2) = ptocas(2)/MAX(pcas(2,5),TINY10)
         gacas(2) = pcas(2,4)/MAX(pcas(2,5),TINY10)
         becas(2) = bgcas(2)/gacas(2)
      ELSE
         DO k = 1 , 5
            pcas(2,k) = ZERO
            IF ( k.LE.3 ) coscas(2,k) = ZERO
         END DO
         ptocas(2) = ZERO
         bgcas(2) = ZERO
         gacas(2) = ZERO
         becas(2) = ZERO
      END IF
 
C radii of nuclei (mm) modified by the wall-depth of the Woods-Saxon-
C potential (see CONUCL)
      rnuc(1) = (RPRoj+4.605D0*pdif)*FM2MM
      rnuc(2) = (RTArg+4.605D0*pdif)*FM2MM
C impact parameter (the projectile moving along z)
      bimpc(1) = ZERO
      bimpc(2) = BIMpac*FM2MM
 
C get position of initial hadron in projectile/target rest-syst.
      DO k = 1 , 4
         vtxcas(1,k) = WHKk(k,Idxcas)
         vtxcas(2,k) = VHKk(k,Idxcas)
      END DO
 
      icas = 1
      i2 = 2
      IF ( Ncas.EQ.-1 ) THEN
         icas = 2
         i2 = 1
      END IF
 
      IF ( ptocas(icas).LT.TINY10 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) ptocas
99010    FORMAT (1X,'INUCAS:   warning! zero momentum of initial',
     &           '  hadron ',/,20X,2E12.4)
 
C rejection
         Irej = 1
         GOTO 99999
      END IF
 
C reset spectator flags
      nspe = 0
      idxspe(1) = 0
      idxspe(2) = 0
      idspe(1) = 0
      idspe(2) = 0
 
C formation length (in fm)
C     IF (LCAS) THEN
C        DEL0 = ZERO
C     ELSE
      del0 = TAUfor*bgcas(icas)
      IF ( ITAuve.EQ.1 ) THEN
         amt = pcas(icas,1)**2 + pcas(icas,2)**2 + pcas(icas,5)**2
         del0 = del0*pcas(icas,5)**2/amt
      END IF
C     ENDIF
C   sample from exp(-del/del0)
      del1 = -del0*LOG(MAX(DT_RNDM(del0),TINY10))
C save formation time
      tausa1 = del1/bgcas(icas)
      rel1 = tausa1*bgcas(i2)
 
      del = del1
      tausam = del/bgcas(icas)
      rel = tausam*bgcas(i2)
 
C special treatment for negative particles unable to escape
C nuclear potential (implemented for ap, pi-, K- only)
      labsor = .FALSE.
      IF ( (IICh(idcas).EQ.-1) .AND. (idcas.LT.20) ) THEN
C   threshold energy = nuclear potential + Coulomb potential
C   (nuclear potential for hadron-nucleus interactions only)
         ethr = AAM(idcas) + EPOt(icas,idcas) + ETAcou(icas)
         IF ( pcas(icas,4).LT.ethr ) THEN
            DO k = 1 , 5
               pcas1(k) = pcas(icas,k)
            END DO
C   "absorb" negative particle in nucleus
            CALL DT_ABSORP(idcas,pcas1,Ncas,nspe,idspe,idxspe,0,irej1)
            IF ( irej1.NE.0 ) THEN
               Irej = 1
               GOTO 99999
            ELSE
               IF ( nspe.GE.1 ) labsor = .TRUE.
            END IF
         END IF
      END IF
 
C if the initial particle has not been absorbed proceed with
C "normal" cascade
      IF ( .NOT.labsor ) THEN
 
C   calculate coordinates of hadron at the end of the formation zone
C   transport-time and -step in the rest system where this step is
C   treated
         dstep = del*FM2MM
         dtime = dstep/becas(icas)
         rstep = rel*FM2MM
         IF ( (Ip.GT.1) .AND. (It.GT.1) ) THEN
            rtime = rstep/becas(i2)
         ELSE
            rtime = ZERO
         END IF
C   save step whithout considering the overlapping region
         dstep1 = del1*FM2MM
         dtime1 = dstep1/becas(icas)
         rstep1 = rel1*FM2MM
         IF ( (Ip.GT.1) .AND. (It.GT.1) ) THEN
            rtime1 = rstep1/becas(i2)
         ELSE
            rtime1 = ZERO
         END IF
C   transport to the end of the formation zone in this system
         DO k = 1 , 3
            vtxca1(icas,k) = vtxcas(icas,k) + dstep1*coscas(icas,k)
            vtxca1(i2,k) = vtxcas(i2,k) + rstep1*coscas(i2,k)
            vtxcas(icas,k) = vtxcas(icas,k) + dstep*coscas(icas,k)
            vtxcas(i2,k) = vtxcas(i2,k) + rstep*coscas(i2,k)
         END DO
         vtxca1(icas,4) = vtxcas(icas,4) + dtime1
         vtxca1(i2,4) = vtxcas(i2,4) + rtime1
         vtxcas(icas,4) = vtxcas(icas,4) + dtime
         vtxcas(i2,4) = vtxcas(i2,4) + rtime
 
         IF ( (Ip.GT.1) .AND. (It.GT.1) ) THEN
            xcas = vtxcas(icas,1)
            ycas = vtxcas(icas,2)
            xnclta = BIMpac*FM2MM
            rnclpr = (RPRoj+RNUCLE)*FM2MM
            rnclta = (RTArg+RNUCLE)*FM2MM
C           RNCLPR = (RPROJ+1.605D0*PDIF)*FM2MM
C           RNCLTA = (RTARG+1.605D0*PDIF)*FM2MM
C           RNCLPR = (RPROJ)*FM2MM
C           RNCLTA = (RTARG)*FM2MM
            rcaspr = SQRT(xcas**2+ycas**2)
            rcasta = SQRT((xcas-xnclta)**2+ycas**2)
            IF ( (rcaspr.LT.rnclpr) .AND. (rcasta.LT.rnclta) ) THEN
               IF ( IDCh(Idxcas).EQ.0 ) NOBam(Idxcas) = 3
            END IF
         END IF
 
C   check if particle is already outside of the corresp. nucleus
         rdist = SQRT((vtxcas(icas,1)-bimpc(icas))**2+vtxcas(icas,2)
     &           **2+vtxcas(icas,3)**2)
         IF ( rdist.GE.rnuc(icas) ) THEN
C   here: IDCH is the generation of the final state part. starting
C   with zero for hadronization products
C   flag particles of generation 0 being outside the nuclei after
C   formation time (to be used for excitation energy calculation)
            IF ( (IDCh(Idxcas).EQ.0) .AND. (NOBam(Idxcas).LT.3) )
     &           NOBam(Idxcas) = NOBam(Idxcas) + icas
            GOTO 100
         END IF
         dist = DLARGE
         distp = DLARGE
         distn = DLARGE
         idxp = 0
         idxn = 0
 
C   already here: skip particles being outside HADRIN "energy-window"
C   to avoid wasting of time
         NINchr(icas,1) = NINchr(icas,1) + 1
         IF ( (ptocas(icas).LE.PLOWH) .OR. (ptocas(icas).GE.PHIH) ) THEN
            NINchr(icas,2) = NINchr(icas,2) + 1
C           WRITE(LOUT,1002) IDXCAS,IDCAS,ICAS,PTOCAS(ICAS),NEVHKK
C1002       FORMAT(1X,'INUCAS:   warning! momentum of particle with ',
C    &             'index ',I5,' (id: ',I3,') ',I3,/,11X,'p_tot = ',
C    &             E12.4,', above or below HADRIN-thresholds',I6)
            nspe = 0
            GOTO 100
         END IF
 
         DO idxhkk = 1 , NOInc
            i = IDXinc(idxhkk)
C   scan DTEVT1 for unwounded or excited nucleons
            IF ( (ISThkk(i).EQ.12+icas) .OR. (ISThkk(i).EQ.14+icas) )
     &           THEN
               DO k = 1 , 3
                  IF ( icas.EQ.1 ) THEN
                     vtxdst(k) = WHKk(k,i) - vtxcas(1,k)
                  ELSE IF ( icas.EQ.2 ) THEN
                     vtxdst(k) = VHKk(k,i) - vtxcas(2,k)
                  END IF
               END DO
               posnuc = vtxdst(1)*coscas(icas,1) + vtxdst(2)
     &                  *coscas(icas,2) + vtxdst(3)*coscas(icas,3)
C   check if nucleon is situated in forward direction
               IF ( posnuc.GT.ZERO ) THEN
C   distance between hadron and this nucleon
                  distnu = SQRT(vtxdst(1)**2+vtxdst(2)**2+vtxdst(3)**2)
C   impact parameter
                  bimnu2 = distnu**2 - posnuc**2
                  IF ( bimnu2.LT.ZERO ) THEN
 
                     IF ( LPRi.GT.4 ) WRITE (LOUt,99020) distnu , 
     &                    posnuc , bimnu2
99020                FORMAT (1X,
     &                       'INUCAS:   warning! inconsistent impact',
     &                       '  parameter ',/,20X,3E12.4)
                     GOTO 50
                  END IF
                  bimnu = SQRT(bimnu2)
C   maximum impact parameter to have interaction
                  idnuc = IDT_ICIHAD(IDHkk(i))
                  idnuc1 = IDT_MCHAD(idnuc)
                  idcas1 = IDT_MCHAD(idcas)
                  DO k = 1 , 5
                     pcas1(k) = pcas(icas,k)
                     pnuc(k) = PHKk(k,i)
                  END DO
C Lorentz-parameter for trafo into rest-system of target
                  DO k = 1 , 4
                     bgta(k) = pnuc(k)/MAX(pnuc(5),TINY10)
                  END DO
C transformation of projectile into rest-system of target
                  CALL DT_DALTRA(bgta(4),-bgta(1),-bgta(2),-bgta(3),
     &               pcas1(1),pcas1(2),pcas1(3),pcas1(4),pptot,px,py,pz,
     &               pe)
C*
C                 CALL DT_SIHNIN(IDCAS1,IDNUC1,PPTOT,SIGIN)
C                 CALL DT_SIHNEL(IDCAS1,IDNUC1,PPTOT,SIGEL)
                  dumzer = ZERO
                  CALL DT_XSHN(idcas1,idnuc1,pptot,dumzer,sigtot,sigel)
                  CALL DT_SIHNAB(idcas1,idnuc1,pptot,sigab)
                  IF ( ((idcas1.EQ.13) .OR. (idcas1.EQ.14)) .AND. 
     &                 (pptot.LT.0.15D0) ) sigel = sigel/2.0D0
                  sigin = sigtot - sigel - sigab
C                 SIGTOT = SIGIN+SIGEL+SIGAB
C*
                  bimmax = SQRT(sigtot/(5.0D0*TWOPI))*FM2MM
C   check if interaction is possible
                  IF ( bimnu.LE.bimmax ) THEN
C   get nucleon with smallest distance and kind of interaction
C   (elastic/inelastic)
                     IF ( distnu.LT.dist ) THEN
                        dist = distnu
                        bint = bimnu
                        IF ( idnuc.NE.idspe(1) ) THEN
                           idspe(2) = idspe(1)
                           idxspe(2) = idxspe(1)
                           idspe(1) = idnuc
                        END IF
                        idxspe(1) = i
                        nspe = 1
C*sr
                        sela = sigel
                        sabs = sigab
                        stot = sigtot
C                       IF ((IDCAS.EQ.2).OR.(IDCAS.EQ.9)) THEN
C                          SELA = SIGEL
C                          STOT = SIGIN+SIGEL
C                       ELSE
C                          SELA = SIGEL+0.75D0*SIGIN
C                          STOT = 0.25D0*SIGIN+SELA
C                       ENDIF
C*
                     END IF
                  END IF
               END IF
               distnu = SQRT(vtxdst(1)**2+vtxdst(2)**2+vtxdst(3)**2)
               idnuc = IDT_ICIHAD(IDHkk(i))
               IF ( idnuc.EQ.1 ) THEN
                  IF ( distnu.LT.distp ) THEN
                     distp = distnu
                     idxp = i
                     posp = posnuc
                  END IF
               ELSE IF ( idnuc.EQ.8 ) THEN
                  IF ( distnu.LT.distn ) THEN
                     distn = distnu
                     idxn = i
                     posn = posnuc
                  END IF
               END IF
            END IF
 50      END DO
 
C there is no nucleon for a secondary interaction
         IF ( nspe.EQ.0 ) GOTO 100
 
C        IF ((IDCAS.EQ.13).AND.((PCAS(ICAS,4)-PCAS(ICAS,5)).LT.0.1D0))
C    &      WRITE(LOUT,*) STOT,SELA,SABS,IDXSPE
         IF ( idxspe(2).EQ.0 ) THEN
            IF ( (idspe(1).EQ.1) .AND. (idxn.GT.0) ) THEN
C              DO 80 K=1,3
C                 IF (ICAS.EQ.1) THEN
C                    VTXDST(K) = WHKK(K,IDXN)-WHKK(K,IDXSPE(1))
C                 ELSEIF (ICAS.EQ.2) THEN
C                    VTXDST(K) = VHKK(K,IDXN)-VHKK(K,IDXSPE(1))
C                 ENDIF
C  80          CONTINUE
C              DISTNU = SQRT(VTXDST(1)**2+VTXDST(2)**2+
C    &                       VTXDST(3)**2)
C              IF ((DISTNU.LT.15.0D0*FM2MM).OR.(POSN.GT.ZERO)) THEN
               idxspe(2) = idxn
               idspe(2) = 8
C              ELSE
C                 STOT = STOT-SABS
C                 SABS = ZERO
C              ENDIF
            ELSE IF ( (idspe(1).EQ.8) .AND. (idxp.GT.0) ) THEN
C              DO 81 K=1,3
C                 IF (ICAS.EQ.1) THEN
C                    VTXDST(K) = WHKK(K,IDXP)-WHKK(K,IDXSPE(1))
C                 ELSEIF (ICAS.EQ.2) THEN
C                    VTXDST(K) = VHKK(K,IDXP)-VHKK(K,IDXSPE(1))
C                 ENDIF
C  81          CONTINUE
C              DISTNU = SQRT(VTXDST(1)**2+VTXDST(2)**2+
C    &                       VTXDST(3)**2)
C              IF ((DISTNU.LT.15.0D0*FM2MM).OR.(POSP.GT.ZERO)) THEN
               idxspe(2) = idxp
               idspe(2) = 1
C              ELSE
C                 STOT = STOT-SABS
C                 SABS = ZERO
C              ENDIF
            ELSE
               stot = stot - sabs
               sabs = ZERO
            END IF
         END IF
         rr = DT_RNDM(dist)
         IF ( rr.LT.sela/stot ) THEN
            iproc = 2
         ELSE IF ( (rr.GE.sela/stot) .AND. (rr.LT.(sela+sabs)/stot) )
     &             THEN
            iproc = 3
         ELSE
            iproc = 1
         END IF
 
         DO k = 1 , 5
            pcas1(k) = pcas(icas,k)
            pnuc(k) = PHKk(k,idxspe(1))
         END DO
         IF ( iproc.EQ.3 ) THEN
C 2-nucleon absorption of pion
            nspe = 2
            CALL DT_ABSORP(idcas,pcas1,Ncas,nspe,idspe,idxspe,1,irej1)
            IF ( irej1.NE.0 ) THEN
               Irej = 1
               GOTO 99999
            ELSE
               IF ( nspe.GE.1 ) labsor = .TRUE.
            END IF
         ELSE
C sample secondary interaction
            idnuc = IDBam(idxspe(1))
            CALL DT_HADRIN(idcas,pcas1,idnuc,pnuc,iproc,irej1)
            IF ( irej1.EQ.1 ) THEN
               Irej = 1
               GOTO 99999
            ELSE IF ( irej1.GT.1 ) THEN
               GOTO 100
            END IF
         END IF
      END IF
 
C update arrays to include Pauli-principle
      DO i = 1 , nspe
         IF ( NWOund(icas).LE.299 ) THEN
            NWOund(icas) = NWOund(icas) + 1
            EWOund(icas,NWOund(icas)) = PHKk(4,idxspe(i))
         END IF
      END DO
 
C dump initial hadron for energy-momentum conservation check
      IF ( LEMcck ) CALL DT_EVTEMC(pcas(icas,1),pcas(icas,2),
     &     pcas(icas,3),pcas(icas,4),1,idum,idum)
 
C dump final state particles into DTEVT1
 
C   check if Pauli-principle is fulfilled
      npauli = 0
      nwtmp(1) = NWOund(1)
      nwtmp(2) = NWOund(2)
      DO i = 1 , NFSp
         npauli = 0
         j1 = 2
         IF ( ((Ncas.EQ.1) .AND. (It.LE.1)) .OR. 
     &        ((Ncas.EQ.-1) .AND. (Ip.LE.1)) ) j1 = 1
         DO j = 1 , j1
            IF ( (npauli.EQ.0) .OR. (j.NE.2) ) THEN
               IF ( j.EQ.1 ) THEN
                  idx = icas
                  pe = PFSp(4,i)
               ELSE
                  idx = i2
                  mode = 1
                  IF ( idx.EQ.1 ) mode = -1
                  CALL DT_LTNUC(PFSp(3,i),PFSp(4,i),pz,pe,mode)
               END IF
C first check if cascade step is forbidden due to Pauli-principle
C (in case of absorpion this step is forced)
               IF ( (.NOT.labsor) .AND. LPAuli .AND. 
     &              ((IDFsp(i).EQ.1) .OR. (IDFsp(i).EQ.8)) ) THEN
C   get nuclear potential barrier
                  pot = EPOt(idx,IDFsp(i)) + AAM(IDFsp(i))
                  IF ( IDFsp(i).EQ.1 ) THEN
                     potlow = pot - EBIndp(idx)
                  ELSE
                     potlow = pot - EBIndn(idx)
                  END IF
C   final state particle not able to escape nucleus
                  IF ( pe.LE.potlow ) THEN
C     check if there are wounded nucleons
                     IF ( (NWOund(idx).GE.1) .AND. 
     &                    (pe.GE.EWOund(idx,NWOund(idx))) ) THEN
                        npauli = npauli + 1
                        NWOund(idx) = NWOund(idx) - 1
                     ELSE
C     interaction prohibited by Pauli-principle
                        NWOund(1) = nwtmp(1)
                        NWOund(2) = nwtmp(2)
                        GOTO 100
                     END IF
                  END IF
               END IF
            END IF
         END DO
      END DO
 
      npauli = 0
      NWOund(1) = nwtmp(1)
      NWOund(2) = nwtmp(2)
 
      DO i = 1 , NFSp
 
         ist = ISThkk(Idxcas)
 
         npauli = 0
         j1 = 2
         IF ( ((Ncas.EQ.1) .AND. (It.LE.1)) .OR. 
     &        ((Ncas.EQ.-1) .AND. (Ip.LE.1)) ) j1 = 1
         DO j = 1 , j1
            IF ( (npauli.EQ.0) .OR. (j.NE.2) ) THEN
               idx = icas
               pe = PFSp(4,i)
               IF ( j.EQ.2 ) THEN
                  idx = i2
                  CALL DT_LTNUC(PFSp(3,i),PFSp(4,i),pz,pe,Ncas)
               END IF
C first check if cascade step is forbidden due to Pauli-principle
C (in case of absorpion this step is forced)
               IF ( (.NOT.labsor) .AND. LPAuli .AND. 
     &              ((IDFsp(i).EQ.1) .OR. (IDFsp(i).EQ.8)) ) THEN
C   get nuclear potential barrier
                  pot = EPOt(idx,IDFsp(i)) + AAM(IDFsp(i))
                  IF ( IDFsp(i).EQ.1 ) THEN
                     potlow = pot - EBIndp(idx)
                  ELSE
                     potlow = pot - EBIndn(idx)
                  END IF
C   final state particle not able to escape nucleus
                  IF ( pe.LE.potlow ) THEN
C     check if there are wounded nucleons
                     IF ( (NWOund(idx).GE.1) .AND. 
     &                    (pe.GE.EWOund(idx,NWOund(idx))) ) THEN
                        NWOund(idx) = NWOund(idx) - 1
                        npauli = npauli + 1
                        ist = 14 + idx
                     ELSE
C     interaction prohibited by Pauli-principle
                        NWOund(1) = nwtmp(1)
                        NWOund(2) = nwtmp(2)
                        GOTO 100
                     END IF
C*sr
C               ELSEIF (PE.LE.POT) THEN
CC              ELSEIF ((PE.LE.POT).AND.(NWOUND(IDX).GE.1)) THEN
CC                 NWOUND(IDX) = NWOUND(IDX)-1
C**
C                  NPAULI = NPAULI+1
C                  IST    = 14+IDX
                  END IF
               END IF
            END IF
         END DO
 
C dump final state particles for energy-momentum conservation check
         IF ( LEMcck ) CALL DT_EVTEMC(-PFSp(1,i),-PFSp(2,i),-PFSp(3,i),
     &        -PFSp(4,i),2,idum,idum)
 
         px = PFSp(1,i)
         py = PFSp(2,i)
         pz = PFSp(3,i)
         pe = PFSp(4,i)
         IF ( ABS(ist).EQ.1 ) THEN
C transform particles back into n-n cms
C LEPTO: leave final state particles in target rest frame
C           IF (MCGENE.EQ.3) THEN
C              PFSP(1,I) = PX
C              PFSP(2,I) = PY
C              PFSP(3,I) = PZ
C              PFSP(4,I) = PE
C           ELSE
            imode = icas + 1
            CALL DT_LTRANS(px,py,pz,pe,PFSp(1,i),PFSp(2,i),PFSp(3,i),
     &                     PFSp(4,i),IDFsp(i),imode)
C           ENDIF
         ELSE IF ( (icas.EQ.2) .AND. (ist.EQ.15) ) THEN
C target cascade but fsp got stuck in proj. --> transform it into
C proj. rest system
            CALL DT_LTRANS(px,py,pz,pe,PFSp(1,i),PFSp(2,i),PFSp(3,i),
     &                     PFSp(4,i),IDFsp(i),-1)
         ELSE IF ( (icas.EQ.1) .AND. (ist.EQ.16) ) THEN
C proj. cascade but fsp got stuck in target --> transform it into
C target rest system
            CALL DT_LTRANS(px,py,pz,pe,PFSp(1,i),PFSp(2,i),PFSp(3,i),
     &                     PFSp(4,i),IDFsp(i),1)
         END IF
 
C dump final state particles into DTEVT1
         igen = IDCh(Idxcas) + 1
         id = IDT_IPDGHA(IDFsp(i))
         ixr = 0
         IF ( labsor ) ixr = 99
         CALL DT_EVTPUT(ist,id,Idxcas,idxspe(1),PFSp(1,i),PFSp(2,i),
     &                  PFSp(3,i),PFSp(4,i),0,ixr,igen)
 
C update the counter for particles which got stuck inside the nucleus
         IF ( (ist.EQ.15) .OR. (ist.EQ.16) ) THEN
            NOInc = NOInc + 1
            IDXinc(NOInc) = NHKk
         END IF
         IF ( labsor ) THEN
C   in case of absorption the spatial treatment is an approximate
C   solution anyway (the positions of the nucleons which "absorb" the
C   cascade particle are not taken into consideration) therefore the
C   particles are produced at the position of the cascade particle
            DO k = 1 , 4
               WHKk(k,NHKk) = WHKk(k,Idxcas)
               VHKk(k,NHKk) = VHKk(k,Idxcas)
            END DO
         ELSE
C   DDISTL - distance the cascade particle moves to the intera. point
C   (the position where impact-parameter = distance to the interacting
C   nucleon), DIST - distance to the interacting nucleon at the time of
C   formation of the cascade particle, BINT - impact-parameter of this
C   cascade-interaction
            ddistl = SQRT(dist**2-bint**2)
            dtime = ddistl/becas(icas)
            dtimel = ddistl/bgcas(icas)
            rdistl = dtimel*bgcas(i2)
            IF ( (Ip.GT.1) .AND. (It.GT.1) ) THEN
               rtime = rdistl/becas(i2)
            ELSE
               rtime = ZERO
            END IF
C   RDISTL, RTIME are this step and time in the rest system of the other
C   nucleus
            DO k = 1 , 3
               vtxca1(icas,k) = vtxcas(icas,k) + coscas(icas,k)*ddistl
               vtxca1(i2,k) = vtxcas(i2,k) + coscas(i2,k)*rdistl
            END DO
            vtxca1(icas,4) = vtxcas(icas,4) + dtime
            vtxca1(i2,4) = vtxcas(i2,4) + rtime
C   position of particle production is half the impact-parameter to
C   the interacting nucleon
            DO k = 1 , 3
               WHKk(k,NHKk) = OHALF*(vtxca1(1,k)+WHKk(k,idxspe(1)))
               VHKk(k,NHKk) = OHALF*(vtxca1(2,k)+VHKk(k,idxspe(1)))
            END DO
C   time of production of secondary = time of interaction
            WHKk(4,NHKk) = vtxca1(1,4)
            VHKk(4,NHKk) = vtxca1(2,4)
         END IF
 
      END DO
 
C modify status and position of cascade particle (the latter for
C statistics reasons only)
      ISThkk(Idxcas) = 2
      IF ( labsor ) ISThkk(Idxcas) = 19
      IF ( .NOT.labsor ) THEN
         DO k = 1 , 4
            WHKk(k,Idxcas) = vtxca1(1,k)
            VHKk(k,Idxcas) = vtxca1(2,k)
         END DO
      END IF
 
      DO i = 1 , nspe
         is = idxspe(i)
C dump interacting nucleons for energy-momentum conservation check
         IF ( LEMcck ) CALL DT_EVTEMC(PHKk(1,is),PHKk(2,is),PHKk(3,is),
     &        PHKk(4,is),2,idum,idum)
C modify entry for interacting nucleons
         IF ( ISThkk(is).EQ.12+icas ) ISThkk(is) = 16 + icas
         IF ( ISThkk(is).EQ.14+icas ) ISThkk(is) = 2
         IF ( i.GE.2 ) THEN
            JDAhkk(1,is) = JDAhkk(1,idxspe(1))
            JDAhkk(2,is) = JDAhkk(2,idxspe(1))
         END IF
      END DO
 
C check energy-momentum conservation
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(dum,dum,dum,dum,4,500,irej1)
         IF ( irej1.NE.0 ) THEN
            Irej = 1
            GOTO 99999
         END IF
      END IF
 
C update counter
      IF ( labsor ) THEN
         NINcco(icas,1) = NINcco(icas,1) + 1
      ELSE
         IF ( iproc.EQ.1 ) NINcco(icas,2) = NINcco(icas,2) + 1
         IF ( iproc.EQ.2 ) NINcco(icas,3) = NINcco(icas,3) + 1
      END IF
 
      RETURN
 
C transport-step but no cascade step due to configuration (i.e. there
C is no nucleon for interaction etc.)
 100  IF ( Lcas ) THEN
         DO k = 1 , 4
C           WHKK(K,IDXCAS) = VTXCAS(1,K)
C           VHKK(K,IDXCAS) = VTXCAS(2,K)
            WHKk(k,Idxcas) = vtxca1(1,k)
            VHKk(k,Idxcas) = vtxca1(2,k)
         END DO
      END IF
 
C9998 CONTINUE
C no cascade-step because of configuration
C (i.e. hadron outside nucleus etc.)
      Lcas = .TRUE.
99999 END SUBROUTINE
