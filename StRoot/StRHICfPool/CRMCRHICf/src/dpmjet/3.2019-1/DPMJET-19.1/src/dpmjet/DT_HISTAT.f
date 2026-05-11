
      SUBROUTINE DT_HISTAT(Idx,Mode)
 
C***********************************************************************
C This version dated 26.02.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION emusam , ONE , TINY14 , TWO , ZERO
      INTEGER i , ib , ic , icev , icev1 , Idx , j , kpopo , maxgen , 
     &        Mode , NDIM , nteva1 , nteva2
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,TINY14=1.0D-14)
      PARAMETER (NDIM=199)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C event history
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C rejection counter
      INCLUDE 'inc/dtrejc'
C statistics: residual nuclei
      INCLUDE 'inc/dtsta2'
C parameter for intranuclear cascade
      INCLUDE 'inc/dtfoti'
 
#ifdef FOR_FLUKA
      INCLUDE '(DIMPAR)'
      INCLUDE '(PAREVT)'
      INCLUDE '(EVAFLG)'
      INCLUDE '(FRBKCM)'
#else
      INCLUDE 'DIMPAR'
      INCLUDE 'PAREVT'
      INCLUDE 'EVAFLG'
      INCLUDE 'FRBKCM'
#endif

C temporary storage for one final state particle
      INCLUDE 'inc/dtfspa'
C event flag used for histograms
      INCLUDE 'inc/dtnorm'
C statistics: double-Pomeron exchange
      INCLUDE 'inc/dtflg2'
 
      DIMENSION emusam(NCOMPX)
 
      CHARACTER*13 cmsg(3)
      DATA cmsg/'not requested' , 'not requested' , 'not requested'/
 
      IF ( Mode.EQ.2 ) THEN
C------------------------------------------------------------------
C filling of histogram with event-record
         IF ( IST.EQ.-1 ) THEN
            IF ( LFRag ) THEN
C   heavy fragments (here: fission products only)
               NEVahy(NOBam(Idx),1,IBAry) = NEVahy(NOBam(Idx),1,IBAry)
     &            + 1
               NEVahy(NOBam(Idx),2,ICHar) = NEVahy(NOBam(Idx),2,ICHar)
     &            + 1
               NEVaht(NOBam(Idx)) = NEVaht(NOBam(Idx)) + 1
            ELSE IF ( IDPdg.EQ.2212 ) THEN
               NEVa(NOBam(Idx),1) = NEVa(NOBam(Idx),1) + 1
            ELSE IF ( IDPdg.EQ.2112 ) THEN
               NEVa(NOBam(Idx),2) = NEVa(NOBam(Idx),2) + 1
            ELSE IF ( IDPdg.EQ.22 ) THEN
               NEVaga(NOBam(Idx)) = NEVaga(NOBam(Idx)) + 1
            ELSE IF ( IDPdg.EQ.80000 ) THEN
               IF ( IDBjt.EQ.207 ) THEN
                  NEVa(NOBam(Idx),3) = NEVa(NOBam(Idx),3) + 1
               ELSE IF ( IDBjt.EQ.208 ) THEN
                  NEVa(NOBam(Idx),4) = NEVa(NOBam(Idx),4) + 1
               ELSE IF ( IDBjt.EQ.209 ) THEN
                  NEVa(NOBam(Idx),5) = NEVa(NOBam(Idx),5) + 1
               ELSE IF ( IDBjt.EQ.210 ) THEN
                  NEVa(NOBam(Idx),6) = NEVa(NOBam(Idx),6) + 1
               END IF
            END IF
         ELSE IF ( (IST.EQ.1) .AND. (.NOT.LFRag) ) THEN
            IF ( IDCh(Idx).GT.maxgen ) maxgen = IDCh(Idx)
         END IF
 
         RETURN
      ELSE IF ( Mode.EQ.3 ) THEN
C------------------------------------------------------------------
C output
 
C*dble Po statistics.
C     WRITE(LOUT,'(1X,A,2I7,2E12.4)')
C    &   '# evts. / # dble-Po. evts / s_in / s_popo :',
C    & ICEVT,KPOPO,XSPRO(1,1,1),XSPRO(1,1,1)*DBLE(KPOPO)/DBLE(ICEVT)
 
C  emulsion treatment
         IF ( NCOmpo.GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010       FORMAT (/,1X,'HISTAT:',14X,'statistics - target emulsion',/,
     &              22X,'----------------------------',/,/,19X,
     &              'mass    charge          fraction',/,39X,
     &              'input     treated',/)
            DO i = 1 , NCOmpo
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99020) i , IEMuma(i) , 
     &              IEMuch(i) , EMUfra(i) , emusam(i)/DBLE(ICEvt)
99020          FORMAT (12X,I2,1X,2I8,6X,F7.3,5X,F7.3)
            END DO
         END IF
 
C  i.n.c. statistics: output
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99030) ICEvt , NREsev(2) , IRInc
99030    FORMAT (/,1X,'HISTAT:',14X,'statistics - intranuclear cascade',
     &           /,22X,'---------------------------------',/,/,1X,
     &        'no. of events for normalization: (accepted final events,'
     &        ,' evt)',4X,I6,/,34X,'(events before evap.-step, evt1)',
     &        I6,/,1X,'no. of rejected events due to intranuclear',
     &        ' cascade',15X,I6,/)
         icev = MAX(ICEvt,1)
         icev1 = icev
         IF ( LEVprt ) icev1 = MAX(NREsev(2),1)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99040)
     &        (DBLE(NINcwo(i))/DBLE(icev),i=1,2) , 
     &        ((DBLE(NINcst(i,j))/DBLE(icev),i=1,2),j=1,4) , KTAuge , 
     &        DBLE(NINcge)/DBLE(icev) , 
     &        (DBLE(NINcco(i,1)+NINcco(i,2)+NINcco(i,3))/DBLE(icev1),
     &        i=1,2) , (DBLE(NINcco(i,2))/DBLE(icev1),i=1,2) , 
     &        (DBLE(NINcco(i,3))/DBLE(icev1),i=1,2) , 
     &        (DBLE(NINcco(i,1))/DBLE(icev1),i=1,2)
99040    FORMAT (1X,
     &           'no. of wounded nucl. in proj./ target (mean per evt)',
     &           5X,F6.2,' /',F6.2,/,1X,
     &           'no. of particles unable to escape',
     &           ' proj./ target (mean per evt)',/,8X,'baryons:  pos. ',
     &           F7.3,' /',F7.3,'   neg. ',F7.3,' /',F7.3,/,8X,
     &           'mesons:   pos. ',F7.3,' /',F7.3,'   neg. ',F7.3,' /',
     &           F7.3,/,1X,
     &           'maximum no. of generations treated (maximum allowed:',
     &           I4,')',/,43X,'(mean per evt)',5X,F6.2,/,1X,
     &           'no. of sec.',
     &           ' interactions in proj./ target (mean per evt1)',F7.3,
     &           ' /',F7.3,/,8X,'out of which by inelastic',
     &           ' interactions',12X,F7.3,' /',F7.3,/,21X,'by elastic ',
     &           'interactions',14X,F7.3,' /',F7.3,/,21X,
     &           'by absorption ','(ap, K-, pi- only)     ',F7.3,' /',
     &           F7.3,/)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99050) NREsev(2) , NREsev(4) , 
     &        IRExci , IRExci(1) + IRExci(2) + IRExci(3)
99050    FORMAT (/,1X,'HISTAT:',14X,'statistics - residual nuclei, ',
     &           'evaporation',/,22X,'-----------------------------',
     &           '------------',/,/,1X,'no. of events for normal.: ',
     &           '(events handled by FICONF, evt)',7X,I6,/,28X,
     &           '(events',' passing the evap.-step, evt1) ',I6,/,1X,
     &           'no. of',' rejected events     (',I4,',',I4,',',I4,')',
     &           22X,I6,/)
 
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99060)
99060    FORMAT (/,22X,'1) before evaporation-step:',/)
         icev = MAX(NREsev(2),1)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99070)
     &        (DBLE(NREsto(i))/DBLE(icev),i=1,2) , 
     &        (DBLE(NREspr(i))/DBLE(icev),i=1,2) , 
     &        (DBLE(NREsnu(i))/DBLE(icev),i=1,2) , 
     &        (DBLE(NREsba(i))/DBLE(icev),i=1,2) , 
     &        (DBLE(NREspb(i))/DBLE(icev),i=1,2) , 
     &        (DBLE(NREsch(i))/DBLE(icev),i=1,2) , 
     &        (EXCdpm(i)/DBLE(icev),i=1,2) , 
     &        (EXCdpm(i+2)/DBLE(icev),i=1,2)
99070    FORMAT (1X,'residual nuclei:  (mean values per evt)',12X,
     &           'proj. / target',/,/,8X,'total number of particles',
     &           15X,2F9.3,/,8X,'out of which: protons',19X,2F9.3,/,22X,
     &           'neutrons',18X,2F9.3,/,22X,'baryons',19X,2F9.3,/,22X,
     &           'pos. baryons',14X,2F9.3,/,8X,'total charge',28X,2F9.3,
     &           /,/,8X,'excitation energy (bef. evap.-step)   ',2E11.3,
     &           /,8X,'excitation energy per nucleon         ',2E11.3,/,
     &           /)
 
C evaporation / fission / fragmentation statistics: output
         icev = MAX(NREsev(2),1)
         icev1 = MAX(NREsev(4),1)
         nteva1 = NEVa(1,1) + NEVa(1,2) + NEVa(1,3) + NEVa(1,4)
     &            + NEVa(1,5) + NEVa(1,6)
         nteva2 = NEVa(2,1) + NEVa(2,2) + NEVa(2,3) + NEVa(2,4)
     &            + NEVa(2,5) + NEVa(2,6)
         IF ( LEVprt ) THEN
            IF ( IEVfss.GE.1 ) cmsg(1) = 'requested    '
            IF ( LFRmbk ) cmsg(2) = 'requested    '
            IF ( LDEexg ) cmsg(3) = 'requested    '
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99080) cmsg , DBLE(nteva1)
     &           /DBLE(icev1) , DBLE(nteva2)/DBLE(icev1) , 
     &           (DBLE(NEVa(i,1))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVa(i,2))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVa(i,3))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVa(i,4))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVa(i,5))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVa(i,6))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVaga(i))/DBLE(icev1),i=1,2) , 
     &           (DBLE(NEVaht(i))/DBLE(icev1),i=1,2)
99080       FORMAT (22X,'2) after  evaporation-step:',/,/,1X,'Fission:',
     &              13X,A13,/,1X,'Fermi-Break-up:',6X,A13,/,1X,'Gamma-',
     &              'deexcitation:',2X,A13,/,/,1X,
     &             'evaporation/deexcitation:  (mean values per evt1)  '
     &             ,'proj. / target',/,/,8X,
     &             'total number of evap. particles',9X,2F9.3,/,8X,
     &             'out of which: protons',19X,2F9.3,/,22X,'neutrons',
     &             18X,2F9.3,/,22X,'deuterons',17X,2F9.3,/,22X,'3-H',
     &             23X,2F9.3,/,22X,'3-He',22X,2F9.3,/,22X,'4-He',22X,
     &             2F9.3,/,8X,'nucl. deexcit. gammas',19X,2F9.3,/,8X,
     &             'heavy fragments',25X,2F9.3,/)
            IF ( IEVfss.GE.1 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99090) NEVafi(1,1) , 
     &              NEVafi(1,2) , NEVafi(2,1) , NEVafi(2,2) , 
     &              DBLE(NEVafi(2,1))/DBLE(MAX(NEVafi(1,1),1))*100.0D0 , 
     &              DBLE(NEVafi(2,2))/DBLE(MAX(NEVafi(1,2),1))*100.0D0
99090          FORMAT (1X,'Fission:   total number of events',14X,2I9,
     &                 /12X,'out of which fission occured',8X,2I9,/,50X,
     &                 '(',F5.2,'%) (',F5.2,'%)',/)
            END IF
C        IF ((LFRMBK).OR.(IFISS.EQ.1)) THEN
C           WRITE(LOUT,3008)
C3008       FORMAT(1X,'heavy fragments - statistics:',7X,'charge',
C    &             '       proj.   / target',/)
C           DO 31 I=1,210
C              IF ((NEVAHY(1,2,I).NE.0).OR.(NEVAHY(2,2,I).NE.0)) THEN
C                 WRITE(LOUT,3009) I,
C    &            (DBLE(NEVAHY(K,2,I))*XSPRO(1,1,1)/DBLE(ICEV1),K=1,2)
C3009             FORMAT(38X,I3,3X,2E12.3)
C              ENDIF
C  31       CONTINUE
C           WRITE(LOUT,3010)
C3010       FORMAT(1X,'heavy fragments - statistics:',7X,'mass  ',
C    &             '       proj.   / target',/)
C           DO 32 I=1,210
C              IF ((NEVAHY(1,1,I).NE.0).OR.(NEVAHY(2,1,I).NE.0)) THEN
C                 WRITE(LOUT,3011) I,
C    &            (DBLE(NEVAHY(K,1,I))*XSPRO(1,1,1)/DBLE(ICEV1),K=1,2)
C3011             FORMAT(38X,I3,3X,2E12.3)
C              ENDIF
C  32       CONTINUE
C           WRITE(LOUT,*)
C        ENDIF
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99100)
99100       FORMAT (22X,'2) after  evaporation-step:',/,/,1X,
     &              'Evaporation:         not requested',/)
         END IF
 
         RETURN
      ELSE IF ( Mode.EQ.4 ) THEN
C------------------------------------------------------------------
C filling of histogram with event-record
C  emulsion treatment
         IF ( NCOmpo.GT.0 ) THEN
            DO i = 1 , NCOmpo
               IF ( IT.EQ.IEMuma(i) ) emusam(i) = emusam(i) + ONE
            END DO
         END IF
         NINcge = NINcge + maxgen
         maxgen = 0
C*dble Po statistics.
         IF ( IPOpo.EQ.1 ) kpopo = kpopo + 1
 
         RETURN
      ELSE IF ( Mode.EQ.5 ) THEN
C------------------------------------------------------------------
C filling of histogram with event-record
         IF ( (ISThkk(Idx).EQ.15) .OR. (ISThkk(Idx).EQ.16) ) THEN
            ib = IIBar(IDBam(Idx))
            ic = IICh(IDBam(Idx))
            j = ISThkk(Idx) - 14
            IF ( ((ABS(ib).EQ.1) .AND. (ic.EQ.1)) .OR. (ic.EQ.0) ) THEN
               NINcst(j,1) = NINcst(j,1) + 1
            ELSE IF ( (ABS(ib).EQ.1) .AND. (ic.EQ.-1) ) THEN
               NINcst(j,2) = NINcst(j,2) + 1
            ELSE IF ( (ABS(ib).EQ.0) .AND. (ic.EQ.1) ) THEN
               NINcst(j,3) = NINcst(j,3) + 1
            ELSE IF ( (ABS(ib).EQ.0) .AND. (ic.EQ.-1) ) THEN
               NINcst(j,4) = NINcst(j,4) + 1
            END IF
         ELSE IF ( ISThkk(Idx).EQ.17 ) THEN
            NINcwo(1) = NINcwo(1) + 1
         ELSE IF ( ISThkk(Idx).EQ.18 ) THEN
            NINcwo(2) = NINcwo(2) + 1
         ELSE IF ( ISThkk(Idx).EQ.1001 ) THEN
            ib = IDRes(Idx)
            ic = IDXres(Idx)
            IF ( ic.GT.0 ) THEN
               NEVahy(NOBam(Idx),1,ib) = NEVahy(NOBam(Idx),1,ib) + 1
               NEVahy(NOBam(Idx),2,ic) = NEVahy(NOBam(Idx),2,ic) + 1
            END IF
            NEVaht(NOBam(Idx)) = NEVaht(NOBam(Idx)) + 1
         END IF
         GOTO 99999
      END IF
 
C------------------------------------------------------------------
C initialization
C  emulsion treatment
      IF ( NCOmpo.GT.0 ) THEN
         DO i = 1 , NCOMPX
            emusam(i) = ZERO
         END DO
      END IF
C common /DTSTA2/, statistics on i.n.c., residual nuclei, evap.
      NINcge = 0
      DO i = 1 , 2
         EXCdpm(i) = ZERO
         EXCdpm(i+2) = ZERO
         EXCeva(i) = ZERO
         NINcwo(i) = 0
         NINcev(i) = 0
         NREsto(i) = 0
         NREspr(i) = 0
         NREsnu(i) = 0
         NREsba(i) = 0
         NREspb(i) = 0
         NREsch(i) = 0
         NREsev(i) = 0
         NREsev(i+2) = 0
         NEVaga(i) = 0
         NEVaht(i) = 0
         NEVafi(1,i) = 0
         NEVafi(2,i) = 0
         DO j = 1 , 6
            IF ( j.LE.2 ) NINchr(i,j) = 0
            IF ( j.LE.3 ) NINcco(i,j) = 0
            IF ( j.LE.4 ) NINcst(i,j) = 0
            NEVa(i,j) = 0
         END DO
         DO j = 1 , 210
            NEVahy(1,i,j) = 0
            NEVahy(2,i,j) = 0
         END DO
      END DO
      maxgen = 0
C*dble Po statistics.
      kpopo = 0
 
 
99999 END SUBROUTINE
