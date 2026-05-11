
      SUBROUTINE DT_EVENTA(Id,Ip,It,Ncsy,Irej)
 
C***********************************************************************
C Treatment of nucleon-nucleon interactions in a two-chain             *
C approximation.                                                       *
C  (input) ID       BAMJET-index of projectile hadron (in case of      *
C                   h-K scattering)                                    *
C          IP/IT    mass number of projectile/target nucleus           *
C          NCSY     number of two chain systems                        *
C          IREJ     rejection flag                                     *
C This version dated 15.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , Id , idum , ifp1 , ifp2 , ift1 , ift2 , Ip , Irej , 
     &        irej1 , It , k , mop1 , mop2 , mot1 , mot2 , nc , Ncsy , 
     &        npymem
      DOUBLE PRECISION pp1 , pp2 , pt1 , pt2 , TINY10
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
      DIMENSION pp1(4) , pp2(4) , pt1(4) , pt2(4)
 
      Irej = 0
      NPOint(3) = NHKk + 1
 
C skip following treatment for low-mass diffraction
      IF ( ABS(IFLagd).EQ.1 ) THEN
         NPOint(3) = NPOint(2)
         GOTO 100
      END IF
 
C multiple scattering of chain ends
      IF ( (Ip.GT.1) .AND. (MKCron.NE.0) ) CALL DT_CRONIN(1)
 
      IF ( (It.GT.1) .AND. (MKCron.NE.0) ) CALL DT_CRONIN(2)
      nc = NPOint(2)
C get a two-chain system from DTEVT1
      DO i = 1 , Ncsy
         ifp1 = IDHkk(nc)
         ift1 = IDHkk(nc+1)
         ifp2 = IDHkk(nc+2)
         ift2 = IDHkk(nc+3)
         DO k = 1 , 4
            pp1(k) = PHKk(k,nc)
            pt1(k) = PHKk(k,nc+1)
            pp2(k) = PHKk(k,nc+2)
            pt2(k) = PHKk(k,nc+3)
         END DO
         mop1 = nc
         mot1 = nc + 1
         mop2 = nc + 2
         mot2 = nc + 3
         CALL DT_GETCSY(ifp1,pp1,mop1,ifp2,pp2,mop2,ift1,pt1,mot1,ift2,
     &                  pt2,mot2,irej1)
         IF ( irej1.GT.0 ) THEN
            IRHha = IRHha + 1
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 1 in EVENTA'
            GOTO 200
         END IF
         nc = nc + 4
      END DO
 
C meson/antibaryon projectile:
C sample single-chain valence-valence systems (Reggeon contrib.)
      IF ( (Ip.EQ.1) .AND. (ISIcha.EQ.1) ) THEN
         IF ( IIBar(Id).LE.0 ) CALL DT_VV2SCH
      END IF
 
      IF ( (IREsco.EQ.1) .OR. (IFRag(1).EQ.1) ) THEN
C check DTEVT1 for remaining resonance mass corrections
         CALL DT_EVTRES(irej1)
         IF ( irej1.GT.0 ) THEN
            IRRes(1) = IRRes(1) + 1
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 2 in EVENTA'
            GOTO 200
         END IF
      END IF
 
C assign p_t to two-"chain" systems consisting of two resonances only
C since only entries for chains will be affected, this is obsolete
C in case of JETSET-fragmetation
      CALL DT_RESPT
 
C combine q-aq chains to color ropes (qq-aqaq) (chain fusion)
 
      IF ( LCO2cr ) CALL DT_COM2CR
 
C fragmentation of the complete event
C*uncomment for internal phojet-fragmentation
C     CALL DT_EVTFRA(IREJ1)
 100  CALL DT_EVTFRG(2,idum,npymem,irej1)
      IF ( irej1.GT.0 ) THEN
         IRFrag = IRFrag + 1
 
         IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &         'rejected 3 in EVENTA'
         GOTO 200
      END IF
 
C decay of possible resonances (should be obsolete)
      CALL DT_DECAY1
 
      RETURN
 
 200  IREvt = IREvt + 1
      Irej = 1
      END SUBROUTINE
