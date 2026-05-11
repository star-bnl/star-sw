
      SUBROUTINE DT_DIFEVT(Ifp1,Ifp2,Pp,Mop,Ift1,Ift2,Pt,Mot,Jdiff,Ncsy,
     &                     Irej)
 
C***********************************************************************
C Interface to treatment of diffractive interactions.                  *
C  (input)          IFP1/2        PDG-indizes of projectile partons    *
C                                 (baryon: IFP2 - adiquark)            *
C                   PP(4)         projectile 4-momentum                *
C                   IFT1/2        PDG-indizes of target partons        *
C                                 (baryon: IFT1 - adiquark)            *
C                   PT(4)         target 4-momentum                    *
C  (output)         JDIFF = 0     no diffraction                       *
C                         = 1/-1  LMSD/LMDD                            *
C                         = 2/-2  HMSD/HMDD                            *
C                   NCSY          counter for two-chain systems        *
C                                 dumped to DTEVT1                     *
C This version dated 14.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ddhm , ddtot , DT_RNDM , dumzer , fraddh , 
     &                 fradif , frasd , frasdh , OHALF , ONE , Pp , Pt , 
     &                 sdhm , sdtot , sigel , sigin , sigto , TINY10 , 
     &                 TINY5 , xm
      DOUBLE PRECISION ZERO
      INTEGER IDT_ICIHAD , Ifp1 , Ifp2 , Ift1 , Ift2 , Irej , irej1 , 
     &        Jdiff , kdiff , kp , kproj , kt , ktarg , Mop , Mot , Ncsy
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY10=1.0D-10,TINY5=1.0D-5,
     &           OHALF=0.5D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
 
      DIMENSION Pp(4) , Pt(4)
 
      LOGICAL lfirst
      DATA lfirst/.TRUE./
 
      Irej = 0
      Jdiff = 0
      IFLagd = Jdiff
 
C cm. energy
      xm = SQRT((Pp(4)+Pt(4))**2-(Pp(1)+Pt(1))**2-(Pp(2)+Pt(2))
     &     **2-(Pp(3)+Pt(3))**2)
C identities of projectile hadron / target nucleon
      kproj = IDT_ICIHAD(IDHkk(Mop))
      ktarg = IDT_ICIHAD(IDHkk(Mot))
 
C single diffractive xsections
      CALL DT_SHNDIF(xm,kproj,ktarg,sdtot,sdhm)
C double diffractive xsections
C*!! no double diff yet
C     CALL DT_SHNDIF(XM,KPROJ,KTARG,SDTOT,SDHM,DDTOT,DDHM)
      ddtot = 0.0D0
      ddhm = 0.0D0
C*!!
C total inelastic xsection
C     SIGIN  = DT_SHNTOT(KPROJ,KTARG,XM,ZERO)-DT_SHNELA(KPROJ,KTARG,XM)
      dumzer = ZERO
      CALL DT_XSHN(kproj,ktarg,dumzer,xm,sigto,sigel)
      sigin = MAX(sigto-sigel,ZERO)
 
C fraction of diffractive processes
      fradif = (sdtot+ddtot)/sigin
 
      IF ( lfirst ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) xm , sdtot , sigin
99010    FORMAT (1X,'DIFEVT: single diffraction requested at E_cm = ',
     &           F5.1,' GeV',/,9X,'sigma_sd = ',F4.1,' mb, sigma_in = ',
     &           F5.1,' mb',/)
         lfirst = .FALSE.
      END IF
 
      IF ( (DT_RNDM(ddhm).LE.fradif) .OR. (ISIngd.GT.1) .OR. 
     &     (IDOubd.GT.1) ) THEN
C diffractive interaction requested by x-section or by user
         frasd = sdtot/(sdtot+ddtot)
         frasdh = sdhm/sdtot
C*sr needs to be specified!!
C        FRADDH = DDHM/DDTOT
         fraddh = 1.0D0
C*
         IF ( (DT_RNDM(frasd).LE.frasd) .OR. (ISIngd.GT.1) ) THEN
C   single diffraction
            kdiff = 1
            IF ( DT_RNDM(ddtot).LE.frasdh ) THEN
               kp = 2
               kt = 0
               IF ( ((ISIngd.EQ.4) .OR. (DT_RNDM(ddtot).GE.OHALF)) .AND. 
     &              ISIngd.NE.3 ) THEN
                  kp = 0
                  kt = 2
               END IF
            ELSE
               kp = 1
               kt = 0
               IF ( ((ISIngd.EQ.4) .OR. (DT_RNDM(fraddh).GE.OHALF))
     &              .AND. ISIngd.NE.3 ) THEN
                  kp = 0
                  kt = 1
               END IF
            END IF
         ELSE
C   double diffraction
            kdiff = -1
            IF ( DT_RNDM(fraddh).LE.fraddh ) THEN
               kp = 2
               kt = 2
            ELSE
               kp = 1
               kt = 1
            END IF
         END IF
         CALL DT_DIFFKI(Ifp1,Ifp2,Pp,Mop,kp,Ift1,Ift2,Pt,Mot,kt,Ncsy,
     &                  irej1)
         IF ( irej1.EQ.0 ) THEN
            IFLagd = 2*kdiff
            IF ( (kp.EQ.1) .OR. (kt.EQ.1) ) IFLagd = kdiff
         ELSE
 
            Irej = 1
            GOTO 99999
         END IF
      END IF
      Jdiff = IFLagd
 
99999 END SUBROUTINE
