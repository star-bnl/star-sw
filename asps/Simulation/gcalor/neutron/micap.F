* $Id: micap.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE MICAP
#define ATLAS_MDEBUG 1
C
C CALOR-GEANT interface COMMON
#include "gcalor/calgea.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
C MICAP commons
#include "geant321/mmicap.inc"
#include "geant321/minput.inc"
#include "geant321/mconst.inc"
#include "geant321/mnutrn.inc"
#include "geant321/mapoll.inc"
#include "geant321/mpoint.inc"
#include "geant321/mrecoi.inc"
#include "geant321/mmass.inc"
#include "geant321/mpstor.inc"
#include "geant321/cmagic.inc"
C
C convert Z,A of recoil to CALOR particle code
C only p = 0, D = 7, T = 8, He3 = 9, alpha=10
      DIMENSION NPART(4,0:2)
      DATA ((NPART(I,J),I=1,4),J=0,2)/1 ,-1 ,-1 , -1,
     +                                0 , 7 , 8 , -1,
     +                               -1 ,-1 , 9 , 10/
      LOGICAL NOP
      SAVE
C first check, if ZEBRA still in order
      IF(LD(LMAG1).NE.NMAGIC.OR.LD(LMAG2).NE.NMAGIC) THEN
         WRITE(*,*) ' CALOR: ZEBRA banks screwed up --> STOP'
         WRITE(IOUT,'('' MICAP: Magic number '',I12,'' not found: '',  '
     +   //'      2I12)') NMAGIC,LD(LMAG1),LD(LMAG2)
         STOP
      ENDIF
C       THIS ROUTINE PERFORMS THE RANDOM WALK FOR ALL PARTICLES
   10 CONTINUE
C get material and particle information
      U = UINC(1)
      V = UINC(2)
      W = UINC(3)
      X = 0.0
      Y = 0.0
      Z = 0.0
      BLZNT = 1
      WATE = 1.0
      AGE = 0.0
      NREG = 1
      WTBC = 1.0
C Energy MeV -> eV
      E = EINC * 1.E6
C Material number a la GEANT
      NMED = NCEL
      NMEM=1
C reset counter of heavy/charged and gamma bank
      NMEMR = 0
      NMEMG = 0
      INALB=0
      EOLD=E
      UOLD=U
      VOLD=V
      WOLD=W
      OLDWT=WATE
      XOLD=X
      YOLD=Y
      ZOLD=Z
      BLZON=BLZNT
      MEDOLD=NMED
      OLDAGE=AGE
      I=1
      CALL GTMED(NMED,IMED)
C get total cross-section
      CALL NSIGTA(E,NMED,TSIG,D,D(LFP32),D(LFP33))
C       DETERMINE WHICH ISOTOPE HAS BEEN HIT
      CALL ISOTPE(D,D,D(LFP10),D(LFP12),D(LFP16),D(LFP26),D(LFP27),
     +            E,TSIG,IMED,IIN,IIM)
C       THE PARAMETER (IIN) IS THE POINTER FOR ARRAYS DIMENSIONED BY
C       (NNUC) AND THE PARAMETER (IIM) IS THE POINTER FOR ARRAYS
C       DIMENSIONED BY (NMIX)
      LD(LFP42+IMED-1)=LD(LFP42+IMED-1)+1
      INEU = 0
      NNEU = 0
      NHEVY = 0
      NGAMA = 0
      NPSTOR = 0
#if defined(ATLAS_MDEBUG)
      ANUC = D(LFP34+IIN-1)*1.008665
      ZNUC = LD(LFP13+IIM-1)
#endif
      ATARGT = D(LFP34+IIN-1)*1.008665
      ZTARGT = FLOAT(LD(LFP13+IIM-1))
      CALL COLISN(D,D,D(LFP20),D(LFP21),D(LFP22),D(LFP23),D(LFP24),
     + D(LFP25),D(LFP26),D(LFP27),D(LFP28),D(LFP29),D(LFP30),
     + D(LFP31),D(LFP34),D(LFP35),D(LFP41),D(LFP41+NNUC),
     + D(LFP42),D(LFP42+MEDIA),D(LFP42+2*MEDIA),D(LFP42+3*MEDIA),
     + D(LFP42+4*MEDIA),D(LFP42+5*MEDIA),D(LFP42+6*MEDIA),
     + D(LFP42+7*MEDIA),D(LFP42+8*MEDIA),D(LFP42+9*MEDIA),
     + D(LFP42+10*MEDIA),D(LFP42+11*MEDIA),D(LFP42+12*MEDIA),
     + D(LFP42+13*MEDIA),D(LFP42+14*MEDIA),D(LFP42+15*MEDIA),
     + D(LFP42+16*MEDIA),D(LFP42+17*MEDIA),D(LFP42+18*MEDIA),
     + D(LFP42+19*MEDIA),D(LFP42+20*MEDIA),D(LFP42+21*MEDIA),
     + D(LFP42+22*MEDIA),D(LFP45),D(LFP46),D(LFP13),D(LFP35+NQ*NNUC),
     + D(LFP35+2*NQ*NNUC),IIN,IIM)
      CALL BANKR(D,D,5)
C -------- fill return arrays with generated particles ---------------
C first heavy/charged particles
   20 NPHETC = 0
      NRECOL = 0
      ERMED(1) = 0.0
      EETOT = 0.0
#if defined(ATLAS_MDEBUG)
      PX = 0.0
      PY = 0.0
      PZ = 0.0
      NH = 0
      NG = 0
      ATOT = 0.0
      ZTOT = 0.0
#endif
C -------- store  neutrons -------------------------------------
      INTCAL = 0
C
      DO 30  N=1,NNEU
         CALL GETPAR(IDNEU,N,IERR)
         IF(IERR.EQ.0) THEN
            NPHETC = NPHETC + 1
            IF(NPHETC.GT.MXCP) NPHETC=MXCP
            IPCAL(NPHETC) = 1
C kinetic energy in MeV
#if defined(ATLAS_MDEBUG)
            EETOT = EETOT + EP*1.E-6
            ETOT = EP*1.E-6 + ZN*1.E-6
            PP = SQRT((EP*1.E-6)**2+2*ZN*EP*1.E-12)
            PX = PX + UP*PP
            PY = PY + VP*PP
            PZ = PZ + WP*PP
#endif
            EKINET(NPHETC) = EP * 1.E-6
            UCAL(NPHETC,1) = UP
            UCAL(NPHETC,2) = VP
            UCAL(NPHETC,3) = WP
            CALTIM(NPHETC) = AGEP
         ENDIF
   30 CONTINUE
C -------- store heavy recoil products ------------------------
      DO 40  N=1,NHEVY
         CALL GETPAR(IDHEVY,N,IERR)
         IF(IERR.EQ.0) THEN
C check particle type
#if defined(ATLAS_MDEBUG)
            NH = NH + 1
#endif
            MA = NINT(AMP)
            MZ = NINT(ZMP)
            NOP = .TRUE.
            IF(MA.LE.4.AND.MZ.LE.2) THEN
               IF(NPART(MA,MZ).GT.-1) NOP = .FALSE.
            ENDIF
            IF(NOP) THEN
C get heavy recoil nucleus
               NRECOL = NRECOL + 1
               AMED(NRECOL) = AMP
               ZMED(NRECOL) = ZMP
               ERMED(NRECOL)= EP * 1.E-6
#if defined(ATLAS_MDEBUG)
               ATOT = ATOT + AMP
               ZTOT = ZTOT + ZMP
               EETOT = EETOT + EP*1.E-6
               ETOT = EP*1.E-6 + AMP*931.5
               PP = SQRT(ETOT*ETOT - (AMP*931.5)**2)
               PX = PX + UP*PP
               PY = PY + VP*PP
               PZ = PZ + WP*PP
#endif
               GOTO 40
            ENDIF
C store particle type
            NPHETC = NPHETC + 1
            IF(NPHETC.GT.MXCP) NPHETC=MXCP
            IPCAL(NPHETC) = NPART(MA,MZ)
C kinetic energy in MeV
#if defined(ATLAS_MDEBUG)
            ATOT = ATOT + AMP
            ZTOT = ZTOT + ZMP
            EETOT = EETOT + EP*1.E-6
            ETOT = EP*1.E-6 + AMP*931.5
            PP = SQRT(ETOT*ETOT - (AMP*931.5)**2)
            PX = PX + UP*PP
            PY = PY + VP*PP
            PZ = PZ + WP*PP
#endif
            EKINET(NPHETC) = EP * 1.E-6
            UCAL(NPHETC,1) = UP
            UCAL(NPHETC,2) = VP
            UCAL(NPHETC,3) = WP
            CALTIM(NPHETC) = AGEP
         ENDIF
   40 CONTINUE
C
C----------- get generated gammas --------------------
      DO 50  N=1,NGAMA
         CALL GETPAR(IDGAMA,N,IERR)
         IF(IERR.EQ.0) THEN
            NG = NG + 1
            NPHETC = NPHETC + 1
            IF(NPHETC.GT.MXCP) NPHETC=MXCP
            IPCAL(NPHETC) = 11
#if defined(ATLAS_MDEBUG)
            EETOT = EETOT + EP*1.E-6
            PP = EP*1.E-6
            PX = PX + UP*PP
            PY = PY + VP*PP
            PZ = PZ + WP*PP
#endif
            EKINET(NPHETC) = EP*1.E-6
            UCAL(NPHETC,1) = UP
            UCAL(NPHETC,2) = VP
            UCAL(NPHETC,3) = WP
            CALTIM(NPHETC) = AGEP
C nucleus is in ground state !
            EXMED = 0.0
         ENDIF
   50 CONTINUE
      IF (MTP        .EQ.         2) THEN
         INTCAL = 13
      ELSEIF (MTP    .EQ.        18) THEN
         IF (NHEVY.GT.0) INTCAL = 15
      ELSEIF (MTP    .LT.       100) THEN
         IF (NNEU .GT.0) INTCAL = 20
      ELSEIF (MTP    .EQ.       102) THEN
         INTCAL = 18
      ELSEIF (MTP    .GE.       100) THEN
         IF (NHEVY+NGAMA.GT.0) INTCAL = 16
      ENDIF
      IF(NNEU+NHEVY+NGAMA.GT.0.AND.INTCAL.EQ.0) INTCAL=12
#if defined(ATLAS_MDEBUG)
      IF(NPHETC.GT.0 .and. IDEBUG.GT.1) THEN
         ETOT = EINC + 939.6
         PP = SQRT(ETOT*ETOT - 939.6*939.6)
         PIN = SQRT((UINC(1)*PP)**2+(UINC(2)*PP)**2+(UINC(3)*PP)**2)
         POUT = SQRT(PX**2+PY**2+PZ**2)
         WRITE (LOUT,901) NPHETC,NNEU,NRECOL,EETOT,UINC(1)*PP,
     +          UINC(2)*PP,UINC(3)*PP, PX,PY,PZ,PIN-POUT,NG,NH,NCEL,
     +          ANUC+AN,ZNUC,ATOT,ZTOT
 901     format(' ******* MICAP Collision summary *****',/,
     +           ' * No. of particles :',I4,/,
     +           ' * No. of neutrons  :',I4,/,
     +           ' * No. of heavy rec.:',I4,/,
     +           ' * Total Energy     :',F8.2,' MeV',/,
     +           ' * Px,Py,Pz in      :',3F8.2,/,
     +           ' * Px,Py,Pz out     :',3F8.2,/,
     +           ' * Pin - Pout       :',F8.2,/,
     +           ' * Ngamma           :',I8,/,
     +           ' * Nheavy           :',I8,/,
     +           ' * Medium no.       :',I8,/,
     +           ' * Amed + n         :',F8.1,/,
     +           ' * Zmed             :',F8.1,/,
     +           ' * Aout             :',F8.1,/,
     +           ' * Zout             :',F8.1)
      ENDIF
#endif
      RETURN
      END
