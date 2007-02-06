#include "geant321/pilot.h"
*CMZ :  3.21/02 03/07/94  17.57.24  by  S.Giani
*-- Author :
      SUBROUTINE GTMONP
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Tracks magnetic monopoles throug constant magnetic field     *
C.    *   along z-axis (requires (IFIELD.EQ.3) !). Computes step size  *
C.    *   and propagates particle through step.                        *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *       Authors    P. Schieferdecker ********                    *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcflag.inc"
#include "geant321/gcbank.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcmulo.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcstak.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcunit.inc"
#if defined(CERNLIB_USRJMP)
#include "geant321/gcjump.inc"
#endif
 
#if !defined(CERNLIB_OLD)
#include "geant321/gcvolu.inc"
#include "geant321/gcvdma.inc"
#endif
#if !defined(CERNLIB_SINGLE)
      PARAMETER (EPSMAC=1.E-6)
#endif
#if defined(CERNLIB_SINGLE)
      PARAMETER (EPSMAC=1.E-11)
#endif
      DOUBLE PRECISION GKR,DEMEAN,STOPP,STOPMX,STOPRG,STOPC,EKIPR
      DOUBLE PRECISION ONE,ECPY,R
      DOUBLE PRECISION BETA2
      DOUBLE PRECISION HMAG,HPERP,HPERP2,ELOSS1,ELOSS2

      PARAMETER (THRESH=0.7,ONE=1)
      REAL VNEXT(6)
      SAVE CFLD,CHARG2,RMASS,CUTPRO,IKCUT,STOPC
      REAL MASS, EOUT

      REAL XYZ(3),H(3)          ! position vector, magnetic field
      REAL RKSTEP               ! R-K numerical integration safe step size
      REAL RKBFLD               ! R-K B-field for safe step size computation
      PARAMETER  (RKTOL=1E-5 )  ! R-K tolerance
      PARAMETER  (RKCON=7388.)  ! R-K safety constant from fit

      idebug=2
      charge=1
*      if (idebug.gt.1) WRITE(*,*) "gtmonp called!!!"
* 
*     ------------------------------------------------------------------
*
* *** Particle below energy threshold ? short circuit
*
      IF (GEKIN.LE.CUTHAD) GO TO 100

*
* *** Update local pointers if medium has changed
*

      IF (IUPD.EQ.0) THEN
      if (idebug.gt.2) then
          R = SQRT(VECT(1)**2+VECT(2)**2)
          WRITE(*,'(I4,2X,2(5A4),(6G12.6))') NSTEP,NAMATE,NATMED,
     +        DENS,R,VECT(3), A,Z,RADL,Q(JPROB+9)
      ENDIF
         IUPD  = 1
         JLOSS = LQ(JMA-3)
         JRANG = LQ(JMA-16) + NEK1
         JCOEF = LQ(JMA-18) + 3*NEK1
         CHARG2 = CHARGE*CHARGE
         RMASS  = PMASS/AMASS 
         OMCMOL = Q(JPROB+21)*CHARG2
         CHCMOL = Q(JPROB+25)*ABS(CHARGE)
         CUTPRO = MAX(CUTHAD*RMASS,ELOW(1))
         IKCUT = GEKA*LOG10(CUTPRO) + GEKB
         GKR   = (CUTPRO - ELOW(IKCUT))/(ELOW(IKCUT+1) - ELOW(IKCUT))
         STOPC = (1.-GKR)*Q(JRANG+IKCUT) + GKR*Q(JRANG+IKCUT+1)
         IF (FIELDM.NE.0.) THEN
            CFLD = 3333.*DEGRAD*TMAXFD/ABS(FIELDM*CHARGE)
         ELSE
            CFLD = BIG
         ENDIF
         IF(IMCKOV.EQ.1) THEN
            JTCKOV = LQ(JTM-3)
            JABSCO = LQ(JTCKOV-1)
            JEFFIC = LQ(JTCKOV-2)
            JINDEX = LQ(JTCKOV-3)
            JCURIN = LQ(JTCKOV-4)
            NPCKOV = Q(JTCKOV+1)
         ENDIF
         IF(ISTRA.GT.0) THEN
            JTSTRA = LQ(JMA-19)
            JTSTCO = LQ(JTSTRA-1)
            JTSTEN = LQ(JTSTRA-2)
#if defined(CERNLIB_ASHO)
            IF(ISTRA.EQ.2) THEN
               JTASHO = LQ(JMA-20)
            ENDIF
#endif
         ENDIF
      ENDIF
*
* *** Compute current step size
*
      STEP   = STEMAX
      IPROC  = 103
      GEKRT1 = 1. -GEKRAT

*
*  ** Step limitation due to delta-ray production ?
*       (Cannot be tabulated easily because dependent on AMASS)
*
      IF (IDRAY.GT.0) THEN
         STEPDR = BIG
         IF (GEKIN.GT.DCUTM) THEN
            GAMASS = GETOT+AMASS
            TMAX   = EMASS*GEKIN*GAMASS/(0.5*AMASS*AMASS+EMASS*GETOT)
            IF (TMAX.GT.DCUTM) THEN
               BET2 = GEKIN*GAMASS/(GETOT*GETOT)
               Y    = DCUTM/TMAX
               SIG  = (1.-Y+BET2*Y*LOG(Y))/DCUTM
*              extra term for spin 1/2
               IF (AMASS.GT.0.9) SIG=SIG+0.5*(TMAX-DCUTM)/(GETOT*GETOT)
               SIG = SIG*Q(JPROB+17)*CHARG2*EMASS/BET2
*
               IF (SIG.GT.0.) THEN
                  STEPDR = 1./SIG
                  SDRAY  = STEPDR*ZINTDR
                  IF (SDRAY.LE.STEP) THEN
                     STEP  = SDRAY
                     IPROC = 10
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      IF (STEP.LE.0.) THEN
         STEP  = 0.
         GO TO 110
      ENDIF
*
*  **   Step limitation due to multiple scattering ?
*
      IF (IMULL.GT.0) THEN
         SMULS=MIN(2232.*RADL*VECT(7)**2/CHARGE**2,10.*RADL)
         SMULS  = MAX(STMIN, SMULS )
         IF (SMULS.LT.STEP) THEN
            STEP  = SMULS
            IPROC = 0
         ENDIF
      ENDIF
*
 10   CONTINUE
*
*  **   Step limitation due to Cerenkov production ?
*
      IF (IMCKOV.GT.0) THEN
         CALL GNCKOV
         STCKOV = MXPHOT/MAX(3.*DNDL,1E-10)
         SMULS  = MAX(STMIN, STCKOV)
         IF (SMULS.LT.STEP) THEN
            STEP  = STCKOV
            IPROC = 0
         ENDIF
      ENDIF
*
*  **   Step limitation due to geometry ?
*
      IF (STEP.GE.0.95*SAFETY) THEN
         CALL GTNEXT
         
         IF (IGNEXT.NE.0) THEN
            STEP   = SNEXT + PREC
            IPROC = 0
         ENDIF
*     
*     Update SAFETY in stack companions, if any
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 20 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST    = JSTAK + 3 + (IST-1)*NWSTAK
               Q(JST+11) = SAFETY
 20         CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
*  **   Step limitation due to energy loss? 
*
      BETA2 =1-(PMASS/(PMASS+ELOW(IEKBIN)))**2
      ELOSS1=Q(JLOSS+IEKBIN)*CHARG2*BETA2
      BETA2 =1-(PMASS/(PMASS+ELOW(IEKBIN+1)))**2
      ELOSS2=Q(JLOSS+IEKBIN+1)*CHARG2*BETA2
 11   DEMEAN=(GEKRT1*ELOSS1+GEKRAT*ELOSS2)*STEP
      IF(DEMEAN/GEKIN.GT.DEEMAX) THEN
         STEP=STEP*(DEEMAX*GEKIN/DEMEAN-0.001)
         IF (STEP.LE.STMIN) GOTO 100
         IF(LMEC(NMEC).NE.24) THEN
            NMEC = NMEC + 1
            LMEC(NMEC) = 24
         ELSE
            WRITE(*,'(I4,A)') NSTEP,' GTMONP> 2x EnLossCut!?' 
         ENDIF
         GOTO 11
      ENDIF
*     
*  **   Step limitation due to numerical integration 
*     
*  Numerical integration is for non-uniform fields only, then 
*  safety is given by:
*     RKCONS * DELTA**1/5 * P**2 / g E B 
*     
      IF(IFIELD.NE.3) THEN
         XYZ(1)=VECT(1)
         XYZ(2)=VECT(2)
         XYZ(3)=VECT(3)
         CALL GUFLD(XYZ,H)
         RKBFLD = SQRT(H(1)**2 + H(2)**2 + H(3)**2) 
         IF(RKBFLD.GT.0) THEN
            RKSTEP=RKCON*(RKTOL**0.2)*(VECT(7)**2)/
     +             (ABS(CHARGE)*GETOT*RKBFLD)
            if (idebug.gt.2) WRITE(*,*) 'RKSTEP=',RKSTEP,' STEP=',STEP
            IF (RKSTEP.LT.STEP) THEN
               STEP = RKSTEP
            ENDIF
         ENDIF
      ENDIF
*
* *** Linear transport when no field or very short step
*
 25   IF (IFIELD.EQ.0.OR.STEP.LE.PREC) THEN
*     
         IF (IGNEXT.NE.0) THEN
            DO 30 I = 1,3
               VECTMP  = VECT(I) +STEP*VECT(I+3)
               IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
                  IF(VECT(I+3).NE.0.) THEN
                     VECTMP =
     +               VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*EPSMAC
                     IF(NMEC.GT.0) THEN
                        IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                     ENDIF
                     NMEC=NMEC+1
                     LMEC(NMEC)=104
#if defined(CERNLIB_DEBUG)
                     WRITE(CHMAIL, 10000)
                     CALL GMAIL(0,0)
                     WRITE(CHMAIL, 10100) GEKIN, NUMED, STEP, SNEXT
                     CALL GMAIL(0,0)
10000 FORMAT(' Boundary correction in GTMONP: ',
     +       '    GEKIN      NUMED       STEP      SNEXT')
10100 FORMAT(31X,E10.3,1X,I10,1X,E10.3,1X,E10.3,1X)
#endif
                  ENDIF
               ENDIF
               VOUT(I) = VECTMP
 30         CONTINUE
            INWVOL = 2
            NMEC = NMEC +1
            LMEC(NMEC) = 1
         ELSE
            DO 40 I = 1,3
               VOUT(I)  = VECT(I) +STEP*VECT(I+3)
 40         CONTINUE
         ENDIF
      ELSE
*
* ***   otherwise, swim particle in magnetic field
*
#if !defined(CERNLIB_OLD)
         if(mycoun.gt.1.and.nfmany.gt.0.and.step.ge.safety)then
            nlevel=manyle(nfmany)
            do 99 i=1,nlevel
               names(i)=manyna(nfmany,i)
               number(i)=manynu(nfmany,i)
 99         continue
            call glvolu(nlevel,names,number,ier)
            if(ier.ne.0)print *,'Fatal error in GLVOLU'
            ingoto=0
         endif
#endif
         NMEC = NMEC +1
         LMEC(NMEC) = 4
*
* ***    Swim the monopole!
*     
         ECPY = GETOT
*     Call analytic solution only:
* 50      CALL GPARMP(CHARGE, STEP, AMASS, VECT, VOUT, EOUT)
*     Call R-K numerical integration only:
* 50      CALL GRKTMP(CHARGE, STEP, AMASS, VECT, VOUT, EOUT)
*     Let GUSWMP decide:
  50      CALL GUSWMP(CHARGE, STEP, AMASS, VECT, VOUT, EOUT)
         GETOT     = EOUT
         write(*,*) 'charge, eout ', charge,' ', eout
         GEKIN     = EOUT-AMASS
*     
*     ** When near to boundary, take proper action (cut-step,crossing...)
*     
         IF(STEP.GE.SAFETY)THEN
            INEAR = 0
            IF (IGNEXT.NE.0) THEN
               DO 60 I = 1,3
                  VNEXT(I+3) = VECT(I+3)
                  VNEXT(I) = VECT(I) +SNEXT*VECT(I+3)
 60            CONTINUE
               DO 70 I = 1,3
                  IF (ABS(VOUT(I)-VNEXT(I)).GT.EPSIL) GO TO 80
 70            CONTINUE
               INEAR = 1
            ENDIF
*     
 80         CALL GINVOL (VOUT, ISAME)
            IF (ISAME.EQ.0)THEN
               IF ((INEAR.NE.0).OR.(STEP.LT.EPSIL)) THEN
                  INWVOL = 2
                  NMEC = NMEC +1
                  LMEC(NMEC) = 1
               ELSE
*                 Cut step
                  STEP = 0.5*STEP
                  GETOT = ECPY
                  GEKIN = GETOT-AMASS
                  IF (LMEC(NMEC).NE.24) THEN
                     NMEC = NMEC +1
                     LMEC(NMEC) = 24
                  ENDIF
                  GO TO 50
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
*
* *** apply energy loss : use JLOSS data tables directly instead of
*     stopping range approach (-> beta2 convolution!)
*
      IF (ILOSL.NE.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 3
         DEMEAN=(GEKRT1*ELOSS1+GEKRAT*ELOSS2)*STEP
*     DEBUG
         PVERT(4) = DEMEAN
*     END DEBUG
         IF (ILOSS.EQ.4.OR.IEKBIN.LE.IKCUT+1) THEN
            DESTEP = DEMEAN
         ELSE
            DEMS = DEMEAN
            CALL GFLUCT(DEMS,DESTEP)
         ENDIF
         DESTEP=MAX(DESTEP,0.)
         GEKINT = GEKIN -DESTEP
         IF (GEKINT.LE.(1.01*CUTHAD)) GO TO 100
         DESTEL = DESTEP
         GEKIN  = GEKINT
         GETOT  = GEKIN +AMASS
         VECT(7)= SQRT((GETOT+AMASS)*GEKIN)
         CALL GEKBIN
      ENDIF
*     
      DO 90 I = 1,6
         VECT(I) = VOUT(I)
 90   CONTINUE
     
*     
* *** Correct the step due to multiple scattering
*
      IF (IMULL.NE.0) THEN
         STMULS = STEP
         CORR=0.0001*CHARG2*STEP/RADL/VECT(7)**2
         IF (CORR.GT.0.25) CORR = 0.25
         STEP  = (1.+CORR)*STEP
      ENDIF
*
      SLENG = SLENG + STEP
*
* *** Generate Cherenkov photons if required
*
      IF(IMCKOV.EQ.1) THEN
         CALL GGCKOV
         IF(NGPHOT.NE.0) THEN
            NMEC=NMEC+1
            LMEC(NMEC)=105
         ENDIF
      ENDIF

*
* *** Apply multiple scattering.
*
      IF (IMULL.NE.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 2
         CALL GMULTS
      ENDIF
*
* *** Update time of flight
*
      SUMLIF = SUMLIF -STEP*AMASS/VECT(7)
      TOFG   = TOFG +STEP*GETOT/(VECT(7)*CLIGHT)
      IF (TOFG.GE.TOFMAX) THEN
         ISTOP = 4
         NMEC  = NMEC +1
         LMEC(NMEC) = 22
         GO TO 999
      ENDIF
*
* *** Update interaction probabilities
*     
      IF (IDRAY.GT.0) ZINTDR = ZINTDR - STEP/STEPDR
*
      GO TO 110
*
*  **   Special treatment for overstopped tracks
*
 100  DESTEP = GEKIN
      DESTEL = DESTEP
      GEKIN  = 0.
      GAMMA  = GETOT/AMASS
      GETOT  = AMASS
      VECT(7)= 0.
      INWVOL = 0
      ISTOP  = 2
      NMEC = NMEC + 1
      LMEC(NMEC) = 30
      
*
* *** apply selected process if any
*
 110  IF (IPROC.EQ.0) GO TO 999
      NMEC = NMEC +1
      LMEC(NMEC) = IPROC
*
*  **   Delta-ray ?
*
      IF (IPROC.EQ.10) THEN
         CALL GDRAY
      ENDIF
 999  IF(NGPHOT.GT.0) THEN
         IF(ITCKOV.EQ.2.AND.ISTOP.EQ.0) THEN
*
*  The monopole has produced Cerenkov photons and it is still alive
*  we put it in the stack and we let the photons to be tracked
            NGKINE = NGKINE+1
            GKIN(1,NGKINE) = VECT(4)*VECT(7)
            GKIN(2,NGKINE) = VECT(5)*VECT(7)
            GKIN(3,NGKINE) = VECT(6)*VECT(7)
            GKIN(4,NGKINE) = GETOT
            GKIN(5,NGKINE) = IPART
            TOFD(NGKINE) = 0.
            ISTOP = 1
c----put position as well
            GPOS(1,NGKINE)=VECT(1)
            GPOS(2,NGKINE)=VECT(2)
            GPOS(3,NGKINE)=VECT(3)
         ENDIF
      ENDIF
      if (idebug.gt.1) then
         write (lout,1077) step, (vect(i),i=1,3), (vout(i),i=1,3)
1077     format(' step ',7f12.4) 
      endif
      END
