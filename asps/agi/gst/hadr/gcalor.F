*
* $Id: gcalor.F,v 1.2 1998/04/16 16:58:46 fisyak Exp $
*
* $Log: gcalor.F,v $
* Revision 1.2  1998/04/16 16:58:46  fisyak
* 2nd pass with gstar
*
*
#if defined(CERNLIB_HADR)
#include "geant321/pilot.h"
*CMZ :          16/12/97  14.29.01  by  Pavel Nevski
*CMZ :  1.04/08 31/08/95  12.01.02  by  Christian Zeitnitz
*-- Author : Christian Zeitnitz
      SUBROUTINE GCALOR
********************************************************************
*                                                                  *
* PURPOSE: GEANT interface to CALOR                                *
*                                                                  *
* CALLED BY : GUHADR                                               *
*                                                                  *
* INPUT :  particle, material, and probabilities via GEANT common  *
*                                                                  *
* OUTPUT : COMMON GCKING, DESTEP                                   *
*          KCALL  = -1  : Nothing done                             *
*                 =  0  : NMTC has been called                     *
*                 =  1  : MICAP has been called                    *
*                 =  2  : HETC/SKALE has been called               *
*                 =  3  : FLUKA has been called                    *
*                                                                  *
* AUTHOR : C.Zeitnitz (University of Arizona)                      *
*                                                                  *
********************************************************************
C.
C. --- GEANT Commons
#include "geant321/gcbank.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcphys.inc"
#include "geant321/gctrak.inc"
#include "geant321/gsecti.inc"
#include "geant321/gconst.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcflag.inc"
C --- CALOR - GEANT Interface common
#include "gst/hadr/calgea.inc"
#include "geant321/cerrcm.inc"
#include "geant321/camass.inc"
C
C  Avogadro number multiplied by 1.E-24
      PARAMETER(XNAVO = 0.60221367)
C
      DIMENSION NNPART(12)
      LOGICAL INIT,GOFLUK,DOSKAL,FMICAP,SKALEF,NABSOR,FSTOP
      DOUBLE PRECISION DECIN,DMASS
C
      DATA INIT /.TRUE./
      SAVE INIT
C
      IF ( INIT ) THEN
C
C     initialize CALOR
         CALL CALINI
C
         INIT = .FALSE.
C
      ENDIF
      KCALL = -1
C
C get CALOR particle type
      IPINC = -1
      IF(IPART .LE. 48 )  IPINC = IGECAL(IPART)
#if defined(CERNLIB_CDEBUG1)
C
      PRINT *,' GCALOR: Call CALOR - IPART=',IPART,' GEKIN=',GEKIN
#endif
C
C energy in MeV
 
      EINC   =GEKIN * 1000.0
      UINC(1)=VECT(4)
      UINC(2)=VECT(5)
      UINC(3)=VECT(6)
      KCASE=NAMEC(12)
      NGKINE = 0
      NABSOR = .FALSE.
      FSTOP = .FALSE.
C ----- particle has to be stopped ? -------
      IF(GEKIN.LT.CUTHAD.AND.ITRTYP.EQ.4) THEN
#if defined(CERNLIB_CDEBUG)
C          particle = Pi- => Pi- capture marked by energy of 1 MeV
         IF(IPART.EQ.9) PRINT *,' GCALOR: PI- capture selected '
#endif
         FSTOP = .TRUE.
         ISTOP = 2
         IF(IPART .EQ. 9) THEN
            NABSOR = .TRUE.
            ISTOP = 1
            EINC = 1.0
            IF(GEKIN.GT.EINC/1000.) DESTEP = DESTEP + GEKIN - EINC/
     +      1000.0
            GEKIN = 0.0
            VECT(7) = 0.0
            KCASE = NAMEC(18)
            NMEC = NMEC + 1
            LMEC(NMEC) = 18
         ELSE
            DESTEP = DESTEP + GEKIN
            GEKIN = 0.0
            VECT(7) = 0.0
            IF(IPART.EQ.8.OR.IPART.EQ.11.OR.IPART.EQ.12) THEN
              CALL GDECAY
              KCASE = NAMEC(5)
              NMEC = NMEC + 1
              LMEC(NMEC) = 5
            ENDIF
            RETURN
         ENDIF
      ELSE IF(GEKIN.LT.CUTNEU.AND.IPART.EQ.13) THEN
         IF(GEKIN.LT.1.E-14) EINC=1.E-11
         ISTOP = 1
         NABSOR = .TRUE.
      ENDIF
      IF(ISTOP.EQ.2.OR.GEKIN.EQ.0.0) RETURN
C
C ------------- check if FLUKA has to be called ---------
C ------------------------------------------------- Goto FLUKA ?
C
      DOSKAL = (IPINC.EQ.0 .OR. IPINC.EQ.1) .AND. GEKIN.GT.EMAXP
      DOSKAL = DOSKAL .OR. (GEKIN .GT. EMAXPI .AND. (IPINC .GT. 1))
      IF(ICPROC.GE.0) THEN
         GOFLUK = ICPROC.EQ.3 .OR. IPINC.EQ.-1
         DOSKAL = DOSKAL .AND. ICPROC.EQ.2
      ELSE
         GOFLUK = IPINC .EQ. -1 .OR. GEKIN .GE. ESKALE
         DOSKAL = DOSKAL .AND. .NOT.GOFLUK
         GOFLUK = GOFLUK .OR. (DOSKAL.AND.SKALEF(IPINC,GEKIN,ESKALE))
         GOFLUK = GOFLUK .AND. .NOT.FSTOP .AND. .NOT.NABSOR
      ENDIF
      ICPROC = -1
C ------------------------------------------- call FLUKA
      IF(GOFLUK) THEN
#if defined(CERNLIB_CDEBUG1)
         PRINT *,' CALL FLUKA '
#endif
         CALL FLUFIN
         KCALL = 3
         RETURN
      ENDIF
      CERRF = .FALSE.
      IF(IPINC .EQ. 1 .AND. EINC .LE. 20.0) THEN
C MICAP needs only GEANT material number
         NCEL = NMAT
C --- low energetic neutron -> call micap
#if defined(CERNLIB_MDEBUG)
         PRINT *,' CALL MICAP '
#endif
         CALL MICAP
         KCALL = 1
      ELSE
         NCEL = 1
         AMED(1) = A
         ZMED(1) = Z
         DMED(1) = DENS/A*XNAVO
         IF(INT(A) .EQ. 1) THEN
            HDEN = DMED(1)
         ELSE
            HDEN = 0.0
         ENDIF
C ------- get material parameter for a mixture---------------------
         KK=MIN1(ABS(Q(JMA+11)),100.)
         NCEL = 1
         IF(KK.GT.1) THEN
            HDEN = 0.0
            NCEL = 0
            AMOL = Q(LQ(JMIXT-1) + 2)
            DO 10 K=1,KK
               IF(NINT(Q(JMIXT+K)).EQ.1) THEN
C                           hydrogen density
                  XMOLCM = DENS/AMOL*XNAVO
                  WI = Q(JMIXT+K+2*KK)*AMOL/Q(JMIXT+K)
                  HDEN = HDEN + XMOLCM * WI
               ELSE
                  NCEL = NCEL + 1
                  AMED(NCEL) = Q(JMIXT+K)
                  ZMED(NCEL) = Q(JMIXT+K+KK)
C                                        molekuls/cm^3
                  XMOLCM = DENS/AMOL*XNAVO
C                                     number of atoms per molecule
                  WI = Q(JMIXT+K+2*KK)*AMOL/AMED(NCEL)
C                                        atoms/cm^3
                  DMED(NCEL) = XMOLCM * WI
               ENDIF
#if defined(CERNLIB_CDEBUG)
               PRINT '('' GCALOR: A('',I5,'')='',F7.2,'' Z='',F7.2,    '
     +         //'                '' WI='',F7.3,'' DEN='',F7.3,'' Hden='
     +         //''',F7.3)', K,AMED(K),ZMED(K),WI,DMED(K),HDEN
#endif
   10       CONTINUE
         ENDIF
#if defined(CERNLIB_CDEBUG1)
C --- call hetc -----
         PRINT *,' CALL HETC '
#endif
         CALL CHETC(DOSKAL)
         KCALL = 0
         IF(DOSKAL) KCALL = 2
      ENDIF
C error ocurred in CALOR ?
      IF(CERRF) THEN
         WRITE(IERRU,'('' NEVT,IPART,Ek,NMED,ISTOP,NABSOR,FSTOP :'',   '
     +   //'          I10,I5,G15.6,2I6,2L6)') IEVENT,IPART,GEKIN,NMAT,
     +   ISTOP,NABSOR,FSTOP
      ENDIF
#if defined(CERNLIB_CDEBUG1)
C
C ---------------- store secondaries ---------------
C
      PRINT *,' AFTER CALOR NPART=',NPHETC
#endif
      ESUM =0.
      EKSUM = 0.
      PX = 0.
      PY = 0.
      PZ = 0.
      NGKINE = 0
      PSUM = 0.
C
      ZINTHA=GARNDM(6)
      SLHADR=SLENG
      STEPHA=BIG
C
      IF(NPHETC.EQ.0.AND.NABSOR) ISTOP = 2
C neutron has been absorbed -> INTCAL=18
      IF(INTCAL.EQ.18) ISTOP = 1
      IF(NPHETC.LE.0) GOTO 160
C
C too many particles in the CALOR array for GEANT
C happens sometimes with deexitation gammas and evaporation neutrons
C simple approach to combine particles and sum up their energies, but
C forget about momentum conservation
C
      IF(NPHETC.GT.MXGKIN) THEN
   20    CONTINUE
         DO 30 I=1,12
            NNPART(I)=0
   30    CONTINUE
         NNTOT = 0
         DO 40 I=1,NPHETC
            IF(IPCAL(I).NE.-1) THEN
               NNPART(IPCAL(I)+1)=NNPART(IPCAL(I)+1)+1
               NNTOT = NNTOT + 1
            ENDIF
   40    CONTINUE
         IF(NNTOT.LE.MXGKIN) GOTO 100
         JMAX=0
         IMAX=0
         DO 50 I=1,12
            IF(JMAX.LT.NNPART(I)) THEN
               JMAX=NNPART(I)
               IPI=I-1
            ENDIF
   50    CONTINUE
         DO 60 I=1,NPHETC
            IF(IPCAL(I).EQ.IPI) GOTO 70
   60    CONTINUE
   70    I1=I
         DO 80 I=I1+1,NPHETC
            IF(IPCAL(I).EQ.IPI) GOTO 90
   80    CONTINUE
   90    I2=I
         ECINI = EKINET(I1)
         DMASS = DBLE(XMASS(IPI))*1.D3
         DECIN = DBLE(ECINI)
         PPI = SNGL(DSQRT(DECIN*DECIN + 2.D0*DECIN*DMASS))
         IPJ = IPCAL(I2)
         ECINJ = EKINET(I2)
         DECIN = DBLE(ECINJ)
         PPJ = SNGL(DSQRT(DECIN*DECIN + 2.D0*DECIN*DMASS))
         ECIN = SNGL(DBLE(ECINI)+DBLE(ECINJ)+DMASS)
         EKINET(I1) = ECIN
         PP = SNGL(DSQRT(DBLE(ECIN*ECIN) + 2.D0*DBLE(ECIN)*DMASS))
C determine new direction cosines
         UCAL(I1,1) = (PPI*UCAL(I1,1)+PPJ*UCAL(I2,1))/PP
         UCAL(I1,2) = (PPI*UCAL(I1,2)+PPJ*UCAL(I2,2))/PP
         UCAL(I1,3) = (PPI*UCAL(I1,3)+PPJ*UCAL(I2,3))/PP
         USUM = SQRT(UCAL(I1,1)**2+UCAL(I1,2)**2+UCAL(I1,3)**2)
C normalize direction cosines
         IF(USUM.LT.0.0001) THEN
C direction is isotropic distributed
            CALL AZIRN(SINA,COSA)
            COSP = SFLRAF(DUM)
            SINP = SQRT(1.0-COSP*COSP)
            UCAL(I1,1) = SINP * COSA
            UCAL(I1,2) = SINP * SINA
            UCAL(I1,3) = COSP
         ELSE
            UCAL(I1,1) = UCAL(I1,1)/USUM
            UCAL(I1,2) = UCAL(I1,2)/USUM
            UCAL(I1,3) = UCAL(I1,3)/USUM
         ENDIF
C particle I2 vanished
         IPCAL(I2)=-1
         GOTO 20
C end of particle combination
  100    CONTINUE
C sort particles
         I2=NPHETC
         DO 120 I = 1,NPHETC
            IF(I.GE.I2) GOTO 130
            IF(IPCAL(I).EQ.-1) THEN
               DO 110 J = I2,I,-1
                  IF(IPCAL(J).NE.-1) THEN
                     IPCAL(I) = IPCAL(J)
                     EKINET(I) = EKINET(J)
                     UCAL(I,1) = UCAL(J,1)
                     UCAL(I,2) = UCAL(J,2)
                     UCAL(I,3) = UCAL(J,3)
                     I2 = J-1
                     GOTO 120
                  ENDIF
  110          CONTINUE
            ENDIF
  120    CONTINUE
  130    CONTINUE
         NPHETC=MXGKIN
      ENDIF
C
      IF(INTCAL.LT.1.OR.INTCAL.GT.30) INTCAL=12
      KCASE = NAMEC(INTCAL)
      IF(INTCAL.NE.12) THEN
        NMEC = NMEC + 1
        LMEC(NMEC) = INTCAL
      ENDIF
      DO 140 I=1,NPHETC
         IP=IPCAL(I)
         IGPART=ICALGE(IP)
         IF ( IGPART.EQ.0 ) THEN
            PRINT*,'>>> ERROR GCALOR: Particle type ',IP, ' not '
     +      //'implemented in GEANT'
            GOTO 140
         ENDIF
#if defined(CERNLIB_CDEBUG1)
         PRINT *,' GCALOR after CALOR: IP=',IGPART,' Ekin=',EKINET(I)
#endif
C
C store particle
         ECIN = EKINET(I)/1000.0
         IF(ECIN.LT.1.E-15) GOTO 140
         DECIN = DBLE(ECIN)
         DMASS = DBLE(XMASS(IP))
         PP = SNGL(DSQRT(DECIN*DECIN + 2.0D0*DECIN*DMASS))
#if defined(CERNLIB_CDEBUG)||defined(CERNLIB_MDEBUG)
         IF(IP.GE.2.AND.IP.LE.4) THEN
           ESUM = ESUM + ECIN + XMASS(IP)
         ELSE
           ESUM = ESUM + ECIN
         ENDIF
         EKSUM = EKSUM + ECIN
#endif
         PX = PX + PP*UCAL(I,1)
         PY = PY + PP*UCAL(I,2)
         PZ = PZ + PP*UCAL(I,3)
#if defined(CERNLIB_CDEBUG)||defined(CERNLIB_MDEBUG)
         PRINT '('' IP,Ek,Px,Py,Pz ='',I4,4(1X,E15.7))',
     +          IGPART,ECIN,PX,PY,PZ
#endif
C generated particle eq incoming
         IF(NPHETC.EQ.1 .AND. IGPART.EQ.IPART) THEN
            VECT(4) = UCAL(I,1)
            VECT(5) = UCAL(I,2)
            VECT(6) = UCAL(I,3)
            VECT(7) = PP
            GEKIN = ECIN
            GETOT = SNGL(DECIN + DMASS)
            TOFG = TOFG + CALTIM(I)
            ISTOP = 0
            IF(NABSOR) ISTOP = 2
            GOTO 160
         ENDIF
C
         NGKINE=NGKINE+1
         GKIN(1,NGKINE) = PP*UCAL(I,1)
         GKIN(2,NGKINE) = PP*UCAL(I,2)
         GKIN(3,NGKINE) = PP*UCAL(I,3)
C the total energy is critical for ECIN below 1.E-8 GeV because of
C single precision of GKIN (normalization when mass is added)!!
C luckely GEANT does use only the momentum components when storing the
C particle on the stack.
         GKIN(4,NGKINE) = SNGL(DECIN+DMASS)
         GKIN(5,NGKINE) = FLOAT(IGPART)
         TOFD(NGKINE)   = CALTIM(I)
         GPOS(1,NGKINE) = VECT(1)
         GPOS(2,NGKINE) = VECT(2)
         GPOS(3,NGKINE) = VECT(3)
         IF(NGKINE.GE.MXGKIN) GOTO 150
C
  140 CONTINUE
  150 CONTINUE
C particle lost its identity
      ISTOP=1
  160 CONTINUE
#if defined(CERNLIB_CDEBUG)||defined(CERNLIB_MDEBUG)
      EPREC = AMED(1)*0.93149432
      PIX = VECT(4)*VECT(7)
      PIY = VECT(5)*VECT(7)
      PIZ = VECT(6)*VECT(7)
      ESUM = ESUM + ERMED(1)/1000. + AMASS
      PSUM = SQRT((PX-PIX)**2 + (PY-PIY)**2 + (PZ-PIZ)**2)-
     +       SQRT((EPREC+ERMED(1)/1000.)**2 -EPREC**2)
      PRINT '('' AFTER INTERACTION: Einc,Pinc = '',2F7.3,
     + '' Ediff,Pdiff='',2F7.3)',GETOT,VECT(7),
     + GETOT-ESUM,  PSUM
#endif
C
C
      NGKINE = MIN(NGKINE,MXGKIN)
C
C score kinetic energy of recoil nucleus (given in MeV)
CZ      DESTEP = DESTEP + ERMED * 1.E-3
  170 RETURN
      END
#endif
