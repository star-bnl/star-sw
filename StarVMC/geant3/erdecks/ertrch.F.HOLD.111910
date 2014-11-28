*CMZ :          31/07/2008  08.02.14  by  Alessandra Filippi / Stefano Piano
*CMZ :          24/11/95  16.28.16  by  S.Ravndal
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE ERTRCH
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    Average charged track is extrapolated by one step           *
C.    *                                                                *
C.    *    ==>Called by : ERTRGO                                       *
C.    *       Original routine : GTHADR                                *
C.    *       Authors   M.Maire, E.Nagy  *********                     *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gconst.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gckine.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcmulo.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcunit.inc"
#include "geant321/ertrio.inc"
#include "geant321/erwork.inc"

#if defined(CERNLIB_SINGLE)
      PARAMETER (EPSMAC=1.E-11)
#endif
#if !defined(CERNLIB_SINGLE)&&!defined(CERNLIB_IBM)
      PARAMETER (EPSMAC=5.E-6)
#endif
#if !defined(CERNLIB_SINGLE)&&defined(CERNLIB_IBM)
      PARAMETER (EPSMAC=5.E-5)
#endif
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION GKR,DEMEAN,STOPP1,STOPP2,STOPMX,STOPRG,STOPC
      DOUBLE PRECISION EKIPR
#endif
* modified by A.Rotondi & A.Fontana 
* march 2007
* (added and saved previous step PRSTEP for use with Urban model)
      REAL VNEXT(6),PRSTEP
      SAVE CFLD,CHARG2,RMASS,CUTPRO,IKCUT,STOPC,PRSTEP
*
*
** part modified by A.Filippi & S.Piano to comply with GTHADR (3.21) algorhytm 
** february09
* --->
      REAL THRESH
      REAL CUTEN
      LOGICAL ISHAD
      PARAMETER(THRESH=0.7, ONE = 1.)
      DATA ISHAD/.FALSE./
* <---------------------------------------------------------------------
C.
C.    ------------------------------------------------------------------
*
*
* *** Update local pointers if medium has changed
*
*
      IF (IUPD.EQ.0) THEN
         IUPD  = 1
         CHARG2 = CHARGE*CHARGE
         IF (IPART.LE.3) THEN
            CUTEK  = CUTELE
            RMASS  = 1.
*
** part modified by A.Filippi & S.Piano to comply with GTHADR (3.21) algorhytm 
** february09
* --->
            ISHAD = .FALSE.
            IF(IPART.EQ.3) THEN    ! electrons
              JRANG  = LQ(JMA-15)
              JCOEF  = LQ(JMA-17)
              JLOSS  = LQ(JMA-1)
            ELSEIF(IPART.EQ.2) then   ! positrons
              JRANG  = LQ(JMA-15) + NEK1
              JLOSS  = LQ(JMA-1)  + NEK1
              JCOEF  = LQ(JMA-17) + 3*NEK1
            ENDIF
         ELSE IF (IPART.LE.6) THEN
            CUTEK  = CUTMUO
            RMASS  = 1.
            ISHAD = .FALSE.
            JRANG  = LQ(JMA-16)
            JLOSS  = LQ(JMA-2)
            JCOEF  = LQ(JMA-18)
         ELSE
            CUTEK  = CUTHAD
            RMASS  = PMASS/AMASS
            ISHAD = .TRUE.
            JRANG  = LQ(JMA-16) + NEK1
            JLOSS  = LQ(JMA-3)
            JCOEF  = LQ(JMA-18) + 3*NEK1
* <--------------------------------------------------------------------
         ENDIF
         CUTPRO = MAX(CUTEK*RMASS,ELOW(1))
         IKCUT = GEKA*LOG10(CUTPRO) + GEKB
         GKR   = (CUTPRO - ELOW(IKCUT))/(ELOW(IKCUT+1) - ELOW(IKCUT))
         STOPC = (1.-GKR)*Q(JRANG+IKCUT) + GKR*Q(JRANG+IKCUT+1)
         CFLD  = 0.
         IF (FIELDM.NE.0.) CFLD = 3333.*DEGRAD*TMAXFD/ABS(FIELDM*CHARGE)
      ENDIF
*
* *** Compute current step size
*
      STEP   = BIG
      GEKRT1 = 1. - GEKRAT
*
* *** Step limitation due to energy loss (stopping range) ?
*
      IF (ILOSS*DEEMAX.GT.0.) THEN
*
** part modified by A.Filippi & S.Piano to comply with GTHADR (3.21) algorhytm 
** february09
* --->
         IF(GEKRAT.LT.THRESH) THEN
            I1 = MAX(IEKBIN-1,1)
         ELSE
            I1 = MIN(IEKBIN,NEKBIN-1)
         ENDIF
         I1 = 3*(I1-1)+1
         XCOEF1 = Q(JCOEF+I1)
         XCOEF2 = Q(JCOEF+I1+1)
         XCOEF3 = Q(JCOEF+I1+2)
         IF(XCOEF1.NE.0) THEN
            STOPP = -XCOEF2+SIGN(ONE,XCOEF1)* SQRT(XCOEF2
     +      **2 -(XCOEF3-GEKIN*RMASS/XCOEF1))
         ELSE
            STOPP = - (XCOEF3-GEKIN*RMASS)/XCOEF2
         ENDIF
         STOPMX = (STOPP - STOPC)/(RMASS*CHARG2)
         IF (STOPMX.LT.MIN(STEP,STMIN)) THEN
            STEP = STOPMX
            IPROC = 0
            IF(STEP.LE.0.)THEN
               DESTEP = GEKIN
               GEKIN  = 0.
               GETOT  = AMASS
               VECT(7)= 0.
               INWVOL = 0
               ISTOP  = 2
               NMEC = NMEC + 1
               LMEC(NMEC) = 30
               GO TO 100
            ENDIF
            GO TO 10
         ENDIF

         EKF = (1. - BACKTR*DEEMAX)*GEKIN*RMASS
         IF (EKF.LT.ELOW(1)) THEN
            EKF = ELOW(1)
            IKF = 1
         ELSEIF (EKF.GE.ELOW(NEKBIN)) THEN
            IKF = NEKBIN
            IF (EKF.GE.ELOW(NEK1)) THEN
               IKF = NEK1-1
               EKF = ELOW(NEK1)*0.99
            ENDIF
         ELSE
            IKF=GEKA*LOG10(EKF)+GEKB
         ENDIF
         GKR=(EKF-ELOW(IKF))/(ELOW(IKF+1)-ELOW(IKF))
         IF(GKR.LT.THRESH) THEN
            IK1 = MAX(IKF-1,1)
         ELSE
            IK1 = MIN(IKF,NEKBIN-1)
         ENDIF
         IK1 = 3*(IK1-1)+1
         YCOEF1=Q(JCOEF+IK1)
         YCOEF2=Q(JCOEF+IK1+1)
         YCOEF3=Q(JCOEF+IK1+2)
         IF(YCOEF1.NE.0.) THEN
            SLOSP = -YCOEF2+SIGN(ONE,YCOEF1)*SQRT(YCOEF2**2- (YCOEF3-
     +      EKF/YCOEF1))
         ELSE
            SLOSP = - (YCOEF3-EKF)/YCOEF2
         ENDIF
         SLOSP = ABS(STOPP - SLOSP)
         SLOSS = MAX(STMIN, SLOSP/(RMASS*CHARG2) )
         IF (SLOSS.LT.STEP) THEN
            STEP = SLOSS
            IPROC = 0
         ENDIF

      ENDIF

   10 CONTINUE
* <--------------------------------------------------------------------
*
* *** Step limitation due to energy loss in magnetic field ?
*
      IF (IFIELD*FIELDM.NE.0.) THEN
         SFIELD = CFLD*VECT(7)
         IF (SFIELD.LT.STEP) STEP = SFIELD
      ENDIF
*
* *** Compute point where to store error matrix
*
      LERST  = 0
      STEPER = BIG
      ASCL1  = BIG
      DO 20 IPR = 1,NEPRED
         STEPE  = BIG
c         IF (LELENG) STEPE = ERLENG(IPR) - SLENG
         IF (LEPLAN) THEN
            SCAL1 = 0.
            SCAL2 = 0.
            DO 18 I=1,3
               SCAL1 = SCAL1 + ERPLO(I,4,IPR)*(ERPLO(I,3,IPR)-VECT(I))
               SCAL2 = SCAL2 + ERPLO(I,4,IPR)*VECT(I+3)
   18       CONTINUE
            STEPE = SCAL1/SCAL2
         ENDIF
         IF (STEPE.LE.PREC) STEPE = BIG
c *******************************
c     A. Rotondi and L. Lavezzi (sept 2010)
c     when leleng (track length propagation) 
c     if track length (ERLENG) <= 0 then the
c     particle must not move; else if the step
c     is <= PREC it is set = to it
         IF (LELENG) THEN
            STEPE = ERLENG(IPR) - SLENG
            IF(ERLENG(IPR).LE.0.) THEN
               STEPE = 0
            ELSE IF(STEPE.LE.PREC) THEN
               STEPE = PREC
            ENDIF
         ENDIF
c *******************************



         IF (STEPE.LT.STEPER) THEN
            STEPER = STEPE
            INLIST = IPR
            IF (LEPLAN) ASCL1  = ABS (SCAL1)
         ENDIF
   20 CONTINUE
      IF (STEPER.LE.STEP)  THEN
         STEP  = STEPER
         LERST = 1
      ENDIF
*
* *** Step limitation due to geometry ?
*
      LNEXT  = 0
      IF (STEP.GE.0.95*SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
            STEP   = SNEXT + PREC
            LNEXT = 1
            IF ((STEPER-SNEXT).GT.(2*PREC)) LERST = 0
         ENDIF
      ENDIF
*
* *** Linear transport when no field or very short step
*
      IF (IFIELD.EQ.0.OR.STEP.LE.2*PREC) THEN
        IF (IGNEXT.NE.0) THEN
          DO 25 I = 1,3
            VECTMP = VECT(I) +STEP*VECT(I+3)
            IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
                  IF(VECT(I+3).NE.0.) THEN
                     VECTMP =
     +               VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*EPSMAC
#if defined(DEBUG)
                     WRITE(CHMAIL, 10000)
                     CALL GMAIL(0,0)
                     WRITE(CHMAIL, 10100) GEKIN, NUMED, STEP, SNEXT
                     CALL GMAIL(0,0)
10000 FORMAT(' Boundary correction in ERTRCH: ',
     +       '    GEKIN      NUMED       STEP      SNEXT')
10100 FORMAT(31X,E10.3,1X,I10,1X,E10.3,1X,E10.3,1X)
#endif
                  ENDIF
            ENDIF
            VOUT(I) = VECTMP
   25     CONTINUE
            INWVOL = 2
            NMEC = NMEC +1
            LMEC(NMEC) = 1
        ELSE
            DO 30 I = 1,3
               VOUT(I)  = VECT(I) +STEP*VECT(I+3)
   30       CONTINUE
        ENDIF
        DO 35 I = 4,6
          VOUT(I)  = VECT(I)
   35   CONTINUE
	GOTO 74
      ENDIF
*
* *** otherwise, swim particle in magnetic field
*
      NMEC = NMEC +1
      LMEC(NMEC) = 4
*
   50 LERST = 0
      LNEXT = 0
      CALL GUSWIM (CHTR , STEP, VECT, VOUT)
*
*     When near to boundary, take proper action (cut-step,crossing...)
      IF (STEP.GE.SAFETY) THEN
         INEAR = 0
         IF (IGNEXT.NE.0) THEN
           DO 51 I = 1,3
               VNEXT(I+3) = VECT(I+3)
               VNEXT(I) = VECT(I) +SNEXT*VECT(I+3)
   51      CONTINUE
           DO 52 I = 1,3
             IF (ABS(VOUT(I)-VNEXT(I)).GT.EPSIL) GO TO 55
   52      CONTINUE
           INEAR = 1
         ENDIF
*
   55    CALL GINVOL (VOUT,ISAME)
         IF (ISAME.EQ.0) THEN
           IF ((INEAR.NE.0).OR.(STEP.LT.EPSIL)) THEN
             INWVOL = 2
             NMEC = NMEC +1
             LMEC(NMEC) = 1	
             LNEXT = 1
           ELSE
*              Cut step
             STEP = 0.5*STEP
             IF (LMEC(NMEC).NE.24) THEN
               NMEC = NMEC +1
               LMEC(NMEC) = 24
             ENDIF
             GOTO 50
           ENDIF
         ENDIF
*
      ENDIF
*
*
*     preset plane reached  ?
   74 CONTINUE
c     25/02/2010: A. Rotondi and L. Lavezzi: sometimes when STEP is 
c     less than ASCL1 but their difference is very small (less than 
c     1E-6) the plane is not found properly (if the plane is virtual)
c     IF ((LEPLAN).AND.(STEP.GE.ASCL1)) THEN
      IF ((LEPLAN).AND.((STEP.GE.ASCL1)
     +     .OR.(ABS(STEP - ASCL1).LT.PREC))) THEN
         SCAL3 = 0.
         DO 28 I=1,3
            SCAL3=SCAL3+ERPLO(I,4,INLIST)*(ERPLO(I,3,INLIST)-VOUT(I))
   28    CONTINUE
         ASCL3 = ABS(SCAL3)
         SSCL1 = ASCL1/SCAL1
         IF (SCAL3*SSCL1.LT. -PREC) THEN
*            Cut step
             STEP  = STEP*(ASCL1/(ASCL1+ASCL3))
             NMEC  = NMEC +1
             LMEC(NMEC) = 24
             GOTO 50
         ELSE
c     25/02/2010: A. Rotondi and L. Lavezzi: for the same 
c     reason of change at line 335
c     IF(ASCL3.LE.PREC) LERST = 1
            IF(ASCL3.LE.(3. * PREC))  LERST = 1
         ENDIF
      ENDIF
*
        DO 75 I=1,6
           VECT(I) = VOUT(I)
   75   CONTINUE
*
      IF (LELENG.AND.(STEP.GE.STEPER)) LERST = 1
*
      SLENG = SLENG + STEP
*
* *** Now apply selected mechanisms
*
      IF (LNEXT.EQ.1) THEN
          INWVOL = 2
          NMEC = NMEC + 1
          LMEC(NMEC) = 1
      ENDIF
*
* *** apply energy loss : find the kinetic energy corresponding
*      to the new stopping range = stopmx -/+ step
*      (take care of the backtracking !)
*
      IF (ILOSS*DEEMAX.GT.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 3
*
** part modified by A.Filippi & S.Piano to comply with GTHADR (3.21) algorhytm 
** february09
*  --->
         STOPRG = STOPP - BACKTR*STEP*RMASS*CHARG2
	 IFK = IEKBIN
	 IF(XCOEF1.NE.0.) THEN
            EKIPR = XCOEF1*(XCOEF3+STOPRG*(2.*XCOEF2+STOPRG))
         ELSE
            EKIPR = XCOEF2*STOPRG+XCOEF3
         ENDIF

         DEMEAN=GEKIN - EKIPR/RMASS
         IF(DEMEAN.LE.5.*GEKIN*EPSMAC) THEN
            DEMEAN=(GEKRT1*Q(JLOSS+IEKBIN)+GEKRAT*Q(JLOSS+IEKBIN+1))
     +      *STEP*CHARG2
         ENDIF
c
         DESTEP = DEMEAN
         DESTEP=MAX(DESTEP,0.)
         GEKINT = GEKIN -BACKTR*DESTEP

         IF(ISHAD) THEN
            CUTEN = 1.*CUTEK      ! was 1.01 in GTHADR AF aug08
         ELSE
            CUTEN = CUTEK
         ENDIF
	 IF(GEKINT.GT.CUTEN) THEN
           DESTEL = DESTEP
           GEKIN  = GEKINT	
           GETOT  = GEKIN +AMASS
           VECT(7)= SQRT((GETOT+AMASS)*GEKIN)
           CALL G3EKBIN
         ELSE
            DESTEP = GEKINT
            DESTEL = DESTEP
            GEKIN  = 0.
            GETOT  = AMASS
            VECT(7)= 0.
            INWVOL = 0
            ISTOP  = 2
            NMEC = NMEC + 1
            LMEC(NMEC) = 30
         ENDIF
*---------------------------------------------------------------------
*     modified by A.Rotondi & A.Fontana 
*     october 2008 correcting a bug in the march 2007 version
*     (added DESTEP in the calling string for 
*     calculation with Urban model) 
         CALL ERLAND (STEP,Z,A,DENS,VECT(7),GETOT,AMASS,DESTEP,DEDX2)
         DEDX2  = DEDX2*CHARG2*CHARG2
	 PRSTEP = DESTEP
      ENDIF
 100  CONTINUE
*
* *** Propagate error matrix
*
      IF (.NOT. LEONLY) CALL ERPROP
*
* *** Store informations
*
      IF(LERST.EQ.1) THEN
         NMEC = NMEC + 1
         LMEC(NMEC) = 27
         CALL ERSTOR
      ENDIF
*
      END
