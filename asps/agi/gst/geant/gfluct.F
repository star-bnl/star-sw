*
* $Id: gfluct.F,v 1.2 1998/04/16 16:59:32 fisyak Exp $
*
* $Log: gfluct.F,v $
* Revision 1.2  1998/04/16 16:59:32  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          14/10/97  20.16.21  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GFLUCT(DEMEAN,DE)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Subroutine to decide which method is used to simulate        *
C.    *   the straggling around the mean energy loss.                  *
C.    *                                                                *
C.    *                                                                *
C.    *   DNMIN:  <---------->1<-------->30<--------->50<--------->    *
C.    *                                                                *
C.    *  STRA=0   :                                                    *
C.    *   LOSS=2  <----------GLANDZ-------------------><--GLANDO-->    *
C.    *   LOSS=1,3<---------------------GLANDZ-------------------->    *
C.    *   LOSS=4              no call to GFLUCT is done                *
C.    *   STRA=1  <-----------PAI---------------------><--GLANDZ-->    *
C.    *   STRA=2  <---PAI----><---ASHO---><----PAI----><--GLANDZ-->    *
C.    *                                                                *
C.    *   DNMIN :  an estimation of the number of collisions           *
C.    *            with energy close to the ionization energy          *
C.    *            (see PHYS333)                                       *
C.    *                                                                *
C.    *   Input  : DEMEAN (mean energy loss)                           *
C.    *   Output : DE   (energy loss in the current step)              *
C.    *                                                                *
C.    *    ==>Called by : GTELEC,GTMUON,GTHADR                         *
C.    *                                                                *
C.    *   Modifications:                                               *
C.    *PN,27/07/97: For Gamma<1.1 PAI fluctuations are scaled as 1/v**2*
C.    *             Brems losses are added to ionization for all models*
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gconst.inc"
#include "geant321/gcmate.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcmulo.inc"
#include "geant321/gcphys.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
*
      PARAMETER (DGEV=0.153536 E-3, DNLIM=50)
      PARAMETER (ASHMIN=1,ASHMAX=30)
**
      DE=DEMEAN
      IF (STEP. LE.0) return
      IF (ILOSS.LE.0) return
 
      POTI  = Q(JPROB+9)
      GAMMA = GETOT/AMASS
      BETA  = VECT(7)/GETOT
      XI    = DGEV*CHARGE**2*STEP*DENS*Z/(A*BETA*BETA)
      DNMIN = MIN(XI,DEMEAN)/POTI
*
*PN,27/06/97:  PAI properly tabulated for GAMMA higher than 1.1
      IF (ILOSS.GT.1 .and. DNMIN.GE.DNLIM) THEN
 
         CALL GLANDO(ILOSS,STEP,Z,A,DENS,VECT(7),GETOT,AMASS,DELAND,I)
         DE = DEMEAN + DELAND
 
      else IF (ISTRA.GT.0 .and. DNMIN.LT.DNLIM) THEN
#if defined(CERNLIB_ASHO)
*        ISTRA = 2 --> PAI + URBAN + ASHO
         IF (DNMIN.GE.ASHMIN.AND.DNMIN.LT.ASHMAX .AND.ISTRA.EQ.2) THEN
             CALL GASHO(VECT(7),AMASS,STEP,DE)
         ELSE
             DE = GSTREN(GAMMA,DCUTE,STEP)
             If (Gamma.LE.1.1) DE=DE*.173554/(BETA*BETA)
         ENDIF
#endif
#if !defined(CERNLIB_ASHO)
         DE = GSTREN(max(GAMMA,1.1),DCUTE,STEP)
         If (Gamma.LE.1.1) DE=DE*.173554/(BETA*BETA)
#endif
      ELSE
 
         DEDX  = DEMEAN/STEP
         CALL GLANDZ(Z,STEP,VECT(7),GETOT,DEDX,DE,POTI,Q(JPROB+ 10))
 
      ENDIF
*
*        Add brem losses to ionisation
*        - wrong these are LOSSES, not a measured IONIZATION !!!
*
*      NB = 0
*      IF (ITRTYP.EQ.2) NB=2*NEK1
*      IF (ITRTYP.EQ.5) NB=  NEK1
*      IF (NB.GT.0) THEN
*         JBASE = LQ(JMA-1)+NB+IEKBIN
*         DE = DE +(1.-GEKRAT)*Q(JBASE)+GEKRAT*Q(JBASE+1)
*      ENDIF
*
      END
