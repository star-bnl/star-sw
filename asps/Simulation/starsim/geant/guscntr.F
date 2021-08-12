*
* $Id: guscntr.F,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Log: guscntr.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:45  nevski
*  first working release
*
* Revision 1.3  2000/01/06 22:09:18  nevski
* unlimited track/vertex version
*
* Revision 1.2  1998/04/16 16:59:48  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :  1.30/00 12/05/97  15.31.50  by  Pavel Nevski
*CMZU:  1.00/00 17/03/94  10.22.21  by  Andrea DellAcqua
*-- Author :    *MRT : by Pavel Nevski
*----------------------------------------------------------------------*
      SUBROUTINE GUSCNTR(PRECOR)
*                                                                      *
* Description :  routine to control a looping near a boundary          *
* Arguments   :  PRECOR  (modified) GEANT tracking precision           *
*                                                                      *
* Author      :  Pavel Nevski                       Date : 25/05/93    *
* Modification:  Never done                                            *
*----------------------------------------------------------------------*
#include "commons/typing.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "atlsim/agctrac.inc"
*
      INTEGER ICNT,IPRI,MSTEP,LU
      REAL SAFEOLD,ONLY,PRECOR
      DATA ICNT,MSTEP /0,0/
c
      IF (IGTRACK(1).lt.0) Return
      IF (ICNT.EQ.10) ISTRACK(1) = ISTRACK(1) + 1
c
      IPRI=0
      SAFEOLD = SAFETY+STEP
      ONLY=GONLY(NLEVEL)
C   Check IGNEXT==1 & SAFETY<0 & STEP<10*PREC & ONLY>0
      IF (IGNEXT.EQ.1 .AND. SAFETY.LT.0 .AND. STEP.LT.10*PREC
     *    .AND.  ONLY.GT.0 .AND. INWVOL.EQ.0) THEN
         ICNT=ICNT+1
      ELSE
         ICNT=0
      END IF
C   Check ICNT==10
      IF (ICNT.GT.10) THEN
         IFIELD =0
      END IF
C   Check 11<=ICNT<=19
      IF (11.LE.ICNT.AND.ICNT.LE.19) THEN
         PRECOR=PRECOR*2
      END IF
C   Check 20<=ICNT<=20
      IF (20.LE.ICNT.AND.ICNT.LE.20) THEN
         IPRI = IDEBUG
      END IF
C   Check NSTEP>=MSTEP+1000
      IF (NSTEP.GE.MSTEP+1000) THEN
         MSTEP = NSTEP
         IPRI=0
      END IF
C   Check NSTEP==MAXNST
      IF (NSTEP.EQ.MAXNST) THEN
         IPRI = IDEBUG
      END IF
C   If Amax1(Abs(Vect(1)),Abs(Vect(2)))>Rmx \ Abs(Vect(3))>Zmx [Istop=1;]
C   Check IPRI>0
      IF (IPRI.LE.0) Return
      CALL GPCXYZ
      LU=LOUT
      WRITE(LU,10) NSTEP,ICNT,IGNEXT,PREC,SAFETY,SNEXT,PRECOR
10    FORMAT(5x,'Nstep,icnt,IGnext,Prec,Safety,Snext,Precor=',
     >       2I5,I3,4F10.4)
      END         ! GUSCNTR

