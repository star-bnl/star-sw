*
* $Id: guscntr.F,v 1.2 1998/04/16 16:59:48 fisyak Exp $
*
* $Log: guscntr.F,v $
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
#include "gst/atcdes/typing.inc"
*  - GEANT common GCTMED (version 3.15)
#include "geant321/gctmed.inc"
*  - GEANT common GCTRACK (version 3.15)
#include "geant321/gctrak.inc"
*  - Multi-level current volume description
#include "geant321/gcvolu.inc"
*  - Geant logical units
#include "geant321/gcunit.inc"
*
      INTEGER ICNT,IPRI,MSTEP,LU
      REAL SAFEOLD,ONLY,PRECOR
      DATA ICNT,MSTEP /0,0/
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
      IF (ICNT.EQ.10) THEN
         IFIELD =0
      END IF
C   Check 11<=ICNT<=18
      IF (11.LE.ICNT.AND.ICNT.LE.18) THEN
         PRECOR=PRECOR*2
      END IF
C   Check 20<=ICNT<=20
      IF (20.LE.ICNT.AND.ICNT.LE.20) THEN
         IPRI =1
      END IF
C   Check NSTEP>=MSTEP+1000
      IF (NSTEP.GE.MSTEP+1000) THEN
         MSTEP = NSTEP
         IPRI=0
      END IF
C   Check NSTEP==MAXNST
      IF (NSTEP.EQ.MAXNST) THEN
         IPRI=3
      END IF
C   If Amax1(Abs(Vect(1)),Abs(Vect(2)))>Rmx \ Abs(Vect(3))>Zmx [Istop=1;]
C   Check IPRI>0
      IF (IPRI.LE.0)GO TO 1
      CALL GPCXYZ
      LU=LOUT
      WRITE(LU,10) NSTEP,ICNT,IGNEXT,PREC,SAFETY,SNEXT,PRECOR
10    FORMAT(5x,'Nstep,icnt,IGnext,Prec,Safety,Snext,Precor=',
     >       2I5,I3,4F10.4)
1     CONTINUE
      END         ! GUSCNTR
