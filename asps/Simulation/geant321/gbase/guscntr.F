*
* $Id: guscntr.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: guscntr.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1  1998/02/10 13:04:47  atgeant3
* *** empty log message ***
*
*
#include "geant321/pilot.h"
      SUBROUTINE GUSCNTR(PRECOR)
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* Name : GUSCNTR                                                       *
*        (module)                                                      *
*                                                                      *
* Description :                                                        *
*        G U S C N T R (PRECOR)                                        *
*                                                                      *
*        .....       routine to control a looping near a boundary      *
*        .....                                                         *
*                                                                      *
* Arguments   :                                                        *
*                                                                      *
*        PRECOR  ([in | out | in/out]) [description]                   *
*                                                                      *
* Banks/Tables:                                                        *
*        [name]  ([filled | used | modified])                          *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* Author : Pavel Nevski                       Date : 25/05/93          *
*                                                                      *
* Last modifications :                                                 *
*        Never done                                                    *
*                                                                      *
* Keywords :                                                           *
*        [keywords]                                                    *
*----------------------------------------------------------------------*
      IMPLICIT NONE
*  - GEANT common GCTMED (version 3.15)
C   God I hope not, this must have come from Chris's convertion
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
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
     *    .AND.  ONLY.GT.0) THEN
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
10    FORMAT(' Nstep,icnt,IGnext,Prec,Safety,Snext,Precor=',2I5,I3,4F10.
     *4)
1     END         ! GUSCNTR
