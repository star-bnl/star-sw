*
* $Id: gzebra.F,v 1.4 1998/08/20 16:51:28 perev Exp $
*
* $Log: gzebra.F,v $
* Revision 1.4  1998/08/20 16:51:28  perev
* decrease default size geant=1Mw paw=1Mw
*
* Revision 1.3  1998/07/20 20:17:30  perev
* Mods for exe SGI64 and HP rubber Zebra
*
* Revision 1.2  1998/04/16 16:59:50  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          07/12/97  17.29.02  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GZEBRA(NZEB)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to initialise ZEBRA store (//)                   *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    *       Modifications:                                           *
C.    *   PN: Ellastic zebra inspired by V.Perevozchikov               *
C.    ******************************************************************
C.
#include "gst/atcdes/typing.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
       INTEGER I1,I2,Itry,LL,KWW,memget,LOCF,NZEB,iudivd
C.
C.    ------------------------------------------------------------------
C.
       NZEBRA = abs(NZEB)
       NZEBRA = max(NZEBRA,100000)
       I1     = LOCF   (IQ)
       Do iTry=0,5
          I2  = memget (NZEBRA*4+1000)
          i2  = iudivd (I2,4)
          If (I2.GT.I1) GO TO 11
          NZEBRA = NZEBRA/2
          PRINT *,'* GZEBRA: requested memory size reduced to ',
     >               Nzebra,' *'
       enddo
       PRINT *,' iTRY,NZEBRA,I1,I2 =', iTRY,NZEBRA,I1,I2
       STOP    ' GZEBRA: FATAL - CAN NOT ALLOCATE MEMORY '
C
  11   LL     = max    (I2-I1,0)
       NZEBRA = LL + NZEBRA
       KWW    = LL + 100
C
       CALL MZSTOR (IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,
     >                                    LQ(KWW+100),LQ(NZEBRA-100))
       IF (IDEBUG.GE.3) CALL MZLOGL(IXSTOR,0)
       I1     = KWBANK
       CALL MZWORK (IXSTOR,LQ(I1),LQ(KWW),5)
       CALL GWORK  (KWBANK-100)
C
      END
 
 
 
 
 
