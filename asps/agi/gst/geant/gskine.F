*
* $Id: gskine.F,v 1.2 1998/04/16 16:59:41 fisyak Exp $
*
* $Log: gskine.F,v $
* Revision 1.2  1998/04/16 16:59:41  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          25/03/98  00.14.32  by  Pavel Nevski
*CMZ :  1.30/00 23/03/97  22.06.36  by  Pavel Nevski
*CMZ :  3.21/02 28/03/94  01.30.59  by  S.Giani
*-- Author :
      SUBROUTINE GSKINE(PLAB,IPART,NV,BUF,NWBUF,NT)
C.
C.    ******************************************************************
C.    *                                                                *
C     *       Store kinematics of track NT into data structure         *
C     *       Track is coming from vertex NV                           *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUKINE,GUSTEP                        *
C.    *       Authors    R.Brun, F.Carena, M.Hansroul  *********       *
C.    *                                                                *
C.    *    Modifications:                                              *
C.    *    PN: put all unknown particles as Geantino                   *
C.    ******************************************************************
C.
#include "gst/atcdes/typing.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcpush.inc"
#include "geant321/gcunit.inc"
#include "gst/atcdes/quest.inc"
 
      REAL      PLAB(3),BUF(1),P2,AMASS,E
      INTEGER   LGKINE,IPART,NV,NWBUF,NT,NTI,IER,JV,JPA,JK,JUK,NTG,
     >          I,J,NFREE,JKIN
C.
C.    ------------------------------------------------------------------
C.
      CALL AGNEED (IER)
      IF (IER.NE.0)     GO TO 90
 
      IF (NV.LE.0)      GO TO 10
      IF (JVERTX.LE.0)  GO TO 90
      IF (NV.GT.NVERTX) GO TO 90
      JV = LQ(JVERTX- NV)
      IF (JV.LE.0)      GO TO 90
C
   10 NT     = NTRACK + 1
      IF (JKINE.EQ.0) THEN
         CALL MZBOOK(IXDIV,JKINE,JKINE,1,'KINE',NCKINE,NCKINE,2,2,0)
         IQ(JKINE-5)=0
      ENDIF
      J = LgKINE(JKIN,NT)
      NTI=NT-IQ(JKIN+2)
      IF(NTI.GT.IQ(JKIN-2)) CALL MZPUSH(IXDIV,JKIN,NPKINE,0,'I')
      IF(NTI.GT.IQ(JKIN-2)) THEN
         PRINT *,' GSKINE ERROR: WRONG NTRACK ',NTRACK
         GOTO 90
      ENDIF
C
      IF (JPART.LE.0) GO TO 90
      JPA = 0
      IF (IPART.LE.IQ(JPART-2)) JPA = LQ(JPART- IPART)
      IF (JPA.LE.0)             JPA = LQ(JPART- 48)
 
      P2     = PLAB(1)**2 + PLAB(2)**2 + PLAB(3)**2
      AMASS  = Q(JPA + 7)
      E      = SQRT(P2 + AMASS*AMASS)
C
C            Store kinematics in JKINE
C
      NTRACK = NT
      IQ(JKIN+1)=NTRACK
      CALL MZBOOK(IXDIV,JK,JKIN,-NTI,'KINE',1,1,8,3,0)
      Q(JK + 1) = PLAB(1)
      Q(JK + 2) = PLAB(2)
      Q(JK + 3) = PLAB(3)
      Q(JK + 4) = E
      Q(JK + 5) = IPART
      Q(JK + 6) = NV
C
C            Copy user words
C
      IF(NWBUF.GT.0)THEN
         CALL MZBOOK(IXDIV,JUK,JK,-1,'KINU',0,0,NWBUF,3,0)
         IQ(JUK-5)=NT
C
         DO 15 I=1,NWBUF
            Q(JUK+I)=BUF(I)
   15    CONTINUE
      ENDIF
C
C
C            Connect track NT to vertex NV
C
      IF(NV.GT.0)THEN
         JV = LQ(JVERTX- NV)
         NTG = Q(JV + 7)
         NFREE=IQ(JV-1)-7-NTG
         IF(NFREE.LE.0)CALL MZPUSH(IXDIV,JV,0,max(2,NTG/5),'I')
         Q(JV + NTG + 8) = NT
         Q(JV + 7) = NTG + 1
      ENDIF
C
      GO TO 99
C
C            Error
C
  90  NT = 0
  99  RETURN
      END
 
 
