*
* $Id: rzread.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: rzread.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:29  nevski
*  first working release
*
* Revision 1.5  2000/01/06 02:42:37  nevski
* elastic zebra updated
*
* Revision 1.2  1997/01/28 10:43:18  jamie
* bug fix for dble precision on record boundary from Marthe Brun
*
* Revision 1.1.1.1  1996/03/06 10:47:26  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE RZREAD(V,N,IPC,IFORM)
*
************************************************************************
*
*           Read N words from input buffer into V
*
* Called by RZIN,RZINS,RZVIN
*
*  Author  : R.Brun, B.Holl
*  Written : 07.05.86
*  Last mod: 17.05.93 Change test on IFOCON(1) to .LT.0
*        PN: 08.08.97 When exchange format does not conside with native,
*            (-FQXISN selected), if last copied record comes from LROUT,
*            current IRIN value was not saved.
*        PN: 27.08.99 declare V integer to avoid egcs complains
************************************************************************
#if !defined(CERNLIB_FQXISN)
#include "zebra/zunit.inc"
#endif
#include "zebra/rzcl.inc"
#include "zebra/rzclun.inc"
#include "zebra/rzcout.inc"
#include "zebra/rzk.inc"
#include "zebra/mzioc.inc"
      INTEGER V(*)
*
*-----------------------------------------------------------------------
*
*           Find first record
*
      NL1=LREC-IP1+1
      IF(IPC.LE.NL1)THEN
         IRS=IR1
         IS1=IP1+IPC-1
      ELSE
         NBEF=(IPC-NL1-1)/LREC
         IRS=IR2+NBEF
         IS1 =IPC-NL1-NBEF*LREC
      ENDIF
*
      LRIN=LQ(KQSP+LTOP-7)
      IF(LRIN.EQ.0)THEN
         CALL MZBOOK(JQPDVS,LRIN,LTOP,-7,'RZIN',0,0,LREC+1,2,-1)
         IQ(KQSP+LRIN-5)=IQ(KQSP+LTOP-5)
         IQ(KQSP+LTOP+KIRIN)=0
         IRIN=0
      ELSE
         IRIN=IQ(KQSP+LTOP+KIRIN)
      ENDIF
      LROUT=LQ(KQSP+LTOP-6)
      IF(LROUT.EQ.0)THEN
         IROUT=0
      ELSE
         IROUT=IQ(KQSP+LTOP+KIROUT)
      ENDIF

      IF(IRS.NE.IRIN)THEN
         IF(IRS.NE.IROUT)THEN
            CALL RZIODO(LUN,LREC,IRS,IQ(KQSP+LRIN+1),1)
            IF(IQUEST(1).NE.0)GO TO 90
            IRIN=IRS
            IQ(KQSP+LTOP+KIRIN)=IRIN
         ENDIF
      ENDIF
*
*           Read record into array V
*
#if !defined(CERNLIB_FQXISN)
      IF(IMODEX.GT.0.AND.IFORM.NE.1)THEN
         NWFOTT = N
         NWFODN = 0
         IF(IFORM.GT.0)THEN
            MFO(1) = IFORM
            MFO(2) = -1
            JFOEND = 2
         ENDIF
      ENDIF
#endif
      NLEFT=LREC-IS1+1
      IF(N.LE.NLEFT)THEN
         NP1=N
      ELSE
         NP1=NLEFT
      ENDIF

      IF(IRS.NE.IROUT)THEN
#if !defined(CERNLIB_FQXISN)
         IF(IMODEX.GT.0.AND.IFORM.NE.1)THEN
            NWFOAV=NP1
            CALL FZICV(IQ(KQSP+LRIN+IS1),V)
            IF(NWFOAV.GT.0.OR.IFOCON(1).LT.0)GO TO 95
            IF(NWFOAV.LT.0)IDOUB1=IQ(KQSP+LRIN+IS1+NP1-1)
            IQUEST(1)=0
         ELSE
#endif
            CALL UCOPY(IQ(KQSP+LRIN+IS1),V,NP1)
#if !defined(CERNLIB_FQXISN)
         ENDIF
#endif
      ELSE
#if !defined(CERNLIB_FQXISN)
         IF(IMODEX.GT.0.AND.IFORM.NE.1)THEN
            NWFOAV=NP1
            CALL FZICV(IQ(KQSP+LROUT+IS1),V)
            IF(NWFOAV.GT.0.OR.IFOCON(1).LT.0)GO TO 95
            IF(NWFOAV.LT.0)IDOUB1=IQ(KQSP+LROUT+IS1+NP1-1)
            IQUEST(1)=0
         ELSE
#endif
            CALL UCOPY(IQ(KQSP+LROUT+IS1),V,NP1)
#if !defined(CERNLIB_FQXISN)
         ENDIF
#endif
      ENDIF

      IF(NP1.LT.N)THEN
         NR=(N-NP1-1)/LREC+1
         IF(IRS.EQ.IR1)THEN
            IRS=IR2
         ELSE
            IRS=IRS+1
         ENDIF
         DO 60 I=1,NR
            IRin=IRS+I-1
            IF(I.NE.NR)THEN
#if !defined(CERNLIB_FQXISN)
               IF(IMODEX.GT.0.AND.IFORM.NE.1)THEN
                  CALL RZIODO(LUN,LREC,IRin,IQ(KQSP+LRIN+1),1)
                  IF(IQUEST(1).NE.0)GO TO 90
                  IQ(KQSP+LTOP+KIRIN)=IRIN
                  IF(NWFOAV.LT.0)THEN
                     CALL UCOPY2(IQ(KQSP+LRIN+1),IQ(KQSP+LRIN+2),LREC)
                     IQ(KQSP+LRIN+1)=IDOUB1
                     NWFOAV=LREC
                     CALL FZICV(IQ(KQSP+LRIN+1),V)
                     CALL UCOPY2(IQ(KQSP+LRIN+2),IQ(KQSP+LRIN+1),LREC)
                  ELSE
                     NWFOAV=LREC
                     CALL FZICV(IQ(KQSP+LRIN+1),V)
                  ENDIF
                  IF(NWFOAV.GT.0.OR.IFOCON(1).LT.0)GO TO 95
                  IF(NWFOAV.LT.0)IDOUB1=IQ(KQSP+LROUT+LREC)
                  IQUEST(1)=0
               ELSE
#endif
                  CALL RZIODO(LUN,LREC,IRin,V(NP1+1),1)
                  IF(IQUEST(1).NE.0)GO TO 90
#if !defined(CERNLIB_FQXISN)
               ENDIF
#endif
               NP1=NP1+LREC
            ELSE
               NL=N-NP1
               IF(IRIN.NE.IROUT)THEN
                  CALL RZIODO(LUN,LREC,IRIN,IQ(KQSP+LRIN+1),1)
                  IF(IQUEST(1).NE.0)GO TO 90
                  IQ(KQSP+LTOP+KIRIN)=IRIN
#if !defined(CERNLIB_FQXISN)
                  IF(IMODEX.GT.0.AND.IFORM.NE.1)THEN
                     IF(NWFOAV.LT.0)THEN
                       CALL UCOPY2(IQ(KQSP+LRIN+1),IQ(KQSP+LRIN+2),LREC)
                       IQ(KQSP+LRIN+1)=IDOUB1
                       NWFOAV=NL
                       CALL FZICV(IQ(KQSP+LRIN+1),V)
                       CALL UCOPY2(IQ(KQSP+LRIN+2),IQ(KQSP+LRIN+1),LREC)
                     ELSE
                       NWFOAV=NL
                       CALL FZICV(IQ(KQSP+LRIN+1),V)
                     ENDIF
                     IF(NWFOAV.GT.0.OR.IFOCON(1).LT.0)GO TO 95
                     IF(NWFOAV.LT.0)IDOUB1=IQ(KQSP+LRIN+NL)
                     IQUEST(1)=0
                  ELSE
#endif
                     CALL UCOPY(IQ(KQSP+LRIN+1),V(NP1+1),NL)
#if !defined(CERNLIB_FQXISN)
                  ENDIF
#endif
               ELSE
#if !defined(CERNLIB_FQXISN)
                 IF(IMODEX.GT.0.AND.IFORM.NE.1)THEN
                   IF(NWFOAV.LT.0)THEN
                     CALL UCOPY2(IQ(KQSP+LROUT+1),IQ(KQSP+LROUT+2),LREC)
                     IQ(KQSP+LROUT+1)=IDOUB1
                     NWFOAV=NL
                     CALL FZICV(IQ(KQSP+LROUT+1),V)
                     CALL UCOPY2(IQ(KQSP+LROUT+2),IQ(KQSP+LROUT+1),LREC)
                   ELSE
                     NWFOAV=NL
                     CALL FZICV(IQ(KQSP+LROUT+1),V)
                   ENDIF
                   IF(NWFOAV.GT.0.OR.IFOCON(1).LT.0)GO TO 95
                   IF(NWFOAV.LT.0)IDOUB1=IQ(KQSP+LROUT+NL)
                   IQUEST(1)=0
                 ELSE
#endif
                     CALL UCOPY(IQ(KQSP+LROUT+1),V(NP1+1),NL)
#if !defined(CERNLIB_FQXISN)
                  ENDIF
#endif
               ENDIF
            ENDIF
  60     CONTINUE
      ENDIF
  90  CONTINUE
#if defined(CERNLIB_QMVAX)
      IF(IRELAT.NE.0)UNLOCK(UNIT=LUN)
#endif
#if !defined(CERNLIB_FQXISN)
      GO TO 99
  95  IQUEST(1) =4
      IQUEST(11)=NWFOTT
      IQUEST(12)=NWFORE
      IQUEST(13)=NWFOAV
      IQUEST(14)=NWFODN
      IF(JBYT(IQ(KQSP+LTOP),15,3)-3.GE.-2) WRITE(IQLOG,1000)
 1000 FORMAT(' RZREAD. Error during conversion into native format')
#endif
  99  RETURN
      END

