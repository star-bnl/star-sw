*
* $Id: gsxyz.F,v 1.1.1.2 2009/02/01 17:10:33 fisyak Exp $
*
* $Log: gsxyz.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:33  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:53  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:56  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:41  hristov
* Separate distribution  of Geant3
*
* Revision 1.2  2000/05/20 14:48:51  fca
* Do not rely on the memory being 0 in JX bank
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:43  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE G3SXYZ
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Store space point VECT in banks JXYZ                     *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Authors    R.Brun, F.Carena  *********                   *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcpush.inc"
#include "geant321/gckine.inc"
C.
C.    ------------------------------------------------------------------
C.
      IF(JXYZ.EQ.0)THEN
C
C               Create mother bank JXYZ
C
         CALL MZBOOK(IXDIV,JXYZ,JXYZ,1,'JXYZ',NCKINE,NCKINE,0,3,0)
         IQ(JXYZ-5)=0
      ENDIF
C
      NXYZ=IQ(JXYZ-2)
      IF(ITRA.GT.NXYZ)THEN
         NPUSH=MAX(10,ITRA-NXYZ)
         CALL MZPUSH(IXDIV,JXYZ,NPUSH,0,'I')
      ENDIF
C
C              Create bank to store points for primary track ITRA
C
      JX=LQ(JXYZ-ITRA)
      IF(JX.EQ.0)THEN
         CALL MZBOOK(IXDIV,JX,JXYZ,-ITRA,'JXYZ',0,0,NCJXYZ,3,0)
         Q(JX+1)=NCJXYZ-2.
         Q(JX+2)=3.
         IQ(JX)=NTMULT
C
C              Check if there is enough space to store current point
C
      ELSE
         NFREE=Q(JX+1)
         IF(NFREE.LE.6)THEN
            CALL MZPUSH(IXDIV,JX,0,NPJXYZ,'I')
            JX=LQ(JXYZ-ITRA)
            Q(JX+1)=Q(JX+1)+NPJXYZ
         ENDIF
C
         IF(NTMULT.NE.IQ(JX))THEN
            IQ(JX)=NTMULT
            IPOINT=JX+Q(JX+2)
            IF(Q(IPOINT).NE.0.) THEN
               Q(JX+2)=Q(JX+2)+3.*Q(IPOINT)+2.
               Q(JX+1)=Q(JX+1)-2.
               IPOINT=JX+Q(JX+2)
               Q(IPOINT)=0
            ENDIF
         ENDIF
      ENDIF
C
      IPOINT=JX+Q(JX+2)
      JP=IPOINT+3.*Q(IPOINT)+1.
      Q(IPOINT)=Q(IPOINT)+1.
      Q(IPOINT+1)=IPART
      DO 10 I=1,3
  10  Q(JP+I)=VECT(I)
      Q(JX+1)=Q(JX+1)-3.
C
      END
