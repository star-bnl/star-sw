* $Id: dzdgds.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzdgds.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:22  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:09  nevski
* dzdoc for agi
*
      SUBROUTINE DZDGDS(CHBANK,CLINE,NLT,NLU)

*...DZDGDS returns the bank descriptor in CLINE

      CHARACTER*80 CLINE

      CHARACTER CHBANK*(*)

*------OTTO-  KEY vector for RZIN/RZOUT

      INTEGER KEYVEC(2)

#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
#include "zebra/zbcdk.inc"
#include "dzdoc/linout.inc"
#include "dzdoc/docparq.inc"
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"

*--         One particular bank descriptor desired

      CLINE = ' '
      NLU = 1
      CALL UCTOH(CHBANK,KEYVEC(1),4,4)
*--   Read information in CWD on RZ file

      NCHA = LEN(CHBANK)
      IF(NCHA .GE. 8)THEN
           CALL UCTOH(CHBANK(5:),KEYVEC(2),4,4)
      ELSE
           CALL UCTOH('NONE',KEYVEC(2),4,4)
      ENDIF
      I1000=100000
      CALL RZIN(0,LQBKD,2,KEYVEC,I1000,'D')
      IF (IQUEST(1).NE.0 .OR. LQBKD .EQ. 0)THEN
           NLT = 0
           NLU = 0
           GOTO 999
      ENDIF
      INSERT = 25
      CLINE = 'XX'
      NCHOLL = (IQ(KQSP+LQBKD+INSERT)/16 - 1)*4
      IF(NCHOLL .GT. 0)THEN
            IF(NCHOLL .GT. 80)NCHOLL = 80
            INSERT = 27
            CALL UHTOC(IQ(KQSP+LQBKD+INSERT),4,CLINE,NCHOLL)
            IILF=INDEX(CLINE,'$LF')
            IF(IILF.NE.0)CLINE(IILF:)=' '
            NLT = INDXBC(CLINE,' ')
*      WRITE(*,*)' NCHOLL,NLT,LQBKD ', NCHOLL,NLT,LQBKD
      ENDIF
      CALL MZDROP(0,LQBKD,' ')
      LQBKD=0
  999 END
***********************************************************************
