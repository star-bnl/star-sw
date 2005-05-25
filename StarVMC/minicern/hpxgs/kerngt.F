*
* $Id: kerngt.F,v 1.1.1.1 2005/05/25 20:36:46 fisyak Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  2005/05/25 20:36:46  fisyak
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  1999/05/18 15:55:30  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:49:57  mclareni
* Kernlib
*
*
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    KERNGT          .VERSION KERNHPX  1.03  940511
C ORIG.  16/03/89
C
C--   Store Pam title into /SLATE/ for printing by GSHOWF

      COMMON /SLATE/ MCH(40)
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     + ' KERNHPX  1.04  1995/09/28 19.17        >')

      CALL CLEFT (VIDQQ,1,0)
      CALL UCTOH1   (
     + 'KERNHPX  1.04  1995/09/28 19.17         '
     +,  MCH, 40)
      CALL GSHOWF (LUN)
      RETURN
      END
