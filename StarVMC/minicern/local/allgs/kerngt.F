*
* $Id: kerngt.F,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
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
C CERN PROGLIB#         KERNGT          .VERSION KERNVMI  1.01  891208
C ORIG.  3/02/89  K.M.STORR
C
C--   STORE PAM TITLE INTO /SLATE/ FOR PRINTING BY GSHOWF

      COMMON /SLATE/ ISLATE(40)

      CALL UCTOH1 (
     + ' KERNVMI  1.10  1995/07/18 13.50        '
     +,            ISLATE, 40 )
      CALL GSHOWF (LUN)
      RETURN
      END
