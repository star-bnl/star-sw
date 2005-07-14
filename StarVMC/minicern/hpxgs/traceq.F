*
* $Id: traceq.F,v 1.1.1.1 2005/05/25 20:36:46 fisyak Exp $
*
* $Log: traceq.F,v $
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
      SUBROUTINE TRACEQ (LUNP,LEV)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNHPX  1.04  950928
C ORIG.  3/05/95  FR, JZ
C
*     unit number for standard output
      PARAMETER    (LUNSTP=6)

      LUN = LUNP
      IF (LUN.EQ.0)  LUN = LUNSTP
      WRITE (LUN,9001)
 9001 FORMAT (/' TRACEQ.  Trace-back printed to standard output.')

      CALL TRACEQC
      RETURN
      END
