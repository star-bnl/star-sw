*CMZ :          22/01/99  15.54.06  by  Rene Brun
*-- Author :    Federico Carminati   28/11/98
      SUBROUTINE GCOHIT(CHCOMM,IADD)
#include "asps/Simulation/starsim/include/commons/agcrdig.inc"
*
      CHARACTER *(*) CHCOMM
#if defined(CERNLIB_DECS) || defined(CERNLIB_LXIA64)
      INTEGER*8 GCADDB, GCADDI, GCADDF, GCADDL, GCADDC, GCADDD, IADD
#else          
      INTEGER GCADDB, GCADDI, GCADDF, GCADDL, GCADDC, GCADDD
#endif
*
      IF(CHCOMM.EQ.'AGCRDIG') THEN
         IADD=GCADDI(idigi)
      ELSE IF(CHCOMM.EQ.'AGCHITV') THEN
         IADD=GCADDI(Iprin)
      ELSE
         WRITE(6,123) CHCOMM
         IADD=0
      ENDIF
*
 123  FORMAT(' *** GCOMAD: cannot load common ',A)
      END
