*
* $Id: csrmsl.F,v 1.2 1998/04/16 17:02:21 fisyak Exp $
*
* $Log: csrmsl.F,v $
* Revision 1.2  1998/04/16 17:02:21  fisyak
* 2nd pass with gstar
*
*
* ----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          23/05/97  19.31.06  by  Pavel Nevski
*CMZ :  1.19/01 14/11/94  18.10.10  by  Fons Rademakers
*-- Author :    Vladimir Berezhnoi   07/02/94
      SUBROUTINE CSRMSL(NAME1)
      CHARACTER *(*)NAME1
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      COMMON /CSTBCS/ ITBC,ITBS
      CHARACTER *80 LIBNAME,NAME,SYMBOL*32
      INTEGER CSLTGP
      COMMON /CSDEBUG/ ICSDEBUG
 
      if (ICSDEBUG.gt.0) print *,' ===> in CSRMSL (',NAME1,') <=== '
*
*-- match only file names (remove path)
*
      NAME = NAME1
      LN = LENOCC(NAME)
      IF (INDEX(NAME(:LN),'/') .EQ. 0) THEN
         LIBNAME = NAME
         NAME = '/'//LIBNAME
         LN = LN + 1
      ELSE
         DO 5 I = LN, 1, -1
            IF (NAME(I:I) .EQ. '/') THEN
               NAME = NAME(I:)
               LN = LENOCC(NAME)
               GOTO 6
            ENDIF
 5       CONTINUE
      ENDIF
*
 6    CONTINUE
*
      N=0
 1    CALL CS_SHL_GET(N,LIBNAME)
      IF (LIBNAME.NE.' ') THEN
        LL=LENOCC(LIBNAME)
        LS=LL-LN-2
        IF (LS .LT. 1) LS = 1
        IF (LIBNAME(LS:LL) .EQ. NAME(1:LN)//'.sl') THEN
           NS=-1
 10        CALL CS_SHL_SYMBOLS(LIBNAME(1:LL),NS,SYMBOL)
           IF(NS.EQ.-2) GO TO 20
           L=LENOCC(SYMBOL)
           IF(SYMBOL(L:L).EQ.'_') L=L-1
           CALL CLTOU (SYMBOL(:L))
           CALL CSCHID(SYMBOL(:L))
           IT=CSLTGP(IPVS)
           IF(IT.GT.0)CALL CSDPRO(IT)
           go to 10
 
 20        CALL CS_SHL_UNLOAD(LIBNAME(1:LL))
           ITBS=ITBS-1
           RETURN
        ENDIF
        N=N+1
        GOTO 1
      ENDIF
#endif
      END
