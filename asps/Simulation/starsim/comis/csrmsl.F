*
* $Id: csrmsl.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: csrmsl.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.3  2002/02/21 01:11:50  nevski
* extend pathname in gexec command
*
* Revision 1.2  2001/06/15 23:00:20  nevski
* if-endif corrected
*
* Revision 1.1  2001/02/27 10:15:13  nevski
*  first working release
*
* Revision 1.2  1998/04/16 17:02:21  fisyak
* 2nd pass with gstar
*
*
* ----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          23/05/97  19.31.06  by  Pavel Nevski
*CMZ :  1.19/01 14/11/94  18.10.10  by  Fons Rademakers
*-- Author :    Vladimir Berezhnoi   07/02/94
#if !defined(CERNLIB_COMISF)
      SUBROUTINE CSRMSL(NAME1)
      CHARACTER *(*)NAME1
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      COMMON /CSTBCS/ ITBC,ITBS
      CHARACTER*256 LIBNAME,NAME,SYMBOL*32
      INTEGER CSLTGP
      COMMON /CSDEBUG/ ICSDEBUG
*
*-- match only file names (remove path)
*
      NAME = NAME1
      LN = LENOCC(NAME)
      if (ICSDEBUG.gt.0) print *,'==> in CSRMSL1 (',NAME1(:LN),') <=='
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
*     if (ICSDEBUG.gt.0) print *,'==> in CSRMSL2 (',NAME(1:LN,') <=='
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

#endif
