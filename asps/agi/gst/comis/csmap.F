*
* $Id: csmap.F,v 1.2 1998/04/16 17:02:20 fisyak Exp $
*
* $Log: csmap.F,v $
* Revision 1.2  1998/04/16 17:02:20  fisyak
* 2nd pass with gstar
*
*
* ------------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          31/10/97  13.20.19  by  Pavel Nevski
*CMZ :  1.18/14 19/09/94  10.05.14  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSMAP(NAME)
      CHARACTER*(*) NAME
      COMMON /CSTBCS/ ITBC,ITBS
      CHARACTER *80 LIBNAME,SYMBOL*32
      INTEGER CS_SHL_LOAD,CSLTGP
      COMMON /CSDEBUG/ ICSDEBUG
 
      if (ICSDEBUG.gt.0) print *,' ===> in CSMAP (',name,') <==='
      LIBNAME=NAME
      N=LENOCC(LIBNAME)
      CALL CUTOL(LIBNAME(1:N))
      IF(ITBS.NE.0)THEN
        NS=-1
        CALL CS_SHL_SYMBOLS(LIBNAME(1:N),NS,SYMBOL)
        IF(NS.EQ.-2)GO TO 20
 10     L=LENOCC(SYMBOL)
        IF(SYMBOL(L:L).EQ.'_')L=L-1
        CALL CSCHID(SYMBOL(:L))
        IT=CSLTGP(IPVS)
        IF(IT.GT.0)CALL CSDPRO(IT)
        CALL CS_SHL_SYMBOLS(LIBNAME(1:N),NS,SYMBOL)
        IF(NS.NE.-2)GO TO 10
        CALL CS_SHL_UNLOAD(LIBNAME(1:N))
        ITBS=ITBS-1
 20     CONTINUE
      ENDIF
      IERR=CS_SHL_LOAD(LIBNAME(1:N))
      IF(IERR.EQ.0)THEN
        ITBS=ITBS+1
      ELSE
        CALL CS_SHL_UNLOAD(LIBNAME(1:N))
      ENDIF
      END
 
