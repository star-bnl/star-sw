*
* $Id: cshlopt.F,v 1.2 1998/04/16 17:02:19 fisyak Exp $
*
* $Log: cshlopt.F,v $
* Revision 1.2  1998/04/16 17:02:19  fisyak
* 2nd pass with gstar
*
*
* ----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          23/05/97  18.43.27  by  Pavel Nevski
*CMZ :  1.18/01 30/03/94  11.58.04  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSHLOPT(TEXT,CHVAR)
      CHARACTER*(*)TEXT,CHVAR
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      COMMON /CSDEBUG/ ICSDEBUG
 
      if (ICSDEBUG.gt.0) print *,' ===> in CSHLOPT <=== '
      CALL CUTOL(CHVAR)
      IF(CHVAR(1:1).EQ.'p')THEN
        CHPATH=TEXT
        IF(CHPATH.EQ.' ')CHPATH='./'
        L=LENOCC(CHPATH)
        IF(CHPATH(L:L).NE.'/')CHPATH=CHPATH(:L)//'/'
      ELSEIF(CHVAR(1:1).EQ.'f')THEN
        CHF77=TEXT
      ELSEIF(CHVAR(1:1).EQ.'c')THEN
        CHCC=TEXT
      ELSEIF(CHVAR(1:1).EQ.' ')THEN
        PRINT *,' PATH=',CHPATH(:LENOCC(CHPATH))
        PRINT *,' FORT=',CHF77(:LENOCC(CHF77))
        PRINT *,'   CC=',CHCC(:LENOCC(CHCC))
      ELSE
        PRINT *,' CS.SET: unknown option:',CHVAR
        PRINT *,' possible options are: path, f77, cc'
      ENDIF
#endif
      END
 
