*
* $Id: getver.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: getver.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:26  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.8  1997/06/20 18:40:43  japost
*   Updating the version number after commiting several changes:
*   to gthion, gflufin, ghetun
*   Remaining problem: gdrawp, ghadr?
*
* Revision 1.7  1997/01/06 17:16:29  cernlib
* Remove ginit.F- and getver.F-
*
* Revision 1.6  1996/11/13 11:19:06  ravndal
* new cradle number /06
*
* Revision 1.5  1996/04/15 14:16:04  ravndal
* Slash instead of underscore
*
* Revision 1.4  1996/03/27 08:29:14  cernlib
* In line 40 the Hollerith had less than thespecified 40 characters.
*
* Time to get rid of these HOLLERITH
*
* Revision 1.3  1996/03/15 14:51:40  ravndal
* 96A version of GEANT
*
* Revision 1.2  1996/03/07 13:18:00  ravndal
* Preparing version number for 96A release
*
* Revision 1.1.1.1  1995/10/24 10:20:12  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 06/12/94  15.11.22  by  S.Ravndal
*-- Author :
      SUBROUTINE GETVER(CHVER,CHDAT,HVERS)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     Routine to get the current version number from a pam       *
C.    *     file. This routine is cracking the title given in          *
C.    *     HOLLERITH format, because CMZ and old versions of PATCHY   *
C.    *     do not support the character title sequence QFTITLCH       *
C.    *                                                                *
C.    *    ==>Called by : GINIT                                        *
C.    *       Author    F.Carminati *********                          *
C.    *                                                                *
C.    ******************************************************************
C.
      CHARACTER*60 CHTIT
      CHARACTER*6 CHFOR
      CHARACTER*8 CHVER,CHDAT
*
      CHVER = ' '
      HVERS = 0.
      CHDAT = ' '
*
      WRITE(CHTIT,'(
     + 40H GEANT   3.21/08  230697                         
     +)')
      DO 10 JSLASH=1,60
         IF(CHTIT(JSLASH:JSLASH).EQ.'/') THEN
            JSL=JSLASH
            GOTO 20
         ENDIF
   10 CONTINUE
      GOTO 999
*
   20 CONTINUE
      DO 30 JBLA=JSL,1,-1
         IF(CHTIT(JBLA-1:JBLA-1).EQ.' ') THEN
            JBEG=JBLA
            GOTO 40
         ENDIF
   30 CONTINUE
      GOTO 999
*
   40 CONTINUE
      DO 50 JBLA=JSL,60
         IF(CHTIT(JBLA+1:JBLA+1).EQ.' ') THEN
            JEND=JBLA
            GOTO 60
         ENDIF
   50 CONTINUE
      GOTO 999
*
   60 WRITE(CHFOR,'(''(F'',I1,''.2)'')') JSL-JBEG
      READ(CHTIT(JBEG:JSL-1),CHFOR) HMAIN
      READ(CHTIT(JSL+1:JEND),'(I2)')   ISUBV
      HVERS=HMAIN+ISUBV*.001
      NCHA=JEND-JBEG+1
      CHVER=' '
      IF(NCHA.EQ.8) THEN
         CHVER=CHTIT(JBEG:JEND)
      ELSE
         CHVER=' '//CHTIT(JBEG:JEND)
      ENDIF
*
      CHDAT=CHTIT(JEND+3:JEND+11)
*
  999 END
