*
* $Id: csfilx.F,v 1.2 1998/04/16 17:02:18 fisyak Exp $
*
* $Log: csfilx.F,v $
* Revision 1.2  1998/04/16 17:02:18  fisyak
* 2nd pass with gstar
*
*
#include "comis/pilot.h"
*CMZ :          02/12/97  12.36.02  by  Pavel Nevski
*CMZ :  1.18/00 25/01/94  18.26.31  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSFILX(NAME)
***---------------------------------
#include "comis/cslun.inc"
#if defined(CERNLIB_IBM)
         CHARACTER *80 VMIBM
#endif
      CHARACTER *(*) NAME
      IF(ISTFIL.NE.0)THEN
        CLOSE(LUNFIL)
        ISTFIL=0
      ENDIF
#if defined(CERNLIB_VAX)
      OPEN( LUNFIL,FILE=NAME,SHARED,READONLY,STATUS='OLD' ,ERR=1)
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      OPEN( LUNFIL,FILE=NAME,STATUS='OLD' ,ERR=1)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_PAW))
      VMIBM=NAME
      L=LENOCC(NAME)
      DO 77 I=1,L
         IF(VMIBM(I:).EQ.'.')VMIBM(I:I)=' '
  77  CONTINUE
      IS=INDEX(VMIBM,'/')
      IF(IS.NE.0)VMIBM(IS:IS)=' '
      OPEN( LUNFIL,FILE='/'//VMIBM,STATUS='UNKNOWN' ,ERR=1)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_PAW))
      CALL KUOPEN ( LUNFIL, NAME, 'OLD', ISTAT )
      IF ( ISTAT .NE. 0 )               GO TO 1
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN ( LUNFIL, NAME, 'OLD', ISTAT )
      IF ( ISTAT .NE. 0 )               GO TO 1
#endif
      ISTFIL=1
  1   RETURN
*      CALL CSSOUT('FILE WAS NOT OPEN')
*      CALL CSSOUT(NAME)
      END
