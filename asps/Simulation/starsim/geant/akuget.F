*
* $Id: akuget.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: akuget.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:31  nevski
*  first working release
*
* Revision 1.1  2000/01/07 00:31:55  nevski
* cvs header record corrected
*
*
#include "geant321/pilot.h"
*
      SUBROUTINE AKUGET(NAME)
      CHARACTER*(*) NAME
      CHARACTER*4   cnam
      INTEGER IVOLUM
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
      Cnam=NAME
      IF (JVOLUM.LE.0) RETURN
      Call GLOOK(NAME,IQ(JVOLUM + 1),IQ(JVOLUM - 1),IVOLUM)
      IF (IVOLUM.GT.0) RETURN
      CALL CLTOU(NAME)
      IF (IDEBUG.LE.0) RETURN
      IF (CNAM.NE.' ') WRITE(LOUT,1111) Cnam,NAME
1111  FORMAT (' GXDRAW: ',a,'  not found, converted to ',a)
      END



