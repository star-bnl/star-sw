*
* $Id: agcheck.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: agcheck.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1  1998/02/10 13:04:42  atgeant3
* *** empty log message ***
*
*
********************************************************************                                                                    3
*
      SUBROUTINE A G C H E C K
*
* description: closing of the geometry sometime is not done properly
*              due to users faults - try to fix it if posssible
*
*******************************************************************
C
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
C
      INTEGER IFLAG/0/,JNAME/0/
      IFLAG=0
C Check JNAME==0
      IF (JNAME.EQ.0) CALL UCTOH('GPAR',JNAME,4,4)
*
C    Check JGPAR<=0 | JGPAR>=NZEBRA
         IF (JGPAR.LE.0 .OR. JGPAR.GE.NZEBRA) THEN
         IFLAG=1
      ELSE IF(IQ(JGPAR-1).LE.0) THEN
         IFLAG=2
      ELSE IF(IQ(JGPAR-3).NE.IQ(JGPAR-1)) THEN
         IFLAG=3
      ELSE IF(IQ(JGPAR-4).NE.JNAME) THEN
         IFLAG=4
      END IF
*
C    Check IFLAG>1
         IF (IFLAG.GT.1) THEN
         WRITE(LOUT,1010)IFLAG,JGPAR
1010     FORMAT(' AGCHECK: GPAR error',I3,I9,', doing GGCLOS')
         CALL GGCLOS
      END IF
1     CONTINUE
      END
