* $Id: gnzdlis.F,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gnzdlis.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.2  2001/03/02 21:03:13  nevski
* agmer splitted, gnz restored
*
* Revision 1.1  2001/02/27 10:14:17  nevski
*  first working release
*
      SUBROUTINE GNZDLIS(IGENP,IMOTH,NDAU,IDAU)
C---------------------------------------------------------------------
C GNZDLIS
C
C Input : IGENP,IMOTH,NDAU
C
C Output: NDAU,IDAU
C
C Original Version.                                 R. DeWolf   04/03/91
C Printing added for truncated list                     "       17/08/92
C Modification: Fortran compliance (PC Compiler)    S.O'Neale 29/09/96
C---------------------------------------------------------------------
#include "commons/typing.inc"
#include "gncpar.inc"
#include "genz/gnccon.inc"
#include "genz/gncevt.inc"
*CDE, GNCHEP.
#include "slac_do.inc"
#include "genp_do.inc"
C
      INTEGER IGENP(*),IMOTH,NDAU,IDAU(*)
      INTEGER I,IOFF,MODUL,NDAUM,JM1,JM2,NPA
      CHARACTER*4 CIDH
C
C Is this the right bank?
C
      i=-4
      CALL UHTOC(IGENP(i),4,CIDH,4)
      IF(CIDH.NE.'GENP' .and. CIDH.NE.'GENT') THEN
         WRITE(LGNPRI,10000)CIDH
10000    FORMAT(' GNZDLIS: Bank passed is not GENP,  IDH = ',A4,'!')
         GOTO 999
      ENDIF
C
C Get modularity. This value is returned.
C
C===>
      IF      (CIDH.EQ.'GENP') then
         MODUL  = MOD(IGENP(JOSYS3),NPAKAC)
         NPA    = MOD(IGENP(JOSYS2),NPAKAC)
         NHDWAC = 3
      else IF (CIDH.EQ.'GENT') then
         i=-1
         MODUL  = 15
         NPA    = IGENP(i)/MODUL
         NHDWAC = 0
      endif
C===>
C
C Check if the modularity is big enough:
C
      IF(MODUL.LT.10)THEN
         WRITE(LGNPRI,10100)MODUL
10100    FORMAT(' GNZDLIS: MODUL =',I4,',  Cannot decode event! ')
         GOTO 999
      ENDIF
C
C Get number of particles:
C
C
C Loop over particles
      NDAUM = NDAU
      NDAU = 0
      DO 10 I=1,NPA
         IOFF = (I-1)* MODUL + NHDWAC
C Store the array contents:
         JM1 = IGENP(IOFF+JOMOT1)
         JM2 = IGENP(IOFF+JOMOT2)
         IF(JM2.GE.0)THEN
            IF(ABS(JM1).EQ.IMOTH.OR.JM2.EQ.IMOTH)THEN
               NDAU = NDAU + 1
               IF(NDAU.GT.NDAUM)THEN
                  WRITE(LGNPRI,10200)NDAUM
10200 FORMAT(' GNZDLIS: Daughter list truncated after',I5,
     + ' particles.')
                  GOTO 20
               ENDIF
               IDAU(NDAU) = I
            ENDIF
         ELSE
            IF(ABS(JM1).LE.IMOTH.AND.IMOTH.LE.ABS(JM2))THEN
               NDAU = NDAU + 1
               IF(NDAU.GT.NDAUM)THEN
                  WRITE(LGNPRI,10200)NDAUM
                  GOTO 20
               ENDIF
               IDAU(NDAU) = I
            ENDIF
         ENDIF
C
   10 CONTINUE
C
   20 CONTINUE
C
  999 END

****************************** dummy *****************************
           subroutine GNZGROT(AROT,GNZMAG,ROTMA)
C Get current rotation...
           end
           subroutine GNZROTA(PIN,ROTMA,PINO)
C Apply current rotation..
           end
           subroutine GNZSTEP(PIN,GNZMAG,DELT,QNEG,AM,POUT,VSTEP,ANGR)
           end

