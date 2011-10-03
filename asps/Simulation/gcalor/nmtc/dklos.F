* $Id: dklos.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE DKLOS
C
#include "gcalor/ccomon.inc"
#include "gcalor/cmagnt.inc"
#include "gcalor/ctuctw.inc"
#include "gcalor/celstc.inc"
C
      MT = MAT
      GOTO(10,10,20,30,20,40,40),ITYP
   10 DKWT =1.
C*************ADD MAGNET BODGE TO DELSIG********************
      DELSIG=SIGMX(ITYP,MT)+TOTELS+BODGE
C      DELSIG = SIGMX(ITYP,MT ) + TOTELS
      RETURN
   20 SIGDK= 0.001259/SQRT ((EC(NO)/139.9+1.)**2. -1.)
      GO TO 50
   30 CALL CERROR('DKLOS$')
   40 SIGDK= 1.587E-5/SQRT ((EC(NO)/107.+1.)**2. -1.)
C   50 DELSIG = SIGMX(ITYP,MT ) - SIGDK
C********ADD MAGNET BODGE TO DELSIG AND ALLOW FOR IT IN DKWT*******
   50 DELSIG = SIGMX(ITYP,MT ) - SIGDK+BODGE
C      DKWT = DELSIG/SIGMX(ITYP,MT )
      DKWT = DELSIG/(SIGMX(ITYP,MT )+BODGE)
      RETURN
      END
