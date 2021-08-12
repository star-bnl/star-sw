* $Id: gfxzrm.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
* $Log: gfxzrm.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.2  2002/10/01 23:30:33  nevski
* more pile-up tools
*
* Revision 1.1  2001/02/27 10:15:37  nevski
*  first working release
*
      SUBROUTINE GFXZRM(NLEV_0,X,Y,Z,TET1,PHI1,TET2,PHI2,TET3,PHI3,TYPE)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     For the last Volume in the GVOLUM list returns its         *
C.    *     POSITIONS AND ROTATION MATRICE parameters relative         *
C.    *     to the previous, more generally 'BASE' level               *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcvolu.inc"
#include "geant321/gconst.inc"
C.
      DIMENSION XM(3), XD(3)
      DIMENSION ROWMAT(3)
      LOGICAL ROTATE
C
      NLEV_0 = NLEVEL                  ! daughter level
      NLEV   = NLEVEL-1                ! mother level
      TYPE   = -1
      IF (NLEV .LE. 0)  GO TO 777
C
* translate global position of the daughter volume into mother coordinates
C
      NLEVEL = NLEV
      CALL GMTOD(GTRAN(1,NLEV_0), XD, 1)
      X    = XD(1)
      Y    = XD(2)
      Z    = XD(3)
C
* translate global axis of the daughter volume into the mother coordinates
C
      DO JB= 1, 3
C
         CALL GMTOD(GRMAT(1+3*(JB-1), NLEV_0), ROWMAT, 2)
         CALL GFANG(ROWMAT, COSTH,SINTH, COSPH,SINPH, ROTATE)
         TET = ATAN2(SINTH,COSTH)*RADDEG
         PHI = ATAN2(SINPH,COSPH)*RADDEG
C
         IF    (JB .EQ. 1) THEN
            TET1     = TET
            PHI1     = PHI
         ELSEIF(JB .EQ. 2) THEN
            TET2     = TET
            PHI2     = PHI
         ELSEIF(JB .EQ. 3) THEN
            TET3     = TET
            PHI3     = PHI
         ENDIF
C
      ENDDO
C
      TYPE = GRMAT(10, NLEV)
C
 777  CONTINUE
      NLEVEL = NLEV_0
C      print *,' xyz= ', Nlevel,NUMBER(NLevel),
C    *         (Gtran(i,Nlevel),i=1,3),(Grmat(i,Nlevel),i=1,9)
      RETURN
      END

