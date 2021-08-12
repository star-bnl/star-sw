* $Id: gfpos.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
*
* $Name:  $
*
* $Log: gfpos.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2002/10/01 23:30:33  nevski
* more pile-up tools
*
*
      SUBROUTINE GFPOS(NMOT,NDAU,X,Y,Z,TET1,PHI1,TET2,PHI2,TET3,PHI3,RT)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Given a Mother and a Daughter levels in the current GVOLUM   *
C.    *   returns relative POSITION and the ROTATION MATRICE           *       
C.    *    of the 'Daughter' in the 'Mother'                           *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcvolu.inc"
#include "geant321/gconst.inc"
C.
      DIMENSION XM(3), XD(3)
      DIMENSION ROWMAT(3)
      LOGICAL   RT
C
      NLEV_0 = NLEVEL
      if (1.gt.NMOT .or. NMOT.ge.NDAU .or. NDAU.gt.Nlevel) RETURN
C
* translate global position of the daughter volume into mother coordinates
C
      NLEVEL = NMOT
      CALL GMTOD(GTRAN(1,NDAU), XD, 1)
      X    = XD(1)
      Y    = XD(2)
      Z    = XD(3)
C
* translate global axis of the daughter volume into the mother coordinates
C
      DO JB= 1,7,3
C
         CALL GMTOD(GRMAT(JB,NDAU), ROWMAT, 2)
         CALL GFANG(ROWMAT, COSTH,SINTH, COSPH,SINPH, RT)
         TET = ATAN2(SINTH,COSTH)*RADDEG
         PHI = ATAN2(SINPH,COSPH)*RADDEG
         IF    (JB .EQ. 1) THEN
            TET1 = TET
            PHI1 = PHI
         ELSEIF(JB .EQ. 4) THEN
            TET2 = TET
            PHI2 = PHI
         ELSE
            TET3 = TET
            PHI3 = PHI
         ENDIF
C
      ENDDO
      NLEVEL = NLEV_0
C      print *,' GFPOS xyz= ', NDAU,NUMBER(NDAU),X,Y,Z,
C    *         TET1,PHI1,TET2,PHI2,TET3,PHI3,RT
      RETURN
      END

