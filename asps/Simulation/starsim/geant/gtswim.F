* $Id: gtswim.F,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: gtswim.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.2  2001/03/02 21:17:22  nevski
* commons changed as in atlas version
*
* Revision 1.1  2001/02/27 10:15:44  nevski
*  first working release
*
      SUBROUTINE GTSWIM
C.
C.    ********************************************************************
C.    *                                                                  *
C.    * swim any particle in magnetic field in case STEP > PREC          *
C.    * New MANY logic and no USRJMP is supposed                         *
C.    *   ==>Called by : GTELEC,GTMUON,GTHADR,GTHION etc                 *
C.    *      Authors  P.Nevski ********                                  *
C.    * 05.10.2000, PN:                                                  *
C.    * default settings for all 0 corresponds to the latest cernlib     *
C.    * version apart from abs(Vout-Vnext) which is believed to be wrong *
C.    *                                                                  *
C.    ********************************************************************
C.
#include "commons/typing.inc"
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "geant321/gckine.inc"
#include "geant321/gctmed.inc"
#include "atlsim/agctrac.inc"
      Logical  First/.true./
      REAL     VNEXT(6),Diff
      Integer  INEAR,ISAME,I,ISW/0/,gtman/0/,gtlog/0/,gtabs/0/
*
      If (first) then
         ISW   = IGTRACK(3)
         gtman = mod(ISW,10)
         gtlog = mod(ISW/10,10)
         gtabs = mod(ISW/100,10)
      endif

         NMEC = NMEC +1
         LMEC(NMEC) = 4
*
*        original 95 logic for many
         if (gtman.gt.0 .and. step.ge.safety) call gtmany
*
   40    CALL GUSWIM (CHARGE, STEP, VECT, VOUT)
*
*  ** When near to boundary, take proper action (cut-step,crossing...)
*
         IF (STEP.GE.SAFETY) THEN
            INEAR = 0
            IF (IGNEXT.NE.0) THEN

*              new 99 logic for crossing here:
               if (gtlog.eq.0) INEAR = 1

               DO I = 1,3
                  VNEXT(I+3) = VECT(I+3)
                  VNEXT(I)   = VECT(I) +SNEXT*VECT(I+3)
               ENDDO

               DO I = 1,3
                  diff = VOUT(I)-VNEXT(I)
                  if (gtabs.eq.0) diff=abs(diff)
                  IF (diff.GT.EPSIL) GO TO 60
               ENDDO

*              new 99 logic for crossing here:
               if (gtlog.eq.0) GO TO 65

               INEAR = 1
   60          CONTINUE
            ENDIF
*
            if (gtman.eq.0) call gtmany
*
            CALL GINVOL (VOUT, ISAME)
            IF (ISAME.EQ.0)THEN
               IF (INEAR.EQ.0 .AND. STEP.GE.EPSIL) THEN
*              Cut step
                  STEP = 0.5*STEP
                  IF (LMEC(NMEC).NE.24) THEN
                     NMEC = NMEC + 1
                     LMEC(NMEC) = 24
                  ENDIF
                  GO TO 40
               ENDIF

   65          INWVOL = 2
               NMEC = NMEC +1
               LMEC(NMEC) = 1

            ENDIF
         ENDIF
*
         CALL UCOPY(VOUT,VECT,6)
*
      END

