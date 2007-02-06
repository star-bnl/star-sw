#include "geant321/pilot.h"

      SUBROUTINE GUSWMP(CHARGE, STEP, MASS, VECT, VOUT, EOUT)
      IMPLICIT NONE
* 
*     ******************************************************************
*     *                                                                *
*     *       User routine to control tracking of one monopole         *
*     *       in a magnetic field                                      *
*     *                                                                *
*     *  Parameters:                                                   *
*     *   input                                                        *
      REAL             CHARGE  ! charge magnetic(!)                    *
      REAL             STEP    ! arc length of the step asked          *
      REAL             MASS    ! monopole mass                         *
      REAL             VECT(*) ! (x,y,z,px/p,py/p,pz/p,p)_initial      *
*     *                                                                *
*     *   output                                                       *
      REAL             VOUT(*) ! (x,y,z,px/p,py/p,pz/p,p)_final        *
      REAL             EOUT    ! final energy                          *
*     *                                                                *
*     *    ==>Called by : <USER>, GTMONP                               *
*     *       Author    M.Mulhearn                                     *
*     *                                                                *
*     ******************************************************************
* 
#include "geant321/gctmed.inc"
*
*    If magnetic field is constant in +z direction, call analytic 
*    parabolic solution, otherwise use R-K numerical integration.
*
      IF (IFIELD.EQ.3) THEN
         CALL GPARMP(CHARGE,STEP,MASS,VECT,VOUT,EOUT)
      ELSE
         CALL GRKTMP(CHARGE,STEP,MASS,VECT,VOUT,EOUT)
      ENDIF
*
*
      END
