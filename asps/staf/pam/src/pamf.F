c:>------------------------------------------------------------------
C:FILE:         pamf.F
C:DESCRIPTION:  Physics Analysis Module template.
c:AUTHOR:       hpl - H.P. Lovecraft, hplovecraft@cthulhu.void
C:BUGS:         -- STILL IN DEVELOPMENT --
C:HISTORY:      00jan96-v000a-hpl- Creation.
c:>------------------------------------------------------------------

c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 PAM
C:DESCRIPTION:  Physics Analysis Module template.
C:ARGUMENTS:    TB1     - table 1
C:ARGUMENTS:    TB2     - table 2
C:RETURN VALUE: STAF Condition Value
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION PAMF(
     1                t1_h,                t1 ,
     2                t2_h,                t2 ) 
      IMPLICIT NONE
#include "pamf.inc"
      WRITE(*,*) 'PAMF: Starting ######################################'
                                                        !IGNORE!
      WRITE(*,*) ' name = ',T1_H.NAME                   !IGNORE!
     +          ,' nok = ',T1_H.NOK                     !IGNORE!
     +          ,' maxlen = ',T1_H.MAXLEN               !IGNORE!
c     WRITE(*,*) SCALARS_SPEC
      WRITE(*,*) ' name = ',T2_H.NAME                   !IGNORE!
     +          ,' nok = ',T2_H.NOK                     !IGNORE!
     +          ,' maxlen = ',T2_H.MAXLEN               !IGNORE!
c     WRITE(*,*) VECTORS_SPEC
                                                        !IGNORE!
      WRITE(*,*) 'PAMF: Stopping ######################################'
      PAMF=STAFCV_OK
      RETURN
      END
