c:Copyright 1996, Lawrence Berkeley National Laboratory
c:>---------------------------------------------------------------------
C:FILE:		pam0.F
C:DESCRIPTION:	Physics Analysis Module template.
c:AUTHOR:      	cet - Craig E. Tull, cetull@lbl.gov
C:BUGS:		-- STILL IN DEVELOPMENT --
C:HISTORY:	16dec95-v000a-cet- creation
c:>---------------------------------------------------------------------

c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 PAM0
C:DESCRIPTION:	Sample Physics Analysis Module
C:ARGUMENTS:	table structures
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------

      INTEGER*4 FUNCTION PAM0(TB1_H,TB1,TB2_H,TB2)
      IMPLICIT NONE

#include "pam0.inc"


      WRITE(*,*)'PAM0: Starting #######################################'

      WRITE(*,*) ' name = ',TB1_H.NAME
     +          ,' nok = ',TB1_H.NOK
     +          ,' maxlen = ',TB1_H.MAXLEN
      WRITE(*,*) ' name = ',TB2_H.NAME
     +          ,' nok = ',TB2_H.NOK
     +          ,' maxlen = ',TB2_H.MAXLEN

      WRITE(*,*)'PAM0: Normal successful completion ###################'

      PAM0 = STAFCV_OK

      RETURN
      END
