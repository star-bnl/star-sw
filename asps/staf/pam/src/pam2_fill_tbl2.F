c:Copyright 1996, Lawrence Berkeley National Laboratory
c:>---------------------------------------------------------------------
C:FILE:		pam2_fill_tbl2.F
C:DESCRIPTION:	Physics Analysis Module template.
c:AUTHOR:      	cet - Craig E. Tull, cetull@lbl.gov
C:BUGS:		-- STILL IN DEVELOPMENT --
C:HISTORY:	16dec95-v000a-cet- creation
c:>---------------------------------------------------------------------

c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 PAM2_FILL_TBL2
C:DESCRIPTION:	Sample Physics Analysis Module
C:ARGUMENTS:	table structures
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------

      INTEGER*4 FUNCTION PAM2_FILL_TBL2(TB1_H,TB1,TB2_H,TB2)
      IMPLICIT NONE

#include "pam2_fill_tbl2.inc"

      INTEGER*4 I,NOK

      WRITE(*,*)'PAM2_FILL_TBL2: Starting #############################'

      WRITE(*,*)' name = ',TB1_H.NAME
     +		,' nok = ',TB1_H.NOK
     +		,' maxlen = ',TB1_H.MAXLEN
      WRITE(*,*)' name = ',TB2_H.NAME
     +		,' nok = ',TB2_H.NOK
     +		,' maxlen = ',TB2_H.MAXLEN

      TB2_H.NOK = 0
      DO I = 1,TB1_H.NOK
	 IF( I .GT. TB2_H.MAXLEN )THEN
	    PAM2_FILL_TBL2 = STAFCV_BAD
	    RETURN
	 ENDIF
	 TB2(I).ID = 200000 + I
	 TB2(I).RADIUS = 
     +      SQRT(TB1(I).X**2+TB1(I).Y**2+TB1(I).Z**2)
	 TB2(I).PHI = ASIN(TB1(I).X/TB1(I).Y)
	 TB2(I).THETA =
     +      ACOS(TB1(I).Z/SQRT(TB1(I).X**2+TB1(I).Y))
	 TB2_H.NOK = TB2_H.NOK + 1
      ENDDO

      WRITE(*,*)'PAM2_FILL_TBL2: Normal successful completion #########'

      PAM2_FILL_TBL2 = STAFCV_OK

      RETURN
      END
