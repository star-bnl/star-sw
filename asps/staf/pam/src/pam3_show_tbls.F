c:Copyright 1996, Lawrence Berkeley National Laboratory
c:>---------------------------------------------------------------------
C:FILE:		pam3_show_tbls.F
C:DESCRIPTION:	Physics Analysis Module template.
c:AUTHOR:      	cet - Craig E. Tull, cetull@lbl.gov
C:BUGS:		-- STILL IN DEVELOPMENT --
C:HISTORY:	16dec95-v000a-cet- creation
c:>---------------------------------------------------------------------

c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 PAM3_SHOW_TBLS
C:DESCRIPTION:	Sample Physics Analysis Module
C:ARGUMENTS:	table structures
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------

      INTEGER*4 FUNCTION PAM3_SHOW_TBLS(TB1_H,TB1,TB2_H,TB2)
      IMPLICIT NONE

#include "pam3_show_tbls.inc"

      INTEGER*4 I,NOK

      WRITE(*,*)'PAM3_SHOW_TBLS: Starting #############################'

      WRITE(*,*)' name = ',TB1_H.NAME
     +		,' nok = ',TB1_H.NOK
     +		,' maxlen = ',TB1_H.MAXLEN
      WRITE(*,*)' name = ',TB2_H.NAME
     +		,' nok = ',TB2_H.NOK
     +		,' maxlen = ',TB2_H.MAXLEN

      DO I = 1,TB1_H.NOK
	 WRITE(*,*)I,TB1(I).ID,TB1(I).X,TB1(I).Y,TB1(I).Z
	 WRITE(*,*)I,TB2(I).ID,TB2(I).RADIUS,TB2(I).PHI
     +			,TB2(I).THETA
      ENDDO

      WRITE(*,*)'PAM3_SHOW_TBLS: Normal successful completion #########'

      PAM3_SHOW_TBLS = STAFCV_OK

      RETURN
      END
