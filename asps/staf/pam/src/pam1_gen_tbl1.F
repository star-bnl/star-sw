c:Copyright 1996, Lawrence Berkeley National Laboratory
c:>---------------------------------------------------------------------
C:FILE:		pam1_gen_tbl1.F
C:DESCRIPTION:	Physics Analysis Module template.
c:AUTHOR:      	cet - Craig E. Tull, cetull@lbl.gov
C:BUGS:		-- STILL IN DEVELOPMENT --
C:HISTORY:	05feb96-v001a-cet- Split into three functions
C:HISTORY:	16dec95-v000a-cet- creation
c:>---------------------------------------------------------------------

c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 PAM1_GEN_TBL1
C:DESCRIPTION:	Sample Physics Analysis Module to generate table 1 from
C:		scratch.
C:ARGUMENTS:	table structures
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------

      INTEGER*4 FUNCTION PAM1_GEN_TBL1(TB1_H,TB1)
      IMPLICIT NONE

#include "pam1_gen_tbl1.inc"

      INTEGER*4 I,NOK

      WRITE(*,*)'PAM1_GEN_TBL1: Starting ##############################'

      WRITE(*,*) ' name = ',TB1_H.NAME
     +		,' nok = ',TB1_H.NOK
     +		,' maxlen = ',TB1_H.MAXLEN

      NOK = TB1_H.NOK
c     DO I = 1,NOK
c     WRITE(*,*)I,TB1(I).ID,TB1(I).X,TB1(I).Y,TB1(I).Z
c     ENDDO
      DO I = 1,8
	 NOK = NOK + 1
	 IF( NOK .GT. TB1_H.MAXLEN )THEN
	    PAM1_GEN_TBL1 = STAFCV_BAD
	    RETURN
	 ENDIF
	 TB1(NOK).ID = 100000 + NOK
	 TB1(NOK).X = I**2.
	 TB1(NOK).Y = 2*I+1
	 TB1(NOK).Z = I
	 TB1_H.NOK = NOK
      ENDDO

      WRITE(*,*)'PAM1_GEN_TBL1: Normal successful completion ##########'

      PAM1_GEN_TBL1 = STAFCV_OK

      RETURN
      END
