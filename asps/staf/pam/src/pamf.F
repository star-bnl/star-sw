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

      INTEGER*4 I,J

      WRITE(*,*) '########################################'
      WRITE(*,*) '###  PAMF-check tables'
      WRITE(*,*) '########################################'

      WRITE(*,*) ' name = ',T1_H.NAME
     +          ,' nok = ',T1_H.NOK
     +          ,' maxlen = ',T1_H.MAXLEN
c     WRITE(*,*) SCALARS_SPEC
      WRITE(*,*) ' name = ',T2_H.NAME
     +          ,' nok = ',T2_H.NOK
     +          ,' maxlen = ',T2_H.MAXLEN
c     WRITE(*,*) VECTORS_SPEC

      DO I=1,T1_H.NOK
	  IF( (T1(I).ASHORT .NE. I-1)
     +    .OR.(T1(I).AUSHORT .NE. I-1)
     +    .OR.(T1(I).ALONG .NE. I-1)
     +    .OR.(T1(I).AULONG .NE. I-1)
     +    .OR.(T1(I).AOCTET .NE. I-1)
     +    .OR.(T1(I).AFLOAT .NE. I-1)
     +    .OR.(T1(I).ADOUBLE .NE. I-1) )THEN
	     WRITE(*,*) 'ERROR IN T1 (',I,'):'
     +       ,T1(I).ASHORT	,T1(I).AUSHORT
     +       ,T1(I).ALONG	,T1(I).AULONG
     +       ,T1(I).ACHAR	,T1(I).AOCTET
     +       ,T1(I).AFLOAT	,T1(I).ADOUBLE
	  ENDIF
      ENDDO
      WRITE(*,*) '###  checked ',T1_H.NOK,' rows'
      DO I=1,T2_H.NOK
      DO J=1,3
	  IF( (T2(I).BSHORTS(J) .NE. I+J-2)
     +    .OR.(T2(I).BUSHORTS(J) .NE. I+J-2)
     +    .OR.(T2(I).BLONGS(J) .NE. I+J-2)
     +    .OR.(T2(I).BULONGS(J) .NE. I+J-2)
     +    .OR.(T2(I).BOCTETS(J) .NE. I+J-2)
     +    .OR.(T2(I).BFLOATS(J) .NE. I+J-2)
     +    .OR.(T2(I).BDOUBLES(J) .NE. I+J-2) )THEN
	     WRITE(*,*) 'ERROR IN T2 (',I,',',J,'):'
     +       ,T2(I).BSHORTS(J)	,T2(I).BUSHORTS(J)
     +       ,T2(I).BLONGS(J)	,T2(I).BULONGS(J)
     +       ,T2(I).BCHARS(J:J)	,T2(I).BOCTETS(J)
     +       ,T2(I).BFLOATS(J)	,T2(I).BDOUBLES(J)
	  ENDIF
      ENDDO
      ENDDO
      WRITE(*,*) '###  checked ',T2_H.NOK,' rows'

!BUG  call ds2realloctable(t2_h,t2_p,222)

      PAMF=STAFCV_OK
      RETURN
      END
