C:---------------------------------------------------------------------
C:- Full PAW Stuff
      SUBROUTINE STAF_PAW_INIT()
      IMPLICIT NONE
C
      INTEGER*4 NWPAW
      REAL*4 PAWCOM
      PARAMETER (NWPAW=2000000)
      COMMON/PAWC/PAWCOM(NWPAW)
      INTEGER*4 NWORDS
C
      INTEGER*4 IWTYP/1/
C
      LOGICAL*1 PAW_INITED
      IF(PAW_INITED())RETURN
C
      CALL PAW(NWPAW,IWTYP)
      WRITE(*,*)'PAW initialized'
      CALL KUEXEC('PROMPT ''STAF[]''')
C
      RETURN
      END
