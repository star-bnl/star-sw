C:---------------------------------------------------------------------
C:- Full PAW Stuff
c     ============================================================
      SUBROUTINE STAF_PAW_INIT()
c     ============================================================
      IMPLICIT NONE
C
      INTEGER NWPAW
      REAL PAWCOM
      PARAMETER (NWPAW=1000000) !DPM: changed from 2e6
      COMMON/PAWC/PAWCOM(NWPAW)
      INTEGER NWORDS
c
c     This is the workstation type returned by the user.
      INTEGER IWTYP
C
      INTEGER*4 PAW_INIT_COUNT

 
c
c     Executable Statements:
c     ======================
      IF(PAW_INIT_COUNT().GT.0) RETURN
C
      CALL PAW(NWPAW,IWTYP)
      WRITE(*,*)'TNT: PAW initialized; IWTYP = ', IWTYP
      CALL KUEXEC('PROMPT ''STAF[]''')
C
      RETURN
      END
