C:---------------------------------------------------------------------
C:- PAW/CERN Stuff

      SUBROUTINE STAF_CERN_INIT()
      IMPLICIT NONE

C
      INTEGER*4 NWPAW
      REAL*4 PAWCOM
      PARAMETER (NWPAW=2000000)
      COMMON/PAWC/PAWCOM(NWPAW)
      INTEGER*4 NWORDS
C
      INTEGER*4 IWTYP/1/
      LOGICAL*1 PAWOPT

      IF(PAWOPT)THEN
         WRITE(*,*)'PAW initialized'
         CALL PAW(NWPAW,IWTYP)
         CALL KUEXEC('PROMPT ''STAF[]''')
      ELSE
         CALL MZEBRA(-3)
         CALL MZPAW(500000,' ')

         NWORDS=20000
         CALL KUINIT(NWORDS)
         CALL VECDEF       ! VECTOR commands
         CALL KUEXEC('PROMPT ''staf[]''')
      ENDIF

      RETURN
      END

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

      CALL PAW(NWPAW,IWTYP)
      WRITE(*,*)'PAW initialized'
      CALL KUEXEC('PROMPT ''STAF[]''')

      RETURN
      END

C:---------------------------------------------------------------------
C:- Min PAW Stuff
      SUBROUTINE STAF_KUIP_INIT()
      IMPLICIT NONE

C
      INTEGER*4 NWPAW
      REAL*4 PAWCOM
      PARAMETER (NWPAW=2000000)
      COMMON/PAWC/PAWCOM(NWPAW)
      INTEGER*4 NWORDS
C
      INTEGER*4 IWTYP/1/
      LOGICAL*1 PAWOPT

      CALL MZEBRA(-3)
      CALL MZPAW(500000,' ')

      NWORDS=20000
      CALL KUINIT(NWORDS)
      CALL VECDEF       ! VECTOR commands
      CALL KUEXEC('PROMPT ''staf[]''')

      RETURN
      END

C:---------------------------------------------------------------------
      SUBROUTINE PAWOPT_ON()
      call PAWOPT_ONOFF(1)
      RETURN
      END
C:---------------------------------------------------------------------
      SUBROUTINE PAWOPT_OFF()
      call PAWOPT_ONOFF(2)
      RETURN
      END
C:---------------------------------------------------------------------
      LOGICAL*1 FUNCTION PAWOPT()
      LOGICAL*1 PAWOPT_ONOFF
      PAWOPT = PAWOPT_ONOFF(0)
      RETURN
      END
C:---------------------------------------------------------------------
      LOGICAL*1 FUNCTION PAWOPT_ONOFF(ACT)
      integer*4 act ! action =0 ask, =1 on, =2 off
      LOGICAL*1 state/.FALSE./
      if(act.EQ.1)then
      	state = .TRUE.
      elseif(act.EQ.2)then
      	state = .FALSE.
      endif
      PAWOPT_ONOFF = state
      RETURN
      END
