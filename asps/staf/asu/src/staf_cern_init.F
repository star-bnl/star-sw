C:---------------------------------------------------------------------
C:- Min PAW Stuff
      SUBROUTINE STAF_KUIP_INIT()
      IMPLICIT NONE
C
      INTEGER*4 NWPAW
      PARAMETER (NWPAW=500000)
      REAL*4 PAWCOM
      COMMON/PAWC/PAWCOM(NWPAW)
      INTEGER*4 NWORDS
C
      LOGICAL*1 PAW_INITED
      IF(PAW_INITED())RETURN
C
      CALL MZEBRA(-3)
      CALL MZPAW(NWPAW,' ')
C
      NWORDS=20000
      CALL KUINIT(NWORDS)
      CALL VECDEF       ! VECTOR commands
      WRITE(*,*)'KUIP initialized'
      CALL KUEXEC('PROMPT ''staf[]''')
C
      RETURN
      END
C:---------------------------------------------------------------------
      LOGICAL*1 FUNCTION PAW_INITED()
      LOGICAL*1 FIRSTIME/.TRUE./
      PAW_INITED = .NOT.FIRSTIME
      IF(.NOT.FIRSTIME)RETURN
      FIRSTIME = .FALSE.
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
