C:---------------------------------------------------------------------
C:FILE:         
C:DESCRIPTION:  init PAWC and KUIP, execute $STAF_LOGON
c:AUTHOR:       mc - MCronqvist@lbl.gov
C:HISTORY:      18nov96-v001d-mc - Creation
        subroutine STAF_KUIP_INIT()
        implicit none 
        
        logical   PAW_INITED
        integer   NPAWMEM
        parameter (NPAWMEM = 1000000)
        integer   pawmem(NPAWMEM)
        character ename*10
        character evalue*100
        integer   LENOCC
        common/pawc/pawmem

        integer*4 paw_init_count

	if(paw_init_count().gt.0)then
		write(*,*)'PAW is already INITed'
		return
	endif

        write (6,*) 'Initializing hlimit'
        call hlimit(npawmem)
        
        write (6,*) 'Initializing kuip'
        call kuinit(10000)
        
        write (6,*) 'Initializing vecdef'
        call vecdef()
        
        ename = 'STAF_LOGON'
        call GETENV(ename, evalue)
        if(LENOCC(evalue).gt.0) then
          write(*,'(a)') 'Executing '//evalue(1:LENOCC(evalue))
          call KUEXEL('EXEC $STAF_LOGON')
        endif

        end

C:---------------------------------------------------------------------
      LOGICAL*1 FUNCTION PAW_INITED()
c     LOGICAL*1 FIRSTIME/.TRUE./
      integer*4 paw_init_count
      character*40 foo/'@(#)PAW_INITED 22jul97-1042'/
c     PAW_INITED = .NOT.FIRSTIME
c     IF(.NOT.FIRSTIME)RETURN
	if( paw_init_count() .GT. 0 )then
	    PAW_INITED = .TRUE.
	    FIRSTIME = .FALSE.
	    RETURN
	endif
      PAW_INITED = .FALSE.
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
