	INTEGER*4 FUNCTION tu_btime(bytime)
	
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Converts the byte character string bytime (ascii date/time)
c	to an integer time = # seconds since 1/1/70 00:00:00
C 
C DUMMY ARGUMENTS:
C 
C 
C IMPLICIT INPUTS:
C 
C    bytime = byte bytime(24) character string
C 
C IMPLICIT OUTPUTS:
C 
C 
C FUNCTION VALUE:
C 
C    return value = # seconds since 1/1/70 00:00:00
C 
C SIDE EFFECTS:
C 
C    VAX specific code
C 
C 
	IMPLICIT NONE
	INTEGER*4 status
	INTEGER*4 long_time
	INTEGER*4 qtime(2),q0(2),qd(2)
	INCLUDE '_path:$libdtdef.inc'
	INCLUDE '_path:lib$routines.inc'
	CHARACTER*24 atime
	LOGICAL*1 bytime(24)
	INTEGER*4 i
	 
	DO i = 1, 24
	    if( bytime(i) .gt. 0 ) then
		atime(i:i) = char( bytime(i) )
	    else
		atime(i:i) = ' '
	    end if
	END DO
	status = LIB$CONVERT_DATE_STRING(atime,qtime)
	IF (.not.status) THEN
	    tu_btime = 0
	    return
	END IF
	status = LIB$CONVERT_DATE_STRING('1-JAN-1970 00:00:00.00',q0)
	IF (.not.status) THEN
	    tu_btime = 0
	    return
	END IF
	status = LIB$SUB_TIMES( qtime, q0, qd )
	status = LIB$CVT_FROM_INTERNAL_TIME( LIB$K_DELTA_SECONDS,
	1	long_time, qd )
	tu_btime = long_time
	return
	END
