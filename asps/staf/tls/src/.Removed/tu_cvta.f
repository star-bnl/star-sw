	SUBROUTINE TU_CVTA (strout,strin,lout,newch)
	 
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    converts non alhpanumeric characters to char and trims leading
c	and trailing tabs and spaces
C 
C DUMMY ARGUMENTS:
C 
C 
C IMPLICIT INPUTS:
C 
C    character*(*) strin input
c	character*1 newch
C 
C IMPLICIT OUTPUTS:
C 
c	lout = length of input string
C    strout = output string
C 
C 
C SIDE EFFECTS:
C 
C 
C 
	IMPLICIT NONE
	CHARACTER*(*) strin,strout
	CHARACTER*1 newch
	INTEGER*4 lout
	INTEGER*4 i
	INTEGER*4 index
	LOGICAL*1 first /.true./
	CHARACTER*62 alphanum
	CHARACTER*26 alower,aupper
	CHARACTER*10 digits
	DATA alower /'abcdefghijklmnopqrstuvwxyz'/
	DATA aupper/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
	DATA digits/'0123456789'/
!	[specification_statement]...
	 
	IF (first) THEN
	    first = .false.
	    alphanum = alower//aupper//digits
!	    [executable_statement]...
	END IF
	call tu_strim (strin,strout,lout)
	DO i = 1, lout
	    IF (index(alphanum,strout(i:i)) .le. 0) strout(i:i) = newch
	END DO
!	[executable_statement]...
	RETURN
	END
