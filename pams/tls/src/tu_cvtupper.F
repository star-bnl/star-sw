	SUBROUTINE TU_CVTUPPER (strout,strin)
	 
C 
C FUNCTIONAL DESCRIPTION:	
C 
c	converts lower case in strin to upper case in strout
C 
C DUMMY ARGUMENTS:
C 
C 
C IMPLICIT INPUTS:
C 
C    character*(*) strin input
C 
C IMPLICIT OUTPUTS:
C 
C    strout = output string
C 
C 
C SIDE EFFECTS:
C 
C 
C 
	IMPLICIT NONE
	CHARACTER*(*) strin,strout
	INTEGER*4 lout,lin
	INTEGER*4 i,let
	INTEGER*4 len
c	CHARACTER*26 alower,aupper
c	DATA alower /'abcdefghijklmnopqrstuvwxyz'/
c	DATA aupper/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
	integer alower,zlower,uldif
	data alower,zlower/97,122/
	data uldif/-32/
!	[specification_statement]...
	 
	strout = strin
	lout = len( strout )
	lin = len( strin )
	DO i = 1, min(lin,lout)
	   let = ichar(strin(i:i))
	   if(let.ge.alower .and. let .le.zlower) let=let+uldif
	   strout(i:i) = char(let)
	END DO
!	[executable_statement]...
	RETURN
	END
