	SUBROUTINE tu_strim (strout,strin,strlen)
	 
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    copies strin to strout trimming leading and trailing spaces and tabs
C 
C DUMMY ARGUMENTS:
C 
C 
C IMPLICIT INPUTS:
C 
C    strin
C 
C IMPLICIT OUTPUTS:
C 
C    strout
c	strlen = length of strout
C 
C 
C SIDE EFFECTS:
C 
c	replaces str$trim with tu_strim (stock FORTRAN)
C 
C 
	IMPLICIT NONE
	CHARACTER*(*) strin,strout
	INTEGER*4 strlen,len
	INTEGER*4 i,j,n,i1,ilast,lout
	CHARACTER*3 sptab0
	INTEGER*4 space,tab,null
	DATA space/32/
	DATA tab/9/
	DATA null/0/
	LOGICAL*1 first /.true./
!	[specification_statement]...
	 
	IF (first) THEN
	    first = .false.
	    sptab0(1:1) = char( space )
	    sptab0(2:2) = char( tab )
	    sptab0(3:3) = char( null )
!	    [executable_statement]...
	END IF
	n = len( strin )
	lout = len( strout )
	i1 = 0
	ilast = 0
	i = 0
	DO WHILE (i1 .eq. 0 .and. i .lt. n)
	    i = i + 1
	    IF (index(sptab0,strin(i:i)).le. 0) i1 = i
	END DO
	IF (i1 .le. 0) i1 = 1
	i = n
	DO WHILE (ilast .eq. 0 .and. i .gt. 0)
	    IF (index(sptab0,strin(i:i)) .le. 0) ilast = i
	    i = i - 1
	END DO
	IF (i1 .le. ilast) THEN
	    strlen = ilast - i1 + 1
	    IF( strlen .gt. lout ) strlen = lout
	    strout(1:strlen) = strin(i1:i1+strlen-1)
	ELSE
	    strlen = 0
	END IF
	IF (strlen .le. 0) THEN
	    strout = ' '
	ELSE IF (strlen .lt. lout) THEN
	    strout(strlen+1:lout) = ' '
	END IF
!	[executable_statement]...
	RETURN
	END
	 
!	[program_unit]...
