	SUBROUTINE TU_I2COPY_off(n,from,start,to)
  
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Copies n I*2 words from(start) to.
C 
C FORMAL PARAMETERS:
C  
C     n = # I*2 to copy
c	from = array to copy from
c	start = starting index in from, i.e., to(1) = from(start)
c	to = array to copy to
C  
C [common blocks]
C [design]
C 
C-
	IMPLICIT NONE
	INTEGER*2 from(*),to(*)
	INTEGER*4 n,start,i
!	[specification_statement]...
  
	DO i = 1, n
	    to(i) = from(i+start-1)
	END DO
	RETURN
	END
