	SUBROUTINE TU_I2COPY(n,from,to)
  
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Copies n I*2 words from to.
C 
C FORMAL PARAMETERS:
C  
C     n = # I*2 to copy
c	from = array to copy from
c	to = array to copy to
C  
C [common blocks]
C [design]
C 
C-
	IMPLICIT NONE
	INTEGER*2 from(*),to(*)
	INTEGER*4 n,i
!	[specification_statement]...
  
	DO i = 1, n
	    to(i) = from(i)
	END DO
	RETURN
	END
