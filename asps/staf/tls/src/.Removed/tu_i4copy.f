	SUBROUTINE TU_I4COPY(n,from,to)
  
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Copies n I*4 words from to.
C 
C FORMAL PARAMETERS:
C  
C     n = # I*4 to copy
c	from = array to copy from
c	to = array to copy to
C  
C [common blocks]
C [design]
C 
C-
	IMPLICIT NONE
	INTEGER*4 from(*),to(*)
	INTEGER*4 n,i
!	[specification_statement]...
  
               
               
  
	DO i = 1, n
	    to(i) = from(i)
	END DO
	RETURN
	END
