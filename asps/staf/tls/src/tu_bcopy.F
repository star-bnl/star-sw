	SUBROUTINE TU_BCOPY(nbytes,from,to)
  
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Copies nbytes from to.
C 
C FORMAL PARAMETERS:
C  
C     nbytes = # bytes to copy
c	from = array to copy from
c	to = array to copy to
C  
C [common blocks]
C [design]
C 
C-
	IMPLICIT NONE
	LOGICAL*1 from(*),to(*)
	INTEGER*4 nbytes,i
!	[specification_statement]...
  
               
               
  
	DO i = 1, nbytes
	    to(i) = from(i)
	END DO
	RETURN
	END
