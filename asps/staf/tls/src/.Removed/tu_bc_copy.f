	SUBROUTINE TU_BC_COPY( n, bytes, c )
c
c  Copies n bytes from bytes to the character variable c.
c
c
c	n = number of bytes to copy			(input)
c	bytes = array of bytes				(input)
c	c = character variable				(output)
c
	IMPLICIT NONE
	INTEGER*4 n
	LOGICAL*1 bytes(n)
	CHARACTER*(*) c
	INTEGER*4 nbytes, i, lenc

	lenc = len( c )
	nbytes = min( n, lenc )
	DO i = 1, nbytes
	    c(i:i) = char( bytes(i) )
	END DO
	IF( nbytes .lt. lenc ) c(nbytes+1:) = ' '
	return
	end
