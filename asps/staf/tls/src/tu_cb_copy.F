	SUBROUTINE TU_CB_COPY( n, c, bytes )
c
c  Copies n bytes from c (character) to the array bytes (logical*1).
c
c
c	n = number of bytes to copy			(input)
c	c = character variable				(input)
c	bytes = array of bytes				(output)
c
	IMPLICIT NONE
	INTEGER*4 n
	LOGICAL*1 bytes(n)
	CHARACTER*(*) c
	INTEGER*4 nbytes, i, lenc

	lenc = len( c )
	nbytes = min( n, lenc )
	DO i = 1, nbytes
	    bytes(i) = ichar( c(i:i) )
	END DO
	DO i = nbytes+1, n
	    bytes(i) = ichar( ' ' )
	END DO
	return
	end
