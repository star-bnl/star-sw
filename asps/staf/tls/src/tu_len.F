	INTEGER*4 FUNCTION TU_LEN( c )
c
c	This function returns the length of the character variable
c	c not counting trailing blanks.
c
c  Arguments:
c
c	c = character variable					(input)
c
c
	IMPLICIT NONE
	CHARACTER*(*) c
	INTEGER*4 clen,i

	clen = len( c )
	i = clen
	DO WHILE(i .gt. 0 .and. c(i:i) .eq. ' ')
	    i = i - 1
	END DO
	TU_LEN = i
	RETURN
	END
