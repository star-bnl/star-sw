	INTEGER FUNCTION TU_ELEMENT( element, elnum, delim, source )
c
	IMPLICIT NONE
	CHARACTER*(*) element	! output element substring
	INTEGER elnum		! input element number to be returned (0=first)
	CHARACTER*(1) delim	! input element delimiter character
	CHARACTER*(*) source	! input source string from which elements are
c					extracted
c
c  This routine acts similarly to the VMS STR$ELEMENT function.
c  It returns in 'element' a substring of 'source' delimited by 'delim'.
c  For 'elnum' = 0, the first element is returned.
c  If a zero length element is found (two delim characters in a row)
c  the delimiter character is returned as the element and a success status
c  for the function value.
c  The function value returned is 1 for success and 0
c  if 'elnum' is greater than the number of delimiters found in 'source'.
c-
	integer is, nextd, iel

	is = 1
	nextd = index( source(is:), delim )
	iel = 0
	DO WHILE( iel .lt. elnum .and. nextd .gt. 0)
	    is = is + nextd
	    nextd =index( source(is:), delim )
	    iel = iel + 1
	END DO
	IF( iel .eq. elnum ) THEN
	    IF( nextd .le. 0) nextd = len( source ) - is + 1
	    IF( nextd .gt. 1 ) THEN	! something to return
		element = source(is:is+nextd-1)
		tu_element = 1
	    ELSE
		element = delim
		tu_element = 1
	    END IF
	ELSE
	    tu_element = 0
	END IF
	RETURN
	END
