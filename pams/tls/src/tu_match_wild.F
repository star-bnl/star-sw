	INTEGER FUNCTION TU_MATCH_WILD( string, pattern )
c
	IMPLICIT NONE
	CHARACTER*(*) string	! input character string
	CHARACTER*(*) pattern	! input pattern to match
c
c  This routine is similar to the VMS routine STR$MATCH_WILD.
c  The input 'string' is compared against the 'pattern' and 
c  a return value of 1 indicates a match, 0 indicates no match.
c  'pattern' may contain '*' or '%' as wild card characters.
c  '*' matches zero or more characters and '%' matches any single
c  character.
c
	INTEGER cs, cp, nexts, nextwild
	INTEGER ls, lp, nextstar, nextpercent
	LOGICAL matches, ok

	tu_match_wild = 0

	ls = len( string )
	lp = len( pattern )

	matches = .true.
	cs = 1
	cp = 1
c
c make sure 'string' has no wild cards
c
	IF( index(string,'*') .gt. 0) matches = .false.
	IF( index(string, '%') .gt. 0) matches = .false.
	IF (.not.matches) return
c
	CALL TU_NEXT_WILD( pattern(cp:), nextstar, nextpercent, nextwild )
	IF( nextwild .gt. lp) THEN	! no wild cards
	    IF( ls .ne. lp ) return	! can not match
	    tu_match_wild = index( string, pattern )
	    return
	END IF
	ok = .true.
	DO WHILE (ok)
	    CALL TU_NEXT_WILD( pattern(cp:), nextstar, nextpercent,
     +			 nextwild )
	    IF( nextwild .gt. lp-cp+1 ) THEN	! no more wild cards
		IF( lp-cp .ne. ls-cs ) return	! can not match
		tu_match_wild = index( string(cs:), pattern(cp:) )
		return
	    ELSE IF(nextpercent .eq. 1) THEN	! skip next character
		cp = cp + 1
		cs = cs + 1
	    ELSE IF( nextstar .eq. 1) THEN	! find next string index
		cp = cp + 1
		IF(cp .le. lp) THEN
		    CALL TU_NEXT_WILD( pattern(cp:), nextstar,
     +			 	 nextpercent, nextwild )
		    IF(nextwild .gt. 1) THEN
			nexts = index( string(cs:),pattern(cp:cp+nextwild-2))
			IF(nexts .gt. 0) THEN
			    cs = cs + nexts - 1 + nextwild - 1
			    cp = cp + nextwild - 1
			ELSE
			    tu_match_wild = 0	! does not match
			    return
			END IF
		    END IF
		ELSE
		    tu_match_wild = 1		! trailing * matches
		    return
		END IF
	    ELSE		! check next substring
		IF( (ls-(cs-1)) .lt. nextwild ) THEN
		    tu_match_wild = 0
		    return
		ELSE IF( index(string(cs:cs+nextwild-2),
     +				pattern(cp:cp+nextwild-2)) .le. 0) THEN
		    tu_match_wild = 0
		    return
		ELSE
		    cs = cs + nextwild - 1
		    cp = cp + nextwild - 1
		END IF
	    END IF
	    IF(cp .gt. lp ) ok = .false.
	    IF(cs .gt. ls ) ok = .false.
	END DO
	IF(cs .le. ls) THEN
	    tu_match_wild = 0	! trailing part did not match
	ELSE
	    tu_match_wild = 1	! it matched
	END IF
	RETURN
	END
	    
c------------------------------------------------------------------------------

	SUBROUTINE TU_NEXT_WILD( pattern, nextstar, nextpercent, nextwild )
c
	IMPLICIT NONE
	CHARACTER*(*) pattern	! input character string to search for wild cards
	INTEGER nextstar	! output index of next *
	INTEGER nextpercent	! output index of next %
	INTEGER nextwild	! output min( nextstar, nextpercent )
c
	INTEGER lp
c
	lp = len( pattern )
	nextstar = index( pattern, '*' )
	IF( nextstar .le. 0) nextstar = lp+1
	nextpercent = index( pattern, '%' )
	IF( nextpercent .le. 0 ) nextpercent = lp+1
	nextwild = min( nextstar, nextpercent )
	return
	END
