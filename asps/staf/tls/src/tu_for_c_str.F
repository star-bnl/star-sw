	SUBROUTINE TU_FOR2CSTR(fort,cstr,clen)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Copies a Fortran character variable to a C-style null terminated string.
C 
C FORMAL PARAMETERS:
C  
C     fort = fortran character variable				  (input)
c	cstr = C-style string (an array of logical*1 here)	    (output)
c	clen = number of bytes allocated for cstr		    (input)
C  
C 
C-
	IMPLICIT NONE
	CHARACTER*(*) fort
	CHARACTER*255 buf
	INTEGER*4 clen
	LOGICAL*1 cstr(*)
	INTEGER*4 flen,nlen,i
 
 
	call tu_strim(buf,fort,flen)
	nlen = min( flen, clen-1 )
	DO i = 1, nlen
	    cstr(i) = ichar( buf(i:i) )
	    END DO
	cstr(nlen+1) = 0
	RETURN
	END
 
	SUBROUTINE TU_CSTR2FOR(cstr,fort,flen)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Copies a C-style null terminated string to a Fortran character variable.
c		      ^^^ IMPORTANT ^
C 
C FORMAL PARAMETERS:
C  
C     cstr = C-style string				        (input)
c	fort = Fortran character variable			(output)
c	flen = length of returned FOrtran string		(output)
C  
C 
C-
	IMPLICIT NONE
	LOGICAL*1 cstr(*)
	INTEGER*4 clen
	CHARACTER*(*) fort
	INTEGER*4 flen,nlen,i,ok,idummy
 
 
	i = 0
	flen = len( fort )
	DO WHILE (cstr(i+1).ne.0 .and. i.lt.flen)
	    i = i + 1
c->rm       AIX FORTRAN intrinsic requires promotion of cstr(i)
            idummy = cstr(i)
	    fort(i:i) = char( idummy )
	    END DO
	END
