	SUBROUTINE TU_PARANGE (cexpr,ilo,ihi,rlo,rhi)
	 
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    This routine parses an expression for a range of numbers
c	aaa:bbb
C 
C DUMMY ARGUMENTS:
C 
C    cexpr = character expression
c	ilo = int left limit
c	ihi = int right limit
c	rlo = real left limit
c	rhi = real right limit
C 
C IMPLICIT INPUTS:
C 
C    cexpr
C 
C IMPLICIT OUTPUTS:
C 
C    ilo, ihi, rlo, rhi
C 
C 
C SIDE EFFECTS:
C 
C    [description_or_none]
C 
C 
	IMPLICIT NONE
	CHARACTER*(*) cexpr
	INTEGER*4 ilo,ihi
	REAL*4 rlo,rhi
	INTEGER*4 lc
	INTEGER*4 lcol
	INTEGER*4 ll,lr
!	[specification_statement]...
	 
	lc = len( cexpr )
	lcol = index( cexpr, ':')
	ll = lcol - 1
	lr = lc - lcol
	IF (lc .le. 0) THEN
	    ilo = 0
	    ihi = 0
	    rlo = 0.
	    rhi = 0.
	ELSE IF (lcol .le. 0) THEN
	    read(cexpr,*,err=10) ilo
	    ihi = ilo
	    goto 11
10	    ilo = 0
	    ihi = 0
11	    read(cexpr,*,err=15) rlo
	    rhi = rlo
	    goto 16
15	    rlo = 0.
	    rhi = 0.
16	    continue
	ELSE
	    IF (ll .gt. 0) THEN
		read(cexpr(1:ll),*,err=20) ilo
		goto 21
20		ilo = 0
21		read(cexpr(1:ll),*,err=25) rlo
		goto 26
25		rlo = 0.
26		continue
	    ELSE
		ilo = 0
		rlo = 0.
	    END IF
	    IF (lr .gt. 0) THEN
		read(cexpr(lcol+1:lc),*,err=30) ihi
		goto 31
30		ihi = 0
31		read(cexpr(lcol+1:lc),*,err=35) rhi
		goto 36
35		rhi = 0
36		continue
	    ELSE
		ihi = 0
		rhi = 0.
	    END IF
	END IF
!	[executable_statement]...
	RETURN
	END
