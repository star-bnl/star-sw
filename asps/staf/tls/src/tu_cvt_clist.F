	SUBROUTINE  TU_CVT_CLIST( textin, textout )
c
c	Converts a character variable textin so that a list-directed read
c	will successfully read textout.
c	This is done by adding leading and trailing "'"
c	and converting embedded "'" to double "''".
c
	implicit    none
	character*(*)	textin,textout
	integer*4   li,lo,i,io

	li = len( textin )
	lo = len(textout )

	io = 1
	do i = 1, li
	    if(i.eq. 1) then
		textout(io:io) = ''''
		io = io + 1
	    end if
	    if( textin(i:i) .eq. '''') then
		if( io .lt. lo-1) then
		    textout(io:io+1) = ''''''
		    io = io + 2
		else
		    textout(io:io) = ''''
		    return
		end if
	    else
		if( io .lt. lo ) then
		    textout(io:io) = textin(i:i)
		    io =io + 1
		else
		    textout(io:io) = ''''
		    return
		end if
	    end if
	end do
	textout(io:io) = ''''
	return
	end
