  	subroutine assert(l,line,file)
        logical l
	integer line
	character *(*) file
	if (l) return
	write(*,*) 'Assert on line ',line,' of ',file
	call abort()
	end
