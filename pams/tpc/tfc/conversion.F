* $Id: conversion.F,v 1.2 1998/01/27 01:14:50 fisyak Exp $
* $Log: conversion.F,v $
* Revision 1.2  1998/01/27 01:14:50  fisyak
* Split sources
*
	Integer function pad_id(in,mode)
c
c A conversion function for all our wonderful pad numbering conventions...
c  This is for the outer sector, which we will use in the summer test.....
c	
c mode :	IN= : 		Function Returns:
c ----		---		----------------
c  1		secpad		row#
c  2		secpad		pad# in the row
c  3		secpad		"physical pad#" of Jim Hunter
c
c  4		physical pad#	secpad
c  5		physical pad#	row#
c  6		physical pad#	pad# in the row
c
c  7		1000*row+
c		pad# in row	secpad
c  8		1000*row+
c		pad# in row	physical pad#


	implicit none
	integer in,mode
	integer secpad_offset(1:45)	! offset of secpad for a given row
	logical first/.true./

	integer npads_row(1:45)		! number of pads in a row
	integer row,ipad
	data npads_row/ 88, 96,104,112,118,
     1			126,134,142,150,156,
     2			166,174,184, 98,100,
     3			102,104,106,106,108,
     4			110,112,112,114,116,
     5			118,120,122,122,124,
     6			126,128,128,130,132,
     7			134,136,136,138,140,
     8			142,144,144,144,144/

	integer physical_pad_offset(1:45) ! offset into Jim Hunter's physical
					  ! pad convention for a given row

	if (first) then
	  first=.false.
	  secpad_offset(1) = 0
	  do row=2,45
	    secpad_offset(row) = secpad_offset(row-1)+npads_row(row-1)
	  enddo
	  physical_pad_offset(45) = -1		! note Jim starts at physpad=0
	  do row=44,1,-1
	    physical_pad_offset(row) = physical_pad_offset(row+1)+
     +		npads_row(row+1)
	  enddo
	endif

	if (mode.eq.1) then
	  do row=45,1,-1
	    if (in-secpad_offset(row).gt.0) goto 1
	  enddo
 1	  pad_id = row
	elseif (mode.eq.2) then
	  do row=45,1,-1
	    if (in-secpad_offset(row).gt.0) goto 2
	  enddo
 2	  pad_id = in - secpad_offset(row)
	elseif (mode.eq.3) then
	  do row=45,1,-1
	    if (in-secpad_offset(row).gt.0) goto 3
	  enddo
 3	  pad_id = in - secpad_offset(row) + physical_pad_offset(row)
	elseif (mode.eq.4) then
	  do row=1,45
	    if (in-physical_pad_offset(row).gt.0) goto 4
	  enddo
 4	  pad_id = in - physical_pad_offset(row) + secpad_offset(row)
	elseif (mode.eq.5) then
	  do row=1,45
	    if (in-physical_pad_offset(row).gt.0) goto 5
	  enddo
 5	  pad_id = row
	elseif (mode.eq.6) then
	  do row=1,45
	    if (in-physical_pad_offset(row).gt.0) goto 6
	  enddo
 6	  pad_id = in - physical_pad_offset(row)
	elseif (mode.eq.7) then
	  row = in/1000
	  ipad = mod(in,1000)
	  pad_id = secpad_offset(row) + ipad
	elseif (mode.eq.8) then
	  row = in/1000
	  ipad = mod(in,1000)
	  pad_id = physical_pad_offset(row) + ipad
	endif
	end


