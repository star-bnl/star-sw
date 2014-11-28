* $Id: begran.F,v 1.2 1998/02/14 23:02:38 fisyak Exp $
* $Log: begran.F,v $
* Revision 1.2  1998/02/14 23:02:38  fisyak
* Split Fortran sources
*
      real function begran(I)
*
* random number between (0,1)
* see also function begrang
* 
* BTW, The GEANT routine GRNDM is a copy of ranecu.
*
* Doug Wright, Feb 1994
*
* The IBM complains if you replace r with begran.
      logical first
      data first /.true./

      if( first ) then
        call ranecq(i1,i2,1,' ')
        first = .false.
      endif

      call ranecu(r,1,1)
      begran = r
      end
