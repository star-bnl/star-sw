* $Id: tgc_cross_and_dip.F,v 1.2 1998/01/27 01:56:38 fisyak Exp $
* $Log: tgc_cross_and_dip.F,v $
* Revision 1.2  1998/01/27 01:56:38  fisyak
* Split sources
*
      integer function tgc_cross_and_dip(sector,pglobal,cross,dip)
      implicit none

#include "PAM.inc"

C     Description: Calculates the dip and the crossing angle 
C     of a track characterized by a vector pglobal that is tangent to it
C     in a sector
C     Input arguments: 
C     sector     - sector number
c     pglobal    - vector tangent to the track
C     Output arguments:
c     cross      - crossing angle
c     dip        - dip angle 

      integer sector, iret
      real    pglobal(3), plocal(3), cross, dip
      integer tgc_global_to_local_p

      iret=tgc_global_to_local_p(sector,pglobal,plocal)
      if(plocal(2)**2/(plocal(1)**2+plocal(2)**2+plocal(3)**2)
     >    .lt.0.00001) then
        tgc_cross_and_dip = STAFCV_BAD
      else
        cross=atan(plocal(1)/plocal(2))
        dip=atan(plocal(3)/plocal(2))
        tgc_cross_and_dip = STAFCV_OK
      endif
      end
