      integer function tpt_find_slic(z,nzslic,islic)
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variable.
C
      implicit none
#include "PAM.inc"
      integer nzslic,islic,ntmp,itim,nhalf
      real z,dzslic,zreal,zint,dz

      ntmp=nzslic/2
      dzslic=225.0/real(ntmp)
      nhalf=ntmp*2
      zreal= abs(z)/dzslic
      zint=int(zreal)
      dz=zreal-zint
      itim = (zint + 1.0)*2.0
      if(dz.lt.0.25) then
          islic=itim-1
      else if(dz.gt.0.75) then
          islic=itim+1
      else
          islic=itim
      endif
      if( z .lt. 0.0.and.islic.gt.1 ) islic=islic+nhalf
      tpt_find_slic=STAFCV_OK
      end
