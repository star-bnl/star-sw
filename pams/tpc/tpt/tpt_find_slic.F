* $Id: tpt_find_slic.F,v 1.4 1998/03/29 05:03:01 sakrejda Exp $
* $Log: tpt_find_slic.F,v $
* Revision 1.4  1998/03/29 05:03:01  sakrejda
* setup done on the first call
*
* Revision 1.3  1998/02/11 19:32:55  sakrejda
* tpt protected agains overflows, tpt_find_slic.F,tpt_row_sect_pnt.F simplified (and tested)
*
* Revision 1.2  1998/01/27 00:35:45  fisyak
* Split sources
*
      integer function tpt_find_slic(z,nzslic,islic)
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variable.
C
      implicit none
#include "PAM.inc"
#include "tpt_mxdim.inc"

      integer nzslic                   ! number of slices
      integer islic                    ! slice number for z
      integer ntmp,itim,nhalf,iret
      integer ifirst                   ! initialisation flag
      integer tgc_drift_volume_length  ! function returns drift length
      real    z,dzslic,zreal,zint,dz,dvl,dummy
      real    z_local
      data ifirst /0/
      save ifirst, ntmp, nhalf, dzslic

c     calculate all the necessary numbers only the first time around
      if(ifirst.eq.0) then
         ifirst=1
         iret=tgc_drift_volume_length(dvl,dummy,dummy)
         ntmp=nzslic/2
         dzslic=dvl/real(ntmp)
         nhalf=ntmp*2
      endif

      if(abs(z).gt.dvl) then
        z_local=sign(dvl,z)
      else
        z_local=z
      endif

      zreal= (z_local+dvl)/dzslic


      zint=int(zreal)
      dz=zreal-zint
      itim = (zint + 1.0)*2.0
      if(dz.lt.0.25) then
          islic=itim-1
      else if(dz.gt.0.75.and.islic.lt.mxslic) then
          islic=itim+1
      else
          islic=itim
      endif

      tpt_find_slic=STAFCV_OK
      end
