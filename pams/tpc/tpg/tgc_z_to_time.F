* $Id: tgc_z_to_time.F,v 1.4 1998/09/01 21:36:36 sakrejda Exp $
* $Log: tgc_z_to_time.F,v $
* Revision 1.4  1998/09/01 21:36:36  sakrejda
* trigger offset added
*
* Revision 1.3  1998/07/08 02:44:01  sakrejda
* truncation problem fixed - time bucket was too small by 1
*
* Revision 1.2  1998/01/27 01:56:45  fisyak
* Split sources
*
      integer function tgc_z_to_time(z,pad,padrow,sector,time)
C     Description: Converts z in the local coordinates to TIME BUCKET NUMBER
c     mal 05feb97
c
C     Input arguments:
C     z - z coodinate in the local coordinate system
C     pad, padrow, sector - identifies electronics channel

C     Returns:
C     time - time bucket number

      implicit none
#include "PAM.inc"

      integer flag/1/
      integer tgc_drift_velocity
      integer tgc_clock_frequency
      integer tgc_z_inner_offset
      integer tgc_trigger_offset
      integer iret, pad, padrow, sector, time
      real dummy1, dvel, clock, z
      real vdrift, clock_local, t0_offset, trigger_offset
      real z_offset, z_inner_offset, zoff
      save

      if (flag.eq.1) then
         flag=0
         iret = tgc_drift_velocity(dvel, dummy1, dummy1)
         iret = tgc_clock_frequency(clock, dummy1, dummy1)
         iret = tgc_z_inner_offset(z_inner_offset, dummy1, dummy1)
         iret = tgc_trigger_offset(trigger_offset,dummy1, dummy1)

         vdrift=dvel
         clock_local=clock
         z_offset=z_inner_offset
         t0_offset=-trigger_offset
      endif
*     iret = tgc_t0(t0, pad, padrow, sector, dummy1, dummy1)



      zoff=0
      if(padrow.lt.14)zoff=z_offset
      time=((z+zoff)/vdrift + t0_offset)*clock_local+1.0

      tgc_z_to_time = STAFCV_OK      

      end
