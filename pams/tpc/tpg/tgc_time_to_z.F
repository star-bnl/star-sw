* $Id: tgc_time_to_z.F,v 1.4 1998/09/01 21:36:34 sakrejda Exp $
* $Log: tgc_time_to_z.F,v $
* Revision 1.4  1998/09/01 21:36:34  sakrejda
* trigger offset added
*
* Revision 1.3  1998/07/08 02:44:44  sakrejda
* z positioned in the middle of a bucket
*
* Revision 1.2  1998/01/27 01:56:43  fisyak
* Split sources
*
      integer function tgc_time_to_z(time,pad,padrow,sector,z)

      implicit none
#include "PAM.inc"

C     Description: Converts TIME BUCKET NUMBER to z in the local coordinates
c     mal 05feb97
c
C     Input arguments:
C     time - time bucket number
C     pad, padrow, sector - identifies electronics channel

C     Returns:
C     z - z coodinate in the local coordinate system


      integer flag/1/
      integer tgc_drift_velocity
      integer tgc_clock_frequency
      integer tgc_z_inner_offset
      integer tgc_trigger_offset
      integer iret, pad, padrow, sector, time

      real dummy1, dvel, clock, z
      real dvel_local, clock_local, t0_offset, trigger_offset
      real z_inner_offset, z_offset, zoff
      save

      if (flag.eq.1) then
         flag=0
         iret = tgc_drift_velocity(dvel, dummy1, dummy1)
         iret = tgc_clock_frequency(clock, dummy1, dummy1)
         iret = tgc_z_inner_offset(z_inner_offset, dummy1, dummy1)
         iret = tgc_trigger_offset(trigger_offset,dummy1, dummy1)
*
         dvel_local   = dvel
         clock_local  = clock
         z_offset     = z_inner_offset
         t0_offset    = -trigger_offset
      endif
*     iret = tgc_t0(t0, pad, padrow, sector, dummy1, dummy1)


      zoff=0
      if(padrow.lt.14)zoff=z_offset
      z = dvel_local*((float(time)-0.5)/clock_local - t0_offset)-zoff

      tgc_time_to_z = STAFCV_OK      

      end
