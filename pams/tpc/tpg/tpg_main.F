      integer function tpg_main(pad_plane_h,pad_plane,
     +     tpg_detector_h, tpg_detector,
     +     tpg_pad_h, tpg_pad)
      implicit none

#include "tpg_main.inc"

C     Description: Initialisation of the geometry package.
C     Description: In the debug mode it verifies the transformations.
C     Input arguments:
C     pad_plane_h - header of the pad plane table
C     pad_plane  - pad plane table 
c
c added by malisa 05feb97:
c     tpg_detector_h
c     tpg_detector - contains drift velocity and bucket length
c     tpg_pad_h
c     tpg_pad - contains t0_offset (and other info) for each pad
c
c 
C  210.0 replaced by drift_length initialised from the tpg_detector table.
C  I. Sakrejda 19/12/97


      real    dummy1, dummy2, dummy3, dummy4, dummy5
      integer tgc_pad_to_x, tgc_row_to_y, tgc_x_to_pad
      integer tgc_y_to_row
      integer tgc_time_to_z,tgc_z_to_time ! added 05feb97 - mal
      integer tgc_drift_volume_length
      integer tgc_drift_velocity, tgc_clock_frequency
      integer tgc_z_inner_offset
      integer iret
      integer init_flag, idebug

CCCCCCCCCCCC
      integer i,j,tgc_local_to_global,tgc_global_to_local
      integer tgc_row_par
      real xlocal(3), xglobal(3), xback(3), pad
      integer idummy
CCCCCCCCCCCC

      save

      data init_flag/0/, idebug/0/
*
      if(init_flag.eq.0) then
         iret=tgc_drift_volume_length(dummy5,tpg_detector_h,tpg_detector)
         iret=tgc_z_inner_offset(dummy5,tpg_detector_h,tpg_detector)
         iret=tgc_drift_velocity(dummy1,tpg_detector_h,tpg_detector)
         iret=tgc_clock_frequency(dummy1, tpg_detector_h,tpg_detector)
         iret=tgc_pad_to_x(1.0,1.0,dummy3,pad_plane_h,pad_plane)
         iret=tgc_row_to_y(1.0,dummy2,pad_plane_h,pad_plane)
         iret=tgc_x_to_pad(1.0,1.0,dummy3,pad_plane_h,pad_plane)
         iret=tgc_y_to_row(60.0,dummy2,pad_plane_h,pad_plane)
         iret=tgc_row_par(1,1.0,dummy3,dummy4,pad_plane_h,pad_plane)
         init_flag=1
      endif
CCCCC

      if(idebug.eq.1) then
      xlocal(1)=0.0
      xlocal(2)=80.0
      xlocal(3)=0.0
      do i=1,24
        iret=tgc_local_to_global(i,xlocal,xglobal)
        write(44,*)i,xlocal,xglobal
        iret=tgc_global_to_local(j,xglobal,xback)
        write(55,*)j,xglobal,xback
      end do
      do i=1,45
        iret=tgc_pad_to_x(0.5*real(pad_plane(1).npads(i))+0.5,real(i),
     >                   xlocal(1),dummy1,dummy2)
        iret=tgc_x_to_pad(real(i),xlocal(1),pad,dummy1,dummy2)
        iret=tgc_row_to_y(real(i),xlocal(2),dummy1,dummy2)
        write(33,*)i,0.5*real(pad_plane(1).npads(i))+0.5,
     >             pad,xlocal(1),xlocal(2)
        end do
*
      do i=1,24
         do j=1,45
         iret=tgc_row_par(i,real(j),xlocal(1),xlocal(2),dummy1,dummy2)
         write(66,*)i,j,xlocal(1),xlocal(2)
         end do
      enddo
      iret=tgc_drift_velocity(dummy1,tpg_detector_h,tpg_detector)
      write(6,*) 'drift velocity',dummy1
      iret=tgc_clock_frequency(dummy1, tpg_detector_h,tpg_detector)
      write(6,*) 'clock frequency', dummy1
      iret= tgc_z_to_time(210.0,1,1,1,idummy)
      write(6,*) 'time bucket for 210cm ',idummy
      iret= tgc_time_to_z(400,1,1,1,dummy1)
      write(6,*) 'z for time bucket 400 ', dummy1

      endif

CCCCCC
      tpg_main=STAFCV_OK
      end


      integer function tgc_local_to_global(isect,xlocal,xglobal)
      implicit none
#include "PAM.inc"

      integer isect
      real xlocal(3), xglobal(3)

C     Description: Conversion from local to global coordinate system
C     Input arguments:
C     isect  - sector number
C     xlocal - coordinates of a point in the local coordinate system
C     Returns:
C     xglobal- coordinates of a point in the global coordinate system
C
      integer iret, tgc_drift_volume_length
      real b_rot, pi_six, drift_vol_length, dummy1
*
      pi_six=2.0*atan(1.0)/3.0
      iret=tgc_drift_volume_length(drift_vol_length, dummy1, dummy1)
      if(isect.le.12) then    
        b_rot=real(isect)*pi_six
        xglobal(3)=drift_vol_length-xlocal(3)
      else
        b_rot=real(24-isect)*pi_six
        xglobal(3)=-drift_vol_length+xlocal(3)
      endif
      xglobal(1)=xlocal(1)*cos(b_rot)+xlocal(2)*sin(b_rot)
      xglobal(2)=xlocal(2)*cos(b_rot)-xlocal(1)*sin(b_rot)
      tgc_local_to_global=STAFCV_OK
      end

      integer function tgc_global_to_local(isect,xglobal,xlocal)
      implicit none
#include "PAM.inc"
      integer isect
      real xlocal(3), xglobal(3)
C
C     Description: Conversion from global to local coordinate system
C     Input arguments:
C     xglobal - coordinates of a point in the global coordinate system
C     returns:
C     isect - sector number
C     xlocal - coordinates of a point in a local coordinate system
C
      integer iret, tgc_drift_volume_length
      integer first
      real b_rot, angle, dummy1
      real twopi, pi_twelve, pi_six, drift_vol_length
      save

      data first/1/
C
      if(first.eq.1) then
       twopi=8.0*atan(1.0)
       pi_twelve=atan(1.0)/3.0
       pi_six=pi_twelve*2.0
       first=0
       iret=tgc_drift_volume_length(drift_vol_length,dummy1, dummy1)
      endif
C
      angle = atan2(xglobal(2),xglobal(1))
      if(angle.lt.0)angle=angle+twopi
      isect=int((angle+pi_twelve)/pi_six)
      if(isect.eq.12)isect=0
      if(xglobal(3).gt.0) then
         xlocal(3)=drift_vol_length-xglobal(3)
         isect=15-isect
         if(isect.gt.12) isect=isect-12
         b_rot=real(isect)*pi_six
      else
         isect=isect+9
         if(isect.le.12) isect=isect+12
         xlocal(3)=xglobal(3)+drift_vol_length
         b_rot=real(24-isect)*pi_six
      endif
      xlocal(1)=xglobal(1)*cos(b_rot)-xglobal(2)*sin(b_rot)
      xlocal(2)=xglobal(1)*sin(b_rot)+xglobal(2)*cos(b_rot)
      tgc_global_to_local=STAFCV_OK
      end

      integer function tgc_sector_row_pad_time_to_local(isector,
     >pad,row,time,xlocal)
      implicit none
#include "PAM.inc"

      integer isector
      real    pad, row, time, xlocal(3)
C
C     Description: Conversion between the sector, row, pad, time to the local
C     coordinate system
C     Input arguments:
C     isector - sector number
C     pad     - pad number
C     row     - row number
C     time    - time bucket
C     Returns:
C     xlocal  - position of a pixel in local coordinate system
C 
      integer iret
      real    dummy1,dummy2
      integer tgc_pad_to_x
      integer tgc_row_to_y
      integer tgc_time_to_z
      iret=tgc_pad_to_x(pad,row,xlocal(1),dummy1,dummy2)
      iret=tgc_row_to_y(row,xlocal(2),dummy1,dummy2)
      iret=tgc_time_to_z(time,xlocal(3))
      tgc_sector_row_pad_time_to_local=STAFCV_OK
      end

      integer function tgc_pad_to_x(pad,row,x,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*) 

      real pad, row, x

C     Description: Converts pad and row number to the x
C     coordinate in the local coordinate system
C     Returns:
C     x - x coordinate in the local coordinate system

      integer flag,i
      real pitch_in,pitch_out,pitch
      real pad_num(45)
      real pad_int, pad_half, pad_dif
      save

      data flag /1/
      if(flag.eq.1) then
         pitch_in=pad_plane(1).pad_sep_in
         pitch_out=pad_plane(1).pad_sep_out
         do i=1,45
           pad_num(i)=pad_plane(1).npads(i)
         enddo
         flag=0
      endif
      if(row.lt.13.5) then
          pitch=pitch_in
      else
          pitch=pitch_out
      endif
      pad_int=int(pad+0.5)
      pad_half=pad_num(int(row+0.5))/2.0
      pad_dif=pad-pad_int
      x=(pad_int-pad_half-0.5+pad_dif)*pitch
      tgc_pad_to_x=STAFCV_OK
      end
      integer function tgc_row_to_y(row,y,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)


      real row,y
C     Description: Converts row number to the y coordinate in the
C     local coordinate system
C     Input arguments:
C     row - row number
C     pad_plane - table with the pad plane parameters
C                 needed for initialisation
C     Returns:
C     y - y coordinate in the local coordinate system

      real row_cent(45)
      real row_wid_in, row_wid_out,wid,int_row,dif_row
      integer flag, i
      save
      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane(1).rad(i)
        enddo
        row_wid_in=pad_plane(1).pad_len_in
        row_wid_out=pad_plane(1).pad_len_out
        flag=0
      endif
      int_row=int(row+0.5)
      wid=row_wid_out
      if(int_row.ge.14) wid=row_wid_in
      dif_row=row-real(int_row)
      y=row_cent(int_row)+dif_row*wid
      tgc_row_to_y=STAFCV_OK
      end

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
      integer iret, pad, padrow, sector, time

      real dummy1, dvel, clock, z
      real dvel_local, clock_local, t0_offset
      real z_inner_offset, z_offset, zoff
      save

      if (flag.eq.1) then
         flag=0
         iret = tgc_drift_velocity(dvel, dummy1, dummy1)
         iret = tgc_clock_frequency(clock, dummy1, dummy1)
         iret = tgc_z_inner_offset(z_inner_offset, dummy1, dummy1)
*
         dvel_local   = dvel
         clock_local  = clock
         z_offset     = z_inner_offset
      endif
*     iret = tgc_t0(t0, pad, padrow, sector, dummy1, dummy1)
      t0_offset=0.0

      zoff=0
      if(padrow.lt.14)zoff=z_offset
      z = dvel_local*(time/clock_local - t0_offset)-zoff

      tgc_time_to_z = STAFCV_OK      

      end

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
      integer iret, pad, padrow, sector, time
      real dummy1, dvel, clock, z
      real vdrift, clock_local, t0_offset
      real z_offset, z_inner_offset, zoff
      save

      if (flag.eq.1) then
         flag=0
         iret = tgc_drift_velocity(dvel, dummy1, dummy1)
         iret = tgc_clock_frequency(clock, dummy1, dummy1)
         iret = tgc_z_inner_offset(z_inner_offset, dummy1, dummy1)
         vdrift=dvel
         clock_local=clock
         z_offset=z_inner_offset
      endif
*     iret = tgc_t0(t0, pad, padrow, sector, dummy1, dummy1)
      t0_offset=0.0


      zoff=0
      if(padrow.lt.14)zoff=z_offset
      time=((z+zoff)/vdrift + t0_offset)*clock_local

      tgc_z_to_time = STAFCV_OK      

      end

      integer function tgc_drift_velocity(dvel,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real dvel, dvel_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         dvel_local = tpg_detector(1).vdrift
      endif
ccc
      dvel = dvel_local
      tgc_drift_velocity = STAFCV_OK

      end

      integer function tgc_clock_frequency(clock,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real clock, clock_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         clock_local = tpg_detector(1).clock_frequency
      endif
ccc
      clock=clock_local
      tgc_clock_frequency = STAFCV_OK

      end

      integer function tgc_z_inner_offset(z_offset,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real z_offset, z_offset_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         z_offset_local = tpg_detector(1).z_inner_offset
      endif
ccc
      z_offset=z_offset_local
      tgc_z_inner_offset = STAFCV_OK

      end

      integer function tgc_local_to_global_p(isect,plocal,pglobal)
      implicit none
#include "PAM.inc"
      integer isect
      real plocal(3), pglobal(3)
C     Description: Converts momentum from local to global
C     Input arguments:
C     isect - sector number
C     plocal - momentum in local coordinate system
C     Returns:
C     pglobal - momentum in global coordinate system
C     
      real  b_rot, pi_six
      pi_six = 2.0*atan(1.0)/3.0
      pglobal(3)=-plocal(3)
      if(isect.le.12) then    
        b_rot=real(isect)*pi_six
      else
        b_rot=real(24-isect)*pi_six
      endif
      pglobal(1)=plocal(1)*cos(b_rot)+plocal(2)*sin(b_rot)
      pglobal(2)=plocal(2)*cos(b_rot)-plocal(1)*sin(b_rot)
      tgc_local_to_global_p=STAFCV_OK
      end

      integer function tgc_global_to_local_p(isect,pglobal,plocal)
      implicit none
#include "PAM.inc"

      integer isect
      real plocal(3)
      real pglobal(3)
C     Description: Converts momentum from global to local coordinate
C     system
C     Input arguments:
C     Returns:
C     plocal - momentum in local coordinate system

      real b_rot, pi_six
      plocal(3)=-pglobal(3)
      pi_six = 2.0*atan(1.0)/3.0
      if(isect.le.12) then    
        b_rot=real(isect)*pi_six
      else
        b_rot=real(24-isect)*pi_six
      endif
      plocal(1)=pglobal(1)*cos(b_rot)-pglobal(2)*sin(b_rot)
      plocal(2)=pglobal(1)*sin(b_rot)+pglobal(2)*cos(b_rot)
      tgc_global_to_local_p=STAFCV_OK
      end

      integer function tgc_local_to_row_pad_time(xlocal,
     >pad,row,time)
      implicit none
#include "PAM.inc"

      real xlocal(3)
      real pad, row, time

C     Description: Converts local coordinates to pad number
C     row number and time bucket
C     Input arguments:
C     xlocal - local coordinates
C     Returns:
C     pad - pad number
C     row - row number
C     time - time bucket number
C
      real dummy1, dummy2
      integer iret
      integer tgc_x_to_pad, tgc_y_to_row 
      
      time=0
      iret=tgc_x_to_pad(row,xlocal(1),pad,dummy1,dummy2)
      iret=tgc_y_to_row(xlocal(2),row,dummy1,dummy2)
      tgc_local_to_row_pad_time=STAFCV_OK
      end
      


      integer function tgc_x_to_pad(row,x,pad,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)

      real row, x, pad
C     Description: Converts x coordinate in the local coordinate system
C     to the pad number.
C     Input arguments:
C     row - row number
C     x - x coordinate in the local coordinate system
C     Returns:
C     pad - pad number 

      integer flag,i
      real pitch_in,pitch_out,pitch
      real pad_num(45)

      save

      data flag /1/
C
      if(flag.eq.1) then
         pitch_in=pad_plane(1).pad_sep_in
         pitch_out=pad_plane(1).pad_sep_out
         do i=1,45
           pad_num(i)=pad_plane(1).npads(i)
         enddo
         flag=0
      endif
      if(row.lt.13.5) then
          pitch=pitch_in
      else
          pitch=pitch_out
      endif
      pad = x/pitch+0.5+0.5*pad_num(int(row+0.5))
      tgc_x_to_pad=STAFCV_OK
      end

      integer function tgc_y_to_row(y,row,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)

      real y, row
C     Description: Converts y to row number
C     Input arguments:
C     y - y in local coordinate system
C     Returns:
C     row - row number

      integer flag, i
      real row_cent(45)
      real row_wid_in, row_wid_out,dif_row
      real row_wid(45)
      save

      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane(1).rad(i)
        enddo
        row_wid_in=pad_plane(1).pad_len_in
        row_wid_out=pad_plane(1).pad_len_out
        do i=1,13
           row_wid(i)=row_wid_in*0.5
        end do
        do i=14,45
           row_wid(i)=row_wid_out*0.5
        end do
        flag=0
      endif
      do i=1,45
         dif_row=y-row_cent(i)
         if(abs(dif_row).le.row_wid(i))then
           row=real(i)+0.5*dif_row/row_wid(i)
           go to 50
         endif
      end do
50    continue
      tgc_y_to_row=STAFCV_OK
      end
      
      integer function tgc_row_par(isect,row,a,b,pad_plane_h,pad_plane)
      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)

      real    a, b, row, det
      integer isect

C     Description: Returns parametrisation(ax+by=1) for a row
C     in the isect sector
C     Input arguments:
C     isect - sector number
C     row   - row number
C     Returns:
C     a,b - row parameterisation

      real    row_cent(45),plocal(3),pglobal(3), xlocal(3), xglobal(3)
      integer flag, iret, tgc_local_to_global_p, tgc_local_to_global
      integer i,irow
      save

      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane(1).rad(i)
        enddo
        plocal(1)=1.0
        plocal(2)=0.0
        plocal(3)=0.0
        flag=0
      endif
      irow=int(row+0.5)
      iret=tgc_local_to_global_p(isect,plocal,pglobal)
      xlocal(2)=row_cent(irow)
      xlocal(1)=0.0
      xlocal(3)=10.0
      iret=tgc_local_to_global(isect,xlocal,xglobal)
      det = pglobal(1)*xglobal(2)-pglobal(2)*xglobal(1) 
      if(abs(det).gt.0.000001) then
          a=-pglobal(2)/det
          b= pglobal(1)/det
          tgc_row_par = STAFCV_OK
      else
          tgc_row_par = STAFCV_BAD
      endif
      end

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
      integer function tgc_drift_volume_length(xlength,
     >                   tpg_detector_h, tpg_detector)
      implicit none
c     tpg_detector_h
c     tpg_detector - contains drift distance length

#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*)

      real    length , xlength
      integer flag

      save

      data flag /1/
C
      if(flag.eq.1) then
          length=tpg_detector(1).drift_length
          flag=0
      endif
      xlength=length
      tgc_drift_volume_length=STAFCV_OK
      end
           

















