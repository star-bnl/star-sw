      integer function tgc_main(pad_planeh,pad_plane)
      implicit none

#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

      RECORD / table_head_st /pad_planeh
      RECORD / pad_plane_row_st /pad_plane 

C     Description: Initialisation of the geometry package.
C     Description: In the debug mode it verifies the transformations.
C     Input arguments:
C     pad_planeh - header of the pad plane table
C     pad_plane  - pad plane table 

      real    dummy1, dummy2, dummy3, dummy4
      integer tgc_pad_to_x, tgc_row_to_y, tgc_x_to_pad
      integer tgc_y_to_row
      integer iret
      integer init_flag, idebug

CCCCCCCCCCCC
      integer i,j,tgc_local_to_global,tgc_global_to_local
      integer tgc_row_par
      real xlocal(3), xglobal(3), xback(3), pad
CCCCCCCCCCCC

      save

      data init_flag/0/, idebug/0/
*
      if(init_flag.eq.0) then
         iret=tgc_pad_to_x(1.0,1.0,dummy3,pad_planeh,pad_plane)
         iret=tgc_row_to_y(1.0,dummy2,pad_planeh,pad_plane)
         iret=tgc_x_to_pad(1.0,1.0,dummy3,pad_planeh,pad_plane)
         iret=tgc_y_to_row(60.0,dummy2,pad_planeh,pad_plane)
         iret=tgc_row_par(1,1.0,dummy3,dummy4,pad_planeh,pad_plane)
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
        iret=tgc_pad_to_x(0.5*real(pad_plane.npads(i))+0.5,real(i),
     >                   xlocal(1),dummy1,dummy2)
        iret=tgc_x_to_pad(real(i),xlocal(1),pad,dummy1,dummy2)
        iret=tgc_row_to_y(real(i),xlocal(2),dummy1,dummy2)
        write(33,*)i,0.5*real(pad_plane.npads(i))+0.5,
     >             pad,xlocal(1),xlocal(2)
        end do
      endif
      do i=1,24
         do j=1,45
         iret=tgc_row_par(i,real(j),xlocal(1),xlocal(2),dummy1,dummy2)
         write(66,*)i,j,xlocal(1),xlocal(2)
         end do
      enddo

CCCCCC
      tgc_main=tucod_ok
      end


      integer function tgc_local_to_global(isect,xlocal,xglobal)
      implicit none

      integer isect
      real xlocal(3), xglobal(3)

C     Description: Conversion from local to global coordinate system
C     Input arguments:
C     isect  - sector number
C     xlocal - coordinates of a point in the local coordinate system
C     Returns:
C     xglobal- coordinates of a point in the global coordinate system
C

      real b_rot, pi_six
*
      pi_six=2.0*atan(1.0)/3.0
      if(isect.le.12) then    
        b_rot=real(isect)*pi_six
        xglobal(3)=210.0-xlocal(3)
      else
        b_rot=real(24-isect)*pi_six
        xglobal(3)=-210.0+xlocal(3)
      endif
      xglobal(1)=xlocal(1)*cos(b_rot)+xlocal(2)*sin(b_rot)
      xglobal(2)=xlocal(2)*cos(b_rot)-xlocal(1)*sin(b_rot)
      end

      integer function tgc_global_to_local(isect,xglobal,xlocal)
      implicit none
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
      integer first
      real b_rot, angle
      real twopi, pi_twelve, pi_six
      save

      data first/1/
C
      if(first.eq.1) then
       twopi=8.0*atan(1.0)
       pi_twelve=atan(1.0)/3.0
       pi_six=pi_twelve*2.0
       first=0
      endif
C
      angle = atan2(xglobal(2),xglobal(1))
      if(angle.lt.0)angle=angle+twopi
      isect=int((angle+pi_twelve)/pi_six)
      if(isect.eq.12)isect=0
      if(xglobal(3).gt.0) then
         xlocal(3)=210.0-xglobal(3)
         isect=15-isect
         if(isect.gt.12) isect=isect-12
         b_rot=real(isect)*pi_six
      else
         isect=isect+9
         if(isect.le.12) isect=isect+12
         xlocal(3)=xglobal(3)+210.0
         b_rot=real(24-isect)*pi_six
      endif
      xlocal(1)=xglobal(1)*cos(b_rot)-xglobal(2)*sin(b_rot)
      xlocal(2)=xglobal(1)*sin(b_rot)+xglobal(2)*cos(b_rot)
      end

      integer function tgc_sector_row_pad_time_to_local(isector,
     >pad,row,time,xlocal)
      implicit none

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
      end

      integer function tgc_pad_to_x(pad,row,x,pad_planeh,pad_plane)

      implicit none

#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

      RECORD / table_head_st /pad_planeh
      RECORD / pad_plane_row_st /pad_plane 

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
         pitch_in=pad_plane.pad_sep_in
         pitch_out=pad_plane.pad_sep_out
         do i=1,45
           pad_num(i)=pad_plane.npads(i)
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

      end
      integer function tgc_row_to_y(row,y,pad_planeh,pad_plane)

      implicit none

#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

      RECORD / table_head_st /pad_planeh
      RECORD / pad_plane_row_st /pad_plane


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
          row_cent(i)=pad_plane.rad(i)
        enddo
        row_wid_in=pad_plane.pad_len_in
        row_wid_out=pad_plane.pad_len_out
        flag=0
      endif
      int_row=int(row+0.5)
      wid=row_wid_out
      if(int_row.ge.14) wid=row_wid_in
      dif_row=row-real(int_row)
      y=row_cent(int_row)+dif_row*wid
      end

      integer function tgc_time_to_z(time,z)
      implicit none

      real time, z

C     Description: Converts time to z in the local coordinate system
C     Not ready yet.
C     Input arguments:
C     time - time bucket number
C     Returns:
C     z - z coodinate in the local coordinate system

      z=0.0
      end

      integer function tgc_local_to_global_p(isect,plocal,pglobal)
      implicit none
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
      end

      integer function tgc_global_to_local_p(isect,pglobal,plocal)
      implicit none
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

      end

      integer function tgc_local_to_row_pad_time(xlocal,
     >pad,row,time)
      implicit none

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
      end
      


      integer function tgc_x_to_pad(row,x,pad,pad_planeh,pad_plane)

      implicit none

#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

      RECORD / table_head_st /pad_planeh
      RECORD / pad_plane_row_st /pad_plane

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
      real pad_int, pad_half, pad_dif
      save

      data flag /1/
C
      if(flag.eq.1) then
         pitch_in=pad_plane.pad_sep_in
         pitch_out=pad_plane.pad_sep_out
         do i=1,45
           pad_num(i)=pad_plane.npads(i)
         enddo
         flag=0
      endif
      if(row.lt.13.5) then
          pitch=pitch_in
      else
          pitch=pitch_out
      endif
      pad = x/pitch+0.5+0.5*pad_num(int(row+0.5))

      end

      integer function tgc_y_to_row(y,row,pad_planeh,pad_plane)

      implicit none

#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

      RECORD / table_head_st /pad_planeh
      RECORD / pad_plane_row_st /pad_plane

      real y, row
C     Description: Converts y to row number
C     Input arguments:
C     y - y in local coordinate system
C     Returns:
C     row - row number

      integer flag, i
      real row_cent(45)
      real row_wid_in, row_wid_out,wid,int_row,dif_row
      real row_wid(45)
      save

      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane.rad(i)
        enddo
        row_wid_in=pad_plane.pad_len_in
        row_wid_out=pad_plane.pad_len_out
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
      end

      integer function tgc_z_to_time(z,time)
      implicit none
      real z, time
C     Description: Converts z (local coordinates) to time
C     Input arguments:
C     z - z coordinate in local coordinate system
C     Returns:
C     time - time bucket number
C
      time=0
      end
      
      integer function tgc_row_par(isect,row,a,b,pad_planeh,pad_plane)
      implicit none

#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

      RECORD / table_head_st /pad_planeh
      RECORD / pad_plane_row_st /pad_plane

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
          row_cent(i)=pad_plane.rad(i)
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
          tgc_row_par = tucod_ok
      else
          tgc_row_par = tucod_fail
      endif
      end





















