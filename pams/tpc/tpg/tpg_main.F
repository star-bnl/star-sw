* $Id: tpg_main.F,v 1.16 1998/09/01 21:36:36 sakrejda Exp $
* $Log: tpg_main.F,v $
* Revision 1.16  1998/09/01 21:36:36  sakrejda
* trigger offset added
*
* Revision 1.15  1998/07/12 05:29:57  sakrejda
* flag moved to a common, so it is not overwritten, not good, but working...
*
* Revision 1.14  1998/01/27 01:56:45  fisyak
* Split sources
*
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
      integer tgc_trigger_offset
      integer iret
      integer init_flag, idebug

CCCCCCCCCCCC
      integer i,j,tgc_local_to_global,tgc_global_to_local
      integer tgc_row_par
      real xlocal(3), xglobal(3), xback(3), pad
      integer idummy
CCCCCCCCCCCC
      integer flag_length
      common /mydum/flag_length

      save

      data init_flag/0/, idebug/0/
      flag_length=1
*
      if(init_flag.eq.0) then
         iret=tgc_drift_volume_length(dummy5,tpg_detector_h,tpg_detector)
         iret=tgc_z_inner_offset(dummy5,tpg_detector_h,tpg_detector)
         iret=tgc_trigger_offset(dummy5,tpg_detector_h,tpg_detector)
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
