      integer function svt_sevb(svt_adc,svt_adc_n,
     1                          svt_time,svt_time_n,
     1                          svt_anode,svt_anode_n,
     1                          svt_wafer,svt_wafer_n,
     1                          svt_event,svt_event_n)
      implicit none

c     description: this routine assemble an svt sub event
c     from the different structures

c     author: C. Pruneau
c     created: March 17, 93

c     argument/global declaration
c     ===========================
      include 'svt_inc.f'

      record /svt_adc_st/   svt_adc(svt_adc_nmax)
      record /svt_time_st/  svt_time(svt_time_nmax)
      record /svt_anode_st/ svt_anode(svt_anode_nmax)
      record /svt_wafer_st/ svt_wafer(svt_wafer_nmax)
      record /svt_event_st/ svt_event(svt_event_nmax)

c     local declaration
c     =================
      
      integer i_wafer, i_anode, i_clus, i_sample, i_event

c     executable code
c     ===============

c     check space allocation

      svt_event_n = 3*(svt_wafer_n+svt_anode_n+svt_time_n) + svt_adc + 5

      if (svt_event_n.gt.svt_event_nmax) then

c     status is fatal error - event too big for output buffer

         svt_sevb = -1

      else if (svt_wafer_n.gt.svt_wafer_nmax) then

c     status is fatal error - wafer data too big

         svt_sevb = -3

      else if (svt_anode_n.gt.svt_anode_nmax) then

c     status is fatal error - anode data too big

         svt_sevb = -5

      else if (svt_time_n.gt.svt_time_nmax) then

c     status is fatal error - time data too big

         svt_sevb = -7

      else if (svt_adc_n.gt.svt_adc_nmax) then

c     status is fatal error - adc data too big

         svt_sevb = -9

      else
         svt_sevb = 0
      end if

      if (svt_sevb.ne.0) return

c     begin copy

      i_event = 1

      svt_event(i_event+1).idata = svt_wafer_n
      svt_event(i_event+2).idata = svt_anode_n
      svt_event(i_event+3).idata = svt_time_n
      svt_event(i_event+4).idata = svt_adc_n
      i_event = i_event + 4

c     copy svt_wafer_st

      do i_wafer = 1, svt_wafer_n

         svt_event(i_event+1).idata = svt_wafer(i_wafer).wafer
         svt_event(i_event+2).idata = svt_wafer(i_wafer).nanode
         svt_event(i_event+3).idata = svt_wafer(i_wafer).ptr
         i_event = i_event + 3

      end do

c     copy svt_anode_st

      do i_anode = 1, svt_anode_n

         svt_event(i_event+1).idata = svt_anode(i_anode).anode
         svt_event(i_event+2).idata = svt_anode(i_anode).nclus
         svt_event(i_event+3).idata = svt_anode(i_anode).ptr
         i_event = i_event + 3

      end do

c     copy svt_time_st

      do i_time = 1, svt_time_n

         svt_event(i_event+1).idata = svt_time(i_time).time
         svt_event(i_event+2).idata = svt_time(i_time).ndc
         svt_event(i_event+3).idata = svt_time(i_time).ptr
         i_event = i_event + 3

      end do

c     copy svt_adc_st

      do i_adc = 1, svt_adc_n

         svt_event(i_event+1).idata = svt_adc(i_adc).adc
         i_event = i_event + 1

      end do

c     total size

      svt_event(1) = i_event

      if (i_event.eq.5+3*svt_wafer_n+3*svt_anode_n+3*svt_time_n+svt_adc) then

c     status is fatal error in the copy

         svt_sevb = -1

c     status is OK

         svt_sevb = 0

      end if

      return
      end
