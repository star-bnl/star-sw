c
c     svt_clus.f
c
c     cluster finder for the svt.
c     zero suppression simulation
c
c     Author: C. Pruneau, WSU, March 3, 92
c
c     Modules in this file:
c
c     integer function svt_asics(svt_asics_data)
c
c
c=====================================================================================
      integer function svt_asics(svt_asics_data,svt_asics_data_n,
     1     svt_asics_calib,svt_asics_calib_n,
     1     svt_asics_para,svt_asics_para_n)
      implicit none
c
c     perform pedestal subtraction and
c     finds cluster on a given wafer and output zero suppresed info. 
c     HERE I ASSUME A CLUSTER IS A SEQUENCE OF SAMPLES ABOVE THRESHOLD
C     IN ONE ANODE. I.E. AN ACTUAL HIT WILL BE COMPOSED OF POSSIBLY
C     MANY OF THIS "CLUSTERS"
c     this is meant to be the code ran by the ASICs
c
c     returns 0 if no error occured
c            -1 if some fatal error occured
c             1 if some non fatal error occured
c
c     svt_asics_data is a struc containing the svt_asics_data information relative to the current
c     wafer being analysed.
c
c     svt_asics_para().n_anode     : number of active anodes on current wafer
c     svt_asics_para().n_sample    : number of samples read out 
c     svt_asics_para().high        : high threshold
c     svt_asics_para().n_high      : number of contiguous pixels required to be above high threshold
c     svt_asics_para().low         : low threshold
c     svt_asics_para().n_before    : number of pixels recorded on the leading edge before low threshold
c     svt_asics_para().n_after     : number of pixels recorded on the trailing edge after low threshold

c     svt_asics_data().q           : charge read out, it is an array of dimension: n_sample x n_anode
c     svt_asics_data().select      : on output, array of pixels selected for readout
c

c     global declarations
c     ===================

      include 'svt_inc.f'

      record  /svt_asics_data_st/svt_asics_data(svt_asics_data_nmax)
      record  /svt_asics_calib_st/svt_asics_calib(svt_asics_calib_namx)
      record  /svt_asics_para_st/svt_asics_para(svt_asics_para_nmax)

c     local declarations
c     ==================

      integer anode, i, i1, i2, n
      integer imin, imax, ilast

      logical done1, done2

c     executable
c     ==========
c     assume success

      svt_clus = 0

c     loop on all wafers present

      do iwafer = 1, svt_asics_data_n

         iw = svt_asics_data.wafer

c     reset select array

      do anode = 1, svt_asics_para(iw).n_anode
         do i = 1, svt_asics_para(iw).n_sample
            svt_asics_data(iw).select(i,anode) = 0
         end do
      end do

c     start analysis here

      do anode = 1, svt_asics_para(iw).n_anode

         imin  = 1
         imax  = svt_asics_para(iw).n_samples + 1
         done1 = .false.
         i1    = imin

         do while (.not.done1)
            i1 = i1 + 1
            if (svt_asics_data(iw).q(i1,anode).gt.svt_asics_para(iw).high) then

c     check if enough samples above high threshold
               done2  = .false.
               i2     = i1
               n      = 1
               do while (.not.done2)
                  i2 = i2 + 1
                  if (i2.lt.imax) then
                     if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para_high) then
                        n = n + 1
                     else
                        done2 = .true.
                     end if
                  else
                     done2 = .true.
                  end if 
               end do

               if (n.ge.svt_asics_para(iw).n_high) then

c     have a cluster - proceed to selection of samples above high threshold

                  do i2 = i1, i1+n-1
                     svt_asics_data(iw).select(i2,anode) = 1
                  end do

c     look backward to see how many samples above low threshold

                  done2 = .false.
                  i2 = i1
                  do while (.not.done2)
                     i2  = i2 - 1
                     if (i2.gt.imin) then
                        if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para(iw).low) then
                           svt_asics_data(iw).select(i2,anode) = 1
                        else
                           done2 = .true.
                        end if
                     else
                        done2 = .true.
                     end if
                  end do

c     select leading samples backward

                  done2 = .false.
                  i2p = 0
                  i2  = i2 + 1
                  do while (.not.done2)
                     i2  = i2 - 1
                     i2p = i2p + 1
                     if (i2p.le.svt_asics_para(iw).nlow.and.i2.gt.imin) then
                        svt_asics_data(iw).select(i2,anode) = 1
                     else
                        done2 = .true.
                     end if
                  end do

c     look forward to see how many samples above low threshold

                  done2 = .false.
                  i2 = i1 + n
                  do while (.not.done2)
                     i2  = i2 + 1
                     if (i2.le.imax) then
                        if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para(iw).low) then
                           svt_asics_data(iw).select(i2,anode) = 1
                        else
                           done2 = .true.
                        end if
                     else
                        done2 = .true.
                     end if
                  end do
                  
c     select trailing samples

                  done2 = .false.
                  i2p = 0
                  i2 = i2 - 1
                  do while (.not.done2)
                     i2  = i2  + 1
                     i2p = i2p + 1
                     if (i2p.le.svt_asics_para(iw).nhigh.and.i2.gt.imax) then
                        svt_asics_data(iw).select(i2,anode) = 1
                     else
                        done2 = .true.
                     end if
                  end do

               else

c     not a good cluster, proceed forward...

                  i1 = i1 + n
               end if

            end if
         end do 

      end do 

      do anode = 1, svt_asics_para(iw).n_anode
         do i1 = 1, svt_asics_para(iw).n_sample
            i2 = i1 + svt_asics_para(iw).sample*(anode-1)
            i3 = mod(i2,8)
            i2 = 1 + (i2-i3)/8
            if (svt_asics_data(iw).select.gt.0) then
               svt_asics_data(iw).mask(i2) = bset(svt_asics_data(iw).mask(i2),i3)
               write(6,*) ' anode, sample, i2, i3, v:',anode,i1,i2,i3,svt_asics_data(iw).mask(i2)
            end if
         end do
      end do

      end do

      return
      end

c=========================================================================================

      integer function svt_pack(svt_asics_data,svt_asics_data_n,
     1     svt_asics_para,svt_asics_para_n,
     1     svt_adc,svt_adc_n,
     1     svt_time,svt_time_n,svt_anode,svt_anode_n,
     1     svt_wafer,svt_wafer_n)
      implicit none

c     argument/global declaration
c     ===========================
      include 'svt_inc.f'

      record /svt_asics_data_st/ svt_asics_data(svt_asics_data_nmax)
      record /svt_asics_para_st/ svt_asics_para(svt_asics_para_nmax)
      record /svt_adc_st/   svt_adc(svt_adc_nmax)
      record /svt_time_st/  svt_time(svt_time_nmax)
      record /svt_anode_st/ svt_anode(svt_anode_nmax)
      record /svt_wafer_st/ svt_wafer(svt_wafer_nmax)

c     local declaration
c     =================

      integer i_wafer, i_anode, i_clus, i_sample
      integer nwafer, nanode, nclus, nsample
      logcial in_wafer, in_anode, in_sequence
      integer anode_ptr, time_ptr, adc_ptr

c     executable code
c     ===============

c     assume success

      svt_pack = 0

c     packing of the arrays

      svt_adc_n   = 0
      svt_time_n  = 0
      svt_anode_n = 0
      svt_wafer_n = 0

      nwafer = 0

      in_wafer    = .false.
      do i_wafer = 1, n_wafer

         nanode = 0
         in_anode    = .false.

         do i_anode = 1, n_anode

            nclus = 0
            in_sequence = .false.

            do i_sample = 1, n_sample            
               if (svt_asics_data(i_wafer).select(i_sample,i_anode).gt.0) then

                  in_anode = .true.
                  in_wafer = .true.

                  if (in_sequence) then
                     nadc = nadc + 1
                     svt_adc_n = svt_adc_n + 1
                     svt_adc(svt_adc_n).adc = svt_asics_data(iw).q(i_sample,i_anode)
                  else 
                     in_sequence = .true.

                     adc_ptr     = svt_adc_n + 1
                     nadc        = nadc + 1
                     svt_adc_n   = svt_adc_n + 1
                     
                     svt_adc(svt_adc_n).adc = svt_asics_data(iw).q(i_sample,i_anode)
                  end if
               else
                  if (in_sequence) then
                     nclus      = nclus + 1
                     svt_time_n = svt_time_n + 1

                     svt_time(svt_time_n).time = i_sample
                     svt_time(svt_time_n).nadc = nadc
                     svt_time(svt_time_n).ptr  = adc_ptr
                     
                     in_sequence = .false.
                     nadc = 0
                  else
                  end if
               end if
            end do

            if (in_anode) then
               nanode      = nanode + 1
               svt_anode_n = svt_anode_n + 1

               svt_anode(svt_anode_n).anode = i_anode
               svt_anode(svt_anode_n).nclus = nclus
               svt_anode(svt_anode_n).ptr   = time_ptr
            end if

         end do

         if (in_wafer) then
            nwafer      = nwafer + 1
            svt_wafer_n = svt_wafer_n + 1

            svt_wafer(svt_wafer_n).wafer  = i_wafer
            svt_wafer(svt_wafer_n).nanode = nanode
            svt_wafer(svt_wafer_n).ptr    = anode_ptr
         end if

      end do
      
      return
      end

c=========================================================================================

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


c=========================================================================================

      integer function svt_sev_unpack(svt_adc,svt_adc_n,
     1                          svt_time,svt_time_n,
     1                          svt_anode,svt_anode_n,
     1                          svt_wafer,svt_wafer_n,
     1                          svt_event,svt_event_n)
      implicit none

c     description: this routine unpacks an svt sub event
c     to the different structures

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
      svt_wafer_n = svt_event.ev(1)
      svt_anode_n = svt_event.ev(2)
      svt_time_n  = svt_event.ev(3)
      svt_adc_n   = svt_event.ev(4)

c     check space allocation

      if (svt_event_n .ne. svt_event.ev(1)) then

c     status is fatal error - input data inconsistency

         svt_sev_unpack = -11

      else if (svt_event_n.gt.svt_event_nmax) then

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

      i_event = 5

c     copy svt_wafer_st

      do i_wafer = 1, svt_wafer_n

         svt_wafer(i_wafer).wafer   = svt_event.ev(i_event+1) 
         svt_wafer(i_wafer).nanode  = svt_event.ev(i_event+2)
         svt_wafer(i_wafer).ptr     = svt_event.ev(i_event+3)
         i_event = i_event + 3

      end do

c     copy svt_anode_st

      do i_anode = 1, svt_anode_n

         svt_anode(i_anode).anode   = svt_event.ev(i_event+1)
         svt_anode(i_anode).nclus   = svt_event.ev(i_event+2)
         svt_anode(i_anode).ptr     = svt_event.ev(i_event+3)
         i_event = i_event + 3

      end do

c     copy svt_time_st

      do i_time = 1, svt_time_n

         svt_time(i_time).time      = svt_event.ev(i_event+1)
         svt_time(i_time).ndc       = svt_event.ev(i_event+2)
         svt_time(i_time).ptr       = svt_event.ev(i_event+3)
         i_event = i_event + 3

      end do

c     copy svt_adc_st

      do i_adc = 1, svt_adc_n

         svt_adc(i_adc).adc         = svt_event.ev(i_event+1)
         i_event = i_event + 1

      end do

      return
      end



c===========================================================================

      integer function svt_fcluster(svt_adc,svt_adc_n,
     1                          svt_time,svt_time_n,
     1                          svt_anode,svt_anode_n,
     1                          svt_wafer,svt_wafer_n,
     1                          svt_cluster,svt_cluster_n)
      implicit none

c     description: this routine performs cluster search on every wafer and output
c     the results into structure svt_clus

c     author: C. Pruneau
c     created: March 17, 93

c     argument/global declaration
c     ===========================
      include 'svt_inc.f'

c     executable code
c     ===============

      done = .false.

      if (svt_wafer_n.lt.1) then
         done = .true.
         write(6,*) '-e-svt_fclus: no data on input'
      end if

      i_wafer = 0

      do while (.not.done)

         i_wafer = i_wafer + 1

c     reset wafer work area to zero

         do ianode = 1, nanodemax
            do isample = 1, nsamplemax
               svt_work(isample,ianode) = 0.
            end do
         end do

c     copy current wafer to work array

         iwafer     = svt_wafer(i_wafer).wafer
         nanode     = svt_wafer(i_wafer).nanode
         frst_anode = svt_wafer(i_wafer).ptr

         do i_anode = 0, nanode-1
            ianode    = svt_anode(frst_anode+i_anode).anode
            nclus     = svt_anode(frst_anode+i_anode).nclus
            frst_clus = svt_anode(frst_anode+i_anode).ptr
            
            do i_time = 0, nclus-1
               itime    = svt_time(frst_clus+i_time).time
               nadc     = svt_time(frst_clus+i_time).nadc
               frst_adc = svt_time(frst_clus+i_time).ptr
               
               iclus = iclus + 1
               if (svt_clus(iclus).eq.0) then
                  
c     this clus not part of cluster analysed yet start a new one...
                  
                  icluster = icluster + 1
                  svt_clus(iclus) = 1
                  
                  iianode = ianode
                  nianode = 1
                  svt_cluster(icluster).anode(nianode).anode = ianode
                  svt_cluster(icluster).anode(nianode).time  = itime
                  
c     search for additional anodes in this cluster
                  
                  searching_anode = .true.
                  iiianode = 0
                  do while (searching_anode)
                     
                     iiianode = iiianode + 1
                     next_ianode    = svt_anode(frst_anode+i_anode+iiianode).anode
                     next_nclus     = svt_anode(frst_anode+i_anode+iiianode).nclus
                     next_frst_clus = svt_anode(frst_anode+i_anode+iiianode).ptr
                     if (next_ianode-1.eq.svt_cluster(icluster).anode(nianode).anode) then
                        searching_clus = .true.
                        iiitime = 0 
                        do while (searching_clus) 
                           iiitime = iiitime + 1
                           if (iiitime.next_nclus) then
                              searching_clus = .false.
                           else
                              next_itime    = svt_time(next_frst_clus+i_time).time
                              next_nadc     = svt_time(next_frst_clus+i_time).nadc
                              next_frst_adc = svt_time(next_frst_clus+i_time).ptr
                              if ((abs(next_itime - svt_cluster(icluster).anode(nianode).time).lt.ntime) then
                                 searching_clus = .false.
                                 
                              end if
                           end if
                        end do
                     else
                        searching_anode = .false.
                     end if
                     
                     
c     do i_adc = 0, nadc-1
c     svt_work(itime+i_adc,ianode) = svt_adc(frst_adc+i_adc)
c     end do
                  end if
               end do
               
            end do
            
c     search for cluster candidates on current wafer

         svt_high  = svt_asics_para(iwafer).high
         svt_low   = svt_asics_para(iwafer).low
         svt_nhigh = svt_asics_para(iwafer).nhigh
         svt_nlow  = svt_asics_para(iwafer).nlow
         svt_nbef  = svt_asics_para(iwafer).n_before
         svt_naft  = svt_asics_para(iwafer).n_after

         do i_anode = 1, nanodemax
            do i_time = 1, nsamplemax
               if (svt_work(i_time,i_anode).gt.svt_high) then

c     got a cluster...

                  

               end if
            end do
         end do

c     rough estimate of cluster position and size, get fitting parameter first guest

c     perform fit of current cluster

c     output cluster to structure

      end do
