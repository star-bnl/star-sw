      integer function svt_asics(
     1     svt_asics_data_h,  svt_asics_data,
     1     svt_asics_calib_h, svt_asics_calib,
     1     svt_asics_para_h,  svt_asics_para)
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
      record  /svt_asics_calib_st/svt_asics_calib(svt_asics_calib_nmax)
      record  /svt_asics_para_st/svt_asics_para(svt_asics_para_nmax)

c     local declarations
c     ==================

      integer anode, i, i1, i2, n, i2p, i3
      integer imin, imax, ilast
      integer iwafer, iw

      logical done1, done2

c     executable
c     ==========
c     assume success

      svt_asics = 0
      
c     loop on all wafers present
       
      do iwafer = 1, svt_asics_data_n
         
         iw = svt_asics_data(iwafer).wafer
         
c     reset select array
         
         do anode = 1, svt_asics_para(iw).n_anode
            do i = 1, svt_asics_para(iw).n_sample
               svt_asics_data(iw).select(i,anode) = 0
            end do
         end do
         
c     start analysis here
         
 
         do anode = 1, svt_asics_para(iw).n_anode
            
            imin  = 1
            imax  = svt_asics_para(iw).n_sample + 1
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
                        if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para(iw).high) then
                           n = n + 1
                        else
                           done2 = .true.
                        end if
                     else
                        done2 = .true.
                     end if 
                  end do

                  if (n.ge.svt_asics_para(iw).nhigh) then
                     
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
               i2 = i1 + svt_asics_para(iw).n_sample*(anode-1) - 1
               i3 = mod(i2,32)
               i2 = 1 + (i2-i3)/32
               if (svt_asics_data(iw).select(i1,anode).gt.0) then
                  svt_asics_data(iw).mask(i2) = ibset(svt_asics_data(iw).mask(i2),i3)
                  write(6,*) ' anode, sample, i2, i3, v:',anode,i1,i2,i3,svt_asics_data(iw).mask(i2)
               end if
            end do
         end do
         
      end do
      
      return
      end

