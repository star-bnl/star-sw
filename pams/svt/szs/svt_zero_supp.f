      integer function svt_asics(
     1     svt_asics_data_h,   svt_asics_data,
     1     svt_asics_calib_h,  svt_asics_calib,
     1     svt_asics_para_h,   svt_asics_para,
     1     svt_wafer_h,        svt_wafer,
     1     svt_anode_h,        svt_anode,
     1     svt_time_h,         svt_time,
     1     svt_adc_h,          svt_adc)
      implicit none

c     created July 15, 1993 by C. Pruneau, WSU

c     description: perform pedestal subtraction and
c     finds cluster on a given wafer and output zero suppresed info. 
c     HERE I ASSUME A CLUSTER IS A SEQUENCE OF SAMPLES ABOVE THRESHOLD
C     IN ONE ANODE. I.E. AN ACTUAL HIT WILL BE COMPOSED OF POSSIBLY
C     MANY OF THIS "CLUSTERS"
c     this is meant to be the code ran by the ASICs
c
c     argument declarations
c     =====================
c     Input Arguments: 
c
c     svt_asics_data_h     : header to table svt_asics_data
c     svt_asics_data       : rows of table   svt_asics_data_st
c     svt_asics_calib_h    : header to table svt_asics_calib
c     svt_asics_calib      : rows of table   svt_asics_calib_st
c     svt_asics_para_h     : header to table svt_asics_para
c     svt_asics_para       : rows of table   svt_asics_para_st

c     Output Arguments:
c
c     svt_wafer_h          : header to table svt_wafer
c     svt_wafer            : rows of table   svt_wafer_st
c     svt_anode_h          : header to table svt_anode
c     svt_anode            : rows of table   svt_anode_st
c     svt_time_h           : header to table svt_time
c     svt_time             : rows of table   svt_time_st
c     svt_adc_h            : header to table svt_adc
c     svt_adc              : rows of table   svt_adc_st


      INCLUDE 'tas_structures_inc'
      INCLUDE 'tas_user_codes_inc'

      INCLUDE 'svt_asics_data_pars_inc'
      INCLUDE 'svt_asics_data_st_inc'
      INCLUDE 'svt_asics_calib_pars_inc'
      INCLUDE 'svt_asics_calib_st_inc'
      INCLUDE 'svt_asics_para_pars_inc'
      INCLUDE 'svt_asics_para_st_inc'
      INCLUDE 'svt_wafer_pars_inc'
      INCLUDE 'svt_wafer_st_inc'
      INCLUDE 'svt_anode_pars_inc'
      INCLUDE 'svt_anode_st_inc'
      INCLUDE 'svt_time_pars_inc'
      INCLUDE 'svt_time_st_inc'
      INCLUDE 'svt_adc_pars_inc'
      INCLUDE 'svt_adc_st_inc'


      RECORD/ table_head_st        / svt_asics_data_h
      RECORD/ svt_asics_data_st    / svt_asics_data(*)
      RECORD/ table_head_st        / svt_asics_calib_h
      RECORD/ svt_asics_calib_st   / svt_asics_calib(*)
      RECORD/ table_head_st        / svt_asics_para_h
      RECORD/ svt_asics_para_st    / svt_asics_para(*)
      RECORD/ table_head_st        / svt_wafer_h
      RECORD/ svt_wafer_st         / svt_wafer(*)
      RECORD/ table_head_st        / svt_anode_h
      RECORD/ svt_anode_st         / svt_anode(*)
      RECORD/ table_head_st        / svt_time_h
      RECORD/ svt_time_st          / svt_time(*)
      RECORD/ table_head_st        / svt_adc_h
      RECORD/ svt_adc_st           / svt_adc(*)

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
       
      do iwafer = 1, svt_asic_data.nok
         
         iw = svt_asics_data(iwafer).wafer
         
c     perform pedestal subtraction/gain equalization
c     and reset select array

         size = svt_asics_para(iw).n_anode * svt_asics_para(iw).n_sample
         do i = 1, size
            svt_asics_data(iw).q(i) = svt_asics_calib(1).gain(i) * (svt_asics_data(iw).q(i) - svt_asics_calib(1).ped(i))
            svt_asics_data(iw).select(i) = 0
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

