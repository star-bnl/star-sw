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

