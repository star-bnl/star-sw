!    rl0_rl0_data_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: rl0_data
! table descriptor name: rl0_data_desc_st                                            
!    row structure name: rl0_data_row_st                                             
!           description: Level 0 trigger data
!
       STRUCTURE /rl0_data_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(rl0_data_nv)
       END STRUCTURE ! rl0_data_desc_st

       STRUCTURE /rl0_data_row_st/
! bits = Bit word
           INTEGER*4 bits
! fpga1_ctb(64) = Fpga sum for 1st tree CTB
           INTEGER*4 fpga1_ctb(64)
! fpga1_mwc(64) = Fpga sum for 1st tree MWC
           INTEGER*4 fpga1_mwc(64)
! fpga2_ctb(32) = Fpga sum for 2nd tree CTB
           INTEGER*4 fpga2_ctb(32)
! fpga2_mwc(32) = Fpga sum for 2nd tree MWC
           INTEGER*4 fpga2_mwc(32)
! fpga3_ctb(4) = Fpga sum for 3rd tree CTB
           INTEGER*4 fpga3_ctb(4)
! fpga3_mwc(4) = Fpga sum for 3rd tree MWC
           INTEGER*4 fpga3_mwc(4)
! fpga4_ctb = 4th fpga tree tot ctb
           INTEGER*4 fpga4_ctb
! fpga4_mwc = 4th Fpga tree for MWC
           INTEGER*4 fpga4_mwc
! fpga5_tot = Fpga sum for 5th tree Total Multiplicity
           INTEGER*4 fpga5_tot
! info(64) = Level zero information
           INTEGER*4 info(64)
       END STRUCTURE      ! rl0_data_row_st
!  Last mod. for rl0_data:   762211102 Fri Feb 25 16:18:22 1994
!
