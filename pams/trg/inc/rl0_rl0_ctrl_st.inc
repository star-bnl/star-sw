!    rl0_rl0_ctrl_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: rl0_ctrl
! table descriptor name: rl0_ctrl_desc_st                                            
!    row structure name: rl0_ctrl_row_st                                             
!           description: Controls the level 0 emulation
!
       STRUCTURE /rl0_ctrl_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(rl0_ctrl_nv)
       END STRUCTURE ! rl0_ctrl_desc_st

       STRUCTURE /rl0_ctrl_row_st/
! ctb_pixel_out = Number of pixels output for CTB
           INTEGER*4 ctb_pixel_out
! det_set = Detector Set ID number
           INTEGER*4 det_set
! mwc_pixel_out = Number of pixels output for MWC
           INTEGER*4 mwc_pixel_out
! nthres = Multiplicity threshold
           INTEGER*4 nthres
! segment(2) = Determines how segments are summed in RL
           INTEGER*4 segment(2)
! evc_cut = VC energy cut
           REAL*4  evc_cut
! fpga_gain(9) = Gain for FPGA trees
           REAL*4  fpga_gain(9)
! z_cut = Maximum allowed dz
           REAL*4  z_cut
       END STRUCTURE      ! rl0_ctrl_row_st
!  Last mod. for rl0_ctrl:   761409715 Wed Feb 16 09:41:55 1994
!
