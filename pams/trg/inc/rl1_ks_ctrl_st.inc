!    rl1_ks_ctrl_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: ks_ctrl
! table descriptor name: ks_ctrl_desc_st                                             
!    row structure name: ks_ctrl_row_st                                              
!           description: K-S test control parameters for level 1.
!
       STRUCTURE /ks_ctrl_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(ks_ctrl_nv)
       END STRUCTURE ! ks_ctrl_desc_st

       STRUCTURE /ks_ctrl_row_st/
! ctb_eta = Number of eta bins in CTB
           INTEGER*4 ctb_eta
! ctb_phi = Number of phi bins in CTB
           INTEGER*4 ctb_phi
! mwc_eta = Number of eta bins per endcap
           INTEGER*4 mwc_eta
! mwc_phi = Number of phi bins per endcap
           INTEGER*4 mwc_phi
! n_bins = Number of compressed 1-D bins.
           INTEGER*4 n_bins
! switch = switch to turn module on.
           INTEGER*4 switch
! ref_data(100) = K-S test reference.
           INTEGER*4 ref_data(100)
! ref_data_flag = reference data flag
           INTEGER*4 ref_data_flag
! threshold = K-S test selectivity threshold.
           REAL*4  threshold
       END STRUCTURE      ! ks_ctrl_row_st
!  Last mod. for ks_ctrl:   759596937 Wed Jan 26 10:08:57 1994
!
