!    rl1_pw_ctrl_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: pw_ctrl
! table descriptor name: pw_ctrl_desc_st                                             
!    row structure name: pw_ctrl_row_st                                              
!           description: Controll table for level 1 1 dimensionalpower spectrum code.
!
       STRUCTURE /pw_ctrl_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(pw_ctrl_nv)
       END STRUCTURE ! pw_ctrl_desc_st

       STRUCTURE /pw_ctrl_row_st/
! bnd_width = power spectrum band width
           INTEGER*4 bnd_width
! ctb_eta = Number of eta bins in CTB
           INTEGER*4 ctb_eta
! ctb_phi = Number of phi bins in CTB
           INTEGER*4 ctb_phi
! mwc_eta = Number of eta bins in MWC
           INTEGER*4 mwc_eta
! mwc_phi = Number of phi bins in MWC
           INTEGER*4 mwc_phi
! n_bins = Number of level 1 1-D bins
           INTEGER*4 n_bins
! nul_hyp = switch for null hypothesis
           INTEGER*4 nul_hyp
! switch = switch to turn module on
           INTEGER*4 switch
! time_loop = number of do loop iterations
           INTEGER*4 time_loop
! c_coef(100) = cos coefficient
           REAL*4  c_coef(100)
! ctb_fact = factor for CTB input data
           REAL*4  ctb_fact
! mwc_fact = factor for MWC input data
           REAL*4  mwc_fact
! s_coef(100) = sin coefficient
           REAL*4  s_coef(100)
       END STRUCTURE      ! pw_ctrl_row_st
!  Last mod. for pw_ctrl:   787946742 Tue Dec 20 13:05:42 1994
!
