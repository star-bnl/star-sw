!    rl0_rl0ctb_cal_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: rl0ctb_cal
! table descriptor name: rl0ctb_cal_desc_st                                          
!    row structure name: rl0ctb_cal_row_st                                           
!           description: Level 0 Calibration Table for CTB
!
       STRUCTURE /rl0ctb_cal_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(rl0ctb_cal_nv)
       END STRUCTURE ! rl0ctb_cal_desc_st

       STRUCTURE /rl0ctb_cal_row_st/
! fit(2) = Fit for ADC to hit calibration
           REAL*4  fit(2)
       END STRUCTURE      ! rl0ctb_cal_row_st
!  Last mod. for rl0ctb_cal:   758587065 Fri Jan 14 17:37:45 1994
!
