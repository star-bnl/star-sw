!    rl0_rl0mwc_cal_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: rl0mwc_cal
! table descriptor name: rl0mwc_cal_desc_st                                          
!    row structure name: rl0mwc_cal_row_st                                           
!           description: Clibration of MWC in RL0
!
       STRUCTURE /rl0mwc_cal_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(rl0mwc_cal_nv)
       END STRUCTURE ! rl0mwc_cal_desc_st

       STRUCTURE /rl0mwc_cal_row_st/
! fit(2) = slope and interc. adc.vs.hits mwc eta+
           REAL*4  fit(2)
       END STRUCTURE      ! rl0mwc_cal_row_st
!  Last mod. for rl0mwc_cal:   758587281 Fri Jan 14 17:41:21 1994
!
