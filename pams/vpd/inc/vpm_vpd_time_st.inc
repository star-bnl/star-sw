!    vpm_vpd_time_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: vpd_time
! table descriptor name: vpd_time_desc_st                                            
!    row structure name: vpd_time_row_st                                             
!           description: Determines time minimum from each       segment of VPD
!
       STRUCTURE /vpd_time_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(vpd_time_nv)
       END STRUCTURE ! vpd_time_desc_st

       STRUCTURE /vpd_time_row_st/
! id = primary key
           INTEGER*4 id
! neg_time = Min time for VPD eta lt 0
           REAL*4  neg_time
! pos_time = Min time for VPD eta gt 0
           REAL*4  pos_time
       END STRUCTURE      ! vpd_time_row_st
!  Last mod. for vpd_time:   749769837 Mon Oct  4 17:23:57 1993
!
