!    rl1_ks_out_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: ks_out
! table descriptor name: ks_out_desc_st                                              
!    row structure name: ks_out_row_st                                               
!           description: K-S test level 1 output table.
!
       STRUCTURE /ks_out_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(ks_out_nv)
       END STRUCTURE ! ks_out_desc_st

       STRUCTURE /ks_out_row_st/
! accept = Accept - 1, or reject -0 event.
           INTEGER*4 accept
! cpu = CPU level 1 processing time.
           REAL*4  cpu
! ks_d = K-S test d parameter.
           REAL*4  ks_d
! ks_sig = K-S test significance value.
           REAL*4  ks_sig
       END STRUCTURE      ! ks_out_row_st
!  Last mod. for ks_out:   758570317 Fri Jan 14 12:58:37 1994
!
