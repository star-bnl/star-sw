!    rl1_pw_out_st.inc
!     (tast_inc.ace)
!
!-----------------------------------------------------------------------------
!   Table: pw_out
! table descriptor name: pw_out_desc_st                                              
!    row structure name: pw_out_row_st                                               
!           description: Level 1 output table for autocorrelationanalysis
!
       STRUCTURE /pw_out_desc_st/
           RECORD /table_head_st/ h
           RECORD /table_vinfo_st/ vinfo(pw_out_nv)
       END STRUCTURE ! pw_out_desc_st

       STRUCTURE /pw_out_row_st/
! aut_cpu = CPU time to do autocorrelation
           REAL*4  aut_cpu
! autocorr(100) = autocorrelation output
           REAL*4  autocorr(100)
! autonull(100) = autocorrelation minus null hyp.
           REAL*4  autonull(100)
! c1(100) = cos coefficient from null hyp.
           REAL*4  c1(100)
! phase = phase value
           REAL*4  phase
! pow_cpu = CPU time to do power spectra
           REAL*4  pow_cpu
! power = power value
           REAL*4  power
! s1(100) = sin coefficient from null hyp.
           REAL*4  s1(100)
       END STRUCTURE      ! pw_out_row_st
!  Last mod. for pw_out:   783377928 Fri Oct 28 16:58:48 1994
!
