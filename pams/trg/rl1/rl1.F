****************************************************************
      INTEGER FUNCTION rl1( lvl0h, lvl0, clah, cla, l1ah, l1a, clbh, clb, l1bh, l1b)
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C
C     lvl0h:      level 0 output header
C     lvl0:       level 0 output
C     clah:       level 1 input control header - ks test
C     cla:        level 1 input control - ks test
C     l1ah:       level 1 output header - ks test
C     l1a:        level 1 output - ks test
C     clbh:       level 1 input control header - autocorrelation analysis
C     clb:        level 1 input control - autocorrelation analysis
C     l1bh:       level 1 output header - autocorrelation analysis
C     l1b:        level 1 output - autocorrelation analysis
C
C   Output arguments : 
C     iok = user status code
C     
C   Functional Description : 
C     Main Trigger Level 1 analysis code.
C   Created  12-Feb-1996   Zoran Milosevich
C   Copyright 1996    Carnegie Mellon University
C   Error conditions : 
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "rl0_rl0_data_pars.inc"
#include "rl0_rl0_data_st.inc"
#include "rl1_ks_ctrl_pars.inc"
#include "rl1_ks_ctrl_st.inc"
#include "rl1_ks_out_pars.inc"
#include "rl1_ks_out_st.inc"
#include "rl1_pw_ctrl_pars.inc"
#include "rl1_pw_ctrl_st.inc"
#include "rl1_pw_out_pars.inc"
#include "rl1_pw_out_st.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st   / lvl0h 
      RECORD / rl0_data_row_st / lvl0(*)
      RECORD / table_head_st   / clah
      RECORD / ks_ctrl_row_st  / cla(*)
      RECORD / table_head_st   / l1ah
      RECORD / ks_out_row_st   / l1a(*)
      RECORD / table_head_st   / clbh
      RECORD / pw_ctrl_row_st  / clb(*)
      RECORD / table_head_st   / l1bh
      RECORD / pw_out_row_st   / l1b(*)

C
      INTEGER nok
      INTEGER ierr

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       =====================
        INTEGER I,J,K
*
*   
C       Executable Statements:
C=======================================================================
C     
	rl1 = tucod_ok	
	if ( cla(1).switch .ne. 0 ) call ks_1d_pdy ( lvl0h, lvl0, clah, cla, l1ah, l1a )
	if ( clb(1).switch .ne. 0 ) call ac_1d_tat ( lvl0h, lvl0, clbh, clb, l1bh, l1b )
	return 
	end