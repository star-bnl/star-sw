* $Id: tfs_ini.F,v 1.6 1999/10/11 16:53:13 sakrejda Exp $
* $Log: tfs_ini.F,v $
* Revision 1.6  1999/10/11 16:53:13  sakrejda
* Obsolete parameters removed and the old code to do the pileup
* deleted (not Akio's , akio's is still in!). It did not work anyway.
*
* Revision 1.5  1999/07/19 19:15:49  genevb
* Use StMessMgr
*
* Revision 1.4  1998/12/13 21:36:18  sakrejda
* parameters and constants cleaned up, bmpar removed
*
* Revision 1.3  1998/07/02 19:13:16  sakrejda
* B field obtained via gufld call
*
* Revision 1.2  1998/03/03 23:03:14  hardtke
* use constants files
*
* Revision 1.1  1998/01/27 01:47:23  fisyak
* Split sources
*
      SUBROUTINE TFS_INI(par_h, par, ctrl_h, ctrl, errflg)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     
C     Input arguments:
C     par_h   = header for tfs fspar table
C     par     = rows of tfs fspar table
C     ctrl_h  = header for tfs ctrl table
C     ctrl    = rows of tfs ctrl table
C     errflg  = error flag -- .false. - INIT successful; .true. - INIT failed
C
C     Output arguments:
C
C     Functional Description:
C     
C     Author:
C     Peter G. Jones  LBL  (510)-486-5436
C     
C     Creation Date:
C     13-Feb-1992
C     
C     Modefied
C     By                Date                    Reason
C     --                ----                    ------
C     Dhammika W.      03/29/1997       Modefied the calculation of
C                                       diffusion coefficients in the
C                                       pad response function and the pad
C                                       resolution function  to accommodate
C                                       zero B-field case.
C     D. Hardtke       03/02/98         Delete unused constants
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fspar.inc"
#include "tfs_fsctrl.inc"
#include "math_constants.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st  / par(*)
      RECORD / table_head_st / ctrl_h
      RECORD / tfs_fsctrl_st / ctrl(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)

      REAL SIGMA02(4), SIGMAD2(4)
      REAL PADL(2), PADL2(2), ANG_WIREF(4), ANG_PADF(4), ION_POT,
     >     WIRE_SPACE, WIRE_SPACE2, CLUS_DIS_P10
      COMMON /PADRES/ SIGMA02, SIGMAD2,
     >     PADL,PADL2,ANG_WIREF,ANG_PADF,ION_POT,WIRE_SPACE,
     >     WIRE_SPACE2,CLUS_DIS_P10

      REAL SPRF_02(2), SPRF_DR2(2), SPRF_TA2(2)
      REAL SIG_LDIFF2, SIG_SHAPE2
      COMMON /PADREF/ SPRF_02, SPRF_DR2, SPRF_TA2,
     >     SIG_LDIFF2, SIG_SHAPE2
      
      REAL ALOW, AHIGH, ASCALE
      COMMON /ASCALE/ ALOW, AHIGH, ASCALE
C     ALOW   - max value of crossing angle for pad response to be valid
C     AHIGH  - min value of crossing angle for projected width to be valid
C     ASCALE - is the linear scale factor in-between

      REAL RSEP(2), ZSEP, RPLIM(2), ZLIM
      COMMON /TPMERG/ RSEP, ZSEP, RPLIM, ZLIM
      
      REAL DRIVEL, NEVENT, DELTIM, EVRATE
      LOGICAL ERRFLG
      
      REAL  CONST, OMEGA_TAU
      REAL  CLUS_PER_CM_AR, CLUS_PER_CM_CH4, F_AR, F_CH4, PRESSURE

      INTEGER  n0
      INTEGER tgc_drift_velocity
      INTEGER iret
      REAL    dummy
      INTEGER tgc_drift_volume_length
      REAL    xlength

CDH test variables:
      real origin(3), bfield(3)      
      data origin /3*0/

      PARAMETER(CLUS_PER_CM_AR  = 22.3,
     >          CLUS_PER_CM_CH4 = 24.3,
     >          F_AR = 0.90,
     >          F_CH4 = 0.10,
     >          PRESSURE = 1.0)

C=========================== Begin Executable Code =======================
C23456789012345678901234567890123456789012345678901234567890123456789012
      
      ERRFLG = .FALSE.
      if( par(1).nsect .gt. mxsector ) then
         call StError(' TFS_INI: Fatal error')
         call StError(' Reason: Number of sectors exceeds MXSECTOR')
         ERRFLG = .TRUE.
         RETURN
      endif
      if( par(1).nrow .gt. mxrow ) then
         call StError(' TFS_INI: Fatal error')
         call StError(' Reason: Number of rows exceeds MXROW')
         ERRFLG = .TRUE.
         RETURN
      endif
c     get drift velocity
      iret = tgc_drift_velocity(drivel, dummy, dummy)

c     get the TPC length
      iret = tgc_drift_volume_length(xlength,dummy,dummy)
      
C     pad response function (prf)
      
C     modefy sprf_dr2 & sigmad2 to accomodate zero B_filed case.
C     In the new definition, sprf_dr0 in  tfs_fspar represents the
C     field free single electron diffusion;
C     now  sprf_dr2 = sprf_dr0^2/(C + (omega*tau)^2)
C     omega -- electron cyclotron frequency, tau -- mean collision time
C     For the time being (as of 03/29/1997), for non-zero B-filed, we use 
C     C=2.8 & omega*tau=2.34 (see STAR CDR p. 4C-15 & S.R. Amendolia et al. 
C     NIM A244 (1986) 516) and for zero B-field, C=1 & omega*tau=0. 
C     *** DSW  03/29/1997 ***
      call gufld(origin,bfield)
      if ( bfield(3).ne. 0. ) then
         Const     = 2.8
         omega_tau = 2.34
      else
         Const     = 1.0
         omega_tau = 0.0
      endif

      sprf_02(1)  = ctrl(1).sprf_0(1)**2
      sprf_02(2)  = ctrl(1).sprf_0(2)**2
      sprf_dr2(1) = ctrl(1).sprf_dr0(1)**2/(Const + omega_tau**2)
      sprf_dr2(2) = ctrl(1).sprf_dr0(2)**2/(Const + omega_tau**2)
      sprf_ta2(1) = ctrl(1).sprf_ta(1)**2
      sprf_ta2(2) = ctrl(1).sprf_ta(2)**2

       
C     longitudinal coefficient of diffusion (units :- sqrt(cm))
      sig_ldiff2 = par(1).sldiff**2
      
C     sigma of longitudinal pulse due to electronics shaping time -
C     TSHAPE is a FWHM in nanoseconds, DRIVEL is cm/microseconds

      sig_shape2 = ( drivel * par(1).tshape / 2.36 *1.0e-9)**2
      
C     spatial resolution

C     Modefy the calculation of  sigmad2 to accommodate the changes to
C     the calculation of sprf_dr2, above (see STAR CDR p. 4C-28).
C     ***  DSW  03/29/1997  ***

      wire_space   = 0.4
      wire_space2  = wire_space**2

      padL(1)      = par(1).padlen(1)
      padL(2)      = par(1).padlen(2)
      padL2(1)     = par(1).padlen(1)**2
      padL2(2)     = par(1).padlen(2)**2

C     Following calculation is needed to determine the  inter cluster
C     distance for P10.
C     See Roy B.'s mail to DSW dated May 7, 1997.
      clus_dis_P10 = pressure*
     >     (f_Ar*clus_per_cm_Ar + f_CH4*clus_per_cm_CH4)

C     P10 ionization potential  in GeV
      Ion_Pot = 28.5e-9 

      if (ctrl(1).sys_test.eq.0) then
         sigma02(1)   = ctrl(1).sigma0(1)**2
         sigma02(2)   = ctrl(1).sigma0(2)**2
         sigmad2(1)   = sprf_dr2(1)
         sigmad2(2)   = sprf_dr2(2)
         ang_wireF(1) = ctrl(1).ang_wireF(1)
         ang_wireF(2) = ctrl(1).ang_wireF(2)
         ang_padF(1)  = ctrl(1).ang_padF(1)
         ang_padF(2)  = ctrl(1).ang_padF(2)
      elseif (ctrl(1).sys_test.ge.1) then
         sigma02(3)   = ctrl(1).sigma0(3)**2
         sigma02(4)   = ctrl(1).sigma0(4)**2
         sigmad2(3)   = ctrl(1).sprf_dr0(3)**2
         sigmad2(4)   = ctrl(1).sprf_dr0(4)**2
         ang_wireF(3) = ctrl(1).ang_wireF(3)
         ang_wireF(4) = ctrl(1).ang_wireF(4)
         ang_padF(3)  = ctrl(1).ang_padF(3)
         ang_padF(4)  = ctrl(1).ang_padF(4)
      endif
      
C     calculate limits for the transition from prf to projected width
      alow  = ctrl(1).alpha_lo * C_RAD_PER_DEG
      ahigh = ctrl(1).alpha_hi * C_RAD_PER_DEG
      ascale = ( ahigh - alow )
      
C     calculate merging criterion
      rsep(1) = ctrl(1).drmin * par(1).padwid(1)
      rsep(2) = ctrl(1).drmin * par(1).padwid(2)
      zsep    = ctrl(1).dzmin * xlength / par(1).zbmax
      
C     calculate merging limits
      rplim(1) = ctrl(1).merge_r * par(1).padwid(1)
      rplim(2) = ctrl(1).merge_r * par(1).padwid(2)
      zlim     = ctrl(1).merge_z * xlength / par(1).zbmax
      
 999  return
      end






