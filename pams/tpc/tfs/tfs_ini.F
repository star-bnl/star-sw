* $Id: tfs_ini.F,v 1.3 1998/07/02 19:13:16 sakrejda Exp $
* $Log: tfs_ini.F,v $
* Revision 1.3  1998/07/02 19:13:16  sakrejda
* B field obtained via gufld call
*
* Revision 1.2  1998/03/03 23:03:14  hardtke
* use constants files
*
* Revision 1.1  1998/01/27 01:47:23  fisyak
* Split sources
*
      SUBROUTINE TFS_INI(par_h, par, ctrl_h, ctrl, bmpar_h, bmpar,
     >                    errflg)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     
C     Input arguments:
C     par_h   = header for tfs fspar table
C     par     = rows of tfs fspar table
C     ctrl_h  = header for tfs ctrl table
C     ctrl    = rows of tfs ctrl table
C     bmpar_h = header for tfs bmpar table
C     bmpar   = rows of tfs bmpar table
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
#include "tfs_bmpar.inc"
#include "math_constants.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st  / par(*)
      RECORD / table_head_st / ctrl_h
      RECORD / tfs_fsctrl_st / ctrl(*)
      RECORD / table_head_st / bmpar_h
      RECORD / tfs_bmpar_st  / bmpar(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)

      REAL SIGMA02(4), SIGMAD2(4), SIGMAT2(4), SIGMA_L
      REAL PADL(2), PADL2(2), ANG_WIREF(4), ANG_PADF(4), ION_POT,
     >     WIRE_SPACE, WIRE_SPACE2, CLUS_DIS_P10
      COMMON /PADRES/ SIGMA02, SIGMAD2, SIGMAT2, SIGMA_L,
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
      
      REAL DZXING, EVPXNG
      INTEGER NXINGS, NXTRIG
      COMMON /EVMERG/ DZXING, NXINGS, NXTRIG, EVPXNG
C     DZXING - z separation between beam bunches
C     NXINGS - number of beam crossings per TPC live time
C     NXTRIG - the number of the triggered crossing
C     EVPXNG - average number of events per beam crossing
      
C     Local variables
      
      REAL DRIVEL, TXING, NEVENT, DELTIM, EVRATE
      LOGICAL ERRFLG
      
      REAL  CONST, OMEGA_TAU
      REAL  CLUS_PER_CM_AR, CLUS_PER_CM_CH4, F_AR, F_CH4, PRESSURE

      INTEGER  n0

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
         WRITE(6,'(A)') ' TFS_INI: Fatal error'
         WRITE(6,'(A)') ' Reason: Number of sectors exceeds MXSECTOR'
         ERRFLG = .TRUE.
         RETURN
      endif
      if( par(1).nrow .gt. mxrow ) then
         WRITE(6,'(A)') ' TFS_INI: Fatal error'
         WRITE(6,'(A)') ' Reason: Number of rows exceeds MXROW'
         ERRFLG = .TRUE.
         RETURN
      endif
      
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

      sprf_02(1)  = par(1).sprf_0(1)**2
      sprf_02(2)  = par(1).sprf_0(2)**2
      sprf_dr2(1) = par(1).sprf_dr0(1)**2/(Const + omega_tau**2)
      sprf_dr2(2) = par(1).sprf_dr0(2)**2/(Const + omega_tau**2)
      sprf_ta2(1) = par(1).sprf_ta(1)**2
      sprf_ta2(2) = par(1).sprf_ta(2)**2

       
C     longitudinal coefficient of diffusion (units :- sqrt(cm))
      sig_ldiff2 = par(1).sldiff**2
      
C     sigma of longitudinal pulse due to electronics shaping time -
C     TSHAPE is a FWHM in nanoseconds, DRIVEL is cm/microseconds

      sig_shape2 = ( par(1).drivel * par(1).tshape / 2.36 / 1000. )**2
      
C     spatial resolution

C     Modefy the calculation of  sigmad2 to accommodate the changes to
C     the calculation of sprf_dr2, above (see STAR CDR p. 4C-28).
C     ***  DSW  03/29/1997  ***

C     n0 -- Number of electrons liberated per cm.  04/17/1997  DSW
      n0 = par(1).n0_pcm
      
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

      if (par(1).sys_test.eq.0) then
         sigma02(1)   = par(1).sigma0(1)**2
         sigma02(2)   = par(1).sigma0(2)**2
C     sigmad2(1)   = sprf_dr2(1)/(n0*par(1).padlen(1))
C     sigmad2(2)   = sprf_dr2(2)/(n0*par(1).padlen(2))
         sigmad2(1)   = sprf_dr2(1)
         sigmad2(2)   = sprf_dr2(2)
         sigmat2(1)   = par(1).sigmat(1)**2
         sigmat2(2)   = par(1).sigmat(2)**2
         ang_wireF(1) = par(1).ang_wireF(1)
         ang_wireF(2) = par(1).ang_wireF(2)
         ang_padF(1)  = par(1).ang_padF(1)
         ang_padF(2)  = par(1).ang_padF(2)
      elseif (par(1).sys_test.ge.1) then
         sigma02(3)   = par(1).sigma0(3)**2
         sigma02(4)   = par(1).sigma0(4)**2
         sigmad2(3)   = par(1).sprf_dr0(3)**2
         sigmad2(4)   = par(1).sprf_dr0(4)**2
         sigmat2(3)   = par(1).sigmat(1)**2
         sigmat2(4)   = par(1).sigmat(2)**2
         ang_wireF(3) = par(1).ang_wireF(3)
         ang_wireF(4) = par(1).ang_wireF(4)
         ang_padF(3)  = par(1).ang_padF(3)
         ang_padF(4)  = par(1).ang_padF(4)
      endif

      sigma_l      = par(1).sigmal
      
C     calculate limits for the transition from prf to projected width
      alow  = par(1).alpha_lo * C_RAD_PER_DEG
      ahigh = par(1).alpha_hi * C_RAD_PER_DEG
      ascale = ( ahigh - alow )
      
C     calculate merging criterion
      rsep(1) = par(1).drmin * par(1).padwid(1)
      rsep(2) = par(1).drmin * par(1).padwid(2)
      zsep    = par(1).dzmin * par(1).tpclen / par(1).zbmax
      
C     calculate merging limits
      rplim(1) = par(1).merge_r * par(1).padwid(1)
      rplim(2) = par(1).merge_r * par(1).padwid(2)
      zlim     = par(1).merge_z * par(1).tpclen / par(1).zbmax
      
C     calculate number of events per gate opening
      if (ctrl(1).ppflag .ne. 0) then
         drivel = par(1).drivel * 1.e+06
         deltim = 2.0*par(1).tpclen / drivel
         evrate = bmpar(1).xsec * 1.e-27 * bmpar(1).lumin
         nevent = evrate * deltim
         txing  = bmpar(1).txing * 1.e-09
         nxings = deltim / txing
         nxtrig = nint( nxings / 2. )
         evpxng = nevent / nxings
         dzxing = drivel * txing

         WRITE(6,'(A,F5.2)') ' average pp events per crossing: ',evpxng
      else
         nxings = 1
         nxtrig = 1
      endif
      
 999  return
      end






