* $Id: tfs_gen_padres.F,v 1.8 1998/08/25 21:15:01 sakrejda Exp $
* $Log: tfs_gen_padres.F,v $
* Revision 1.8  1998/08/25 21:15:01  sakrejda
* correction to always output cossing and dip angle in degrees, STAR convention
*
* Revision 1.7  1998/07/02 19:11:29  sakrejda
* B field obtained via gufld call
*
* Revision 1.6  1998/04/02 20:54:40  sakrejda
* more proofing...
*
* Revision 1.4  1998/03/28 01:08:17  sakrejda
* Protected agains dividing by 0, sometimes gstar gives hits with 0 ionisation which translates into 0 electrons. In such cases I set number of electrons to 1
*
* Revision 1.3  1998/03/03 23:03:13  hardtke
* use constants files
*
* Revision 1.2  1998/02/19 01:28:37  hardtke
* fixed z resolution
*
* Revision 1.1  1998/01/27 01:47:22  fisyak
* Split sources
*
      SUBROUTINE TFS_GEN_PADRES( par_h, par, tphit_h, tphit )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       par_h   = header for TFS fspar table
C       par    = rows of TFS fspar table
C       tphit_h = header of TCL tphit table
C       tphit  = rows of TFC tphit table
C
C     Output Arguments:
C       values in tphit table
C
C     Functional Description:
C       This routine assigns a spatial width and a spatial resolution
C       to the TPC space-points gotten from GEANT. The spatial width
C       (aka the pad response) and the pad resolution are a function of
C       the drift length, pad crossing angle and the dip angle. The
C       parameterizations used here are described in STAR notes (#33
C       and #34). The values of the coefficients to these expressions
C       have been determined from EOS and ALEPH measurements. These
C       are contained in the parameter table.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       16-Mar-1993
C
C     Modefied
C     By                Date                    Reason
C     --                ----                    ------
C     Dhammika W.      03/29/1997       Modefied the calculation of
C                                       wmin & tlen  to accommodate
C                                       zero B-field case.
C     David Hardtke    02/14/1998       fix z resolution calculation.
C     DH               03/02/98         use math and physical constants
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fspar.inc"
#include "tcl_tphit.inc"
#include "math_constants.inc"
#include "phys_constants.inc"

C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st / par(*)
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits
      
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
      
C     Local variables
C     
C     General physical constants now in math_constants and phys_constants
C  
      
      integer i,j,k, isect, istart, iend, ipad, iret
      integer tgc_global_to_local_p
      
      real px, py, pz
      real xi, yi, zi
      real xo, yo, zo
      real aout, bout, xtemp, ytemp, atemp
      real ctheta, theta, pbend, diptan, rad, wcirc, alpha, lambda
      real sigma_x, sigma_y, sigma_tr, sigma_response
      real dlen, talpha2, calpha2, wproj, wmin, wprf, tlen, padlen
      real p_local(3),p_global(3)
      real theta_const
      real Nh,Neff_b,Neff_h,siga2(2),sigb2(2),sigc2(2),sigd2(2)
      real clus_per_cm_P10,rel_dedx,ds_ave
      real AA, BB, CC
      real xlocal(3),Bfield(3)
      DATA xlocal /3*0/

C=========================== Begin Executable Code =======================

C     loop over sector-rows

      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
            ipad = 2
            if( j .le. par(1).irmax ) ipad = 1
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            do k = istart, iend
               
C     do not consider rejected hits ( flag = 1-99 )
               if( mod(tphit(k).flag,100) .ne. 0 ) go to 100
               
C     get space point
               
               xi = tphit(k).x
               yi = tphit(k).y
               zi = tphit(k).z
               
C     momenta were copied into dx, dy, dz for safe-keeping
               
               px = tphit(k).dx
               py = tphit(k).dy
               pz = tphit(k).dz
               tphit(k).dx = 0
               tphit(k).dy = 0
               tphit(k).dz = 0
               
CCCC  calculate the tangent to the padrow (y = a*x + b)
CCCC  
CCCC  Note to users: intercept value 'bout' is not currently
CCCC  used. Its correctness can not guarenteed.
CCCC  CALL TFS_SEGIVE( par_h, par i, j, aout, bout )
CCCC  
CCCC  calculate the crossing angle as dot-product of tangent
CCCC  to the padrow and local momentum at the center of pad
CCCC  
CCCC  xtemp  = 1.0
CCCC  ytemp  = xtemp*aout
CCCC  atemp = sqrt( (xtemp**2 + ytemp**2) * (px**2 + py**2) )
CCCC  if( abs(atemp) .lt. 0.000001) then
CCCC  flag hit for discarding
CCCC  tphit(k).flag = tphit(k).flag + 2
CCCC  go to 100
CCCC  endif
CCCC  ctheta = abs( xtemp*px + ytemp*py ) / atemp
CCCC  catch rounding errors
CCCC  if( ctheta .gt. 1. ) ctheta = 1.
CCCC  theta  = acos( ctheta ) ! angle wrt pad row
CCCC  alpha  = cpi2 - abs( theta ) ! angle wrt normal
               
C     tan(px/py) in the local sector coordinates immediately gives the 
C     pad-crossing angle.  -- Apr 10, 96  --- DSW ---
               
               p_global(1) = px 
               p_global(2) = py
               p_global(3) = pz
               iret  = tgc_global_to_local_p(i,p_global,p_local)
C
               if(abs(p_local(2)).gt.0.001) then
                    alpha = atan(p_local(1)/p_local(2)) ! angle wrt normal
               else
                    alpha = C_PI_2-0.01
               endif
c
               theta = C_PI_2 - abs(alpha) ! angle wrt pad row
c              output angle should be in degree,but the angle in tfs is 
c              in radiu
c              ******Qun Li  21/08/98
c              tphit(k).alpha = alpha !output angle should be in degree
               tphit(k).alpha = alpha*C_DEG_PER_RAD 
               
C     calculate the dip angle
               
               pbend = sqrt( px**2 + py**2 )
               if( pbend .lt. 0.000001)  then
C     flag hit for discarding
                  tphit(k).flag = tphit(k).flag + 3
                  go to 100
               endif

C     Note: diptan = 1. / ( tangent of the dip angle )
C     it is coded this way to avoid overflow when pz = 0
               diptan          = abs(pz)/ pbend

c     output angle should be in degree,but the angle in tfs is in radiu
c     ******Qun Li  21/08/98
c              tphit(k).lambda = atan( diptan )
c              lambda          = tphit(k).lambda
               tphit(k).lambda = atand( diptan )
               lambda          = tphit(k).lambda*C_RAD_PER_DEG

C     Modefy the code to accommodate zero B-filed magnitude.
C     Added a piece of code to handle zero B-field case.
C     ***  DSW  03/29/1997 ***
               xlocal(1)=tphit(k).x
               xlocal(2)=tphit(k).y
               xlocal(3)=tphit(k).z
               call gufld(xlocal,Bfield)
               if ( Bfield(3).ne. 0. ) then
                  
C     evaluate the length of the track over the padrow
C     wmin - is the track length along the padrow
C     tlen  - is the approximated track arc-length
                  
C     1st -  approximate with a circle
CIS   Bfield is in kG, C_D_CURVATURE is (GeV/c)(1/kG)(1/cm)

                  rad = sqrt( px**2 + py**2 ) / 
     +                   ((C_D_CURVATURE)*abs(Bfield(3)))
                  if( rad .lt. par(1).padlen(ipad) ) then
C     this is a spiral - cannot calculate track length
                     wcirc = 2.0 * rad
                     if( wcirc .lt. par(1).padwid(ipad) ) then
                        wcirc = par(1).padwid(ipad)
                     endif
                     tlen = 0
                  else
                     padlen = par(1).padlen(ipad)/2.
                     wcirc = 2. * sqrt( 2.*rad*padlen - padlen**2.)
                     tlen = 2.*rad*atan(wcirc/(2.*(rad-padlen)))
                  endif
                  
C     2nd - calculate from a  linear geometrical projection
                  if( abs(theta) .gt. 0.0001) then
                     wproj = abs( par(1).padlen(ipad) / tan(theta) )
C     compare with the circle width and take smaller
                     wmin = min( wproj , wcirc )
                     if ( wmin .eq. wproj )

     >                    tlen = sqrt(wproj**2+par(1).padlen(ipad)**2)
                  else
                     wmin = wcirc
                  endif
               else
C     zero B-filed case.
C     For track lengths that go across an entire single pad length,
C     it is probably reasonable to approximate wmin=pad width & 
C     tlen=pad length.
C     *** DSW  03/29/1997 ***

                  theta_const = par(1).padlen(ipad)/par(1).padwid(ipad)
                  if ( tan(theta).gt.theta_const )then
                     wmin = par(1).padwid(ipad)
                     tlen = par(1).padlen(ipad)
                  else
                     wmin = abs( par(1).padlen(ipad) / tan(theta) )
                     tlen = sqrt(wmin**2+par(1).padlen(ipad)**2)
                  endif
               endif
C     calculate half-width to determine merging criterion
               wmin = wmin/2.
C     add something for diffusion
               dlen = abs( par(1).tpclen - abs(zi) ) ! in cm
               wmin = wmin + sqrt( sprf_dr2(ipad) * dlen )
               
C     for crossing angles close to 90 degrees wmin is a better
C     approximation for wprf - the spatial width calculated from
C     the pad response function.
               
C               if( theta .gt. 0.01) then ! 0.6 degree
               if( theta .gt. 0.0175) then ! 1 degree
                  talpha2 = tan(alpha)**2
                  wprf = sprf_02(ipad) +
     >                 sprf_dr2(ipad) * dlen * (1 + talpha2) +
     >                 sprf_ta2(ipad) * talpha2
                  wprf = sqrt(wprf)
C     scale wprf to meet wmin between 45.0 < alpha < 72.0 degrees
                  if( alpha .ge. alow .and. alpha .lt. ahigh ) then
                     sigma_response = wprf +
     >                    (alpha - alow)/ascale * abs(wmin - wprf)
                  elseif (alpha.lt.alow) then
                     sigma_response = wprf
                  else
                     sigma_response = wmin
                  endif
               else
                  sigma_response = wmin
               endif
               
C     store the pad/z-response width in tphit table
               
               tphit(k).prf = sigma_response
               if( tphit(k).prf .gt. par(1).reslim )
     >              tphit(k).prf = par(1).reslim
               if( tlen .eq. 0 ) then
                  tphit(k).zrf = par(1).reslim
               else
C     z-width (half width plus something for diffusion)
                  tphit(k).zrf = 0.5 * ( tlen*diptan ) + ! projected width
     >                 sqrt( sig_ldiff2 * dlen + ! diffusion
     >                 sig_shape2 ) ! shaping time
                  if( tphit(k).zrf .gt. par(1).reslim )
     >                 tphit(k).zrf = par(1).reslim
               endif
               

C     Calculate spatial resolution along the padrow
C     Spatial resolution calculation is updated to accomadate the fitting of
C     pad resolution function (in p184 of Blum & Rolandi) to Roy B.'s 
C     calculated widths.

               Nh     = tphit(k).q/Ion_Pot
CIS   The following line was added, because gstar sometimes feeds us with
CIS   hits that have 0 energy loss. The following is safe but not
CIS   necessarily correct. Should be investigated further.

               if(Nh.lt.1) Nh=1
C                  clus_per_cm_P10 = rel_dedx*clus_dis_P10
C                  ds_ave = 1./clus_per_cm_P10
C                  Neff_b = 2.7*wire_space/ds_ave
C     We need to find out how to calculate rel_dedx for any give geant track.
C     Until then use Neff_b = 24.84 which is a good approximation.
               Neff_b = 24.84
               Neff_h = Neff_b*((padL(ipad)/wire_space)**0.54)
               talpha2 = tan(alpha)**2
               calpha2 = cos(alpha)**2

               if( theta .lt. 0.0175 ) then
                  sigma_tr = sigma_response / sqrt(12.0)
               else
                  if(par(1).sys_test.eq.0)then
C     sigma_tr = sqrt( sigma02(ipad) +
C     >                    sigmad2(ipad) * dlen/calpha +
C     >                    sigmat2(ipad) * talpha2 * calpha )
                     
                     siga2(ipad)  = sigma02(ipad)/Nh**2
                     sigb2(ipad)  = sigmad2(ipad)/Nh
                     sigc2(ipad)  = ang_wireF(ipad)*wire_space2/
     >                    (12.*Neff_b)
                     sigd2(ipad)  = ang_padF(ipad)*
     >                    (padL2(ipad)-wire_space2)/
     >                    (12.*Neff_h)
C     Calculate pad resolution. Use Eq. 6.34 in Blum & Rolandi (p. 184)
C     General simulations.
                     sigma_tr = sqrt( siga2(ipad)       +
     >                    sigb2(ipad)*dlen/calpha2      +
     >                    (sigc2(ipad)+sigd2(ipad))*tan(alpha)**2 )
                     
                  elseif(par(1).sys_test.eq.1)then
                     siga2(ipad)  = sigma02(ipad+2)/Nh**2
                     sigb2(ipad)  = sigmad2(ipad+2)/Nh
                     sigc2(ipad)  = ang_wireF(ipad+2)*wire_space2/
     >                    (12.*Neff_b)
                     sigd2(ipad)  = ang_padF(ipad+2)*
     >                    (padL2(ipad)-wire_space2)/
     >                    (12.*Neff_h)
C     Calculate pad resolution. Use Eq. 6.34 in Blum & Rolandi (p. 184)
C     System test  simulations.
                     sigma_tr = sqrt( siga2(ipad)       +
     >                    sigb2(ipad)*dlen/calpha2      +
     >                    (sigc2(ipad)+sigd2(ipad))*
     >                    abs(tan(alpha))**1.2 )
                  elseif(par(1).sys_test.eq.2)then
C     Use a parametrization solely based on the  cosmic ray test data in the
C     limited crossing angle region  [-45, 45] degrees.  --- DSW  6/17/97 ---
                     sigma_tr = 0.04  
     >                    + 0.09*(abs(alpha)/(C_RAD_PER_DEG*45.))
     >                    + 0.04*(abs(alpha)/(C_RAD_PER_DEG*45.))**2
     >                    + 0.32*(abs(alpha)/(C_RAD_PER_DEG*45.))**8
                  endif
               endif
               
C     Calculate  the error in Z coordinate (sigma_l). Use Eq. 6.40 in
C     Blum & Roland (p. 188).   DSW  May 29, 1997.
C
CDH   I suspect that the variables defined here are different than what is
c     in Blum and Rolandi.  Using the substitution rules (B&R eqn. 6.10),
c     alpha is the DIP ANGLE (?).  DH January 14, 1998.
               if (par(1).sys_test.eq.0) then
                  sigb2(ipad)  = sigmad2(ipad)/Nh
               elseif(par(1).sys_test.eq.1)then
                  sigb2(ipad)  = sigmad2(ipad+2)/Nh
               endif
               
CDH               AA  = sigb2(ipad)*dlen/calpha2
CDH               BB  = wire_space2*tan(alpha)**2/(12.*Neff_b)
CDH               CC  = 0.64*wire_space2/(180.*Neff_b) 
               AA  = sigb2(ipad)*dlen/cos(lambda)**2
               BB  = wire_space2*tan(lambda)**2/(12.*Neff_b)
               CC  = 0.64*wire_space2/(180.*Neff_b)
               
               if(par(1).sys_test.lt.2)then
CDH                  if( theta .lt. 0.0175 ) then
CDH                     sigma_l      = sqrt( CC )
CDH                  else
CDH                     sigma_l      = sqrt( AA + BB + CC )
CDH                  endif
CDH   add temporary fudge factor of 0.07**2. DH January 15, 1998. 
                  sigma_l = sqrt( 0.0049 + AA + BB + CC )
               elseif(par(1).sys_test.eq.2)then
C     Use a parametrization solely based on the  cosmic ray test data in the
C     limited dip angle region  [-45, 45] degrees. --- DSW  6/17/97 ---
                  sigma_l = 0.04 
     >                 + 0.09*(abs(lambda)/(C_RAD_PER_DEG*45.))
     >                 + 0.04*(abs(lambda)/(C_RAD_PER_DEG*45.))**2
     >                 + 0.32*(abs(lambda)/(C_RAD_PER_DEG*45.))**8
                  sigma_l = 2.*sigma_l
                  
               endif

C     calculate the new smeared coords. and update tphit table
               
               isect = i
CCCC  if( i .gt. 12 )  isect = i - 12

C     Here we want use the new sector numbering scheme (STAR Note SN0229).
C     Proper conversion of isect ---> rotation angle is taken care of inside 
C     TFS_HIT_SMEAR.  -- 03 APR, 96  DSW --

               CALL TFS_HIT_SMEAR( isect, xi, yi, zi, xo, yo, zo,
     >              sigma_tr, sigma_l, sigma_x, sigma_y)
               tphit(k).x = xo
               tphit(k).y = yo
               tphit(k).z = zo
               tphit(k).dx = sigma_x
               tphit(k).dy = sigma_y
               tphit(k).dz = sigma_l
               
 100           continue
C     End loop over hits
            end do
C     End loop over rows
         end do
C     End loop over sectors
      end do
      
C     WRITE(6,*) ' TFS_GEN_PADRES: number hits discarded ', nh_discard
      RETURN
      end

