      SUBROUTINE TFS_ERROR(sigma_tr, sigma_l, alpha, diptan, eloss, sigma_response,z,irow)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
      real aplha, diptan,eloss, sigma_response, z
      integer irow
C
C     Output Arguments:
      real sigma_tr, sigma_l
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
C       Iwona Sakrejda
C
C     Creation Date:
C       24-Jun-2000
C
C-----------------------------------------------------------------------
#include "math_constants.inc"
#include "phys_constants.inc"

C-----------------------------------------------------------------------
  
C Following items should be initialised from a data base.
      
    
      REAL SIGMA0(2), SIGMA02(2)
      DATA SIGMA0/0.0135, 0.0303/ !intrinsic resolution term inner/outer
      REAL SPRF_DR0(3)
      DATA SPRF_DR0 /0.0575, 0.0346, 0.024 / !zero B-field prf drift term ;
      REAL SIGMAD2(2)
      REAL SIGMA_DL
      DATA SIGMA_DL/0.035/ ! Longitudinal difusion
      REAL PADL(2), PADL2(2)
      DATA PADL/1.2, 2.0/ !pad length inner/outer
      REAL ANG_WIREF(2)
      DATA ANG_WIREF /1.0,1.0/
      REAL ANG_PADF(2)
      DATA ANG_PADF /1.0,1.0/
      REAL ION_POT
      DATA ION_POT /28.5e-9 / ! P10 ionization potential  in GeV
      REAL WIRE_SPACE, WIRE_SPACE2
      DATA WIRE_SPACE /0.4/
      
C     General physical constants now in math_constants and phys_constants
C  
      
      integer ipad, iret
      real theta, diptan, alpha, lambda
      real dlen, talpha2, calpha2
      real Nh,Neff_b,Neff_h,siga2(2),sigb2(2),sigc2(2),sigd2(2)
      real AA, BB, CC
      real xlocal(3),Bfield(3)
      real xlength,stemp
      integer tpc_drift_volume_length
      DATA xlocal /3*0/
      Real atan

C=========================== Begin Executable Code =======================
      iret = tpc_drift_volume_length(xlength)
      dlen = xlength - z
      theta = C_PI_2 - abs(alpha) ! angle wrt pad row
C     calculate the dip angle
      lambda          = atan(diptan)
      call gufld(xlocal,Bfield)
      ipad = 1
      if(irow.gt.13) ipad=2
      wire_space2=wire_space**2
      padl2(1)=padl(1)**2
      padl2(2)=padl(2)**2
      sigma02(1)=sigma0(1)**2
      sigma02(2)=sigma0(2)**2
      if(abs(bfield(3)).lt.0.0002) then
         stemp=SPRF_DR0(1)
      else if(abs(bfield(3)).lt.2.6) then
         stemp=SPRF_DR0(2)
      else
         stemp=SPRF_DR0(3)
      endif
      sigmad2(1)=stemp**2
      sigmad2(2)=stemp**2
     

C     Calculate spatial resolution along the padrow
C     Spatial resolution calculation is updated to accomadate the fitting of
C     pad resolution function (in p184 of Blum & Rolandi) to Roy B.'s 
C     calculated widths.

      Nh     = eloss/Ion_Pot
CIS   The following line was added, because gstar sometimes feeds us with
CIS   hits that have 0 energy loss. The following is safe but not
CIS   necessarily correct. Should be investigated further.
      if(Nh.lt.1) Nh=1
      Neff_b = 24.84
      Neff_h = Neff_b*((padL(ipad)/wire_space)**0.54)
      talpha2 = tan(alpha)**2
      calpha2 = cos(alpha)**2
      if( theta .lt. 0.0175 ) then
           sigma_tr = sigma_response / sqrt(12.0)
      else
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
     >                sigb2(ipad)*dlen/calpha2      +
     >               (sigc2(ipad)+sigd2(ipad))*tan(alpha)**2 )
      endif
               
C     Calculate  the error in Z coordinate (sigma_l). Use Eq. 6.40 in
C     Blum & Roland (p. 188).   DSW  May 29, 1997.
C
CDH   I suspect that the variables defined here are different than what is
c     in Blum and Rolandi.  Using the substitution rules (B&R eqn. 6.10),
c     alpha is the DIP ANGLE (?).  DH January 14, 1998.
      sigb2(ipad)  = sigmad2(ipad)/Nh
      AA  = sigb2(ipad)*dlen/cos(lambda)**2
      BB  = wire_space2*tan(lambda)**2/(12.*Neff_b)
      CC  = 0.64*wire_space2/(180.*Neff_b)
      sigma_l = sqrt( 0.0049 + AA + BB + CC )
      RETURN
      end

