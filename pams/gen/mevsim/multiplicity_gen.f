      Subroutine track_gen(gpid,N,model_type,T,sigma,expvel,
     1     reac_plane_cntrl,PSIr,Vn,
     2     pt_cut_min,pt_cut_max,eta_cut_min,eta_cut_max,
     3     y_cut_min,y_cut_max,
     4     phi_cut_min,phi_cut_max,n_scan_pts,rad,pid,status,
     5     n_integ_pts)
      implicit none
CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
      Common/track/ pout(npid,4,factorial_max)
      real*4 pout
      Common/Geant/geant_mass(200),geant_charge(200),
     1             geant_lifetime(200),geant_width(200)
      real*4 geant_mass,geant_charge,geant_lifetime,geant_width

      real*4 T,sigma,expvel,pt_cut_min,pt_cut_max,eta_cut_min
      real*4 eta_cut_max,phi_cut_min,phi_cut_max,mass
      real*4 dpt,deta,facmax,pt,eta,y,rapidity,dNdpty,dNdp
      real*4 pt_trial,eta_trial,y_trial,ranf,rad,phi
      real*4 y_cut_min,y_cut_max,pseudorapidity
      real*4 PSIr,Vn(nflowterms,4),facmax_phi,dNdphi
      real*4 Vnptmax,Vnymax,Vn_pt_y,Vntmp(nflowterms)
      real*4 masstmp,Mass_function

      integer gpid,N,model_type,npt,neta,n_scan_pts,ipt,ieta
      integer Track_count,i,j,pid,status
      integer reac_plane_cntrl
      integer n_integ_pts

      do i = 1,factorial_max
      do j = 1,4
         pout(pid,j,i) = 0.0
      end do
      end do

      mass = geant_mass(gpid)
      npt  = n_scan_pts
      neta = n_scan_pts

CCC  Determine maximum value of model distribution in (pt,eta) range:
    
      dpt = (pt_cut_max - pt_cut_min)/float(npt - 1)
      deta = (eta_cut_max - eta_cut_min)/float(neta - 1)
      facmax = 0.0
      do ipt = 1,npt
         pt = pt_cut_min + dpt*float(ipt - 1)
         do ieta = 1,neta
            eta = eta_cut_min + deta*float(ieta - 1)
            y   = rapidity(mass,pt,eta)
            dNdp = dNdpty(1.0,pt,eta,y,mass,T,sigma,expvel,
     1                    model_type,1,pid)
            if(dNdp .gt. facmax) facmax = dNdp
         end do
      end do

CCC   If dNdp always underflows exp() range, then facmax will stay 
CCC   equal to 0.0, thus causing a divide by 0.0 error below. 
CCC   Check for this and Return if this is the case.  This event will
CCC   be aborted in this instance.

      if(facmax .eq. 0.0) then
         status = -86
         Return
      else
         status = 1
      end if

CCC   Determine the maximum values of the azimuthal model distribution
CCC   in phi, for case with reaction plane dependence and anisotropic flow
CCC   Find the absolute maximum possible value given the pt and y dependences
CCC   and assuming all Fourier components add with maximum coherence.

      if(reac_plane_cntrl .gt. 1) then
         facmax_phi = 1.0
         do i = 1,nflowterms
            if(i.eq.(2*(i/2))) then ! Select even Fourier components:
               Vnptmax = max(
     1                 abs(Vn(i,1)+Vn(i,2)*pt_cut_min*pt_cut_min),
     2                 abs(Vn(i,1)+Vn(i,2)*pt_cut_max*pt_cut_max))
               Vnymax  = max(
     1                 exp(-Vn(i,3)*y_cut_min*y_cut_min),
     2                 exp(-Vn(i,3)*y_cut_max*y_cut_max))
               if(y_cut_min.lt.0.0 .and. y_cut_max.gt.0.0) then
                  Vnymax  = max(Vnymax,1.0)
               end if
            else ! Select odd Fourier components
               Vnptmax = max(
     1                 abs(Vn(i,1)+Vn(i,2)*pt_cut_min),
     2                 abs(Vn(i,1)+Vn(i,2)*pt_cut_max))
               Vnymax  = max(
     1                 abs(Vn(i,3)+Vn(i,4)*abs(y_cut_min**3)),
     2                 abs(Vn(i,3)+Vn(i,4)*abs(y_cut_max**3)))
               if(y_cut_min.lt.0.0 .and. y_cut_max.gt.0.0) then
                  Vnymax  = max(Vnymax,abs(Vn(i,3)))
               end if
            end if
            facmax_phi = facmax_phi + 2.0*Vnptmax*Vnymax
         end do
CCC   Check facmax_phi; if 0, then event will be aborted. 
         if(facmax_phi.eq.0.0) then
            status = -86
            Return
         else
            status = 1
         end if
      end if

CCC Start Random Track Selection:

      Track_count = 0

100   pt_trial = ranf()*(pt_cut_max - pt_cut_min)+pt_cut_min
      if(model_type.ge.1 .and. model_type.le.4) then
         y_trial = ranf()*(y_cut_max - y_cut_min)+y_cut_min
         eta_trial = pseudorapidity(mass,pt_trial,y_trial)
         if(eta_trial.lt.eta_cut_min .or. eta_trial.gt.eta_cut_max)
     1      go to 100
      else if (model_type.eq.5 .or. model_type.eq.6) then
         eta_trial=ranf()*(eta_cut_max-eta_cut_min)+eta_cut_min
         y_trial = rapidity(mass,pt_trial,eta_trial)
      end if
      dNdp = dNdpty(1.0,pt_trial,eta_trial,y_trial,mass,T,sigma,
     1       expvel,model_type,1,pid)/facmax
      if(ranf() .le. dNdp) then
         Track_count = Track_count + 1

CCC   Determine phi angle:
         if(reac_plane_cntrl .eq. 1) then
            phi = (ranf()*(phi_cut_max - phi_cut_min) +
     1            phi_cut_min)/rad
         else if(reac_plane_cntrl .gt. 1) then
            do i = 1,nflowterms
               Vntmp(i) = Vn_pt_y(i,Vn(i,1),Vn(i,2),Vn(i,3),Vn(i,4),
     1                    pt_trial,y_trial)
            end do
101         phi = ranf()*(phi_cut_max - phi_cut_min)+phi_cut_min
            dNdphi = 1.0
            do i = 1,nflowterms
               dNdphi = dNdphi+2.0*Vntmp(i)*cos(float(i)*(phi-PSIr)
     1      /rad)
            end do
            if(ranf().gt.(dNdphi/facmax_phi)) then
               go to 101
            else
               phi = phi/rad
            end if
         end if

         masstmp = Mass_function(gpid,pid,n_integ_pts)
         Call kinematics(masstmp,pt_trial,y_trial,phi,Track_count,pid)
         if(Track_count .lt. N) then
            go to 100
         else if(Track_count .ge. N) then
            Return
         end if

      else
         go to 100
      end if

      END

      Real*4 Function rapidity(m,pt,eta)
      implicit none
      real*4 m,pt,eta,theta,pz,E,pi,expR

      pi = 3.141592654
      theta = 2.0*ATAN(expR(-eta))
      if(abs(theta - pi/2.0) .lt. 0.000001) then
         pz = 0.0
      else
         pz = pt/tan(theta)
      end if
      E = sqrt(pt*pt + pz*pz + m*m)
      rapidity = 0.5*log((E+pz)/(E-pz))
      Return
      END

      Real*4 Function pseudorapidity(m,pt,y)
      implicit none

CCC   This Function computes the pseudorapidty for a given mass, pt, y:

      real*4 m,pt,y,mt,coshy,E,pzmag,pz,pmag,expR

      if(y.eq.0.0) then
         pseudorapidity = 0.0
         Return
      end if
      mt = sqrt(m*m + pt*pt)
      coshy = 0.5*(expR(y) + expR(-y))
      E = mt*coshy
      pzmag = sqrt(abs(E*E - mt*mt))
      if(y.eq.0.0) then
         pz = 0.0
      else
         pz = (y/abs(y))*pzmag
      end if
      pmag = sqrt(pt*pt + pz*pz)
      if(pt.ne.0.0) then
         pseudorapidity = 0.5*log((pmag+pz)/(pmag-pz))
      else if (pt.eq.0.0) then
         pseudorapidity = 86.0
         write(8,10)
10       Format(10x,'Function pseudorapidity called with pt=0')
      end if
      Return
      END

      Subroutine kinematics(m,pt,y,phi,index,pid)
      implicit none

CCC  This SUBR takes the input particle mass (m), pt, y and phi and
CCC  computes E, px, py, pz and loads the momenta into the index-th
CCC  row of array pout(,,) in Common/track/ .

      integer index,pid
CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
      Common/track/ pout(npid,4,factorial_max)
      real*4 pout

      real*4 m,pt,y,phi,mt,coshy,E,pzmag,px,py,pz,expR

      mt = sqrt(m*m + pt*pt)
      coshy = 0.5*(expR(y) + expR(-y))
      E = mt*coshy
      pzmag = sqrt(abs(E*E - mt*mt))
      if(y.eq.0.0) then
         pz = 0.0
      else
         pz = (y/abs(y))*pzmag
      end if
      px = pt*cos(phi)
      py = pt*sin(phi)

      pout(pid,1,index) = px
      pout(pid,2,index) = py
      pout(pid,3,index) = pz
      pout(pid,4,index) = m

      Return
      END

      Real*4 Function dNdpty(A,pt,eta,y,m,T,sigma,vel,control,ktl,pid)
      implicit none

      real*4 A,pt,eta,y,m,T,sigma,vel
      real*4 pi,mt,coshy,E,ptot,FAC,gamma,yp,sinhyp,coshyp
      real*4 FAC2,FAC3,expR
      integer control, ktl,pid,pt_index,eta_index,index_locate
CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
CCC   Calculates dN/dp^3 using several models:
CCC
CCC      control = 1,  Humanic factorized model
CCC              = 2,  Pratt non-expanding spherical thermal source
CCC              = 3,  Bertsch non-expanding spherical thermal source
CCC              = 4,  Pratt spherical expanding thermally equilibrated
CCC                    source.
CCC              = 5,  Factorized pt, eta bin-by-bin distribution.
CCC              = 6,  Full 2D pt, eta bin-by-bin distribution.
CCC
CCC      ktl     = 0,  to return value of dN/dp^3
CCC      ktl     = 1,  to return value of dN/dpt*dy

      pi = 3.141592654
      mt = sqrt(pt*pt + m*m)
      coshy = 0.5*(expR(y) + expR(-y))
      E = mt*coshy
      ptot = sqrt(E*E - m*m)
      if(ktl .eq. 0) then
         FAC = 2.0*pi*pt*E
      else if(ktl .eq. 1) then
         FAC = 1.0
      end if

      if(control .eq. 1) then
         dNdpty = A*pt*expR(-mt/T)*expR(-y*y/(2.0*sigma*sigma))
         dNdpty = dNdpty/FAC

      else if(control .eq. 2) then
         dNdpty = A*pt*E*expR(-E/T)
         dNdpty = dNdpty/FAC

      else if(control .eq. 3) then
         dNdpty = A*pt*E/(expR(E/T) - 1.0)
         dNdpty = dNdpty/FAC

      else if(control .eq. 4) then
         gamma = 1.0/sqrt(1.0 - vel*vel)
         yp = gamma*vel*ptot/T
         sinhyp = 0.5*(expR(yp) - expR(-yp))
         coshyp = 0.5*(expR(yp) + expR(-yp))
         if(yp .ne. 0.0) then
            FAC2 = sinhyp/yp
         else
            FAC2 = 1.0
         end if
         FAC3 = FAC2+ (T/(gamma*E))*(FAC2 - coshyp)
         dNdpty = A*pt*E*expR(-gamma*E/T)*FAC3
         dNdpty = dNdpty/FAC

      else if(control .eq. 5) then
         pt_index = index_locate(pid,pt,1)
         eta_index = index_locate(pid,eta,2)
         dNdpty = A*pt_bin(pid,pt_index)*eta_bin(pid,eta_index)
         dNdpty = dNdpty/FAC

      else if(control .eq. 6) then
         pt_index = index_locate(pid,pt,1)
         eta_index = index_locate(pid,eta,2)
         dNdpty = A*pt_eta_bin(pid,pt_index,eta_index)
         dNdpty = dNdpty/FAC

      else
         dNdpty = -86.0

      end if

      return
      END

      Integer Function index_locate(pid,arg,kind)
      implicit none

CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
      integer pid,kind,ibin
      real*4 arg

CCC   This Function locates the pt or eta bin number corresponding to the
CCC   input bin mesh, the requested value of pt or eta, for the current
CCC   value of particle type.
CCC
CCC   If kind = 1, then pt bin number is located.
CCC   If kind = 2, then eta bin number is located.

      if(kind .eq. 1) then
         do ibin = 1,n_pt_bins(pid)
            if(arg.le.pt_bin_mesh(pid,ibin)) then
            index_locate = ibin
            Return
            end if
         end do
         index_locate = n_pt_bins(pid)
         write(8,10) pid,arg
10       Format(//10x,'In Function index_locate, for pid = ',I5,
     1   'pt  =',E15.6,' is out of range - use last bin #')
         Return

      else if(kind .eq. 2) then

         do ibin = 1,n_eta_bins(pid)
            if(arg.le.eta_bin_mesh(pid,ibin)) then
            index_locate = ibin
            Return
            end if
         end do
         index_locate = n_eta_bins(pid)
         write(8,11) pid,arg
11       Format(//10x,'In Function index_locate, for pid = ',I5,
     1   'eta =',E15.6,' is out of range - use last bin #')
         Return

      end if
      END

      Real*4 Function expR(x)
      implicit none
      real*4 x
      if(x .gt. 69.0) then
         write(8,10) x
         STOP
      else if(x .lt. -69.0) then
         expR = 0.0
      else
         expR = exp(x)
      end if
10    Format(///10x,'Func. expR(x) called with x = ',E15.6,
     1    ', gt 69.0 - STOP')
      Return
      END

      SUBROUTINE LAGRNG (X,ARG,Y,VAL,NDIM,NFS,NPTS,MAXARG,MAXFS)
	IMPLICIT REAL*4(A-H,O-Z)
C
C     LAGRANGE INTERPOLATION,UNEQUALLY SPACED POINTS
C     ROUTINE OBTAINED FROM R. LANDAU, UNIV. OF OREGON.
C     ARG=VECTOR OF INDEPENDENT VARIABLE CONTAINING MAXARG VALUES.
C     VAL=MATRIX OF FUNCTION VALUES CORRESPONDING TO ARG. (MAXFS
C         FUNCTIONS AT MAXARG VALUES.)
C     X  =VALUE OF INDEP. VARIABLE FOR WHICH INTERPOLATION IS DESIRED.
C     Y  =VECTOR OF MAXFS FUNCTION VALUES RESULTING FROM SIMUL. INTERP.
C     NDIM=NUMBER OF ARG VALUES TO BE USED. (NDIM.LE.MAXARG)
C     NFS=NUMBER OF FUNCTIONS SIMUL. INTERP (NFS.LE.MAXFS)
C     NPTS=NUMBER OF POINTS USED IN INTERPOLATION. (NPTS=2,3,4,5,6)
C
      DIMENSION ARG(MAXARG), VAL(MAXFS,MAXARG), Y(MAXFS)
C
C     -----FIND X0, THE CLOSEST POINT TO X.
C
      NI=1
      NF=NDIM
   10 IF ((X.LE.ARG(NI)).OR.(X.GE.ARG(NF))) GO TO 30
      IF ((NF-NI+1).EQ.2) GO TO 70
      NMID=(NF+NI)/2
      IF (X.GT.ARG(NMID)) GO TO 20
      NF=NMID
      GO TO 10
   20 NI=NMID
      GO TO 10
C
C     ------ X IS ONE OF THE TABLULATED VALUES.
C
   30 IF (X.LE.ARG(NI)) GO TO 60
      NN=NF
   40 NUSED=0
      DO 50 N=1,NFS
   50 Y(N)=VAL(N,NN)
      RETURN
   60 NN=NI
      GO TO 40
C
C     ------- 2 PTS LEFT, CHOOSE SMALLER ONE.
C
   70 N0=NI
      NN=NPTS-2
      GO TO (110,100,90,80), NN
   80 CONTINUE
      IF (((N0+3).GT.NDIM).OR.((N0-2).LT.1)) GO TO 90
      NUSED=6
      GO TO 130
   90 CONTINUE
      IF ((N0+2).GT.NDIM) GO TO 110
      IF ((N0-2).LT.1) GO TO 100
      NUSED=5
      GO TO 130
  100 CONTINUE
      IF (((N0+2).GT.NDIM).OR.((N0-1).LT.1)) GO TO 110
      NUSED=4
      GO TO 130
  110 IF ((N0+1).LT.NDIM) GO TO 120
C
C     ------N0=NDIM, SPECIAL CASE.
C
      NN=NDIM
      GO TO 40
  120 NUSED=3
      IF ((N0-1).LT.1) NUSED=2
  130 CONTINUE
C
C     ------AT LEAST 2 PTS LEFT.
C
      Y0=X-ARG(N0)
      Y1=X-ARG(N0+1)
      Y01=Y1-Y0
      C0=Y1/Y01
      C1=-Y0/Y01
      IF (NUSED.EQ.2) GO TO 140
C
C     ------AT LEAST 3 PTS.
C
      YM1=X-ARG(N0-1)
      Y0M1=YM1-Y0
      YM11=Y1-YM1
      CM1=-Y0*Y1/Y0M1/YM11
      C0=C0*YM1/Y0M1
      C1=-C1*YM1/YM11
      IF (NUSED.EQ.3) GO TO 160
C
C     ------AT LEAST 4 PTS
C
      Y2=X-ARG(N0+2)
      YM12=Y2-YM1
      Y02=Y2-Y0
      Y12=Y2-Y1
      CM1=CM1*Y2/YM12
      C0=C0*Y2/Y02
      C1=C1*Y2/Y12
      C2=-YM1*Y0*Y1/YM12/Y02/Y12
      IF (NUSED.EQ.4) GO TO 180
C
C     ------AT LEAST 5 PTS.
C
      YM2=X-ARG(N0-2)
      YM2M1=YM1-YM2
      YM20=Y0-YM2
      YM21=Y1-YM2
      YM22=Y2-YM2
      CM2=YM1*Y0*Y1*Y2/YM2M1/YM20/YM21/YM22
      CM1=-CM1*YM2/YM2M1
      C0=-C0*YM2/YM20
      C1=-C1*YM2/YM21
      C2=-C2*YM2/YM22
      IF (NUSED.EQ.5) GO TO 200
C
C     ------AT LEAST 6 PTS.
C
      Y3=X-ARG(N0+3)
      YM23=Y3-YM2
      YM13=Y3-YM1
      Y03=Y3-Y0
      Y13=Y3-Y1
      Y23=Y3-Y2
      CM2=CM2*Y3/YM23
      CM1=CM1*Y3/YM13
      C0=C0*Y3/Y03
      C1=C1*Y3/Y13
      C2=C2*Y3/Y23
      C3=YM2*YM1*Y0*Y1*Y2/YM23/YM13/Y03/Y13/Y23
      GO TO 220
  140 CONTINUE
      DO 150 N=1,NFS
  150 Y(N)=C0*VAL(N,N0)+C1*VAL(N,N0+1)
      GO TO 240
  160 CONTINUE
      DO 170 N=1,NFS
  170 Y(N)=CM1*VAL(N,N0-1)+C0*VAL(N,N0)+C1*VAL(N,N0+1)
      GO TO 240
  180 CONTINUE
      DO 190 N=1,NFS
  190 Y(N)=CM1*VAL(N,N0-1)+C0*VAL(N,N0)+C1*VAL(N,N0+1)+C2*VAL(N,N0+2)
      GO TO 240
  200 CONTINUE
      DO 210 N=1,NFS
  210 Y(N)=CM2*VAL(N,N0-2)+CM1*VAL(N,N0-1)+C0*VAL(N,N0)+C1*VAL(N,N0+1)+C
     12*VAL(N,N0+2)
      GO TO 240
  220 CONTINUE
      DO 230 N=1,NFS
  230 Y(N)=CM2*VAL(N,N0-2)+CM1*VAL(N,N0-1)+C0*VAL(N,N0)+C1*VAL(N,N0+1)+C
     12*VAL(N,N0+2)+C3*VAL(N,N0+3)
  240 RETURN
C
      END

      Subroutine FACTORIAL
      implicit none

      integer n
CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
      real*4 FN

CCC   Computes the natural log of n! for n = 0,1,2...(factorial_max -1)
CCC   and puts the result in array FACLOG().
C
CCC   FACLOG(1) = log(0!) = 0.0
CCC   FACLOG(2) = log(1!) = 0.0
CCC   FACLOG(n+1) = log(n!)

      FACLOG(1) = 0.0
      FACLOG(2) = 0.0
      FN = 1.0
      do n = 3,factorial_max
      FN = FN + 1.0
      FACLOG(n) = FACLOG(n-1) + log(FN)
      end do
      Return
      END

      Subroutine MinMax(mean,stdev,n_stdev,min,max)
      implicit none

CCC   Computes range of integration for random number selections

      real*4 mean,stdev,n_stdev,min,max

      min = mean - n_stdev*stdev
      if(min .lt. 0.0) min = 0.0
      max = mean + n_stdev*stdev
      Return
      END

      Subroutine Poisson(min,max,mean,nsteps,integ,xfunc,ndim)
      implicit none

CCC   Computes Poisson distribution from n = min to max;
CCC   Integrates this distribution and records result at each step in
CCC      array integ();
CCC   Records the coordinates in array xfunc().

      integer min,max,mean,nsteps,ndim,i,n
CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
      real*4 mean_real,mean_real_log,expR
      real*4 integ(ndim)
      real*4 xfunc(ndim)
      real*4 Poisson_dist(n_mult_max_steps)
CCC Initialize arrays to zero:

      do i = 1,ndim
         integ(i) = 0.0
         xfunc(i) = 0.0
         Poisson_dist(i) = 0.0
      end do

      mean_real = float(mean)
      mean_real_log = log(mean_real) 

CCC   Compute Poisson distribution from n = min to max:
      do i = 1,nsteps
      n = (i - 1) + min
      Poisson_dist(i) = expR(-mean_real + float(n)*mean_real_log
     1      - FACLOG(n+1))
      end do

CCC   Integrate the Poisson distribution:
      integ(1) = 0.0
      do i = 2,nsteps
      integ(i) = 0.5*(Poisson_dist(i-1) + Poisson_dist(i)) + integ(i-1)
      end do

CCC   Normalize the integral to unity:
      do i = 1,nsteps
      integ(i) = integ(i)/integ(nsteps)
      end do

CCC   Fill xfunc array:
      do i = 1,nsteps
      xfunc(i) = float(i - 1 + min)
      end do

CCC  Extend integ() and xfunc() by one additional mesh point past the
CCC  end point in order to avoid a bug in the Lagrange interpolation
CCC  subroutine that gives erroneous interpolation results within the
CCC  the last mesh bin.

      integ(nsteps + 1) = integ(nsteps) + 0.01
      xfunc(nsteps + 1) = xfunc(nsteps)

      Return
      END

      Subroutine Gaussian(min,max,mean,stdev,npts,integ,xfunc,ndim)
      implicit none

CCC   Compute Gaussian distribution from x = min to max at npts;
CCC   Integrate this distribution and record result at each mesh in
CCC      array integ();
CCC   Record the coordinates in array xfunc().

CCCCC#include "multi_gen.inc"
      integer npid,nmax_integ,n_mult_max_steps,nflowterms
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      parameter (nflowterms = 6)    ! max # of terms in the anisotropic
CCC                                 ! flow model for azimuthal (phi angle)
CCC                                 ! dependence.
CCC   Common FACFAC for Factorials

      integer factorial_max
      parameter (factorial_max = 10000) ! max # multiplicity per event;
CCC                                     ! for any specific particle ID;
CCC                                     ! also used for log(n!).
      Common/FACFAC/ FACLOG(factorial_max)
      real*4 FACLOG
CCC  Common for bin-by-bin distribution input:
CCC  NOTE:  Include file 'Parameter_values.inc' must accompany and 
CCC         precede this file everywhere it occurs.

      integer n_bins_max
      parameter(n_bins_max = 50) ! maximum # of input pt, eta bins

      Common/dist_bin/ pt_start(npid),eta_start(npid),pt_stop(npid),
     1    eta_stop(npid),delta_pt(npid,n_bins_max),
     2    delta_eta(npid,n_bins_max),pt_bin(npid,n_bins_max),
     3    eta_bin(npid,n_bins_max),
     4    pt_eta_bin(npid,n_bins_max,n_bins_max),
     5    pt_bin_mesh(npid,n_bins_max),eta_bin_mesh(npid,n_bins_max),
     6    n_pt_bins(npid),n_eta_bins(npid) 

      integer n_pt_bins,n_eta_bins
      real*4 pt_start,eta_start,pt_stop,eta_stop
      real*4 delta_pt,delta_eta,pt_bin,eta_bin,pt_eta_bin
      real*4 pt_bin_mesh,eta_bin_mesh
      integer npts,ndim,i
      real*4 min,max,mean,stdev,integ(ndim),xfunc(ndim)
      real*4 dm,x,Gauss_dist(nmax_integ),FAC1,FAC2,pi,expR

CCC   Initialize arrays to zero:
      do i = 1,ndim
         integ(i) = 0.0
         xfunc(i) = 0.0
         Gauss_dist(i) = 0.0
      end do

      pi = 3.141592654
      FAC1 = 1.0/(sqrt(2.0*pi)*stdev)
      FAC2 = 2.0*stdev*stdev
      dm = (max - min)/float(npts - 1)

CCC   Compute normalized Gaussian distribution:
      do i = 1,npts
      x = min + dm*float(i-1)
      xfunc(i) = x
      Gauss_dist(i) = FAC1*expR(-((x - mean)**2)/FAC2)
      end do

CCC   Integrate Gaussian distribution over specified range
      integ(1) = 0.0
      do i = 2,npts
      integ(i) = 0.5*(Gauss_dist(i-1) + Gauss_dist(i))*dm + integ(i-1)
      end do

CCC   Normalize integral to unity:
      do i = 1,npts
      integ(i) = integ(i)/integ(npts)
      end do

CCC  Extend integ() and xfunc() by one mesh point to avoid Lagrange
CCC  interpolation subroutine bug:
      integ(npts + 1) = integ(npts) + 0.01
      xfunc(npts + 1) = xfunc(npts)

      Return
      END

      Subroutine Particle_prop
      implicit none

      Common/Geant/geant_mass(200),geant_charge(200),
     1             geant_lifetime(200),geant_width(200)

CCC   Local Variable Type Declarations:
      integer i
      real*4 geant_mass,geant_charge,geant_lifetime,geant_width
      real*4 hbar
      Parameter(hbar = 6.5821220E-25) ! hbar in GeV*seconds

CCC   Fill arrays geant_mass(),geant_charge(),geant_lifetime(),
CCC   geant_width() with particle mass in GeV, charge in units of
CCC   |e|, mean lifetime in seconds, and width in GeV, where
CCC   width*lifetime = hbar.  For lifetimes listed with values of
CCC   1.0E+30 (i.e. stable particles) the width is set to 0.000.
CCC   Row # = Geant Particle ID # code (see Geant Manual 3.10,
CCC   User's Guide, CONS 300-1 and 300-2).  Note, rows 151 and higher
CCC   are used for resonances.  These assume the masses and widths
CCC   specific to the models used to represent the invariant mass
CCC   distributions and therefore may differ slightly from the Particle
CCC   Data Group's listing.

CCC   Initialize arrays to zero:
      do i = 1,200
         geant_mass(i) = 0.0
         geant_charge(i) = 0.0
         geant_lifetime(i) = 0.0
         geant_width(i) = 0.0
      end do

CCC   Set Particle Masses in GeV:
      geant_mass(1)  = 0.0           ! Gamma
      geant_mass(2)  = 0.000511      ! Positron
      geant_mass(3)  = 0.000511      ! Electron
      geant_mass(4)  = 0.0           ! Neutrino
      geant_mass(5)  = 0.105659      ! Muon +
      geant_mass(6)  = 0.105659      ! Muon -
      geant_mass(7)  = 0.134693      ! Pion 0
      geant_mass(8)  = 0.139567      ! Pion +
      geant_mass(9)  = 0.139567      ! Pion -
      geant_mass(10) = 0.49767       ! Kaon 0 Long
      geant_mass(11) = 0.493667      ! Kaon +
      geant_mass(12) = 0.493667      ! Kaon -
      geant_mass(13) = 0.939573      ! Neutron
      geant_mass(14) = 0.93828       ! Proton
      geant_mass(15) = 0.93828       ! Antiproton
      geant_mass(16) = 0.49767       ! Kaon 0 Short
      geant_mass(17) = 0.5488        ! Eta
      geant_mass(18) = 1.11560       ! Lambda
      geant_mass(19) = 1.18936       ! Sigma +
      geant_mass(20) = 1.19246       ! Sigma 0
      geant_mass(21) = 1.19734       ! Sigma -
      geant_mass(22) = 1.31490       ! Xi 0
      geant_mass(23) = 1.32132       ! Xi -
      geant_mass(24) = 1.67245       ! Omega
      geant_mass(25) = 0.939573      ! Antineutron
      geant_mass(26) = 1.11560       ! Antilambda
      geant_mass(27) = 1.18936       ! Antisigma -
      geant_mass(28) = 1.19246       ! Antisigma 0
      geant_mass(29) = 1.19734       ! Antisigma +
      geant_mass(30) = 1.3149        ! Antixi 0
      geant_mass(31) = 1.32132       ! Antixi +
      geant_mass(32) = 1.67245       ! Antiomega +
      geant_mass(33) = 1.7842        ! Tau +
      geant_mass(34) = 1.7842        ! Tau -
      geant_mass(35) = 1.8694        ! D+
      geant_mass(36) = 1.8694        ! D-
      geant_mass(37) = 1.8647        ! D0
      geant_mass(38) = 1.8647        ! Anti D0
      geant_mass(39) = 1.9710        ! F+, now called Ds+
      geant_mass(40) = 1.9710        ! F-, now called Ds-
      geant_mass(41) = 2.2822        ! Lambda C+
      geant_mass(42) = 80.8000       ! W+
      geant_mass(43) = 80.8000       ! W-
      geant_mass(44) = 92.9000       ! Z0
      geant_mass(45) = 1.877         ! Deuteron
      geant_mass(46) = 2.817         ! Tritium
      geant_mass(47) = 3.755         ! Alpha
      geant_mass(48) = 0.0           ! Geantino
      geant_mass(49) = 2.80923       ! He3
      geant_mass(50) = 0.0           ! Cherenkov
      geant_mass(151) = 0.783        ! rho +
      geant_mass(152) = 0.783        ! rho -
      geant_mass(153) = 0.783        ! rho 0
      geant_mass(154) = 0.782        ! omega 0
      geant_mass(155) = 0.95750      ! eta'
      geant_mass(156) = 1.0194       ! phi
      geant_mass(157) = 3.09693      ! J/Psi
      geant_mass(158) = 1.232        ! Delta -
      geant_mass(159) = 1.232        ! Delta 0
      geant_mass(160) = 1.232        ! Delta +
      geant_mass(161) = 1.232        ! Delta ++
      geant_mass(162) = 0.89183      ! K* +
      geant_mass(163) = 0.89183      ! K* -
      geant_mass(164) = 0.89610      ! K* 0
      geant_mass(165) = 0.500        ! sigma 0

CCC   Set Particle Charge in |e|:
      geant_charge( 1) =  0      ! Gamma
      geant_charge( 2) =  1      ! Positron
      geant_charge( 3) = -1      ! Electron
      geant_charge( 4) =  0      ! Neutrino
      geant_charge( 5) =  1      ! Muon+
      geant_charge( 6) = -1      ! Muon-
      geant_charge( 7) =  0      ! Pion0
      geant_charge( 8) =  1      ! Pion+
      geant_charge( 9) = -1      ! Pion-
      geant_charge(10) =  0      ! Kaon 0 long
      geant_charge(11) =  1      ! Kaon+
      geant_charge(12) = -1      ! Kaon-
      geant_charge(13) =  0      ! Neutron
      geant_charge(14) =  1      ! Proton
      geant_charge(15) = -1      ! Antiproton
      geant_charge(16) =  0      ! Kaon 0 short
      geant_charge(17) =  0      ! Eta
      geant_charge(18) =  0      ! Lambda
      geant_charge(19) =  1      ! Sigma+
      geant_charge(20) =  0      ! Sigma0
      geant_charge(21) = -1      ! Sigma-
      geant_charge(22) =  0      ! Xi 0
      geant_charge(23) = -1      ! Xi -
      geant_charge(24) = -1      ! Omega
      geant_charge(25) =  0      ! Antineutron
      geant_charge(26) =  0      ! Antilambda
      geant_charge(27) = -1      ! Anti-Sigma -
      geant_charge(28) =  0      ! Anti-Sigma 0
      geant_charge(29) =  1      ! Anti-Sigma +
      geant_charge(30) =  0      ! AntiXi 0
      geant_charge(31) =  1      ! AntiXi +
      geant_charge(32) =  1      ! Anti-Omega +
      geant_charge(33) =  1      ! Tau +
      geant_charge(34) = -1      ! Tau -
      geant_charge(35) =  1      ! D+
      geant_charge(36) = -1      ! D-
      geant_charge(37) =  0      ! D0
      geant_charge(38) =  0      ! Anti D0
      geant_charge(39) =  1      ! F+, now called Ds+
      geant_charge(40) = -1      ! F-, now called Ds-
      geant_charge(41) =  1      ! Lambda C+
      geant_charge(42) =  1      ! W+
      geant_charge(43) = -1      ! W-
      geant_charge(44) =  0      ! Z0
      geant_charge(45) =  1      ! Deuteron
      geant_charge(46) =  1      ! Triton
      geant_charge(47) =  2      ! Alpha
      geant_charge(48) =  0      ! Geantino (Fake particle)
      geant_charge(49) =  2      ! He3
      geant_charge(50) =  0      ! Cerenkov (Fake particle)
      geant_charge(151) =  1     ! rho+
      geant_charge(152) = -1     ! rho-
      geant_charge(153) =  0     ! rho 0
      geant_charge(154) =  0     ! omega 0
      geant_charge(155) =  0     ! eta'
      geant_charge(156) =  0     ! phi
      geant_charge(157) =  0     ! J/Psi
      geant_charge(158) = -1     ! Delta -
      geant_charge(159) =  0     ! Delta 0
      geant_charge(160) =  1     ! Delta +
      geant_charge(161) =  2     ! Delta ++
      geant_charge(162) =  1     ! K* +
      geant_charge(163) = -1     ! K* -
      geant_charge(164) =  0     ! K* 0
      geant_charge(165) =  0     ! sigma 0

CCC   Set Particle Lifetimes in seconds:
      geant_lifetime( 1) = 1.0E+30       ! Gamma
      geant_lifetime( 2) = 1.0E+30       ! Positron
      geant_lifetime( 3) = 1.0E+30       ! Electron
      geant_lifetime( 4) = 1.0E+30       ! Neutrino
      geant_lifetime( 5) = 2.19703E-06   ! Muon+
      geant_lifetime( 6) = 2.19703E-06   ! Muon-
      geant_lifetime( 7) = 8.4E-17       ! Pion0
      geant_lifetime( 8) = 2.603E-08     ! Pion+
      geant_lifetime( 9) = 2.603E-08     ! Pion-
      geant_lifetime(10) = 5.16E-08      ! Kaon 0 long
      geant_lifetime(11) = 1.237E-08     ! Kaon+
      geant_lifetime(12) = 1.237E-08     ! Kaon-
      geant_lifetime(13) = 889.1         ! Neutron
      geant_lifetime(14) = 1.0E+30       ! Proton
      geant_lifetime(15) = 1.0E+30       ! Antiproton
      geant_lifetime(16) = 8.922E-11     ! Kaon 0 short
      geant_lifetime(17) = 5.53085E-19   ! Eta
      geant_lifetime(18) = 2.632E-10     ! Lambda
      geant_lifetime(19) = 7.99E-11      ! Sigma+
      geant_lifetime(20) = 7.40E-20      ! Sigma0
      geant_lifetime(21) = 1.479E-10     ! Sigma-
      geant_lifetime(22) = 2.90E-10      ! Xi 0
      geant_lifetime(23) = 1.639E-10     ! Xi -
      geant_lifetime(24) = 8.22E-11      ! Omega
      geant_lifetime(25) = 889.1         ! Antineutron
      geant_lifetime(26) = 2.632E-10     ! Antilambda
      geant_lifetime(27) = 7.99E-11      ! Anti-Sigma -
      geant_lifetime(28) = 7.40E-20      ! Anti-Sigma 0
      geant_lifetime(29) = 1.479E-10     ! Anti-Sigma +
      geant_lifetime(30) = 2.90E-10      ! AntiXi 0
      geant_lifetime(31) = 1.639E-10     ! AntiXi +
      geant_lifetime(32) = 8.22E-11      ! Anti-Omega +
      geant_lifetime(33) = 0.303E-12     ! Tau +
      geant_lifetime(34) = 0.303E-12     ! Tau -
      geant_lifetime(35) = 10.62E-13     ! D+
      geant_lifetime(36) = 10.62E-13     ! D-
      geant_lifetime(37) = 4.21E-13      ! D0
      geant_lifetime(38) = 4.21E-13      ! Anti D0
      geant_lifetime(39) = 4.45E-13      ! F+, now called Ds+
      geant_lifetime(40) = 4.45E-13      ! F-, now called Ds-
      geant_lifetime(41) = 1.91E-13      ! Lambda C+
      geant_lifetime(42) = 2.92E-25      ! W+
      geant_lifetime(43) = 2.92E-25      ! W-
      geant_lifetime(44) = 2.60E-25      ! Z0
      geant_lifetime(45) = 1.0E+30       ! Deuteron
      geant_lifetime(46) = 1.0E+30       ! Triton
      geant_lifetime(47) = 1.0E+30       ! Alpha
      geant_lifetime(48) = 1.0E+30       ! Geantino (Fake particle)
      geant_lifetime(49) = 1.0E+30       ! He3
      geant_lifetime(50) = 1.0E+30       ! Cerenkov (Fake particle)
      geant_lifetime(151) = 3.72E-24     ! rho +
      geant_lifetime(152) = 3.72E-24     ! rho -
      geant_lifetime(153) = 3.72E-24     ! rho 0
      geant_lifetime(154) = 7.84E-23     ! omega 0
      geant_lifetime(155) = 3.16E-21     ! eta'
      geant_lifetime(156) = 1.49E-22     ! phi
      geant_lifetime(157) = 9.68E-21     ! J/Psi
      geant_lifetime(158) = 9.27E-24     ! Delta -, Based on gamma = 71 MeV
      geant_lifetime(159) = 9.27E-24     ! Delta 0, -same-
      geant_lifetime(160) = 9.27E-24     ! Delta +, -same-
      geant_lifetime(161) = 9.27E-24     ! Delta ++,-same-
      geant_lifetime(162) = 1.322E-23    ! K* +
      geant_lifetime(163) = 1.322E-23    ! K* -
      geant_lifetime(164) = 1.303E-23    ! K* 0
      geant_lifetime(165) = 1.000E-24    ! sigma 0

CCC   Set Particle Widths in GeV:
      do i = 1,200
         if(geant_lifetime(i) .le. 0.0) then
            geant_width(i) = 0.0
         else if(geant_lifetime(i) .ge. 1.0E+30) then
            geant_width(i) = 0.0
         else
            geant_width(i) = hbar/geant_lifetime(i)
         end if
      end do

      Return
      END

      Real*4 Function Vn_pt_y(n,V1,V2,V3,V4,pt,y)
      implicit none

CCC   Description:  This function computes the pt,y dependent flow
CCC                 parameters Vn(pt,y) for the requested Fourier
CCC                 component, n = 1-6, pt (GeV/c) and y (rapidity).
CCC
CCC   Input:    n    = Fourier component, 1,2...6
CCC             V1   = Constant coefficient in pt dependent term
CCC             V2   = Coefficient of pt dependence
CCC             V3   = Coefficient of y dependence; constant for n=odd,
CCC                    and inverse range squared for Gaussian for n=even.
CCC             V4   = Coefficient of y^3 dependence for n=odd
CCC             pt   = Transverse momentum (GeV/c)
CCC             y    = Rapidity relative to y(C.M.)
CCC
CCC   Output:   Vn_pt_y = Vn(pt,y) based on the model suggested by
CCC                       Art Poskanzer (LBNL, Feb. 2000)
CCC             Vn_pt_y = (V1 + V2*pt*pt)*exp(-V3*y*y) ; n=even
CCC             Vn_pt_y = (V1 + V2*pt)*sign(y)*(V3 + V4*abs(y**3)); n=odd

CCC   Local Variable Type Declarations:

      integer n
      real*4  V1,V2,V3,V4,pt,y,signy

      if(n .eq. (2*(n/2))) then
         Vn_pt_y = (V1 + V2*pt*pt)*exp(-V3*y*y)
      else
         if(y.ge.0.0) then
            signy = 1.0
         else if(y.lt.0.0) then
            signy = -1.0
         end if
         Vn_pt_y = (V1 + V2*pt)*signy*(V3 + V4*abs(y**3))
      end if
      Return
      END

      Subroutine Particle_mass(gpid,pid_index,npts)
      implicit none

CCC   Description:  This subroutine computes the mass distributions for
C     included resonances at 'npts' number of mesh points in mass from the
C     lower mass limit to an upper mass limit (listed below), integrates
C     this mass distribution, normalizes the integral to 1.0, and saves
C     the mass steps and integral function in the arrays in Common/Mass/.
C     The mass distribution integral is then randomly sampled in a later
C     step in order to get the specific resonance mass instances.
C     For non-resonant particles (i.e. either stable or those with negligible
C     widths) this subroutine returns without doing anything, leaving the
C     arrays in Common/Mass/ set to zero.  This subroutine is called for
C     a specific PID index, corresponding to the input list of particle
C     types.
C
C     Input:   gpid       = Geant particle ID code number, see SUBR:
C                           Particle_prop for listing.
C              pid_index  = Particle type array index, determined by input
C                           particle list.
C              npts       = n_integ_pts in calling program; is the number
C                           of mass mesh points used to load the mass
C                           distribution integral.  Note that one extra
C                           mesh point is added to handle the bug in the
C                           Lagrange interpolator, LAGRNG.
C
C     Output:  Mass_integ_save( , ) - mass distribution integral
C              Mass_xfunc_save( , ) - mass distribution mesh
C              These are in Common/Mass/.

CCC   Include files and common blocks:
CCCCC#include "Parameter_values.inc"
CCC   Set array dimension sizes here:

      integer npid,nmax_integ,n_mult_max_steps
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      Common/Geant/geant_mass(200),geant_charge(200),
     1             geant_lifetime(200),geant_width(200)
      real*4 geant_mass,geant_charge,geant_lifetime,geant_width
      Common/Mass/ Mass_integ_save(npid,nmax_integ),
     1             Mass_xfunc_save(npid,nmax_integ)
      real*4 Mass_integ_save,Mass_xfunc_save

CCC   Local Variable Type Declarations:
      integer gpid,pid_index,npts,i
      real*4 dist(nmax_integ),dM,M0,Gamma,GammaS
      real*4 M,Mpi,MK,MN,R_Delta,Jinc,qR
      real*4 Mrho_low,Mrho_high,Momega_low,Momega_high
      real*4 Msig_low,Msig_high, sigomnes
      real*4 Mphi_low,Mphi_high,MDelta_low,MDelta_high
      real*4 MKstar_low,MKstar_high
      real*4 kcm,k0cm,Ecm,E0cm,beta,beta0,Epicm,ENcm,redtotE

CCC   Set Fixed parameter values:
      Parameter(Mpi = 0.1395675)     ! Charged pion mass (GeV)
      Parameter(MK  = 0.493646)      ! Charged kaon mass (GeV)
      Parameter(MN  = 0.938919)      ! Nucleon average mass (GeV)
      Parameter(R_Delta = 0.81)      ! Delta resonance range parameter
      Parameter(Msig_low = 0.28   )  ! Lower rho mass limit
      Parameter(Msig_high = .98)     ! Upper sigma mass limit (GeV)
      Parameter(Mrho_low = 0.28   )  ! Lower rho mass limit
      Parameter(Mrho_high = 1.200)   ! Upper rho mass limit (GeV)
      Parameter(Momega_low = 0.75)   ! Lower omega mass limit (GeV)
      Parameter(Momega_high = 0.81)  ! Upper omega mass limit (GeV)
      Parameter(Mphi_low = 1.009)    ! Lower phi mass limit (GeV)
      Parameter(Mphi_high = 1.029)   ! Upper phi mass limit (GeV)
      Parameter(MDelta_low = 1.1   ) ! Lower Delta mass limit
      Parameter(MDelta_high = 1.400) ! Upper Delta mass limit (GeV)
      Parameter(MKstar_low = 0.74)   ! Lower Kstar mass limit (GeV)
      Parameter(MKstar_high = 1.04)  ! Upper Kstar mass limit (GeV)

CCC   Check npts:
      if(npts.le.1) Return

CCC   Load mass distribution for rho-meson:
      if(gpid.ge.151 .and. gpid.le.153) then
         do i = 1,nmax_integ
            dist(i) = 0.0
         end do
         M0 = geant_mass(gpid)
         Gamma = geant_width(gpid)
         dM = (Mrho_high - Mrho_low)/float(npts-1)
         do i = 1,npts
            M = Mrho_low + dM*float(i-1)
            Mass_xfunc_save(pid_index,i) = M
            kcm = sqrt(0.25*M*M - Mpi*Mpi)
            dist(i) = kcm/((M-M0)**2 + 0.25*Gamma*Gamma)
         end do

CCC   Load mass distribution for sigma-meson:
      else if(gpid.eq.165) then
         do i = 1,nmax_integ
            dist(i) = 0.0
         end do
         dM = (Msig_high - Msig_low)/float(npts-1)
         do i = 1,npts
            M = Mrho_low + dM*float(i-1)
            Mass_xfunc_save(pid_index,i) = M
            dist(i) = sigomnes(M)
         end do

CCC   Load mass distribution for omega-meson:
      else if(gpid.eq.154) then
         do i = 1,nmax_integ
            dist(i) = 0.0
         end do
         M0 = geant_mass(gpid)
         Gamma = geant_width(gpid)
         dM = (Momega_high - Momega_low)/float(npts-1)
         do i = 1,npts
            M = Momega_low + dM*float(i-1)
            Mass_xfunc_save(pid_index,i) = M
            GammaS = Gamma*((M/M0)**3)
            dist(i) = M0*M0*Gamma/((M0*M0 - M*M)**2
     1              + M*M*GammaS*GammaS)
         end do

CCC   Load mass distribution for phi-meson:
      else if(gpid.eq.156) then
         do i = 1,nmax_integ
            dist(i) = 0.0
         end do
         M0 = geant_mass(gpid)
         Gamma = geant_width(gpid)
         dM = (Mphi_high - Mphi_low)/float(npts-1)
         k0cm = sqrt(0.25*M0*M0 - MK*MK)
         E0cm = sqrt(k0cm*k0cm  + MK*MK)
         beta0 = k0cm/E0cm
         do i = 1,npts
            M = Mphi_low + dM*float(i-1)
            Mass_xfunc_save(pid_index,i) = M
            kcm = sqrt(0.25*M*M - MK*MK)
            Ecm = sqrt(kcm*kcm  + MK*MK)
            beta = kcm/Ecm
            dist(i) = 0.25*Gamma*Gamma*((beta/beta0)**3)/
     1                ((M - M0)**2 + 0.25*Gamma*Gamma)
         end do

CCC   Load mass distribution for Delta resonances:
      else if(gpid.ge.158 .and. gpid.le.161) then
         do i = 1,nmax_integ
            dist(i) = 0.0
         end do
         M0 = geant_mass(gpid)
         Gamma = geant_width(gpid)
         dM = (MDelta_high - MDelta_low)/float(npts-1)
         do i = 1,npts
            M = MDelta_low + dM*float(i-1)
            Mass_xfunc_save(pid_index,i) = M
            kcm = ((M*M + Mpi*Mpi - MN*MN)**2)/(4.0*M*M) - Mpi*Mpi
            kcm = sqrt(abs(kcm))
            Epicm = sqrt(kcm*kcm + Mpi*Mpi)
            ENcm  = sqrt(kcm*kcm + MN*MN)
            redtotE = Epicm*ENcm/(Epicm + ENcm)
            Jinc = kcm/redtotE
            qR = kcm*R_Delta/Mpi
            GammaS = 2.0*qR*qR*qR*Gamma/(1.0 + qR*qR)
            dist(i) = (Jinc/(kcm*kcm))*GammaS*GammaS/
     1                ((M - M0)**2 + 0.25*GammaS*GammaS)
         end do

CCC   Load mass distribution for K*(892) resonances:
      else if(gpid.ge.162 .and. gpid.le.164) then
         do i = 1,nmax_integ
            dist(i) = 0.0
         end do
         M0 = geant_mass(gpid)
         Gamma = geant_width(gpid)
         dM = (MKstar_high - MKstar_low)/float(npts-1)
         k0cm = ((M0*M0 + Mpi*Mpi - MK*MK)**2)/(4.0*M0*M0) - Mpi*Mpi
         k0cm = sqrt(k0cm)
         do i = 1,npts
            M = MKstar_low + dM*float(i-1)
            Mass_xfunc_save(pid_index,i) = M
            kcm = ((M*M + Mpi*Mpi - MK*MK)**2)/(4.0*M*M) - Mpi*Mpi
            kcm = sqrt(kcm)
            qR = kcm/k0cm
            GammaS = 2.0*qR*qR*qR*Gamma/(1.0 + qR*qR)
            dist(i) = GammaS*GammaS*M0*M0/
     1                ((M*M - M0*M0)**2 + GammaS*GammaS*M0*M0)
         end do

CCC  Load additional resonance mass distributions here:

      else
         Return        ! Return for Geant PID types without mass distribution
      end if

CCC   Integrate mass distribution from mass_low to mass_high:

      Mass_integ_save(pid_index,1) = 0.0
      do i = 2,npts
         Mass_integ_save(pid_index,i) = 0.5*(dist(i-1) + dist(i))*dM
     1      + Mass_integ_save(pid_index,i-1)
      end do

CCC   Normalize this integral such that the last point is 1.00:
      if(Mass_integ_save(pid_index,npts) .ne. 0.0) then
         do i = 1,npts
         Mass_integ_save(pid_index,i) =
     1   Mass_integ_save(pid_index,i)/Mass_integ_save(pid_index,npts)
         end do
      end if

CCC   Extend integ() and xfunc() by one mesh point to avoid Lagrange
CCC  interpolation subroutine bug:
      Mass_integ_save(pid_index,npts+1) =
     1   Mass_integ_save(pid_index,npts) + 0.01
      Mass_xfunc_save(pid_index,npts+1) =
     1   Mass_xfunc_save(pid_index,npts)

      Return
      END

      Real*4 Function Mass_function(gpid,pid_index,npts)
      implicit none

CCC   Description:  For resonance particles which have mass distributions
C     this function randomly samples the distributions in Common/Mass/
C     and returns a particle mass in GeV in 'Mass_function'.
C     For non-resonant particles this function returns the Geant mass
C     listed in SUBR: Particle_prop.
C
C     Input:   gpid       = Geant particle ID code number, see SUBR:
C                           Particle_prop for listing.
C              pid_index  = Particle type array index, determined by input
C                           particle list.
C              npts       = n_integ_pts in calling program.  Is the number
C                           of mass mesh points for the arrays
C                           in Common/Mass/ minus 1.
C              irand      = random number generator argument/seed
C
C     Output:  Mass_function = particle or resonance mass (GeV)

CCC   Include files and common blocks:
CCCCC#include "Parameter_values.inc"
CCC   Set array dimension sizes here:

      integer npid,nmax_integ,n_mult_max_steps
      parameter (npid = 30)         ! max # of particle ID types
      parameter (nmax_integ = 100)  ! max # integration steps in parameter
CCC                                 ! variance calculation.
      parameter (n_mult_max_steps = 1000)
CCC                                 ! max # integration steps in multiplicity
CCC                                 ! variance calculation (this must be an
CCC                                 ! even integer).
      Common/Geant/geant_mass(200),geant_charge(200),
     1             geant_lifetime(200),geant_width(200)
      real*4 geant_mass,geant_charge,geant_lifetime,geant_width
      Common/Mass/ Mass_integ_save(npid,nmax_integ),
     1             Mass_xfunc_save(npid,nmax_integ)
      real*4 Mass_integ_save,Mass_xfunc_save,ranf

CCC   Local Variable Type Declarations:
      integer gpid,pid_index,npts,i
      real*4 integ(nmax_integ),xfunc(nmax_integ)
      real*4 masstmp

      if(Mass_integ_save(pid_index,npts) .ne. 0.0) then
         do i = 1,npts+1
            integ(i) = Mass_integ_save(pid_index,i)
            xfunc(i) = Mass_xfunc_save(pid_index,i)
         end do
         Call LAGRNG(ranf(),integ,masstmp,xfunc,
     1               npts+1, 1, 5, npts+1, 1)
         Mass_function = masstmp
      else
         Mass_function = geant_mass(gpid)
      end if

      Return
      END
C----------------------------------------------------------------------
      Subroutine boost(betatr,upppx,upppy,upppz,theta,mass,
     1                 pxnew,pynew,pznew)
      implicit none
CCC   Computes velocity addition

CCC   Local Variable Type Declarations:
      real*4  betatr
      real*4  upppx,upppy,upppz,theta,mass,pxnew,pynew,pznew
      real*4  costhe,sinthe,uppx,uppy,uppz,fac1,fac2
      real*4  upx,upy,upz,ux,uy,uz,gamma

      costhe = cos(theta)
      sinthe = sin(theta)

      uppx =  upppx*costhe + upppy*sinthe
      uppy = -upppx*sinthe + upppy*costhe
      uppz =  upppz

      fac1 = sqrt(1.0 - betatr*betatr) 
      fac2 = 1.0 + betatr*uppx
      upx  = (uppx + betatr)/fac2
      upy  = fac1*uppy/fac2
      upz  = fac1*uppz/fac2

      ux   = upx*costhe - upy*sinthe
      uy   = upx*sinthe + upy*costhe
      uz   = upz

      gamma = 1.0/sqrt(1.0 - ux*ux - uy*uy - uz*uz)
      pxnew = gamma*mass*ux
      pynew = gamma*mass*uy
      pznew = gamma*mass*uz

      Return
      END
       REAL FUNCTION SIGOMNES(AM)
       implicit none
       REAL*4 AM,TWOPI,Q,GAM1,GAM2,AM1,AM2,TANF
       REAL*4 THETA,THRAD,T,WAT,BOWLER,SQRBOW,SQRWAT,OMNES
       TWOPI=6.28318530718
       Q=SQRT(AM**2-.077284)
       GAM1=.144*Q
       GAM2=3.778*Q
       AM1=1.017
       AM2=1.038
       TANF=((AM1**2-AM**2)*GAM2+(AM2**2-AM**2)*GAM1)/
     1 ((AM1**2-AM**2)*(AM2**2-AM**2)-GAM1*GAM2)
       THETA=360.0*ATAN(TANF)/TWOPI-15.0
       IF(THETA.LT.0.0.AND.AM.LT..2861) THETA=0.
       IF(THETA.LT.0.0) THETA=THETA+180.
       THRAD=TWOPI*THETA/360.
       T=SIN(THRAD)
       WAT=T**2/Q
       BOWLER=(1.0-T**2)
       SQRBOW=SQRT(BOWLER)
       IF(AM.GT..86) SQRBOW=-SQRBOW
       SQRWAT=SQRT(WAT)
       OMNES=(SQRWAT+SQRBOW)**2
       IF(AM.GT..98) OMNES=0.0
       SIGOMNES=OMNES
       RETURN
       END






