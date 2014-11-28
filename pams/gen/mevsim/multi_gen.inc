CCC   Set array dimension sizes here:

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


