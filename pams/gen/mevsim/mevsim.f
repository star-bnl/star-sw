      PROGRAM MEVSIM 
CCC   Documentation and Description:
CCC
C     This code is intended to provide a quick means of producing
C     uncorrelated simulated events for event-by-event studies,
C     detector acceptance and efficiency studies, etc.  The
C     user selects the number of events, the one-particle distribution
C     model, the particles to include, the ranges in transverse
C     momentum, pseudorapidity and azimuthal angle, the mean
C     multiplicity for each particle type for the event run, the
C     mean temperature, Rapidity width, etc., and the standard deviations
C     for the event-to-event variation in the model parameters.
C     Note that these events are produced in the c.m. frame only.
C    
C     Anisotropic flow may also be simulated by introducing explicit
C     phi-dependence (azimuthal angle) in the particle distributions.  
C     The assumed model is taken from Poskanzer and Voloshin, Phys. Rev.
C     C58, 1671 (1998), Eq.(1), where we use,
C
C          E d^3N/dp^3 = (1/2*pi*pt)*[d^2N/dpt*dy]
C             * [1 + SUM(n=1,nflowterms){2*Vn*cos[n(phi-PSIr)]}]
C
C     with up to 'nflowterms' (currently set to 6, see file
C     Parameter_values.inc) Fourier components allowed.  Vn are
C     coefficients and PSIr is the reaction plane angle.
C     The algebraic signs of the Vn terms for n=odd are reversed
C     from their input values for particles with rapidity (y) < 0
C     as suggested in Poskanzer and Voloshin.
C     The flow parameters can depend on pt and rapidity (y) according
C     to the model suggested by Art Poskanzer (Feb. 2000) and as
C     defined in the Function Vn_pt_y.
C
C     The user may select either to have the same multiplicity per
C     particle type for each event or to let the multiplicity vary
C     randomly according to a Poisson distribution. In addition, an
C     overall multiplicative scale factor can be applied to each
C     particle ID's multiplicity (same factor applied to each PID).
C     This scaling can vary randomly according to a Gaussian from
C     event-to-event.  This is to simulate trigger acceptance
C     fluctuations.  Similarly the
C     parameters of the one-particle distribution models may either
C     be fixed to the same value for each event or allowed to randomly
C     vary about a specified mean with a specified standard deviation
C     and assuming a Gaussian distribution.
C
C     With respect to the reaction plane and anisotropic flow simulation,
C     the user may select among four options:
C        (1) ignore reaction plane and anisotropic flow effects; all
C            distributions will be azimuthally invariant, on average.
C        (2) assume a fixed reaction plane angle, PSIr, for all events
C            in the run.
C        (3) assume a Gaussian distribution with specified mean and
C            standard deviation for the reaction plane angles for the
C            events in the run.  PSIr is randomly determined for each
C            event.
C        (4) assume uniformly distributed, random values for the reaction  
C            plane angles from 0 to 360 deg., for each event in the run.
C
C     The user may also select the anisotropic flow parameters, Vn,
C     to either be fixed for each event, or to randomly vary from event
C     to event according to a Gaussian distribution where the user must
C     specify the mean and std. dev.  For both cases the input file must
C     list the 'nflowterms' (e.g. 6) values of the mean and Std.dev. for
C     the Vn parameters for all particle ID types included in the run.
C
C     The available list of particles has been increased to permit a
C     number of meson and baryon resonances.  For those with broad widths
C     the code samples the mass distribution for the resonance.
C     The resonance shapes
C     are approximately Breit-Wigner and are specific for each resonance
C     case.  The additional particle/resonances include: rho(+,-,0),
C     omega(0), eta', phi, J/Psi, Delta(-,0,+,++) and K*(+,-,0).  Masses
C     are sampled for the rho, omega, phi, Deltas and D*s.
C     Refer to SUBR: Particle_prop and Particle_mass for the explicit
C     parameters, resonance shape models, and sampling ranges.
C
C     The input is from a file, named 'mult_gen.in'.  The output is
C     particle table for a given event.
C     Header information is store in the first particle. Event header 
C     information store in the first particle includes: Impact parameter
C     and phi angle of collision; Generator number 62 mev giving VENUS
C     like central collision and number 80 giving VNI like central; 
C     Energy per nucleon-nucleon collision; Colliding system A.A;
C     Run number; Event number; Day; and Time. 
C     A log file, 'mult_gen.log' is also written which contain config. 
C     messages.
C
C     The method for generating random multiplicities and model parameter
C     values involves the following steps:
C        (1) The Poisson or Gaussian distributions are computed and
C            loaded into function f().
C        (2) The distribution f(x') is integrated from xmin to x
C            and saved from x = xmin to x = xmax.  The range and mesh
C            spaces are specified by the user.
C        (3) The integral of f is normalized to unity where 
C            integral[f(x')](at x = xmin) = 0.0
C            integral[f(x')](at x = xmax) = 1.0
C        (4) A random number generator is called which delivers values
C            between 0.0 and 1.0.  
C        (5) We consider the coordinate x (from xmin to xmax) to be
C            dependent on the integral[f].  Using the random number
C            for the selected value of integral[f] the value of x
C            is obtained by interpolation.
C
C     An interpolation subroutine from Rubin Landau, Oregon State Univ.,
C     is used to do this interpolation; it involves uneven mesh point 
C     spacing.
C
C     The method for generating the particle momenta uses the
C     standard random elimination method and involves the following
C     steps:
C
C     For model_type = 1,2,3,4 which are functions of pt,y (see following):
C        (1) The y range is computed using the pseudorapidity (eta)
C            range and includes ample cushioning around the sides
C            along the eta acceptance edges.
C        (2) The transverse momentum (pt) and rapidity (y) are
C            randomly chosen within the specified ranges.
C        (3) The pseudorapidity is computed for this (pt,y) value
C            (and the mass for each pid) and checked against the
C            pseudorapidity acceptance range.
C        (4) If the pseudorapidity is within range then the one-particle
C            model distribution is calculated at this point and its ratio
C            to the maximum value throughout (pt,eta) acceptance region
C            is calculated.
C        (5) Another random number is called and if less than the ratio
C            from step#4 the particle momentum is used; if not, then 
C            another trial value of (pt,y) is obtained.
C        (6) This continues until the required multiplicity for the
C            specific event and particle type has been satisfied.
C        (7) This process is repeated for the requested number of particle
C            types and events.
C
C     For model_type = 5,6 (see following) which are input bin-by-bin
C     in pt,eta:
C        (1) The transverse momentum (pt) and pseudorapidity (eta) are 
C            randomly chosen within the specified ranges.
C        (2) The one-particle model distribution is calculated at this
C            point and its ratio to the maximum value throughout the
C            (pt,eta) region is calculated.
C        (3) Another random number is called and if less than the ratio
C            from step(2) the particle momentum is used; if not then
C            another trial value of (pt,eta) is obtained.
C        (4) This continues until the required multiplicity for the 
C            specific event and particle type has been satisfied.
C        (5) This process is repeated for the requested number of particle
C            types and events. 
C
C     Problematic parameter values are tested, bad input values are checked
C     and in some cases may be changed so that the program will not crash.
C     In some cases the code execution is stopped.
C     Some distributions and/or unusual model parameter values may cause the
C     code to hang up due to the poor performance of the "elimination"
C     method for very strongly peaked distributions.  These are tested for
C     certain problematic values and if necessary these events are aborted.
C     A message, "*** Event No.    2903 ABORTED:" for example is printed
C     in the 'mult_gen.out' file.  Temperatures .le. 0.01 GeV and rapidity
C     width parameters .le. 0.01 will cause the event to abort.
C
C     The input is described below in the 'read' statements and also in
C     the sample input file.  Some additional comments are as follows:
C
C     (1) n_events - Selected number of events in run. Can be anything
C                    .ge. 1.
C     (2) n_pid_type - Number of particle ID types to include in the
C                      particle list. e.g. pi(+) and pi(-) are counted
C                      separately.  The limit is set by parameter npid
C                      in the accompanying include file 'Parameter_values.inc'
C                      and is presently set at 20.
C     (3) model_type - equals 1,2,3,4,5 or 6 so far.  See comments in
C                      Function dNdpty to see what is calculated.
C                      The models included are:
C                    = 1, Factorized mt exponential, Gaussian rapidity model
C                    = 2, Pratt non-expanding, spherical thermal source model
C                    = 3, Bertsch non-expanding spherical thermal source model
C                    = 4, Pratt spherically expanding, thermally equilibrated
C                         source model.
C                    = 5, Factorized pt and eta distributions input bin-by-bin.
C                    = 6, Fully 2D pt,eta distributions input bin-by-bin.
C                         NOTE: model_type = 1-4 are functions of (pt,y)
C                               model_type = 5,6 are functions of (pt,eta)
C     (4) reac_plane_cntrl - Can be either 1,2,3 or 4 where:
C                          = 1 to ignore reaction plane and anisotropic flow,
C                              all distributions will be azimuthally symm.
C                          = 2 to use a fixed reaction plane angle for all
C                              events in the run.
C                          = 3 to assume a randomly varying reaction plane
C                              angle for each event as determined by a
C                              Gaussian distribution.
C                          = 4 to assume a randomly varying reaction plane
C                              for each event in the run as determined by
C                              a uniform distribution from 0 to 360 deg.
C     (5) PSIr_mean, PSIr_stdev - Reaction plane angle mean and Gaussian
C                                 std.dev. (both are in degrees) for cases
C                                 with reac_plane_cntrl = 2 (use mean value)
C                                 and 3.  Note: these are read in regardless
C                                 of the value of reac_plane_cntrl.
C     (6) MultFac_mean, MultFac_stdev - Overall multiplicity scaling factor
C                                       for all PID types; mean and std.dev.;
C                                       for trigger fluctuations event-to-evt.
C     (7) etaf_mean, etaf_stdev, n_stdev_etaf
C                              Transverse rapidity paramters. Applied to
C                              all particle types
C     (8) pt_cut_min,pt_cut_max - Range of transverse momentum in GeV/c.
C     (9) eta_cut_min,eta_cut_max - Pseudorapidity range
C    (10) phi_cut_min,phi_cut_max - Azimuthal angular range in degrees.
C    (11) n_stdev_mult - Number of standard deviations about the mean value
C                        of multiplicity to include in the random event-to-
C                        event selection process.  The maximum number of
C                        steps that can be covered is determined by
C                        parameter n_mult_max_steps in the accompanying
C                        include file 'Parameter_values.inc' which is
C                        presently set at 1000, but the true upper limit for
C                        this is n_mult_max_steps - 1 = 999.
C    (12) n_stdev_temp - Same, except for the "Temperature" parameter.
C    (13) n_stdev_sigma- Same, except for the rapidity width parameter.
C    (14) n_stdev_expvel - Same, except for the expansion velocity parameter.
C    (15) n_stdev_PSIr   - Same, except for the reaction plane angle
C    (16) n_stdev_Vn     - Same, except for the anisotropy coefficients, Vn.
C    (17) n_stdev_MultFac - Same, except for the multiplicity scaling factor.
C    (18) n_integ_pts - Number of mesh points to use in the random model
C                       parameter selection process.  The upper limit is
C                       set by parameter nmax_integ in the accompanying
C                       include file 'Parameter_values.inc' which is presently
C                       set at 100, but the true upper limit for n_integ_pts
C                       is nmax_integ - 1 = 99. 
C    (19) n_scan_pts  - Number of mesh points to use to scan the (pt,y)
C                       dependence of the model distributions looking for
C                       the maximum value.  The 2-D grid has
C                       n_scan_pts * n_scan_pts points; no limit to size of
C                       n_scan_pts.
C
C**************************************************************************
C    FOR MODEL_TYPE = 1,2,3 or 4:
C    Input the following 7 lines for each particle type; repeat these
C    set of lines n_pid_type times:
C
C         (a) gpid - Geant Particle ID code number
C         (b) mult_mean,mult_variance_control - Mean multiplicity and
C                                               variance control where:
C             mult_variance_control = 0 for no variance in multiplicity 
C             mult_variance_control = 1 to allow Poisson distribution for
C                                       particle multiplicities for all events.
C             Note that a hard limit exists for the maximum possible
C             multiplicity for a given particle type per event.  This is
C             determined by parameter factorial_max in accompanying include
C             file 'common_facfac.inc' and is presently set at 10000.
C         (c) Temp_mean, Temp_stdev - Temperature parameter mean (in GeV)
C             and standard deviation (Gaussian distribution assumed).
C         (d) sigma_mean, sigma_stdev - Rapidity distribution width (sigma)
C             parameter mean and standard deviation (Gaussian distribution
C             assumed).
C         (e) expvel_mean, expvel_stdev - S. Pratt expansion velocity
C             (in units of c) mean and standard deviation (Gaussian 
C             distribution assumed).
C         (f) Vn_mean(k);  k=1,4  - Anisotropic flow parameters, mean values
C                                   for Fourier component n=1.
C         (g) Vn_stdev(k); k=1,4  - Anisotropic flow parameters, std.dev.
C                                   values for Fourier component n=1.
C
C             Repeat the last two lines of input for remaining Fourier
C             components n=2,3...6.  Include all 6 sets of parameters
C             even if these are not used by the model for Vn(pt,y) (set
C             unused parameter means and std.dev. to 0.0).  List 4 values
C             on every line, even though for n=even the 4th quantity is
C             not used.
C
C**************************************************************************
C    FOR MODEL_TYPE = 5 input the following set of lines for each particle
C                       type; repeat these n_pid_type times.
C
C         (a) gpid - Geant Particle ID code number
C         (b) mult_mean,mult_variance_control - Mean multiplicity and
C                                               variance control where:
C             mult_variance_control = 0 for no variance in multiplicity
C             mult_variance_control = 1 to allow Poisson distribution for
C                                       particle multiplicities for all events.
C         (c) pt_start, eta_start - minimum starting values for pt, eta 
C                                   input for the bin-by-bin distributions.
C         (d) n_pt_bins, n_eta_bins - # input pt and eta bins.
C         (e) delta_pt, pt_bin - pt bin size and function value, repeat for
C                                each pt bin.
C         (f) delta_eta, eta_bin - eta bin size and function value, repeat
C                                  for each eta bin.
C         (g) Vn_mean(k);  k=1,4  - Anisotropic flow parameters, mean values
C                                   for Fourier component n=1.
C         (h) Vn_stdev(k); k=1,4  - Anisotropic flow parameters, std.dev.
C                                   values for Fourier component n=1.
C
C             Repeat the last two lines of input for remaining Fourier
C             components n=2,3...6.  Include all 6 sets of parameters
C             even if these are not used by the model for Vn(pt,y) (set
C             unused parameter means and std.dev. to 0.0).  List 4 values
C             on every line, even though for n=even the 4th quantity is
C             not used.
C
C         NOTE: The pt, eta ranges must fully include the requested ranges
C               in input #4 and 5 above; else the code execution will stop.
C
C         Also, variable bin sizes are permitted for the input distributions.
C
C         Also, this input distribution is used for all events in the run;
C         no fluctuations in this "parent" distribution are allowed from 
C         event-to-event.
C
C**************************************************************************
C    FOR MODEL_TYPE = 6 input the following set of lines for each particle
C                       type; repeat these n_pid_type times.
C
C         (a) gpid - Geant Particle ID code number
C         (b) mult_mean,mult_variance_control - Mean multiplicity and
C                                               variance control where:
C             mult_variance_control = 0 for no variance in multiplicity
C             mult_variance_control = 1 to allow Poisson distribution for
C                                       particle multiplicities for all events.
C         (c) pt_start, eta_start - minimum starting values for pt, eta
C                                   input for the bin-by-bin distributions.
C         (d) n_pt_bins, n_eta_bins - # input pt and eta bins.
C         (e) delta_pt - pt bin size, repeat for each pt bin. 
C         (f) delta_eta - eta bin size, repeat for each eta bin.
C         (g) i,j,pt_eta_bin(i,j) - read pt (index = i) and eta (index = j)
C                                   bin numbers and bin value for full 2D space.
C         (h) Vn_mean(k);  k=1,4  - Anisotropic flow parameters, mean values
C                                   for Fourier component n=1.
C         (i) Vn_stdev(k); k=1,4  - Anisotropic flow parameters, std.dev.
C                                   values for Fourier component n=1.
C
C             Repeat the last two lines of input for remaining Fourier
C             components n=2,3...6.  Include all 6 sets of parameters
C             even if these are not used by the model for Vn(pt,y) (set
C             unused parameter means and std.dev. to 0.0).  List 4 values
C             on every line, even though for n=even the 4th quantity is
C             not used.
C
C         NOTE: The pt, eta ranges must fully include the requested ranges
C               in input #4 and 5 above; else the code execution will stop.
C
C         Also, variable bin sizes are permitted for the input distributions.
C
C         Also, this input distribution is used for all events in the run;
C         no fluctuations in this "parent" distribution are allowed from
C         event-to-event.
C
CCCCCC#include "hepevt.inc"
        INTEGER isthep !  status code of the entry 0 
        INTEGER idhep !  particle identity, accordingly to the PDG standard 0 
        INTEGER jmohep !  pointer(s) to position where the mother(s) stored 0 
        INTEGER jdahep !  pointers to position of the first/last daughter 0 
         REAL*4 phep !  p4 and mass (GeV) 0 
         REAL*4 vhep !  production vertex (mm) and time (mm/c) 0 
         DIMENSION isthep(30000),idhep(30000),jmohep(2,30000)
         DIMENSION jdahep(2,30000)
         DIMENSION phep(5,30000),vhep(4)     
CCCCCC#include "headpss.inc"
      COMMON/HEADPSS/PSSHEP(5),VSSHEP(4),IFIRST
      real*4 PSSHEP,VSSHEP
      INTEGER IFIRST
      SAVE /HEADPSS/
C---------------------------------------------------------------------
C       ISAJET Commons
C
        COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
        COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR
        LOGICAl NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR
        COMMON/PRIMAR/NJET,SCM,HALFE,ECM,IDIN(2),NEVENT,NTRIES,NSIGMA
        COMMON/PARTCL/NPTCL,PPTCL(5,4000),IORIG(4000),IDENT(4000)
     1  ,IDCAY(4000)      
        COMMON/DEKIN/NOFILE
        LOGICAL NOFILE,notbk

        LOGICAL MOPI0,MOETA,RHO,DELTA,KSTAR,PHI,JPSI,ETAP,OMG,D0
        SAVE MOPI0,MOETA,RHO,DELTA,KSTAR,PHI,JPSI,ETAP,OMG,D0
C
C       RHO and DELTA Decay Common
C
      CHARACTER*1 CETA,CPI0,CRHO,CDELTA,CKSTAR
      CHARACTER*1 CPHI,CNOF,CJPSI,CETAP,COMG,CD0
        COMMON/NUMLIM/NUMLO,NUMHI  
      integer itdky,itevt,itcom,itlis,numlo,numhi,ldecay,lhievt
      integer lhipar,ifl,njet,idin,nevent,ntries,nsigma,ident,ida2
      integer nptcl,iorig,gntsav,isttemp,idisa,id,iddhep,imo1,ida1
      real*4 scm,halfe,ecm,pptcl,pp,ppnxlv
      dimension id(20),imo1(20),ida1(20),ida2(20),pp(5,20),ppnxlv(5)
      real*4 HEPMASS,V,PPP
      dimension V(3),PPP(3)
      INTEGER IDTRAN, NPTCLSAV, istda, mopss, ksav, idsav, kk, nend
      dimension istda(20)
      INTEGER II,JJ,JJJ,kdahep,itimes,NPT,INTRUN,kmohep
      dimension kmohep(2), kdahep(2)
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
      Common/Mass/ Mass_integ_save(npid,nmax_integ),
     1             Mass_xfunc_save(npid,nmax_integ)
      real*4 Mass_integ_save,Mass_xfunc_save

      integer n_events, n_pid_type, model_type, n_integ_pts
      integer gpid(npid),mult_mean(npid),mult_variance_control(npid)
      integer i,j,k,pid,mult_min,mult_max,n_mult_steps(npid),ievent
      integer mult_event(npid),n_scan_pts,total_mult,n_vertices
      integer event_abort,status_abort, status
      integer iptbin, ietabin
      integer reac_plane_cntrl

      real*4 pt_cut_min,pt_cut_max,eta_cut_min,eta_cut_max
      real*4 y_cut_min,y_cut_max
      real*4 phi_cut_min,phi_cut_max,n_stdev_mult,n_stdev_temp
      real*4 n_stdev_sigma,n_stdev_expvel
      real*4 Temp_mean(npid)
      real*4 Temp_stdev(npid),pi,rad,mult_mean_real,mult_stdev
      real*4 mult_min_real,mult_max_real,ranf
      real*4 Temp_abort, sigma_abort, bin_value

      real*4 mult_integ(n_mult_max_steps),mult_xfunc(n_mult_max_steps)
      real*4 mult_integ_save(npid,n_mult_max_steps)
      real*4 mult_xfunc_save(npid,n_mult_max_steps)
      real*4 mult_event_real

      real*4 Temp_min,Temp_max,integ(nmax_integ),xfunc(nmax_integ)
      real*4 Temp_integ_save(npid,nmax_integ)
      real*4 Temp_xfunc_save(npid,nmax_integ)
      real*4 Temp_event(npid)

      real*4 sigma_stdev(npid),sigma_mean(npid),sigma_min,sigma_max
      real*4 sigma_integ_save(npid,nmax_integ)
      real*4 sigma_xfunc_save(npid,nmax_integ)
      real*4 sigma_event(npid)

      real*4 expvel_stdev(npid), expvel_mean(npid)
      real*4 expvel_min, expvel_max
      real*4 expvel_integ_save(npid,nmax_integ)
      real*4 expvel_xfunc_save(npid,nmax_integ)
      real*4 expvel_event(npid)

CCC   Variables associated with anisotropic flow:

      real*4 PSIr_mean, PSIr_stdev, n_stdev_PSIr, n_stdev_Vn
      real*4 PSIr_min, PSIr_max, PSIr_event
      real*4 PSIr_integ_save(nmax_integ)
      real*4 PSIr_xfunc_save(nmax_integ)
      real*4 Vn_mean(nflowterms,4,npid), Vn_stdev(nflowterms,4,npid)
      real*4 Vn_min, Vn_max
      real*4 Vn1_integ_save(nflowterms,npid,nmax_integ)
      real*4 Vn1_xfunc_save(nflowterms,npid,nmax_integ)
      real*4 Vn2_integ_save(nflowterms,npid,nmax_integ)
      real*4 Vn2_xfunc_save(nflowterms,npid,nmax_integ)
      real*4 Vn3_integ_save(nflowterms,npid,nmax_integ)
      real*4 Vn3_xfunc_save(nflowterms,npid,nmax_integ)
      real*4 Vn4_integ_save(nflowterms,npid,nmax_integ)
      real*4 Vn4_xfunc_save(nflowterms,npid,nmax_integ)
      real*4 Vn_event(nflowterms,4,npid), Vn_event_tmp(nflowterms,4)

CCC   Variables associated with trigger fluctuations:
      real*4 MultFac_mean, MultFac_stdev, n_stdev_MultFac
      real*4 MultFac_min, MultFac_max, MultFac_event
      real*4 MultFac_integ_save(nmax_integ)
      real*4 MultFac_xfunc_save(nmax_integ)

CCC   Variables associated with radial flow:
      real*4 etaf_mean, etaf_stdev, n_stdev_etaf
      real*4 etaf_min, etaf_max,etaf_event
      real*4 etaf_integ_save(nmax_integ)
      real*4 etaf_xfunc_save(nmax_integ)
      real*4 betatr, theta, Etot, mass
      real*4 ux, uy, uz, pxnew, pynew, pznew

      DATA IFIRST/0/
      DATA INTRUN/0/
      IF(IFIRST.EQ.0) THEN

      CALL HepRun(INTRUN)
CCC  Open I/O Files:
        LDECAY=1
        LHIEVT=2
        LHIPAR=3
      open(unit=4,type='old',access='sequential',name='mult_gen.in')
      open(unit=8,type='new',access='sequential',name='mult_gen.log')
C       ISAJET Calls
C
        CALL SETCON
        CALL RESET
        NOPI0=.TRUE.
        NOETA=.FALSE.
        NODCAY=.FALSE.
        notbk=.false.

CCC   File 'mult_gen.in' is the input file for the run.
CCC   File 'mult_gen.log' is a log file for the run.

CCC   Initialize Arrays to Zero:

      do i = 1,npid
         gpid(i) = 0
         mult_mean(i) = 0
         mult_variance_control(i) = 0
         n_mult_steps(i) = 0
         Temp_mean(i) = 0.0
         Temp_stdev(i) = 0.0
         sigma_mean(i) = 0.0
         sigma_stdev(i) = 0.0
         expvel_mean(i) = 0.0
         expvel_stdev(i) = 0.0
         mult_event(i) = 0
         Temp_event(i) = 0.0
         sigma_event(i) = 0.0
         expvel_event(i) = 0.0

         do j = 1,n_mult_max_steps
            mult_integ_save(i,j) = 0.0
            mult_xfunc_save(i,j) = 0.0
         end do

         do j = 1,nmax_integ
            Temp_integ_save(i,j) = 0.0
            Temp_xfunc_save(i,j) = 0.0
            sigma_integ_save(i,j) = 0.0
            sigma_xfunc_save(i,j) = 0.0
            expvel_integ_save(i,j) = 0.0
            expvel_xfunc_save(i,j) = 0.0
            Mass_integ_save(i,j) = 0.0
            Mass_xfunc_save(i,j) = 0.0
         end do
      end do

      do j = 1,nflowterms
         do k = 1,4
            Vn_event_tmp(j,k) = 0.0
         end do
         do i = 1,npid
            do k = 1,4
               Vn_mean(j,k,i)  = 0.0
               Vn_stdev(j,k,i) = 0.0
               Vn_event(j,k,i) = 0.0
            end do
            do k = 1,nmax_integ
               Vn1_integ_save(j,i,k) = 0.0
               Vn1_xfunc_save(j,i,k) = 0.0
               Vn2_integ_save(j,i,k) = 0.0
               Vn2_xfunc_save(j,i,k) = 0.0
               Vn3_integ_save(j,i,k) = 0.0
               Vn3_xfunc_save(j,i,k) = 0.0
               Vn4_integ_save(j,i,k) = 0.0
               Vn4_xfunc_save(j,i,k) = 0.0
            end do
         end do
      end do

      do i = 1,nmax_integ
         PSIr_integ_save(i) = 0.0
         PSIr_xfunc_save(i) = 0.0
         MultFac_integ_save(i) = 0.0
         MultFac_xfunc_save(i) = 0.0
      end do

      do i = 1,n_mult_max_steps
         mult_integ(i) = 0.0
         mult_xfunc(i) = 0.0
      end do

      do i = 1,nmax_integ
         integ(i) = 0.0
         xfunc(i) = 0.0
      end do

      do i = 1,factorial_max
         FACLOG(i) = 0.0
      end do

      do i = 1,60
         geant_mass(i) = 0.0
      end do

      do i = 1,npid
         pt_start(i) = 0.0
         pt_stop(i)  = 0.0
         eta_start(i) = 0.0
         eta_stop(i)  = 0.0
         n_pt_bins(i) = 0
         n_eta_bins(i) = 0
         do j = 1,n_bins_max
            delta_pt(i,j)   = 0.0
            delta_eta(i,j)  = 0.0
            pt_bin(i,j)     = 0.0
            eta_bin(i,j)    = 0.0
            do k = 1,n_bins_max
               pt_eta_bin(i,j,k) = 0.0
            end do
         end do
      end do

CCC  Read Input:

      write(6,*) INTRUN                ! Run number
      read(4,*) VSSHEP(2)              ! Starting No. for the  events to generate
      write(6,*) VSSHEP(2)             ! Starting No. for the  events to generate
      read(4,*) PSSHEP(4)              ! Energy of the Nucleon-Nucleon CM
      write(6,*) PSSHEP(4)              ! Energy of the Nucleon-Nucleon CM
      read(4,*) PSSHEP(5)              ! Stopping No.
      write(6,*) PSSHEP(5)              ! Stopping No.
      read(4,*) CNOF                   ! FLAG Yes FOR NO decay FileY
      IF(CNOF.EQ.'Y') NOFILE = .TRUE.
      IF(CNOF.EQ.'y') NOFILE = .TRUE.
      IF(CNOF.EQ.'N') NOFILE = .FALSE.
      IF(CNOF.EQ.'n') NOFILE = .FALSE.
      IF(NOFILE) THEN
      ELSE      
      OPEN(UNIT=1,FILE='decay.dat',STATUS='OLD',FORM='FORMATTED')
      ENDIF
      read(4,*) CPI0                   ! FLAG PI0 decay yes PI0 DECAY
      IF(CPI0.EQ.'Y') NOPI0 = .FALSE.
      IF(CPI0.EQ.'y') NOPI0 = .FALSE.
      IF(CPI0.EQ.'N') NOPI0 = .TRUE.
      IF(CPI0.EQ.'n') NOPI0 = .TRUE.
      MOPI0=NOPI0
      read(4,*) CETA                   ! FLAG ETA decay yes ETA DECAY
      IF(CETA.EQ.'Y') NOETA = .FALSE.
      IF(CETA.EQ.'y') NOETA = .FALSE.
      IF(CETA.EQ.'N') NOETA = .TRUE.
      IF(CETA.EQ.'n') NOETA = .TRUE.
      MOETA=NOETA
      read(4,*) CETAP                  ! FLAG ETA' decay yes ETA' DECAY
      IF(CETAP.EQ.'Y') ETAP = .FALSE.
      IF(CETAP.EQ.'y') ETAP = .FALSE.
      IF(CETAP.EQ.'N') ETAP = .TRUE.
      IF(CETAP.EQ.'n') ETAP = .TRUE.
      read(4,*) CRHO                   ! FLAG RHO decay yes RHO DECAY
      IF(CRHO.EQ.'Y') RHO = .FALSE.
      IF(CRHO.EQ.'y') RHO = .FALSE.
      IF(CRHO.EQ.'N') RHO = .TRUE.
      IF(CRHO.EQ.'n') RHO = .TRUE.
      read(4,*) COMG                   ! FLAG OMEGA decay yes OMEGA DECAY
      IF(COMG.EQ.'Y') OMG = .FALSE.
      IF(COMG.EQ.'y') OMG = .FALSE.
      IF(COMG.EQ.'N') OMG = .TRUE.
      IF(COMG.EQ.'n') OMG = .TRUE.
      read(4,*) CKSTAR                 ! FLAG KSTAR decay yes KSTAR DECAY
      IF(CKSTAR.EQ.'Y') KSTAR = .FALSE.
      IF(CKSTAR.EQ.'y') KSTAR = .FALSE.
      IF(CKSTAR.EQ.'N') KSTAR = .TRUE.
      IF(CKSTAR.EQ.'n') KSTAR = .TRUE.
      read(4,*) CPHI                   ! FLAG PHI decay yes PHI DECAY
      IF(CPHI.EQ.'Y') PHI = .FALSE.
      IF(CPHI.EQ.'y') PHI = .FALSE.
      IF(CPHI.EQ.'N') PHI = .TRUE.
      IF(CPHI.EQ.'n') PHI = .TRUE.
      read(4,*) CDELTA                 ! FLAG DELTA decay yes DELTA DECAY
      IF(CDELTA.EQ.'Y') DELTA = .FALSE.
      IF(CDELTA.EQ.'y') DELTA = .FALSE.
      IF(CDELTA.EQ.'N') DELTA = .TRUE.
      IF(CDELTA.EQ.'n') DELTA = .TRUE.
      read(4,*) CJPSI                 ! FLAG JPSI decay yes JPSI DECAY
      IF(CJPSI.EQ.'Y') JPSI = .FALSE.
      IF(CJPSI.EQ.'y') JPSI = .FALSE.
      IF(CJPSI.EQ.'N') JPSI = .TRUE.
      IF(CJPSI.EQ.'n') JPSI = .TRUE.
      read(4,*) CD0                   ! FLAG JPSI decay yes D0 DECAY
      IF(CJPSI.EQ.'Y') D0 = .FALSE.
      IF(CJPSI.EQ.'y') D0 = .FALSE.
      IF(CJPSI.EQ.'N') D0 = .TRUE.
      IF(CJPSI.EQ.'n') D0 = .TRUE.
        CALL SETDKY(.FALSE.)
        CALL READIN(IFL)
        CALL IDGEN
      IF(NOFILE) CLOSE(1)
      read(4,*) n_pid_type             ! No. of Geant PID types to include
      read(4,*) model_type             ! Distribution model type (see
CCC                                    ! Function dNdpty for explanation).
      read(4,*) reac_plane_cntrl       ! Reaction plane control option (1-4)
      read(4,*) PSIr_mean,PSIr_stdev   ! Reaction plane angle mean and std.
CCC                                    ! dev., both are in degrees.
      read(4,*) MultFac_mean,MultFac_stdev ! Mult scaling factor mean,stdev.
      read(4,*) etaf_mean, etaf_stdev, n_stdev_etaf
                                       ! radial flow rapidity.
      read(4,*) pt_cut_min,pt_cut_max  ! Min/Max pt range in GeV/c
      read(4,*) eta_cut_min,eta_cut_max ! Min/Max pseudorapidity range
      read(4,*) phi_cut_min,phi_cut_max ! Min/Max azimuthal angular range (deg)
      read(4,*) n_stdev_mult            ! No.(+/-) standard deviation range
CCC                                     ! for multiplicity
      read(4,*) n_stdev_temp            ! No.(+/-) st.dev. range for Temp.
      read(4,*) n_stdev_sigma           ! No.(+/-) st.dev. range for rapidity
CCC                                     ! width, sigma.
      read(4,*) n_stdev_expvel          ! No.(+/-) st.dev. range for expansion
CCC                                     ! velocity.
      read(4,*) n_stdev_PSIr            ! No.(+/-) st.dev. range for PSIr
      read(4,*) n_stdev_Vn              ! No.(+/-) st.dev. range for anisotropic
CCC                                     ! flow parameters Vn.
      read(4,*) n_stdev_MultFac         ! No.(+/-) st.dev. range for multipli-
CCC                                     ! city scaling factor for all PIDs.
      read(4,*) n_integ_pts             ! No. of integration mesh points to use
CCC                                     ! for random parameter fluctuations.
      read(4,*) n_scan_pts              ! No. of pt and eta mesh points to use
CCC                                     ! in scan for maximum value of dN/dpt*dy

CCC   Check Validity and Consistency of Input Parameters so far:

      if(n_pid_type .le. 0) n_pid_type = 1
      if(pt_cut_min .gt. pt_cut_max) then
         write(8,40) pt_cut_min,pt_cut_max
         STOP
      end if
      if(eta_cut_min .gt. eta_cut_max) then
         write(8,41) eta_cut_min,eta_cut_max
         STOP
      end if     
      if(phi_cut_min .gt. phi_cut_max) then
         write(8,42) phi_cut_min,phi_cut_max
         STOP
      end if
40    Format(//10x,'pt_cut_min = ',F7.4,' gt max = ',F7.4,' -STOP')
41    Format(//10x,'eta_cut_min = ',F7.4,' gt max = ',F7.4,' -STOP')
42    Format(//10x,'phi_cut_min = ',F7.4,' gt max = ',F7.4,' -STOP')
      if(n_stdev_mult   .lt. 0.0) n_stdev_mult   = 1.0
      if(n_stdev_temp   .lt. 0.0) n_stdev_temp   = 1.0
      if(n_stdev_sigma  .lt. 0.0) n_stdev_sigma  = 1.0
      if(n_stdev_expvel .lt. 0.0) n_stdev_expvel = 1.0
      if(n_stdev_PSIr   .lt. 0.0) n_stdev_PSIr   = 1.0
      if(n_stdev_Vn     .lt. 0.0) n_stdev_Vn     = 1.0
      if(n_stdev_MultFac .lt. 0.0) n_stdev_MultFac = 1.0
      if(n_integ_pts .le. 0) n_integ_pts = 10
      if(n_scan_pts  .le. 0) n_scan_pts  = 10
      if(n_pid_type .gt. npid) then
         write(8,10) n_pid_type, npid
10       Format(//10x,'No. requested PID types = ',I7,
     1   ', exceeds maximum of ',I7,'; reset')
         n_pid_type = npid
      end if
      if(model_type .lt. 0 .or. model_type .gt. 6) then
         write(8,11) model_type
11       Format(/10x,'model_type = ',I5,' is not allowed; STOP')
         STOP
      end if
      if(n_integ_pts .gt. nmax_integ) then
         write(8,12) n_integ_pts, nmax_integ
12       Format(/10x,'No. integ. pts = ',I7,
     1   ', exceeds maximum of ',I7,'; reset')
         n_integ_pts = nmax_integ
      end if
      if(reac_plane_cntrl .lt. 1 .OR. reac_plane_cntrl .gt. 4) then
         write(8,*) 'reac_plane_cntrl = ',reac_plane_cntrl,'-STOP'
         STOP
      end if

CCC   Force the reaction plane angle (mean value) to be within the 
CCC   range 0 to 360 deg.
      PSIr_mean = PSIr_mean - 360.0*float(int(PSIr_mean/360.0))
      if(PSIr_mean .lt. 0.0) PSIr_mean = PSIr_mean + 360.0
      if(PSIr_stdev .lt. 0.0) then
         write(8,*) 'Reaction plane angle Std. dev. is < 0.0 - STOP'
         STOP
      end if

CCC   Check the multiplicity scaling factor input parameters:
      if(MultFac_mean.lt.0.0 .or. MultFac_stdev.lt.0.0) then
         write(8,48) MultFac_mean, MultFac_stdev
48       Format(//10x,'Multiplicity scaling factor mean or stdev = ',
     1   2F7.4,' - not valid - STOP')
         STOP
      end if

CCC   Check the transverse rapidity factor input parameters:
      if(etaf_mean.lt.0.0 .or. etaf_stdev.lt.0.0
     1       .or. n_stdev_etaf.lt.0) then
         write(8,49) etaf_mean, etaf_stdev, n_stdev_etaf
49       Format(//10x,'transverse rapidity mean or stdev or nstdev= ',
     1   3F7.4,' - not valid - STOP')
         STOP
      end if

CCC   FOR MODEL_TYPE = 1,2,3 or 4; 
CCC   Repeat the following lines of input for each particle ID type:
     
      IF(model_type.ge.1 .and. model_type.le.4) then

      do pid = 1,n_pid_type

         read(4,*) gpid(pid)            ! Geant Particle ID code number

         read(4,*) mult_mean(pid), mult_variance_control(pid)
CCC      Mean multiplicity and variance control where:
CCC           mult_variance_control = 0 for no variance in multiplicity
CCC           mult_variance_control = 1 to allow Poisson distribution for 
CCC                                     particle multiplicities for all events.

         read(4,*) Temp_mean(pid), Temp_stdev(pid)
CCC      Temperature parameter mean (in GeV) and standard deviation (Gaussian
CCC      distribution assumed).

         read(4,*) sigma_mean(pid), sigma_stdev(pid)
CCC      Rapidity distribution width (sigma) parameter mean and standard
CCC      deviation (Gaussian distribution assumed).

         read(4,*) expvel_mean(pid), expvel_stdev(pid)
CCC      S. Pratt expansion velocity (in units of c) mean and standard
CCC      deviation (Gaussian distribution assumed).

         do i = 1,nflowterms
            read(4,*) (Vn_mean(i,k,pid) ,k=1,4)
            read(4,*) (Vn_stdev(i,k,pid),k=1,4)
         end do
CCC      Anisotropic flow Fourier component coefficients specifying the
CCC      azimuthal (phi angle) dependence as defined in Phys. Rev. C58,
CCC      1671 (1998), Eq.(1); 'nflowterms' number of parameters are
CCC      allowed (see file 'Parameter_values.inc' where this is currently
CCC      set to 6); these are the mean and Gaussian std. dev. to be used
CCC      if random, Gaussian variation in the Vn values from event-to-event
CCC      are selected (via reac_plane_cntrl).
CCC      The flow parameters are pt and y dependent; see Function Vn_pt_y
CCC      for the full definitions.

CCC      Check Validity and Consistency of Input Parameters

         if(Temp_mean(pid).le.0.0 .or. Temp_stdev(pid).lt.0.0) then
         write(8,45) pid,Temp_mean(pid),Temp_stdev(pid)
45       Format(//10x,'For pid # ',I7,', Temp_mean or Temp_stdev= ',
     1   2F7.4,' - not valid -STOP')
         STOP
         end if
         if(sigma_mean(pid).le.0.0 .or. sigma_stdev(pid).lt.0.0) then
         write(8,46) pid,sigma_mean(pid),sigma_stdev(pid)
46       Format(//10x,'For pid # ',I7,', sigma_mean or sigma_stdev= ',
     1   2F7.4,' - not valid -STOP')
         STOP
         end if
         if(expvel_mean(pid).lt.0.0.or.expvel_stdev(pid).lt.0.0) then
         write(8,47) pid,expvel_mean(pid),expvel_stdev(pid)
47       Format(//10x,'For pid #',I7,',expvel_mean or expvel_stdev=',
     1   2F7.4,' - not valid -STOP')
         STOP
         end if
         do k = 1,4
            do i = 1,nflowterms
               if(Vn_stdev(i,k,pid) .lt. 0.0) then
                  write(8,*) 'For pid#,term#,k= ',pid,i,k
                  write(8,*) 'Vn_stdev = ',Vn_stdev(i,k,pid)
                  write(8,*) 'which is not valid, must be => 0, STOP'
                  STOP
               end if
            end do
         end do

      end do  !  End of loop over of Particle ID types.

      ELSE IF (model_type .eq. 5) then

CCC      Input for Factorized pt, eta bin-by-bin distribution:

         do pid = 1,n_pid_type
            read(4,*) gpid(pid)
            read(4,*) mult_mean(pid), mult_variance_control(pid)
            read(4,*) pt_start(pid), eta_start(pid)
            read(4,*) n_pt_bins(pid), n_eta_bins(pid)
            do i = 1,n_pt_bins(pid)
               read(4,*) delta_pt(pid,i), pt_bin(pid,i)
            end do
            do i = 1,n_eta_bins(pid)
               read(4,*) delta_eta(pid,i), eta_bin(pid,i)
            end do

         do i = 1,nflowterms
            read(4,*) (Vn_mean(i,k,pid) ,k=1,4)
            read(4,*) (Vn_stdev(i,k,pid),k=1,4)
         end do
CCC      Anisotropic flow Fourier component coefficients specifying the
CCC      azimuthal (phi angle) dependence as defined in Phys. Rev. C58,
CCC      1671 (1998), Eq.(1); 'nflowterms' number of parameters are
CCC      allowed (see file 'Parameter_values.inc' where this is currently
CCC      set to 6); these are the mean and Gaussian std. dev. to be used
CCC      if random, Gaussian variation in the Vn values from event-to-event
CCC      are selected (via reac_plane_cntrl).
CCC      The flow parameters are pt and y dependent; see Function Vn_pt_y
CCC      for the full definitions.

CCC      Evaluate grid and find maximum values of pt and eta for input bins:

            pt_stop(pid) = pt_start(pid)
            do i = 1,n_pt_bins(pid)
            pt_stop(pid) = pt_stop(pid) + delta_pt(pid,i)
            pt_bin_mesh(pid,i) = pt_stop(pid)
            end do
            eta_stop(pid) = eta_start(pid)
            do i = 1,n_eta_bins(pid)
            eta_stop(pid) = eta_stop(pid) + delta_eta(pid,i)
            eta_bin_mesh(pid,i) = eta_stop(pid)
            end do

CCC      Check ranges of pt and eta coverage:

            if(pt_cut_min .lt. pt_start(pid)) then
               write(8,50) pt_cut_min,pt_start(pid)
50             Format(//10x,'pt_cut_min = ',F10.7,' .lt. pt_start =',
     1         F10.7,' - STOP')
               STOP
            end if
            if(pt_cut_max .gt. pt_stop(pid)) then
               write(8,51) pt_cut_max,pt_stop(pid)
51             Format(//10x,'pt_cut_max = ',F10.7,' .gt. pt_stop =',
     1         F10.7,' - STOP')
               STOP
            end if
            if(eta_cut_min .lt. eta_start(pid)) then
               write(8,52) eta_cut_min,eta_start(pid)
52             Format(//10x,'eta_cut_min = ',F10.7,'.lt.eta_start =',
     1         F10.7,' - STOP')
               STOP
            end if
            if(eta_cut_max .gt. eta_stop(pid)) then
               write(8,53) eta_cut_max,eta_stop(pid)
53             Format(//10x,'eta_cut_max = ',F10.7,'.gt.eta_stop =',
     1         F10.7,' - STOP')
               STOP
            end if

         do k = 1,4
            do i = 1,nflowterms
               if(Vn_stdev(i,k,pid) .lt. 0.0) then
                  write(8,*) 'For pid#,term#,k= ',pid,i,k
                  write(8,*) 'Vn_stdev = ',Vn_stdev(i,k,pid)
                  write(8,*) 'which is not valid, must be => 0, STOP'
                  STOP
               end if
            end do
         end do

         end do ! End loop over particle ID types.

      ELSE IF (model_type .eq. 6) then

CCC      Input for Full 2D (pt,eta) bin-by-bin distribution:

         do pid = 1,n_pid_type
            read(4,*) gpid(pid)
            read(4,*) mult_mean(pid), mult_variance_control(pid)
            read(4,*) pt_start(pid), eta_start(pid)
            read(4,*) n_pt_bins(pid), n_eta_bins(pid)
            do i = 1,n_pt_bins(pid)
               read(4,*) delta_pt(pid,i)
            end do
            do i = 1,n_eta_bins(pid)
               read(4,*) delta_eta(pid,i)
            end do

CCC      Evaluate grid and find maximum values of pt and eta for input bins:

            pt_stop(pid) = pt_start(pid)
            do i = 1,n_pt_bins(pid)
            pt_stop(pid) = pt_stop(pid) + delta_pt(pid,i)
            pt_bin_mesh(pid,i) = pt_stop(pid)
            end do
            eta_stop(pid) = eta_start(pid)
            do i = 1,n_eta_bins(pid)
            eta_stop(pid) = eta_stop(pid) + delta_eta(pid,i)
            eta_bin_mesh(pid,i) = eta_stop(pid)
            end do

CCC   Load 2D bin-by-bin distribution:

            do i = 1,n_pt_bins(pid)*n_eta_bins(pid)
               read(4,*) iptbin,ietabin,bin_value
               pt_eta_bin(pid,iptbin,ietabin) = bin_value
            end do

         do i = 1,nflowterms
            read(4,*) (Vn_mean(i,k,pid) ,k=1,4)
            read(4,*) (Vn_stdev(i,k,pid),k=1,4)
         end do
CCC      Anisotropic flow Fourier component coefficients specifying the
CCC      azimuthal (phi angle) dependence as defined in Phys. Rev. C58,
CCC      1671 (1998), Eq.(1); 'nflowterms' number of parameters are
CCC      allowed (see file 'Parameter_values.inc' where this is currently
CCC      set to 6); these are the mean and Gaussian std. dev. to be used
CCC      if random, Gaussian variation in the Vn values from event-to-event
CCC      are selected (via reac_plane_cntrl).
CCC      The flow parameters are pt and y dependent; see Function Vn_pt_y
CCC      for the full definitions.

CCC      Check ranges of pt and eta coverage:

            if(pt_cut_min .lt. pt_start(pid)) then
               write(8,50) pt_cut_min,pt_start(pid)
               STOP
            end if
            if(pt_cut_max .gt. pt_stop(pid)) then
               write(8,51) pt_cut_max,pt_stop(pid)
               STOP
            end if
            if(eta_cut_min .lt. eta_start(pid)) then
               write(8,52) eta_cut_min,eta_start(pid)
               STOP
            end if
            if(eta_cut_max .gt. eta_stop(pid)) then
               write(8,53) eta_cut_max,eta_stop(pid)
               STOP
            end if
         do k = 1,4
            do i = 1,nflowterms
               if(Vn_stdev(i,k,pid) .lt. 0.0) then
                  write(8,*) 'For pid#,term#,k= ',pid,i,k
                  write(8,*) 'Vn_stdev = ',Vn_stdev(i,k,pid)
                  write(8,*) 'which is not valid, must be => 0, STOP'
                  STOP
               end if
            end do
         end do

         end do ! End loop over particle ID types.

      END IF !  End of MODEL_TYPE Options input:
      
CCC   Check some more input parameters; Set constants, and load data tables:

      do pid = 1,n_pid_type
      if(gpid(pid).le.0 .or. gpid(pid).gt.200) then
      write(8,43) pid,gpid(pid)
43    Format(//10x,'For pid # ',I7,', gpid = ',I7,' - not valid -STOP')
      STOP
      end if
      if(mult_variance_control(pid) .lt. 0  .or.
     1   mult_variance_control(pid) .gt. 1)
     2   mult_variance_control(pid) = 0
      if(mult_mean(pid) .le. 0) then
      write(8,44) pid,mult_mean(pid)
44    Format(//10x,'For pid # ',I7,', mult_mean= ',I7,
     1   ' - not valid -STOP')
      STOP
      end if
      end do ! End Particle ID input parameter check and verification loop.

      pi = 3.141592654
      rad = 180.0/pi
      Temp_abort  = 0.01
      sigma_abort = 0.01

CCC   Load particle properties array; mass in GeV, etc.:

      Call Particle_prop

CCC   Load log(n!) values into Common/FACFAC/

      Call FACTORIAL

CCC   Set y (rapidity) range, to be used for model_type = 1-4
      if(eta_cut_min .ge. 0.0) then
         y_cut_min = 0.0
      else
         y_cut_min = eta_cut_min
      end if
      if(eta_cut_max .le. 0.0) then
         y_cut_max = 0.0
      else
         y_cut_max = eta_cut_max
      end if

CCC   Obtain integrals of 1D distribution functions of multiplicity
CCC   (Poisson, integer) and parameters (Gaussian, real*4) for the
CCC   particle model distributions, for each particle ID type.
CCC   These integrated quantities are then used with random number
CCC   selection to generate a distribution of multiplicities and
CCC   parameter values for each event in the run.

CCC   Obtain 1D integrals for Gaussian distributions for reaction plane
CCC   angles:

      if(reac_plane_cntrl .eq. 3) then
         if((n_stdev_PSIr*PSIr_stdev).gt. 0.0) then
            PSIr_min = PSIr_mean - n_stdev_PSIr*PSIr_stdev
            PSIr_max = PSIr_mean + n_stdev_PSIr*PSIr_stdev
            Call Gaussian(PSIr_min,PSIr_max,PSIr_mean,PSIr_stdev,
     1           n_integ_pts,integ,xfunc,nmax_integ)
            do i = 1,n_integ_pts + 1
               PSIr_integ_save(i) = integ(i)
               PSIr_xfunc_save(i) = xfunc(i)
            end do
         else
            PSIr_event = PSIr_mean
         end if
      end if

CCC   Obtain 1D integral for Gaussian distribution for the trigger fluctuation
CCC   simulations via the overall multiplicity scaling factor.
      if((n_stdev_MultFac*MultFac_stdev).gt.0.0) then
         Call MinMax(MultFac_mean,MultFac_stdev,n_stdev_MultFac,
     1               MultFac_min,MultFac_max)
         Call Gaussian(MultFac_min,MultFac_max,MultFac_mean,
     1                 MultFac_stdev,n_integ_pts,integ,xfunc,nmax_integ)
         do i = 1,n_integ_pts + 1
            MultFac_integ_save(i) = integ(i)
            MultFac_xfunc_save(i) = xfunc(i)
         end do
      else
         MultFac_event = MultFac_mean
      end if

CCC   Obtain 1D integral for Gaussian distribution for the transverse flow
CCC   simulations.
      if((n_stdev_etaf*etaf_stdev) .gt. 0.0) then
         Call MinMax(etaf_mean,etaf_stdev,n_stdev_etaf,
     1               etaf_min,etaf_max)
         Call Gaussian(etaf_min,etaf_max,etaf_mean,etaf_stdev,
     1                 n_integ_pts,integ,xfunc,nmax_integ)
         do i = 1,n_integ_pts + 1
            etaf_integ_save(i) = integ(i)
            etaf_xfunc_save(i) = xfunc(i)
         end do
      else
         etaf_event = etaf_mean
      end if


      do pid = 1,n_pid_type

         Call Particle_mass(gpid(pid),pid,n_integ_pts)

         if(mult_variance_control(pid) .ne. 0) then
            mult_mean_real = float(mult_mean(pid))
            mult_stdev = sqrt(mult_mean_real)
            Call MinMax(mult_mean_real,mult_stdev,n_stdev_mult,
     1                  mult_min_real,mult_max_real)
            mult_min = int(mult_min_real)
            mult_max = int(mult_max_real + 1)
            n_mult_steps(pid) = mult_max - mult_min + 1
            if((n_mult_steps(pid) + 1) .gt. n_mult_max_steps) then
               write(8,20) n_mult_steps(pid),n_mult_max_steps
20             Format(//10x,'No. steps in multiplicity integral = ',
     1         I7,' + 1, exceeds max no. of ',I7,'; reset')
            mult_min = mult_mean(pid) - (n_mult_max_steps - 1)/2
            mult_max = mult_mean(pid) + (n_mult_max_steps - 1)/2
            n_mult_steps(pid) = mult_max - mult_min + 1
            end if
            if((mult_max + 1) .gt. factorial_max) then
               write(8,30) mult_max, factorial_max
30             Format(//10x,'In Poisson multiplicity distribution,',
     1         ' max = ',I7,', exceeds limit of ',I7,' - STOP')
               STOP
            end if

            Call Poisson(mult_min,mult_max,mult_mean(pid),
     1      n_mult_steps(pid),mult_integ,mult_xfunc,n_mult_max_steps)
            do i = 1,n_mult_steps(pid) + 1
               mult_integ_save(pid,i) = mult_integ(i)
               mult_xfunc_save(pid,i) = mult_xfunc(i)
            end do
         else if (mult_variance_control(pid) .eq. 0) then
            mult_event(pid) = mult_mean(pid)
         end if

         if(model_type .le. 4) then
         if(Temp_stdev(pid) .ne. 0.0) then
            Call MinMax(Temp_mean(pid),Temp_stdev(pid),n_stdev_temp,
     1                  Temp_min, Temp_max)
            Call Gaussian(Temp_min,Temp_max,Temp_mean(pid),
     1         Temp_stdev(pid),n_integ_pts,integ,xfunc,nmax_integ)
            do i = 1,n_integ_pts + 1
               Temp_integ_save(pid,i) = integ(i)
               Temp_xfunc_save(pid,i) = xfunc(i)
            end do
         else if(Temp_stdev(pid) .eq. 0.0) then
            Temp_event(pid) = Temp_mean(pid)
         end if

         if(sigma_stdev(pid) .ne. 0.0) then
            Call MinMax(sigma_mean(pid),sigma_stdev(pid),n_stdev_sigma,
     1                  sigma_min, sigma_max)
            Call Gaussian(sigma_min,sigma_max,sigma_mean(pid),
     1         sigma_stdev(pid),n_integ_pts,integ,xfunc,nmax_integ)
            do i = 1,n_integ_pts + 1
               sigma_integ_save(pid,i) = integ(i)
               sigma_xfunc_save(pid,i) = xfunc(i)
            end do
         else if(sigma_stdev(pid) .eq. 0.0) then
            sigma_event(pid) = sigma_mean(pid)
         end if

         if(expvel_stdev(pid) .ne. 0.0) then
          Call MinMax(expvel_mean(pid),expvel_stdev(pid),n_stdev_expvel,
     1                  expvel_min, expvel_max)
            Call Gaussian(expvel_min,expvel_max,expvel_mean(pid),
     1         expvel_stdev(pid),n_integ_pts,integ,xfunc,nmax_integ)
            do i = 1,n_integ_pts + 1
               expvel_integ_save(pid,i) = integ(i)
               expvel_xfunc_save(pid,i) = xfunc(i)
            end do
         else if(expvel_stdev(pid) .eq. 0.0) then
            expvel_event(pid) = expvel_mean(pid)
         end if 
         end if 

         if(reac_plane_cntrl .gt. 1) then
            do i = 1,nflowterms
             do k = 1,4
               if((n_stdev_Vn*Vn_stdev(i,k,pid)) .gt. 0.0) then
                  Vn_min = Vn_mean(i,k,pid)-n_stdev_Vn*Vn_stdev(i,k,pid)
                  Vn_max = Vn_mean(i,k,pid)+n_stdev_Vn*Vn_stdev(i,k,pid)
                  Call Gaussian(Vn_min,Vn_max,Vn_mean(i,k,pid),
     1            Vn_stdev(i,k,pid),n_integ_pts,integ,xfunc,nmax_integ)
                  if(k.eq.1) then
                  do j = 1,n_integ_pts + 1
                     Vn1_integ_save(i,pid,j) = integ(j)
                     Vn1_xfunc_save(i,pid,j) = xfunc(j)
                  end do
                  else if(k.eq.2) then
                  do j = 1,n_integ_pts + 1
                     Vn2_integ_save(i,pid,j) = integ(j)
                     Vn2_xfunc_save(i,pid,j) = xfunc(j)
                  end do
                  else if(k.eq.3) then
                  do j = 1,n_integ_pts + 1
                     Vn3_integ_save(i,pid,j) = integ(j)
                     Vn3_xfunc_save(i,pid,j) = xfunc(j)
                  end do
                  else if(k.eq.4) then
                  do j = 1,n_integ_pts + 1
                     Vn4_integ_save(i,pid,j) = integ(j)
                     Vn4_xfunc_save(i,pid,j) = xfunc(j)
                  end do
                  end if
               else
                  Vn_event(i,k,pid) = Vn_mean(i,k,pid)
               end if
             end do
            end do
         end if

      end do  !  End of PID Loop:

CCC   Write Run Header Output:

      write(8,200)
      write(8,201) n_events,n_pid_type
      if(model_type .eq. 1) write(8,202)
      if(model_type .eq. 2) write(8,203)
      if(model_type .eq. 3) write(8,204)
      if(model_type .eq. 4) write(8,205)
      if(model_type .eq. 5) write(8,2051)
      if(model_type .eq. 6) write(8,2052)
      write(8,2053) reac_plane_cntrl
      write(8,2054) PSIr_mean, PSIr_stdev
      write(8,2055) MultFac_mean,MultFac_stdev
      write(8,2056) etaf_mean,etaf_stdev, n_stdev_etaf
      write(8,206) pt_cut_min, pt_cut_max
      write(8,207) eta_cut_min, eta_cut_max
      write(8,2071) y_cut_min,y_cut_max
      write(8,208) phi_cut_min, phi_cut_max
      write(8,209) n_stdev_mult,n_stdev_temp,n_stdev_sigma,
     1             n_stdev_expvel
      write(8,2091) n_stdev_PSIr, n_stdev_Vn
      write(8,2092) n_stdev_MultFac
      write(8,210) n_integ_pts
      write(8,211) n_scan_pts
      write(8,213) (gpid(pid),pid = 1,n_pid_type)
      write(8,214) (mult_mean(pid),pid = 1,n_pid_type)
      write(8,215) (mult_variance_control(pid),pid = 1,n_pid_type)
      if(model_type .le. 4) then
         write(8,216) (Temp_mean(pid),pid = 1,n_pid_type)
         write(8,217) (Temp_stdev(pid),pid = 1,n_pid_type)
         write(8,218) (sigma_mean(pid),pid = 1,n_pid_type)
         write(8,219) (sigma_stdev(pid),pid = 1,n_pid_type)
         write(8,220) (expvel_mean(pid),pid = 1,n_pid_type)
         write(8,221) (expvel_stdev(pid),pid = 1,n_pid_type)
      else if(model_type .ge. 5) then
         write(8,222) (n_pt_bins(pid),pid = 1,n_pid_type)
         write(8,223) (n_eta_bins(pid),pid = 1,n_pid_type)
         write(8,224) (pt_start(pid),pid = 1,n_pid_type)
         write(8,225) (pt_stop(pid),pid = 1,n_pid_type)
         write(8,226) (eta_start(pid),pid = 1,n_pid_type)
         write(8,227) (eta_stop(pid),pid = 1,n_pid_type)
      end if
CCC   Print out flow parameters:
      do pid = 1,n_pid_type
         do i = 1,nflowterms
            write(8,2271) pid,i,(Vn_mean(i,k,pid),k=1,4)
            write(8,2272) pid,i,(Vn_stdev(i,k,pid),k=1,4)
         end do
      end do

200   Format('***  Multiplicity Generator Run Header ***')
201   Format('*    Number of events = ',I7,'; number of Particle ID',
     1       ' types = ',I5)
202   Format('* Factorized mt exponential, Gaussian rapidity model')
203   Format('* Pratt non-expanding, spherical thermal source model')
204   Format('* Bertsch non-expanding spherical thermal source model')
205   Format('* Pratt spherically expanding, thermally equilibrated ',
     1        'source model')
2051  Format('* Factorized pt,eta bin-by-bin Distribution')
2052  Format('* Full 2D pt,eta bin-by-bin Distribution')
2053  Format('* Reaction plane control = ',I5)
2054  Format('* Reaction plane angles - mean and std.dev. = ',2G12.5,
     1          ' (deg)')
2055  Format('* Multiplicity Scaling Factor - mean and std.dev = ',
     1       2G12.5)
2056  Format('* Transverse Rapidity Factor - mean, std.dev and',
     1       ' nstd.dev = ', 3G12.5)
206   Format('* Min, Max range in pt              = ', 2G12.5)
207   Format('* Min, Max range in pseudorapidity  = ', 2G12.5)
2071  Format('* Min, Max range in rapdity + cush  = ', 2G12.5)
208   Format('* Min, Max range in azimuthal angle = ', 2G12.5)
209   Format('* No. std. dev. range used for mult and parameters = ',
     1       5F5.2)
2091  Format('* No. std. dev. range for PSIr, Vn  = ', 2G12.5)
2092  Format('* No. std. dev. range for MultFac   = ',G12.5)
210   Format('* No. integration points for parameter variance = ',
     1       I6)
211   Format('* No. of dN/dp(pt,y) scanning grid points to find ',
     1   'maximum = ', I6)
213   Format('* Geant PID:          ',10I7)
214   Format('* Mean Multiplicity:  ',10I7)
215   Format('* Mult. Var. Control: ',10I7)
216   Format('* Mean Temperature:   ',10F7.4)
217   Format('* Std. Dev. Temp:     ',10F7.4)
218   Format('* Mean Rap. sigma:    ',10F7.4)
219   Format('* Std. Dev. y-sigma:  ',10F7.4)
220   Format('* Mean expansion vel: ',10F7.4)
221   Format('* Std. Dev. exp. vel: ',10F7.4)
222   Format('* No. input pt bins:  ',10I7)
223   Format('* No. input eta bins: ',10I7)
224   Format('* Input pt start:     ',10F7.4)
225   Format('* Input pt stop:      ',10F7.4)
226   Format('* Input eta start:    ',10F7.4)
227   Format('* Input eta stop:     ',10F7.4)
2271  Format('* Anisotropy Vn mean (pid=',I2,',n=',I1,'):',4G12.5)
2272  Format('* Anisotropy Vn std. (pid=',I2,',n=',I1,'):',4G12.5)

CCC  END Run Header Output
         IFIRST=1
      ENDIF

C**************************************************************
C                                                            **
C                     START EVENT LOOP                       **
C                                                            **
C**************************************************************
7777  CALL DATIME(I,J)
      VSSHEP(2) = VSSHEP(2) + 1
      WRITE(6,881) PSSHEP(5), VSSHEP(2)
 881  FORMAT(' I GOT TO HERE',2F10.5)
      IF(psshep(5).LT.vsshep(2)) then
         CALL HEPEnd('  ')
         STOP
      ENDIF
CCC   Compute the Reaction plane angle for this event:
         if(reac_plane_cntrl .eq. 1) then
            PSIr_event = 0.0
         else if(reac_plane_cntrl .eq. 2) then
            PSIr_event = PSIr_mean
         else if(reac_plane_cntrl .eq. 3) then
            if((n_stdev_PSIr*PSIr_stdev) .gt. 0.0) then
               do i = 1,n_integ_pts + 1
                  integ(i) = PSIr_integ_save(i)
                  xfunc(i) = PSIr_xfunc_save(i)
               end do
               Call LAGRNG(ranf(),integ,PSIr_event,xfunc,
     1              n_integ_pts+1,1,5,n_integ_pts+1,1)
CCC         Ensure that the randomly selected reaction plane angle
CCC         is within the 0 to 360 deg range.
               PSIr_event=PSIr_event-360.0*float(int(PSIr_event/360.0))
               if(PSIr_event .lt. 0.0) PSIr_event = PSIr_event + 360.0
            end if
CCC NOTE: If PSIr_stdev=0.0, PSIr_event already calculated (see preceding)
         else if(reac_plane_cntrl .eq. 4) then
            PSIr_event = 360.0*ranf()
         else
            PSIr_event = 0.0
         end if

CCC   Compute the multiplicity scaling factor for simulating trigger
CCC   fluctuations for this event:
         if((n_stdev_MultFac*MultFac_stdev).gt.0.0) then
            do i = 1,n_integ_pts + 1
               integ(i) = MultFac_integ_save(i)
               xfunc(i) = MultFac_xfunc_save(i)
            end do
            Call LAGRNG(ranf(),integ,MultFac_event,xfunc,
     1         n_integ_pts+1,1,5,n_integ_pts+1,1)
         end if

         do pid = 1,n_pid_type
            if(mult_variance_control(pid) .ne. 0) then
               do i = 1,n_mult_steps(pid) + 1
               mult_integ(i) = mult_integ_save(pid,i)
               mult_xfunc(i) = mult_xfunc_save(pid,i)
               end do
               Call LAGRNG(ranf(),mult_integ,mult_event_real,
     1         mult_xfunc,n_mult_steps(pid)+1,1,5,
     2         n_mult_steps(pid)+1,1)
               mult_event(pid) = mult_event_real
            else if(mult_variance_control(pid) .eq. 0) then
               mult_event(pid) = mult_mean(pid)
            end if
            mult_event(pid) = int(MultFac_event*float(mult_event(pid))
     1                            + 0.5)
CCC   Check each multiplicity wrt upper array size limit:
            if(mult_event(pid).gt.factorial_max) then
               write(8,*) 'For event#',ievent,'and pid#',pid,
     1            'multiplicity=',mult_event(pid),
     2            'which is > array size (factorial_max=',
     3             factorial_max,')-STOP'
               STOP
            end if

         if(model_type .le. 4) then

            if(Temp_stdev(pid) .ne. 0.0) then
               do i = 1,n_integ_pts + 1
               integ(i) = Temp_integ_save(pid,i)
               xfunc(i) = Temp_xfunc_save(pid,i)
               end do
               Call LAGRNG(ranf(),integ,Temp_event(pid),xfunc,
     1              n_integ_pts+1,1,5,n_integ_pts+1,1)
            end if

            if(sigma_stdev(pid) .ne. 0.0) then
               do i = 1,n_integ_pts + 1
               integ(i) = sigma_integ_save(pid,i)
               xfunc(i) = sigma_xfunc_save(pid,i)
               end do
               Call LAGRNG(ranf(),integ,sigma_event(pid),xfunc,
     1              n_integ_pts+1,1,5,n_integ_pts+1,1)
            end if

            if(expvel_stdev(pid) .ne. 0.0) then
               do i = 1,n_integ_pts + 1
               integ(i) = expvel_integ_save(pid,i)
               xfunc(i) = expvel_xfunc_save(pid,i)
               end do
               Call LAGRNG(ranf(),integ,expvel_event(pid),xfunc,
     1              n_integ_pts+1,1,5,n_integ_pts+1,1)
            end if

         end if
         
         if(reac_plane_cntrl .gt. 1) then
            do i = 1,nflowterms
             do k = 1,4
               if((n_stdev_Vn*Vn_stdev(i,k,pid)) .gt. 0.0) then
                if(k.eq.1) then
                  do j = 1,n_integ_pts + 1
                     integ(j) = Vn1_integ_save(i,pid,j)
                     xfunc(j) = Vn1_xfunc_save(i,pid,j)
                  end do
                else if (k.eq.2) then
                  do j = 1,n_integ_pts + 1
                     integ(j) = Vn2_integ_save(i,pid,j)
                     xfunc(j) = Vn2_xfunc_save(i,pid,j)
                  end do
                else if (k.eq.3) then
                  do j = 1,n_integ_pts + 1
                     integ(j) = Vn3_integ_save(i,pid,j)
                     xfunc(j) = Vn3_xfunc_save(i,pid,j)
                  end do
                else if (k.eq.4) then
                  do j = 1,n_integ_pts + 1
                     integ(j) = Vn4_integ_save(i,pid,j)
                     xfunc(j) = Vn4_xfunc_save(i,pid,j)
                  end do
                end if
                Call LAGRNG(ranf(),integ,Vn_event(i,k,pid),xfunc,
     1                 n_integ_pts+1,1,5,n_integ_pts+1,1)
               end if ! (For n_stdev_Vn*Vn_stdev=0, already have Vn_event)
             end do
            end do
         end if

         end do !  End Particle ID setup Loop

         event_abort = 1

         if(model_type .le. 4) then
CCC      Check validity of Temperature and sigma parameters:
         do pid = 1,n_pid_type
            if(Temp_event(pid) .le. Temp_abort) event_abort = -86
            if(sigma_event(pid).le.sigma_abort) event_abort = -86
         end do
         end if

         if(event_abort .eq. 1) then

            total_mult = 0
            do pid = 1,n_pid_type
            total_mult = total_mult + mult_event(pid)
            end do
            n_vertices = 0
            status_abort = 1
            do pid = 1,n_pid_type
CCC   Load Anisotropic flow parameters for this event# and PID type
CCC   into temporary array;
               do i = 1,nflowterms
                  do k = 1,4
                     Vn_event_tmp(i,k) = Vn_event(i,k,pid)
                  end do
               end do
CCC   For the specified Geant PID, multiplicity, model type, temperature,
CCC   rapidity width (sigma), and expansion velocity parameter, generate
CCC   random track list:

            Call track_gen(gpid(pid),mult_event(pid),model_type,
     1      Temp_event(pid),sigma_event(pid),expvel_event(pid),
     2      reac_plane_cntrl,PSIr_event,Vn_event_tmp,
     3      pt_cut_min,pt_cut_max,eta_cut_min,eta_cut_max,
     4      y_cut_min,y_cut_max,
     5      phi_cut_min,phi_cut_max,n_scan_pts,rad,pid,status,
     6      n_integ_pts)
            if(status .eq. -86) status_abort = -86
            end do
         end if

C djp Feb 14, 2003
C     Put Lanny's transverse flow code here.

CCC   Select random transverse colective flow velocity:
          if((n_stdev_etaf*etaf_stdev) .gt. 0.0) then
              do i = 1,n_integ_pts + 1
                  integ(i) = etaf_integ_save(i)
                  xfunc(i) = etaf_xfunc_save(i)
              end do
              Call LAGRNG(ranf(),integ,etaf_event,xfunc,
     1                    n_integ_pts+1,1,5,n_integ_pts+1,1)
          end if
CCC   Boost every particle in event.
          do pid = 1,n_pid_type
              do i = 1,mult_event(pid)
                  px   = pout(pid,1,i)
                  py   = pout(pid,2,i)
                  pz   = pout(pid,3,i)
                  mass = pout(pid,4,i)
                  betatr = tanh(etaf_event*sqrt(ranf()))
                  theta  = 2.0*pi*ranf()
                  Etot   = sqrt(mass*mass + px*px + py*py + pz*pz)
                  ux  = px/Etot
                  uy  = py/Etot
                  uz  = pz/Etot
                  Call boost(betatr,ux,uy,uz,theta,mass,
     1                       pxnew,pynew,pznew)
                  pout(pid,1,i) = pxnew
                  pout(pid,2,i) = pynew
                  pout(pid,3,i) = pznew
              end do
          end do


         if(event_abort.eq.1 .and. status_abort.eq.1) then
CCC Event Header Output:
           write(8,2301) PSIr_event
           write(8,2302) MultFac_event
           write(8,2303) etaf_event
           write(8,213) (gpid(pid),pid = 1,n_pid_type)
           write(8,231) (mult_event(pid),pid = 1,n_pid_type)
           if(model_type .le. 4) then
              write(8,232) (Temp_event(pid),pid = 1,n_pid_type)
              write(8,233) (sigma_event(pid),pid = 1,n_pid_type)
              write(8,234) (expvel_event(pid),pid = 1,n_pid_type)
           end if

230   Format(/'***  Next Event:  No. ',I7,'; Total # tracks = ',I10)
2301  Format('* Reaction plane angle = ',G12.5,' (deg)')
2302  Format('* Multiplicity Scaling Factor = ',G12.5)
2303  Format('* Max Transverse Rapidity =  ',10F7.4)
231   Format('* Multiplicity:         ',10I7)
232   Format('* Temperature:          ',10F7.4)
233   Format('* Rapidity Dist. sigma: ',10F7.4)
234   Format('* Expansion Velocity:   ',10F7.4)

CCC   Print out flow parameters for this event:
           do pid = 1,n_pid_type
              do i = 1,nflowterms
                 write(8,2341) pid,i,(Vn_event(i,k,pid),k=1,4)
              end do
           end do
2341  Format('* Anisotropy Vn (pid=',I2,',n=',I1,'):',4G12.5)

           write(8,235) ievent,total_mult,n_vertices
235        Format('EVENT:',3x,3(1x,I6))
           write(8,236)
           write(8,237)
236        Format('*** Tracks for this event ***')
237        Format('* Geant PID #      px          py          pz')
CCC   End Event Header Output:

CCC   Output track kinematics for ievent and pid:
        II = 0
        DO J = 1, 4
        vhep(j)  = 0.0               !  production vertex (mm) and time (mm/c) 
        END DO
           do pid = 1,n_pid_type
           gntsav=gpid(pid)
            do i = 1,mult_event(pid)
        II = II + 1
        isttemp=1
        kdahep(1)  = 0           !  pointers to position of 
        kdahep(2)  = 0           !  the first/last daughter  
        idhep(II)=idtran(gntsav)
        IF(MOPI0) THEN
        isttemp=1
        ELSE
          IF(idhep(II).eq.111) then
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           istda(k)=1
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           enddo
           enddo
        ENDIF
        ENDIF
        IF(MOETA) THEN
        isttemp=1
        ELSE
          IF(idhep(II).eq.221) then
           notbk=.true.
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           istda(k)=1
           id(k)=iddhep(ident(K))
           ida1(k)=0
           ida2(k)=0
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           enddo
           enddo
        ENDIF
        IF(MOPI0) THEN
        ELSE
          IF(notbk) then
          notbk=.false.
          NEND=NPTCLSAV
          do kk=2,NEND
             IF(id(kk).eq.111) then
                nptcl=1
                ident(1)=110
                istda(kk)=3
                do j=1,5
                pptcl(J,1)=pp(J,kk)
                enddo
                CALL DECAY(NPTCL)
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=II+kk-1
                ida1(kk)=II+NPTCLSAV-NPTCL+1
                ida2(kk)=II+NPTCLSAV-1
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
          enddo
        ENDIF
        ENDIF
        ENDIF
        IF(RHO) THEN
        isttemp=1
        ELSE
        IF(idhep(II).eq.113.or.iabs(idhep(II)).eq.10221
     1  .or.iabs(idhep(II)).eq.213) then
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           IF(idhep(II).EQ.10221) ident(1)=111
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(MOPI0) THEN
           ELSE
             IF(id(k).eq.111) then
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(MOPI0) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=110
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        ENDIF
        IF(D0) THEN
        isttemp=1
        ELSE
         IF(IABS(idhep(II)).eq.421) then
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           enddo
           enddo
        ENDIF
      ENDIF
        IF(OMG) THEN
        isttemp=1
        ELSE
         IF(idhep(II).eq.223) then
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(MOPI0) THEN
           ELSE
             IF(id(k).eq.111) then
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(MOPI0) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=110
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        ENDIF
        IF(ETAP) THEN
        isttemp=1
        ELSE
         IF(idhep(II).eq.331) then
           notbk=.true.
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(MOETA) THEN
           ELSE
             IF(id(k).eq.221) then
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(MOETA) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=220
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        IF(MOPI0) THEN
        ELSE
          IF(notbk) then
          notbk=.false.
          NEND=NPTCLSAV
          do kk=2,NEND
             IF(id(kk).eq.111) then
                nptcl=1
                ident(1)=110
                istda(kk)=3
                do j=1,5
                pptcl(J,1)=pp(J,kk)
                enddo
                CALL DECAY(NPTCL)
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=II+kk-1
                ida1(kk)=II+NPTCLSAV-NPTCL+1
                ida2(kk)=II+NPTCLSAV-1
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
          enddo
        ENDIF
        ENDIF
        ENDIF
        IF(PHI) THEN
        isttemp=1
        ELSE
         IF(idhep(II).eq.333) then
           notbk=.true.
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           IF(id(k).eq.311) id(k)=130
           IF(id(k).eq.-311) id(k)=310
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(MOETA) THEN
           ELSE
             IF(id(k).eq.221) then
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(MOETA) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=220
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        IF(MOPI0) THEN
        ELSE
          IF(notbk) then
          notbk=.false.
          NEND=NPTCLSAV
          do kk=2,NEND
             IF(id(kk).eq.111) then
                nptcl=1
                ident(1)=110
                istda(kk)=3
                do j=1,5
                pptcl(J,1)=pp(J,kk)
                enddo
                CALL DECAY(NPTCL)
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=II+kk-1
                ida1(kk)=II+NPTCLSAV-NPTCL+1
                ida2(kk)=II+NPTCLSAV-1
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
          enddo
        ENDIF
        ENDIF
        ENDIF
        IF(JPSI) THEN
        isttemp=1
        ELSE
         IF(idhep(II).eq.443) then
           notbk=.true.
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(RHO) THEN
           ELSE
             IF(id(k).eq.113.or.iabs(id(k)).eq.213) then
               idsav=idisa(id(k))
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(RHO) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=idsav
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        IF(MOPI0) THEN
        ELSE
          IF(notbk) then
          notbk=.false.
          NEND=NPTCLSAV
          do kk=2,NEND
             IF(id(kk).eq.111) then
                nptcl=1
                ident(1)=110
                istda(kk)=3
                do j=1,5
                pptcl(J,1)=pp(J,kk)
                enddo
                CALL DECAY(NPTCL)
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=II+kk-1
                ida1(kk)=II+NPTCLSAV-NPTCL+1
                ida2(kk)=II+NPTCLSAV-1
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
          enddo
        ENDIF
        ENDIF
        ENDIF
        IF(DELTA) THEN
        isttemp=1
        ELSE
          IF(iabs(idhep(II)).eq.1114.or.iabs(idhep(II)).eq.2114.or
     1    .iabs(idhep(II)).eq.2214.or.iabs(idhep(II)).eq.2224) then
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(MOPI0) THEN
           ELSE
             IF(id(k).eq.111) then
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(MOPI0) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=110
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        ENDIF
        IF(idhep(II).eq.313) then
              if(ranf().gt..5) idhep(II)=-313
        ENDIF
        IF(KSTAR) THEN
        isttemp=1
        ELSE
         IF(iabs(idhep(II)).eq.313.or.iabs(idhep(II)).eq.323) then
           ppnxlv(5)=0.0
           isttemp=3
           nptcl=1
           pptcl(1,1)=pout(pid,1,i)
           pptcl(2,1)=pout(pid,2,i)
           pptcl(3,1)=pout(pid,3,i)
           pptcl(5,1)=pout(pid,4,i)
           pptcl(4,1)=sqrt(pptcl(1,1)**2+pptcl(2,1)**2+pptcl(3,1)**2
     1     +pptcl(5,1)**2)
           ident(1)=idisa(idhep(II))
           CALL DECAY(NPTCL)
           NPTCLSAV=NPTCL
           kdahep(1)=II+1
           kdahep(2)=II+NPTCL-1
           do k=2,NPTCL
           ida1(k)=0
           ida2(k)=0
           id(k)=iddhep(ident(K))
           istda(k)=1
           imo1(k)=II
           IF(IABS(id(k)).EQ.311) THEN
              id(k)=310
              IF(ranf().gt..5) id(k)=130
           ENDIF
           do j=1,5
           pp(j,k)=pptcl(j,k)
           IF(MOPI0) THEN
           ELSE
             IF(id(k).eq.111) then
               ppnxlv(j)=pptcl(j,k)
               ida1(k)=II+NPTCL
               ksav=k
               istda(k)=3
               mopss=II+k-1
             ENDIF
           ENDIF
           enddo
           enddo
           IF(MOPI0) THEN
           ELSE
             IF(ppnxlv(5).gt..001) then
                nptcl=1
                pptcl(1,1)=ppnxlv(1)
                pptcl(2,1)=ppnxlv(2)
                pptcl(3,1)=ppnxlv(3)
                pptcl(4,1)=ppnxlv(4)
                pptcl(5,1)=ppnxlv(5)
                ident(1)=110
                CALL DECAY(NPTCL)
                ida2(ksav)=ida1(ksav)+NPTCL-2
                NPTCLSAV=NPTCLSAV+NPTCL-1
                do k=2,NPTCL
                id(k+NPTCLSAV-NPTCL)=iddhep(ident(k))
                istda(k+NPTCLSAV-NPTCL)=1
                imo1(k+NPTCLSAV-NPTCL)=mopss
                ida1(k+NPTCLSAV-NPTCL)=0
                ida2(k+NPTCLSAV-NPTCL)=0
                do j=1,5
                pp(j,k+NPTCLSAV-NPTCL)=pptcl(j,k)
                enddo
                enddo
             ENDIF
           ENDIF
         ENDIF
        ENDIF
        phep(1,II) = pout(pid,1,i)
        phep(2,II) = pout(pid,2,i)
        phep(3,II) = pout(pid,3,i)
CCC   Use mass in pout(pid,4,i) which varies for resonances
        phep(4,II)=sqrt(phep(1,II)**2+phep(2,II)**2
     1   +phep(3,II)**2 + pout(pid,4,i)*pout(pid,4,i))
        isthep(II)     = isttemp     !  status code
        jmohep(1,II)  = 0            !  pointer(s) to position 
        jmohep(2,II)  = 0            !  where the mothers(s) stored   
        jdahep(1,II)  = kdahep(1)    !  pointers to position of 
        jdahep(2,II)  = kdahep(2)    !  the first/last daughter  
CCC   Use mass in pout(pid,4,i) which varies for resonances
        phep(5,II)   = pout(pid,4,i) !  mass (GeV)
        IF(isttemp.ne.1) then
        do k=2,NPTCLSAV
        II = II + 1
        idhep(II) = id(k)
        phep(1,II) = pp(1,k)
        phep(2,II) = pp(2,k)
        phep(3,II) = pp(3,k)
        phep(4,II) = pp(4,k)
        phep(5,II) = pp(5,k)
        isthep(II)    = istda(k)    !  status code
        jmohep(1,II)  = imo1(k)     !  pointer(s) to position 
        jmohep(2,II)  = 0           !  where the mothers(s) stored   
        jdahep(1,II)  = ida1(k)     !  pointers to position of 
        jdahep(2,II)  = ida2(k)     !  the first/last daughter  
        enddo
        ENDIF
           enddo
           enddo
CCC   End track kinematics output.
         NPT = II
      WRITE(6,818) NPT
 818  FORMAT(' NPT ',I10)
      CALL HEPEvent('mevsim',INTRUN,NPT,0.0,PSIr_event,psshep(4)
     1,psshep(3),197.,79.,197.,79.)
         JJJ=0
         DO JJ=1,NPT
        JJJ=JJJ+1
        PPP(1)=phep(1,JJ)
        PPP(2)=phep(2,JJ)
        PPP(3)=phep(3,JJ)
        V(1)=vhep(1)
        V(2)=vhep(2)
        V(3)=vhep(3)
        kmohep(1)=jmohep(1,JJ)
        kmohep(2)=jmohep(2,JJ)
        kdahep(1)=jdahep(1,JJ)
        kdahep(2)=jdahep(2,JJ)
        IF((PPP(1)**2+PPP(2)**2).GT.0.0) THEN
        IF(ABS(PPP(3))/SQRT(PPP(1)**2+PPP(2)**2).LT.0.0005) THEN
           PPP(3)=.001*(RANF()-0.5)*SQRT(PPP(1)**2+PPP(2)**2)
      phep(4,JJ)=SQRT(PPP(1)**2+PPP(2)**2+PPP(3)**2+phep(5,JJ)**2)
        ENDIF
        ENDIF
CCCCC        WRITE(61,6116)JJ,isthep(JJ),idhep(JJ),kmohep,kdahep
CCCCC     1,PPP,phep(4,JJ),phep(5,JJ)
 6116   FORMAT(7I6,5G12.5)
        IF(JJ.EQ.NPT) JJJ=-1
        CALL HEPPart(JJJ,isthep(JJ),idhep(JJ),kmohep,kdahep
     1,PPP,phep(4,JJ),phep(5,JJ),V,vhep(4)) 
         ENDDO
         GO TO 7777
         else
           write(8,250) ievent, event_abort, status_abort
250        Format(/'*** Event No. ',I7,' ABORTED: event_abort,',
     1     'status_abort = ',2I7)
           Return
         end if

      RETURN
      END
      FUNCTION RANF()
      REAL*4 R
      CALL RANLUX(R,1)
      RANF=R
      RETURN
      END 
      


