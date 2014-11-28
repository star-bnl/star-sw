* $Id: hemicosm.F,v 1.2 1998/02/14 23:02:46 fisyak Exp $
* $Log: hemicosm.F,v $
* Revision 1.2  1998/02/14 23:02:46  fisyak
* Split Fortran sources
*
      Subroutine HemiCosm(Len,Rad,Off,MSpec,P_High,P_Low,D_Mode,
     +                    Theta,Phi,P,X,Y,Z,Charge,
     +                    End_Hits,Rate_PD,End_Nsph,Rate_Sph,
     +                    Version)

******-****************************************************************-******
*
*..Description: Simulates Cosmic Rays incident on Detector inside Hemi-sphere
*
*                 This Monte Carlo simulation models the incident Muon
*                 spectrum on a particle detector.
*
*                 A hemi-sphere placement method is used to be able to
*                   A)  generate rays of all Zenith angles (including 90 deg)
*                   B)  estimate the flux rate reliably
*
*                 The simulation expects:
*                   1) Detector mother volume length(cm)  [Len]
*                   2) Detector mother volume radius(cm)  [Rad]
*                   3) Detector mother volume offset(cm)  [Off]
*                   4) Momentum spectrum mode             [MSpec]
*                   5) Momentum Maximum cutoff(GeV/c)     [P_High]
*                   6) Momentum Minimum cutoff(GeV/c)     [P_Low]
*                   7) Data return mode                   [D_Mode]
*                 After the first(initialization) call, only the D_Mode
*                 parameter is relevant.
*
*                 The simulation returns:
*                   1) A momentum Vector(Rad and GeV/c)   [Theta Phi P]
*                   2) A position in 3-space(cm)          [X Y Z]
*                   3) A Muon charge(charge)              [Charge]
*                   4) Events on sphere, Rate on sphere,  [End_Nsph Rate_Sph
*                      Hits on detector, Rate on detector  End_Hits Rate_PD]
*                 The flux information is only returned after the final call,
*                 calculated from the available data concerning momentum and
*                 angular distribution, except for the rate on the sphere
*                 which is available already after the first call.
*                 The sources for each of the spectra are listed in
*                 the corresponding subroutines.
*
*                 The final arguments passed back to the main program are
*                 dependent upon the value of the D_Mode parameter, which sets
*                 what frame of reference you wish the data to be reported
*                 in.  Within the program, the Earth's frame of reference (_E
*                 suffix) is used.  Before the data is reported though, it is
*                 converted to the proper frame of reference depending
*                 on the value of the D_Mode Parameter.  D_Mode selects the
*                 data frame that the calling program wishes reported. The
*                 following options are available:
*                   D_Mode= 0: Null Call
*                      This mode does nothing at all.
*                   D_Mode= 1: Initialization of program
*                      The first call ever done to HemiCosm.  It sets-up
*                      all of the initial parameters. This mode is only
*                      called once.  It also reports some 'useful'
*                      information.
*                   D_Mode= 2: Do one simulation & report data in the detector
*                      frame.  This mode is selected every time a new event is
*                      required.  This will track a Muon from space to
*                      the cylinder and report the data back to the user
*                      in the detector's frame of reference(_D suffix.)
*                      Returns:[Theta Phi P X Y Z Charge]
*                   D_Mode=21 Report data in Earth frame
*                      This mode will not cause a new event to be created,
*                      but will instead report back the data from the
*                      current event in the Earth's frame of reference.
*                      Returns:[Theta Phi X Y Z]
*                   D_Mode=22 Report data in Shadow frame
*                      This mode will not cause a new event to be created,
*                      but will instead report back the data from the
*                      current event as if the particle were to have hit
*                      the X-Y Plane(Earth coordinate Frame).
*                      Returns:[X Y]
*                   D_Mode= 23: Do one simulation & report data in the detector
*                      frame.  This mode is similar to the D_Mode=2 call
*                      except, that in this mode the particle is only tracked
*                      to the hemisphere.  The particles position is returned
*                      in the detector's frame of reference.
*                      Returns:[Theta Phi P X Y Z Charge]
*                   D_Mode= 3 Final call of program
*                      This is the last call for the HemiCosm program.
*                      It is a final report of run data and also
*                      reports other information that can be useful to
*                      ensure that the program is running correctly.
*                      Returns:[Trial Rate_PD]
*                 The User can also choose which momentum spectrum they
*                 wish used by the setting of the P_Mode(MSpec) variable.  The
*                 P_Mode variable is passed into HemiCosm as MSpec and then
*                 stored under the P_Mode variable name.  The following
*                 momentum spectrum representations are present:
*                   P_Mode=0 Constant Spectrum = P_Min
*                   P_Mode=1 1/P^2 Spectrum
*                   P_Mode=2 Spectrum from measured values
*                   P_Mode=3 DarInv Spectrum
*                 The modes are explained in more detail in the flight
*                 subroutine.
*
*                 The Program works in the following outlined steps:
*                     i) Initialize the variables
*
*                    ii) Get Phi(Earth) angle for the momentum vector(Radian)
*                   iii) Get Theta angle(Earth) for the momentum vector(Rad)
*                    iv) Get P(momentum magnitude) of the particle(GeV/c)
*                     v) Get the particles charge(+1 or -1)
*
*                    vi) Calculate where the particle would hit the X-Y plane
*                   vii) Trace the impact point back to find where it would
*                         hit a hemisphere enclosing the particle detector
*                  viii) Convert the data to the Detectors frame of reference
*                    ix) Trace the particle to a Cylinder that just encloses
*                         the detector.
*
*                     x) Report the final data.
*
*..Optional program super-structure:
*    BegCosmFF*                        Optional FFREAD card definitions
*    /beghemicosm_c/ *                 Common block for interface to BEGET
*    BegHemiCosm*                      Optional interface to BEGET package
*      Begcosm*                          Optional filling of /STDHEP/
*
*..Program Structure:
*        HemiCosm(D_Mode=1)                Init phase:
*           Startup                           Detector, flux, printout
*              Shadow                            Area calculation variables
*        HemiCosm(D_Mode=2)                   Event phase:
*           Flight                               Sample phi, theta, p and q
*              GetRan*                              Random number, open (0, 1)
*              Getphi1                              Uniform phi sampling
*              Gettheta1                            Weighted cos(theta)**2
*                 CubeRoot*                            Solve cubic equation
*              GetP0                                P_mode=0:  flat
*              GetP1                                P_mode=1:  1/p**2
*              GetP2                                P_mode=2:  Measured flux
*                 GetP2ini                             First call:  integrate
*                    PhysDatSpec                          Physics Data flux
*                 IntrPol*                             Interpolate
*              GetP3                                P_mode=3:  Theory flux
*                 GetP3ini                             First call:  read data
*                    CosDarNN.dat* files                  Dar integral flux
*                 IntrPol*                             Interpolate
*              GetCharge                            Pick electric charge
*           Locate                               Place ray on detector
*              Shadow                               Hemisphere base area
*                 GetRan*                              Random number
*              Sphere                            Shadow to sphere impact
*              Frame                             Earth to detector rotation
*              Cylinder                          Sphere to cylinder impact
*        HemiCosm(D_Mode=3)                   Finish phase:
*           FinalInfo                            Report counters and rates
*              Shadow                               Report area calculation
*
*   ___
*   *
*    Code or data located in separate files:
*        BegCosmFF        in begcosmff.F
*        BegHemiCosm      in beghemicosm.F
*        /beghemicosm_c/  in beghemicosm_c.inc
*        BegCosm          in begcosm.F
*        GetRan           in getran.F
*        IntrPol          in intrpol.F
*        CubeRoot         in cuberoot.F
*        Dar flux         in cosdarNN.dat, where theta NN = 00, 05, ..., 90.
*
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Laboratory (LBL)
*
*..Copyright  : University of California, Berkeley, (LBL) 1994
*               All rights reserved
*
*..Credits    : Hemisphere method by
*                 J Frederic Kral and Michael P Bringle, LBL
*               Dar inverted integral flux spectrum data files by
*                 Henry Kendall, Massachussetts Institute of Technology (MIT)
*               Additional help from
*                 Henry Kendall and Amit Lath, MIT
*               Funding from
*                 U.S. Department of Energy
*
*..Contact    : J Frederic Kral, JFKral@LBL.Gov
*               
*..Versions   :
*   1.0         Bringle, August  5, 1994 - First development version.
*   2.0         Bringle, August 19, 1994 - Second development version.
*   2.1         Kral,    August 22, 1994 - Add Sphere N events and Rate.
*   2.2         Bringle, August 23, 1994 - Fix anomaly in last bin of DarInv.
*   2.3         Kral,    August 25, 1994 - First production version.
*   2.4         Kral,    Sept   14, 1994 - Correct Physics Data bibliography.
*
******-****************************************************************-******
      Implicit none
*..Version :
      Real*4  HemiVer           ! Program version
      Parameter (HemiVer = 2.4)
*..Input :
      Integer D_Mode           ! Program execution mode
      Integer MSpec            ! Momentum Spectrum Mode
      Real*4 Len               ! Mother Cylinder Length (cm)
      Real*4 Rad               ! Mother Cylinder Rad (cm)
      Real*4 Off               ! Mother Cylinder Offset from ground (cm)
      Real*4 P_Low,P_High      ! Momentum Spectrum Cutoffs (GeV/c)
*..Output:
      Integer Charge           ! Muon Charge (+1 or -1)
      Integer End_Hits         ! Number of particles thrown at the cylinder
      Integer End_Nsph         ! Number of hemi-sphere events
      Real*4 Theta,Phi         ! Momentum direction(radian)
      Real*4 P                 ! Momentum (GeV/c)
      Real*4 X,Y,Z             ! Particle location (cm)
      Real*4 Rate_Sph          ! Predicted Rate (Hz) on the hemisphere
      Real*4 Rate_PD           ! Actual Rate (Hz) of particles hitting
                               !   the detector (See Shadow)
      Real*4  Version          ! Program version (output)
*..Local :
      Integer P_Mode           ! Momentum Spectrum Mode
      Real*4 Det_Len           ! Mother Cylinder Length(cm)
      Real*4 Det_Rad           ! Mother Cylinder Radius(cm)
      Real*4 Det_Off           ! Mother Cylinder Offset from ground(cm)
      Real*4 P_Min,P_Max       ! Momentum Spectrum Cutoffs(GeV/c)

      Integer Flag             ! Reports if particle hit cylinder or not
      Integer Runs             ! Number of particles thrown at the cylinder
      Integer Trials           ! Number of particles hitting the cylinder
      Real*4 dT                ! Time step for each event(seconds)
      Real*4 HemiRad           ! Enclosing hemisphere radius(cm)
      Real*4 Theta_E,Phi_E     ! Momentum direction(Earth Frame)(Radians)

*..Save Variables
      Save dT,HemiRad          ! Save initialized parameters
      Save Theta_E,Phi_E       ! Save incase of a second mode call
      Save Runs,Trials         ! Save for use in final call
      Save Det_Len,Det_Rad     ! Save because are used on every call
      Save Det_Off,P_Mode      ! Save because are used on every call
      Save P_Min,P_Max         ! Save because are used on every call
*=====-================================================================-=======
* Standard create event and report it in the Detector Frame
      If (D_Mode.eq.2) then
        Flag=0
        Trials=Trials+1

        Do While (Flag.ne.1)    ! Loop to ensure a cylinder hit
          Runs=Runs+1

*         Get Particle's physical description
          Call Flight(P_Mode,P_Min,P_Max,Theta_E,Phi_E,P,Charge)

*         Get the particle's location in space
          Call Locate(D_Mode,HemiRad,Det_Len,Det_Rad,Det_Off,Theta_E,
     +                Phi_E,Theta,Phi,X,Y,Z,Flag)
        Enddo


* Reformat the data and report it in the Earth Frame
      Elseif (D_Mode.eq.21) then
        Call Locate(D_Mode,HemiRad,Det_Len,Det_Rad,Det_Off,Theta_E,
     +              Phi_E,Theta,Phi,X,Y,Z,Flag)

* Reformat the data and report it in the Shadow Frame
      Elseif (D_Mode.eq.22) then
        Call Locate(D_Mode,HemiRad,Det_Len,Det_Rad,Det_Off,Theta_E,
     +              Phi_E,Theta,Phi,X,Y,Z,Flag)

* Generate an event and use the hemisphere as the target
      Elseif (D_Mode.eq.23) then
        Trials=Trials+1           !Ratio should be 1:1 for this mode
        Runs=Runs+1

*       Get Particle's physical description
        Call Flight(P_Mode,P_Min,P_Max,Theta_E,Phi_E,P,Charge)

*       Get the particle's location in space
        Call Locate(D_Mode,HemiRad,Det_Len,Det_Rad,Det_Off,Theta_E,
     +              Phi_E,Theta,Phi,X,Y,Z,Flag)

* Initialize the program and swap variable names(First Call)
*   Set these values on startup so subsequent calls to HemiCosm
*   from routines other that the main one(begcosm),ie debug
*   calls, need not include this information.
      Elseif (D_Mode.eq.1) then
        Version = HemiVer       ! Program version
        P_Mode=MSpec            ! Momentum Spectrum Mode
        P_Min=P_Low             ! Low momentum Cutoff
        P_Max=P_High            ! High momentum Cutoff
        If (P_Mode.eq.0) P_Max=P_Min ! In case of constant spectrum
        Det_Rad=Rad             ! Mother Cylinder Radius(cm)
        Det_Len=Len             ! Mother Cylinder Length(cm)
        Det_Off=Off             ! Mother Cylinder Offset from ground(cm)

        Call StartUp(Det_Len,Det_Rad,Det_Off,P_Min,P_Max,P_Mode,
     +                   dT,HemiRad,Version)
        Rate_Sph=1.0/dT         ! Rate on hemi-sphere

* Print the final data of the program(Final Call)
      Elseif (D_Mode.eq.3) then
        Call FinishInfo(Trials,Runs,dT,Rate_PD)
        End_Hits=Trials         ! Number of hits on the detectors
        End_Nsph=Runs           ! Number of hemi-sphere events

* Do completely nothing
      Elseif (D_Mode.eq.0) then !Null Call

* Improper mode select
      Else
        Write(*,*)
        Write(*,*)'HemiCosm:  Error - aborting'
        Write(*,*)'Improper D_Mode selected:'
        Write(*,*)'  D_Mode= ',D_Mode
        STOP                    ! Improper D_Mode
      Endif

 999  Return
      End
