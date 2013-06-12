* Human-readable units
  Replace [GeV] with [*1.0E+0]
  Replace [MeV] with [*1.0E-3]
  Replace [keV] with [*1.0E-6]

* Fake Subroutine for passing a structure and setting cuts
* based on the contents of that structure
  Replace [SET EmCuts(#)] with [
         IF ( #1_Version .gt. 0 ) THEN
            Call GsTPar( ag_imed, 'CUTGAM', #1_CUTGAM );
            Call GsTPar( ag_imed, 'CUTELE', #1_CUTELE );
            Call GsTPar( ag_imed, 'CUTHAD', #1_CUTHAD );
            Call GsTPar( ag_imed, 'CUTNEU', #1_CUTNEU );
            Call GsTPar( ag_imed, 'CUTMUO', #1_CUTMUO );
            Call GsTPar( ag_imed, 'DCUTE',  #1_DCUTE  );
            Call GsTPar( ag_imed, 'DCUTM',  #1_DCUTM  );
            Call GsTPar( ag_imed, 'BCUTE',  #1_BCUTE  );
            Call GsTPar( ag_imed, 'BCUTM',  #1_BCUTM  );
         ENDIF
   ]



c*****************************************************************************
Module ECALGEO6 is the EM EndCap Calorimeter GEOmetry
c--
Created   11/13/2009
Author    Jason Webb, Hal Spinka, Ilya Selyuzhenkov, Alice Bridgeman, Keith Krueger, Michael Betancourt
c--
c-- CVS log appended at the end
c--
c*****************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    EAGA,EALP,ECAL,ECHC,ECVO,ECGH,EFLP,EHMS,
                 ELED,EMGT,EMOD,EPER,EPSB,ERAD,ERCM,ERSM,
                 ESHM,ESEC,ESCI,ESGH,ESPL,ESSP,EMSS,ETAR,
                 EXGT,EXSG,EXPS,EFLS,EBLS

      Structure  EMCG { Version, int Onoff, int fillMode}

      Structure  EMCS { Version,Type,zorg,zend,EtaMin,EtaMax,
                        PhiMin,PhiMax,Offset,
                        Nsupsec,Nsector,Nsection,Nslices,
                        Front,AlinCell,Frplast,Bkplast,PbPlate,LamPlate,
                        BckPlate,Hub,Rmshift,SMShift,GapPlt,GapCel,
                        GapSMD,SMDcentr,TieRod(2),Bckfrnt,GapHalf,Cover,
                        Rtie,slop}

      Structure  EETR { Type,Etagr,Phigr,Neta,EtaBin(13)}

      Structure  ESEC { Isect, FPlmat, Cell, Scint, Nlayer, deltaz, Jiggle(18) }

      Structure  EMXG {Version,Sapex,Sbase,Rin,Rout,F4}

      Structure  EXSE {Jsect,Zshift,Sectype(6)}

      Structure  ESMD {Version, front_layer, back_layer, spacer_layer, base, apex }

      Structure  ECUT {Version, Absorber, Sensitive, Blah }
      Structure  EABS {Version, CUTGAM, CUTELE, CUTNEU, CUTHAD, CUTMUO, DCUTE, DCUTM, BCUTE, BCUTM }
      Structure  ESEN {Version, CUTGAM, CUTELE, CUTNEU, CUTHAD, CUTMUO, DCUTE, DCUTM, BCUTE, BCUTM }

      Integer    I_section,J_section,Ie,is,isec,istrip,Nstr,Type,ii,jj,
                 cut,fsect,lsect,ihalf,filled,i,j,k,i_sector
                       
      Real       center,Plate,Cell,G10,halfi,
                 tan_low,tan_upp,Tanf,RBot,Rtop,Deta,etax,sq2,sq3,
                 dup,dd,d2,d3,rshift,dphi,radiator
								 
      Real       maxcnt,msecwd,mxgten,curr,Secwid,Section,
                 curcl,EtaTop,EtaBot,zwidth,zslice,Gap,megatile,
                 xleft,xright,yleft,yright,current,
                 rth,length,p,xc,yc,xx,yy,rdel,dxy,ddn,ddup

      Real       myPhi
                 
      Integer    N
      Parameter (N=12)

   
      Tanf(etax) = tan(2*atan(exp(-etax)))
 
c--------------------------------------------------------------------------------
c                                                                            Data
c
c FillMode =1 only 2-5 sectors (in the first half) filled with scintillators 
c FillMode =2 all sectors filled (still only one half of one side)
c FillMode =3 both halves (ie all 12 sectors are filled)
c
c OnOff    =0 Do not build geometry
c OnOff    =1 Build West Endcap
c OnOff    =2 Build East Endcap (disabled)
c OnOff    =3 Build Both Endcaps (east disabled)
c
c Note: 

Fill  EMCG                          ! EM EndCAp Calorimeter basic data 
      Version  = 6.1                ! Geometry version 
      OnOff    = 3                  ! Configurations 0-no, 1-west 2-east 3-both
      FillMode = 3                  ! sectors fill mode 
c--
Fill  EMCS                          ! EM Endcap Calorimeter geometry
      Version  = 1                  ! Versioning
      Type     = 1                  ! =1 endcap, =2 fpd edcap prototype
      ZOrg     = 268.763            ! calorimeter origin in z
      ZEnd     = 310.007            ! Calorimeter end in z
      EtaMin   = 1.086              ! upper feducial eta cut 
      EtaMax   = 2.0                ! lower feducial eta cut
      PhiMin   = -90                ! Min phi 
      PhiMax   = 90                 ! Max phi
      Offset   = 0.0                ! offset in x
      Nsupsec  = 6                  ! Number of azimuthal supersectors        
      Nsector  = 30                 ! Number of azimutal sectors (Phi granularity)
      Nslices  = 5                  ! number of phi slices in supersector
      Nsection = 4                  ! Number of readout sections
      Front    = 0.953              ! thickness of the front AL plates
      AlinCell   = 0.02             ! Aluminim plate in cell
      Frplast  = 0.015              ! Front plastic in megatile
      Bkplast  = 0.155              ! Fiber routing guides and back plastic
      Pbplate  = 0.457              ! Lead radiator thickness
      LamPlate  = 0.05              ! Laminated SS plate thickness
      BckPlate = 3.175              ! Back SS plate thickness
      Hub      = 3.81               ! thickness of EndCap hub
      Rmshift  = 2.121              ! radial shift of module
      smshift  = 0.12               ! radial shift of steel support walls
      GapPlt   = 0.3/2              ! HALF of the inter-plate gap in phi
      GapCel   = 0.03/2             ! HALF of the radial inter-cell gap
      GapSMD   = 3.400              ! space for SMD detector                << version 2 -- 3.600 >>
      SMDcentr = 279.542            ! SMD position
      TieRod   = {160.,195}         ! Radial position of tie rods
      Bckfrnt  = 306.832            ! Backplate front Z
      GapHalf  = 0.4                ! 1/2 Gap between halves of endcap wheel
      Cover    = 0.075              ! Cover of wheel half
      Rtie     = 1.0425             ! Radius of tie rod
      Slop     = 0.1400             ! Added to cell containing radiator 6 (formerly hardcoded in geom)
c--
Fill  EMCS                          ! EM Endcap Calorimeter geometry
      Version  = 2                  ! Versioning
      Type     = 1                  ! =1 endcap, =2 fpd edcap prototype
      ZOrg     = 268.763            ! calorimeter origin in z
      ZEnd     = 310.007            ! Calorimeter end in z
      EtaMin   = 1.086              ! upper feducial eta cut 
      EtaMax   = 2.0                ! lower feducial eta cut
      PhiMin   = -90                ! Min phi 
      PhiMax   = 90                 ! Max phi
      Offset   = 0.0                ! offset in x
      Nsupsec  = 6                  ! Number of azimuthal supersectors        
      Nsector  = 30                 ! Number of azimutal sectors (Phi granularity)
      Nslices  = 5                  ! number of phi slices in supersector
      Nsection = 4                  ! Number of readout sections
      Front    = 0.953              ! thickness of the front AL plates
      AlinCell   = 0.02             ! Aluminim plate in cell
      Frplast  = 0.015              ! Front plastic in megatile
      Bkplast  = 0.155              ! Fiber routing guides and back plastic
      Pbplate  = 0.457              ! Lead radiator thickness
      LamPlate  = 0.05              ! Laminated SS plate thickness
      BckPlate = 3.175              ! Back SS plate thickness
      Hub      = 3.81               ! thickness of EndCap hub
      Rmshift  = 2.121              ! radial shift of module
      smshift  = 0.12               ! radial shift of steel support walls
      GapPlt   = 0.3/2              ! HALF of the inter-plate gap in phi
      GapCel   = 0.03/2             ! HALF of the radial inter-cell gap
      GapSMD   = 3.600              ! space for SMD detector              (* from master_geom_bmp.xls *)
      SMDcentr = 279.542            ! SMD position
      TieRod   = {160.,195}         ! Radial position of tie rods
      Bckfrnt  = 306.832            ! Backplate front Z
      GapHalf  = 0.4                ! 1/2 Gap between halves of endcap wheel
      Cover    = 0.075              ! Cover of wheel half
      Rtie     = 0.75               ! Radius of tie rod
      Slop     = 0.0000             ! Added to cell containing radiator 6 (formerly hardcoded in geom)
c--
c---------------------------------------------------------------------------
c--
c-- Supporting documentation:
c-- http://drupal.star.bnl.gov/STAR/system/files/SMD_module_stack.pdf
c--
Fill  ESMD                     ! shower maximum detector information
      Version  = 1             ! versioning information
      front_layer  = 0.161     ! thickness of front layer 
      back_layer   = 0.210     ! thickness of back layer
      base         = 1.0       ! base of the SMD strip
      apex         = 0.7       ! apex of the SMD strip
      spacer_layer = 1.2       ! spacer layer
c--
Fill EETR                      ! Eta and Phi grid values
      Type     = 1             ! =1 endcap, =2 fpd
      EtaGr    = 1.0536        ! eta_top/eta_bot tower granularity
      PhiGr    = 0.0981747     ! Phi granularity (radians)
      NEta     = 12            ! Eta granularity
      EtaBin   = {2.0,1.9008,1.8065,1.7168,1.6317,1.5507,1.4738,
                  1.4007,1.3312,1.2651,1.2023,1.1427,1.086}! Eta rapidities
c--
c---------------------------------------------------------------------------
c--
Fill ESEC        ! Preshower 1 / Radiator 1
      ISect    = 1                           ! Section number   
      Nlayer   = 1                           ! Number of Sci layers along z
      Cell     = 1.505                       ! Cell full width in z
      Scint    = 0.475                       ! Sci layer thickness (4.75mm Bicron)
      deltaz   = -0.014                      ! Amount to shift section in z to align with as-built numbers
      Jiggle   = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ! Degrees to shift EPER in each layer
c--
c-- Note: Jiggle allows one to shift each megatile by Jiggle(i) degrees, where
c-- i indicates the layer within the section of the calorimeter.  This feature
c-- has only been crudely tested... i.e. it compiles and creates a reasonable
c-- set of pictures, but I have not verified that every scintillator shows up...
c-- There could be volume conflicts and this would need to be checked.  --JW
c--
Fill ESEC      ! Preshower 2 / Radiator 2
      ISect    = 2                           ! Section number   
      Nlayer   = 1                           ! Number of Sci layers along z
      Cell     = 1.505                       ! Cell full width in z
      Scint    = 0.475                       ! Sci layer thickness (4.75mm Bicron)
      deltaz   = -0.0182                     ! Amount to shift section in z to align with as-built numbers
      Jiggle   = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ! Degrees to shift EPER in each layer
c--
Fill ESEC      ! Megatiles 3-6 / Radiators 3-5
      ISect    = 3                           ! Section number
      Nlayer   = 4                           ! Number of Sci layers along z
      Cell     = 1.405                       ! Cell full width in z
      Scint    = 0.4                         ! Sci layer thickness
      deltaz   = -0.0145                     ! Amount to shift section in z to align with as-built numbers
      Jiggle   = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ! Degrees to shift EPER in each layer
c--
Fill ESEC      ! Megatiles 7-23 / Radiators 6-23
      ISect    = 4                           ! Section
      Nlayer   = 18                          ! Number of layers along z
      Cell     = 1.405                       ! Cell full width in z
      Scint    = 0.4                         ! Sci layer thickness
      deltaz   = +0.0336                     ! Amount to shift section in z to align with as-built numbers
      Jiggle   = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ! Degrees to shift EPER in each layer
c--
Fill ESEC      ! Postshower
      ISect    = 5                           ! Section
      Nlayer   = 1                           ! Number of  layers along z
      Cell     = 1.505                       ! Cell full width in z
      Scint    = 0.5                         ! Sci layer thickness (5.0mm Kurarary)
      deltaz   = +0.036                      ! Amount to shift section in z to align with as-built numbers
      Jiggle   = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ! Degrees to shift EPER in each layer
c--
c----------------------------------------------------------------------------
c--
Fill EMXG           ! EM Endcap SMD basic data
      Version   = 1                          ! Geometry version
      Sapex     = 0.7                        ! Scintillator strip apex
      Sbase     = 1.0                        ! Scintillator strip base
      Rin       = 77.41                      ! inner radius of SMD plane  
      Rout      = 213.922                    ! outer radius of SMD plane
      F4        = .15                        ! F4 thickness
c--
c----------------------------------------------------------------------------
c--
Fill EXSE           ! First SMD section
      JSect    = 1                           ! Section number
      Zshift   = -1.215                      ! Section width
      sectype  = {4,1,0,2,1,0}               ! 1-V,2-U,3-cutV,4-cutU    
c--
Fill EXSE           ! Second SMD section
      JSect    = 2                           ! Section number   
      Zshift   = 0.                          ! Section width
      sectype  = {0,2,1,0,2,3}               ! 1-V,2-U,3-cutV,4-cutU    
c--
Fill EXSE           ! Third SMD section
      JSect    = 3                           ! Section number   
      Zshift   = 1.215                       ! Section width
      sectype  = {1,0,2,1,0,2}               ! 1-V,2-U,3-cutV,4-cutU    
c--
c--
Fill ECUT                  ! cut selection
    Version   = 1          ! selector
    Absorber  = 0          ! absorber cuts
    Sensitive = 0          ! sensitive cuts
    Blah      = 1      ! meh

Fill EABS                  ! The values below are the untuned defaults in the original geometry
    Version = 0            ! versioning
    CutGAM  =  80 keV      ! gamma transport cut
    CutELE  =   1 MeV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =   1 MeV      ! electron delta ray cut
    DCutM   =   1 MeV      ! muon delta ray cut
    BCutE   = 100 keV      ! electron brem cut
    BCutM   =   1 MeV      ! muon brem cut     

Fill EABS                  ! EM cuts in absorbing material
    Version =   1          ! versioning
    CutGAM  =  10 keV      ! gamma transport cut
    CutELE  =  10 keV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =  10 keV      ! electron delta ray cut
    DCutM   =  10 keV      ! muon delta ray cut
    BCutE   =  10 keV      ! electron brem cut
    BCutM   =  10 keV      ! muon brem cut

Fill EABS                  ! EM cuts in absorbing material
    Version =   2          ! versioning
    CutGAM  =  30 keV      ! gamma transport cut
    CutELE  =  30 keV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =  30 keV      ! electron delta ray cut
    DCutM   =  30 keV      ! muon delta ray cut
    BCutE   =  30 keV      ! electron brem cut
    BCutM   =  30 keV      ! muon brem cut

Fill EABS                  ! EM cuts in absorbing material
    Version =   3          ! versioning
    CutGAM  = 100 keV      ! gamma transport cut
    CutELE  = 100 keV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   = 100 keV      ! electron delta ray cut
    DCutM   = 100 keV      ! muon delta ray cut
    BCutE   = 100 keV      ! electron brem cut
    BCutM   = 100 keV      ! muon brem cut

Fill EABS                  ! EM cuts in absorbing material
    Version =   4          ! versioning
    CutGAM  =   1 MeV      ! gamma transport cut
    CutELE  =   1 MeV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =   1 MeV      ! electron delta ray cut
    DCutM   =   1 MeV      ! muon delta ray cut
    BCutE   =   1 MeV      ! electron brem cut
    BCutM   =   1 MeV      ! muon brem cut



Fill ESEN                  ! The values below are the untuned defaults in the original geometry
    Version = 0            ! versioning
    CutGAM  =  80 keV      ! gamma transport cut
    CutELE  =   1 MeV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =   1 MeV      ! electron delta ray cut
    DCutM   =   1 MeV      ! muon delta ray cut
    BCutE   = 100 keV      ! electron brem cut
    BCutM   =   1 MeV      ! muon brem cut     

Fill ESEN                  ! EM cuts in absorbing material
    Version =   1          ! versioning
    CutGAM  =  10 keV      ! gamma transport cut
    CutELE  =  10 keV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =  10 keV      ! electron delta ray cut
    DCutM   =  10 keV      ! muon delta ray cut
    BCutE   =  10 keV      ! electron brem cut
    BCutM   =  10 keV      ! muon brem cut

Fill ESEN                  ! EM cuts in absorbing material
    Version =   2          ! versioning
    CutGAM  =  30 keV      ! gamma transport cut
    CutELE  =  30 keV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =  30 keV      ! electron delta ray cut
    DCutM   =  30 keV      ! muon delta ray cut
    BCutE   =  30 keV      ! electron brem cut
    BCutM   =  30 keV      ! muon brem cut

Fill ESEN                  ! EM cuts in absorbing material
    Version =   3          ! versioning
    CutGAM  = 100 keV      ! gamma transport cut
    CutELE  = 100 keV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   = 100 keV      ! electron delta ray cut
    DCutM   = 100 keV      ! muon delta ray cut
    BCutE   = 100 keV      ! electron brem cut
    BCutM   = 100 keV      ! muon brem cut

Fill ESEN                  ! EM cuts in absorbing material
    Version =   4          ! versioning
    CutGAM  =   1 MeV      ! gamma transport cut
    CutELE  =   1 MeV      ! electron transport cut
    CutHAD  =   1 MeV      ! hadron transport cut
    CutNEU  =   1 MeV      ! neutron transport cut
    CutMUO  =   1 MeV      ! muon transport cut
    DCutE   =   1 MeV      ! electron delta ray cut
    DCutM   =   1 MeV      ! muon delta ray cut
    BCutE   =   1 MeV      ! electron brem cut
    BCutM   =   1 MeV      ! muon brem cut


c--
c----------------------------------------------------------------------------
c--                                                                 Materials
c--
c-- The following materials do not need to be defined.  GEANT cannot create
c-- a mixture of mixtures.  So I defined these temporarily in order to 
c-- get A and Z, which are then used in defining the more complicated 
c-- mixtures below).
c--
c--
c-- Aluminized mylar.  According to information which I dug up on a google
c-- search, this is typically mylar coated with a thin (1000 angstrom) layer
c-- of aluminium on each side.
c--
c-- http://www.eljentechnology.com/datasheets/EJ590-B10HH%20data%20sheet.pdf
c--
c--      Component Mylar   A=12.875 Z=6.4580 w=0.999
c--      Component Al      A=26.980 Z=13.000 w=0.001
c--      Mixture   AlMylar dens=1.390
c--
c-- G10 Epoxy used in various places
c--
c--      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
c--      Component O     A=16     Z=8    W=0.6*2*16./60.
c--      Component C     A=12     Z=6    W=0.4*8*12./174.
c--      Component H     A=1      Z=1    W=0.4*14*1./174.
c--      Component O     A=16     Z=8    W=0.4*4*16./174.
c--      Mixture   G10   Dens=1.7
c--
c-- Fibreglass cloth used in SMD stackup.  I googled this one too... a self-
c-- described expert quotes typical densities and percent by volume
c-- http://en.allexperts.com/q/Composite-Materials-2430/fiberglass-1.htm
c-- 
c-- glass fiber: 2.6 g/cm3 (17.6%)   resin: 1.3 g/cm3 (82.4%)
c--
c-- Fiberglass density = 1.529 g/cm3
c--
c-- I will assume that G10 epoxy is close enough to the typical resins
c-- used, at least in terms of chemical composition. Then
c--
c--        Component G10   A=18.017     Z=9.013    W=1.3*0.824/(1.3*0.824+2.6*0.176)
c--        Component Si    A=28.08      Z=14       W=2.6*0.176/(1.3*0.824+2.6*0.176)*28.08/60.08
c--        Component O     A=16         Z=8        W=2.6*0.176/(1.3*0.824+2.6*0.176)*32.00/60.08
c--        Mixture   Fiberglass         dens=1.53
c--
c--
c----------------------------------------------------------------------------
c-- Select versions of various geometry data
c--
      Use    EMCG    
      Use    EMCS   Version=2   
      Use    EETR    
c--
      Use    ECUT   Version=1
      Use    EABS   Version=ECUT_Absorber
      Use    ESEN   Version=ECUT_Sensitive
c----------------------------------------------------------------------------
c-- Calculate frequently used quantities
c--
      sq3 = sqrt(3.)                                ! 1/tan(30deg) = sq3
      sq2 = sqrt(2.)
c--
c--
      center  = (emcs_zorg+emcs_zend)/2             ! center of the calorimeter
      tan_upp = tanf(emcs_etamin)                   ! think this is angle pointing to top of calo
      tan_low = tanf(emcs_etamax)                   ! think this is angle pointing to bot of calo
      rth     = sqrt(1. + tan_low*tan_low)          ! ??
      rshift  = emcs_hub * rth                      ! ??
      dup     = emcs_rmshift*tan_upp                !
      dd      = emcs_rmshift*rth                    !
      d2      = rshift + dd                         !
      radiator  = emcs_pbplate + 2*emcs_lamplate    ! thickness of radiator assembly
      dphi = (emcs_phimax-emcs_phimin)/emcs_nsector ! single endcap sector
c--

c----------------------------------------------------------------------------
c--                                                                     BEGIN
      Prin0 emcg_version
        ('ecalgeo version: ', F4.2) 
      Prin0 ecut_absorber
        ('  absorber cuts=',  F4.2 )
      Prin0 ecut_sensitive
        ('  sensitive cuts=', F4.2 )
c--
      IF (emcg_OnOff>0) THEN
c--
c--     Build the EEMC geometry for one half wheel
c--
        Create ECAL
c--
c--     Position the two halves.  Bottom half installed in 2003, top
c--     half in 2004... so we allow logic to allow for the time
c--     evolution of the calorimeter
c--

c--
c--     West Endcap
c--
        IF (emcg_OnOff==1 | emcg_OnOff==3) THEN
           Position ECAL in CAVE z=+center
        ENDIF
        IF (section > emcs_zend) THEN
          Prin1 section, emcs_zend
            (' ECALGEO error: sum of sections exceeds maximum ',2F12.4)
        ENDIF

c--

        if (emcg_OnOff==2 | emcg_OnOff==3) then
             Position ECAL in CAVE z=-center ThetaZ=180
        endif


c--

c--
      EndIF! emcg_OnOff
c--
      Prin1
        ('ECALGEO finished')

c--
c--                                                                       END
c----------------------------------------------------------------------------








c----------------------------------------------------------------- Block ECAL --
c--
Block ECAL    is one EMC EndCap wheel
c--
c-- The EEMC is built from two 180 degree half-wheels tilted at an angle
c-- with respect to zero in the STAR reference frame.  This block is serves
c-- as a logical volume which creates the two half wheels.  
c--
c-- Creates:
c-- + EAGA
c--
      Material  Air
      Material  ECAL_Air isvol=0
      Attribute ECAL   seen=0 colo=7                           !  lightblue
c--
      Shape     CONE   dz=(emcs_zend-emcs_zorg)/2,
                       rmn1=emcs_zorg*tan_low-d2,
                       rmn2=emcs_zend*tan_low-d2,
                       rmx1=emcs_zorg*tan_upp+dup,
                       rmx2=emcs_zend*tan_upp+dup
c--
c--   Setup absorber-type cuts
      Set EmCuts (EABS)
c--
      DO ihalf=1,2
c--
	     filled = 1
	     halfi  = -105 + (ihalf-1)*180
         if (ihalf=2 & emcg_FillMode<3) filled = 0	
c--
         Create and Position EAGA  AlphaZ=halfi
c--
      ENDDO
c--		
EndBlock




c----------------------------------------------------------------- Block EAGA --
c--
Block EAGA        IS HALF OF WHEEL AIR VOLUME FOR  THE ENDCAP MODULE
c--
c-- The eemc is divided into two halves.  one half installed for 2003 run,
c-- second half added for 2004 and beyond.  the eaga block represents one
c-- of these half-wheels.  it is an air volume which will be filled in 
c-- with additional detector components.
c--
c-- Creates:
c-- + EMSS -- steel support block
c-- + ECGH -- air gap between the two halves
c--
C--                        
      Material  ECAL_Air
      Attribute EAGA      seen=0    colo=1   serial=FILLED           ! BLACK
C--
      Shape     CONS   dz=(emcs_zend-emcs_zorg)/2,
                rmn1=emcs_zorg*tan_low-d2 rmn2=emcs_zend*tan_low-d2,
                rmx1=emcs_zorg*tan_upp+dup rmx2=emcs_zend*tan_upp+dup,
                phi1=emcs_phimin phi2=emcs_phimax
c--
c--
      IF ( FILLED .EQ. 1 ) THEN
c--
          Create AND Position EMSS konly='MANY'
c--
          curr  = emcs_zorg 
          curcl = emcs_zend
c--
          Create AND Position ECGH alphaz=90 kOnly='ONLY'
c--
      ENDIF
c--
EndBlock



c----------------------------------------------------------------- Block EMSS --
c--
Block EMSS                             is the steel support of the endcap module
c--
c-- Creates:
c--   + EFLP -- ALUMINIUM FRONT PLATE
c--   + ECVO -- VOLUMES TO CONTAIN RADIATORS AND MEGATILES
c--   + ESHM -- SHOWER MAX DETECTOR VOLUME
c--   + ESSP -- STAINLESS STEEL BACKPLATE
c--   + ERCM -- STAINLESS STEEL TIE-RODS PENETRATING ECVO
c--

c--
c-- Stainless Steel used in various places
c--
      Component  Cr      A=51.9960  Z=24  W=0.19
      Component  Ni      A=58.6934  Z=28  W=0.09
      Component  Fe      A=55.8450  Z=26  W=0.72
      Mixture    Ecal_Steel   DENS=8.03
c--
      Attribute EMSS      seen=1    colo=1              ! BLACK
      Shape     CONS   dz=(emcs_zend-emcs_zorg)/2,
                rmn1=emcs_zorg*tan_low-d2 rmn2=emcs_zend*tan_low-d2,
                rmx1=emcs_zorg*tan_upp+dup rmx2=emcs_zend*tan_upp+dup,
                phi1=emcs_phimin phi2=emcs_phimax
c--
c--   Setup absorber-type cuts
      Set EmCuts (EABS)

c--
c--   Aluminium front plate 
C--
      zslice = emcs_zorg
      zwidth = emcs_front
c--
      Prin1 zslice+zwidth/2
        (' Front Al plate centered at: ', F12.4 )
c--
      Create AND Position EFLP z=zslice-center+zwidth/2
      zslice = zslice + zwidth
C--
      Prin1 zslice
         (' FIRST CALORIMETER STARTS AT:  ',F12.4)
c--
c--   Preshower 1, preshower 2, and calorimeter tiles up to
c--   megatile number six.
c--
      fsect = 1                                          ! first section 
      lsect = 3                                          ! last section
c--
      zwidth = emcs_smdcentr - emcs_gapsmd/2 - zslice    ! width of current slice
c--
      Prin1 zslice+zwidth/2
        ('Sections 1-3 positioned at: ', F12.4 )
c--
      Create AND Position ECVO  z=zslice-center+zwidth/2
c--
      zwidth  = emcs_gapsmd
      zslice  = emcs_smdcentr - emcs_gapsmd/2
c--
      Prin1 section, zslice
        (' 1st calorimeter ends, smd starts at:  ',2f10.5)
      Prin1 zwidth
        (' smd width = ',f10.5 )
c--
      Prin1 zslice+zwidth/2
         ('SMD section centered at:  ', F12.4 )
c--                                                             Do not kill neighbors
      Create AND Position ESHM  z=zslice-center+zwidth/2        kOnly='MANY'
      zslice = zslice + zwidth
c--
      Prin1 zslice
        ('  SMD ends at:  ',f10.5)
c--
c--
      fsect = 4                                             ! first section
      lsect = 5                                             ! last section
c--
c--   Calculate the width of  the last two calorimeter sections
c--
      zwidth = 0
      DO i_section = fsect,lsect
c--
        USE ESEC isect=i_section  
        zwidth  = zwidth + esec_cell*esec_nlayer
c--
      ENDDO
c--
c--   =============================================================
c--
c--   Total width will be between the back plate and the current
c--   position... this effectively turns the geometry into an
c--   accordian... whatever was defined earlier will compress
c--   / expand this section.  so correcting the smd gap will 
c--   result in some small, sub-mm shifts of radiators and 
c--   megatiles... one would like to actually place these 
c--   into their absolute positions.
c--
c--   ==============================================================
c--
      zwidth = emcs_bckfrnt - zslice
c--
      Prin1 zslice+zwidth/2
        ('Sections 4-5 positioned at: ', F12.4 )
c--
      Create AND Position ECVO  z=zslice-center+zwidth/2
c--
      zslice = emcs_bckfrnt
c--
      Prin1 section,zslice
        (' 2nd calorimeter ends, back plate starts at:  ',2f10.5)
c--
      zwidth  = emcs_bckplate
c--
      Create AND Position ESSP    z=zslice-center+zwidth/2
c--
      zslice = zslice + zwidth
c--
      Prin1 zslice
        ('EEMC Al backplate ends at: ',F12.4 )
c--
c-- Done with the calorimeter stackup.  now go back and cut through the
c-- calorimeter stack with the tie rods
c--
c--   slice width will be full calorimeter depth
      zwidth = emcs_zend-emcs_zorg
c--
      Create ERCM
c--
      DO i = 1,2               ! two tie rods along 
         DO j = 1,5            ! each gap between sectors (5 gaps)
            xx = emcs_phimin + j*30
            yy = xx*degrad
            xc = cos(yy)*emcs_tierod(i)
            yc = sin(yy)*emcs_tierod(i)
            Position ERCM z=0 x=xc y=yc  
         ENDDO
      ENDDO
c--
c--   Now add in projective steel bars which form part of the support
c--   structure of the eemc
c--
      rth = emcs_zorg*tan_upp+dup + 2.5/2
      xc = (emcs_zend - emcs_zorg)*tan_upp
      length = .5*(emcs_zend + emcs_zorg)*tan_upp + dup + 2.5/2
      yc = emcs_zend-emcs_zorg
      p = atan(xc/yc)/degrad
c--
      Create EPSB
      DO i = 1,6
c--
         xx = -75 + (i-1)*30
         yy = xx*degrad
         xc = cos(yy)*length
         yc = sin(yy)*length
c--
         Position EPSB X=XC Y=YC  ALPHAZ=XX
c--
      ENDDO
c--
EndBlock








c----------------------------------------------------------------- Block ECVO --
c--
Block ECVO                  is one of endcap volume with megatiles and radiators
c--
c-- CreateS:
c-- + EMOD -- Responsible for creating esec which, in a glorious example
c--           of spaghetti code, turns around and creates esec, which is
c--           responsible for creating the radiators before and after the
c--           smd layers.
C--
      Material  ECAL_Air
      Attribute ECVO   seen=1 colo=3                            ! GREEN
      Shape     CONS   dz=zwidth/2,
                rmn1=zslice*tan_low-dd,
                rmn2=(zslice+zwidth)*tan_low-dd,
                rmx1=zslice*tan_upp+dup,
                rmx2=(zslice+zwidth)*tan_upp+dup
c--
c--   Loop over the SIX SECTORS in the current half-wheel.  determine
c--   whether the sector is filled or not, and create the "module".
c--   By 'module', we really mean endcap sector.  (Lots of code in the
c--   EEMC borrows from the barrel, and so barrel modlues get mapped
c--   to EEMC sectors).
c--
      DO i_sector = 1,6
c--
         IF (1 < I_SECTOR < 6 | EMCG_FILLMODE > 1) THEN
			 filled = 1
         ELSE
			 filled = 0
         ENDIF
c--
         d3 = 75 - (i_sector-1)*30
         Create AND Position EMOD alphaz=d3   ncopy=i_sector
c--
       ENDDO
c--
EndBlock


c----------------------------------------------------------------- Block ESHM --
c--
Block ESHM                                            is the shower max  section
c--
c-- CreateS:
c-- + ESPL -- SHOWER MAXIMUM DETECTOR PLANES
c-- + ERSM -- TIE RODS W/IN THE SHOWER MAXIMUM DETECTOR
c--
      Material  ECAL_Air
      Attribute ESHM   seen=1   colo=4           !  BLUE
c--
      Shape     CONS   dz=zwidth/2,
                rmn1=(zslice*tan_low)-dd,
                rmn2=(zslice+zwidth)*tan_low-dd,
                rmx1=(zslice)*tan_upp+dup,
                rmx2=(zslice+zwidth)*tan_upp+dup,
                phi1=emcs_phimin phi2=emcs_phimax
c--
      USE EMXG 
c--
      maxcnt = emcs_smdcentr
      Prin1 zslice, section, center
        (' === z start for smd,section:  ',3f12.4, ' === ')
c--
c--   Loop over the three possible locations for the smd planes and
c--   create them.  note that code w/in espl will decide which of
c--   5 types of smd planes are created... u, v, cutu,cutv or spacer.
c--
       DO j_section = 1,3
c--
          USE EXSE jsect=j_section
c--
          current = exse_zshift
          secwid  = emxg_sapex + 2.*emxg_f4
          section = maxcnt + exse_zshift
c--
          Prin1 j_section,current,section,secwid
            (' layer, z, width :  ',i3,3f12.4)
c--
          rbot=section*tan_low
          rtop=section*tan_upp
c--
          Prin1 j_section,rbot,rtop
            (' layer, rbot,rtop :  ',i3,2f12.4)
c--
          Prin1 j_section, center+current
            (' smd layer=',I1,' z=',F12.4 )
c--                                                           Do not kill neighbors
          Create and Position ESPL z=current                  kOnly='MANY'
c--
       ENDDO
c--
c--    Add in the tie rods which penetrate the SMD layers
c--
       Create ERSM
c--
       DO i = 1,2
		  DO j = 1,5
		  	xx = emcs_phimin + j*30
			yy = xx*degrad
			xc = cos(yy)*emcs_tierod(i)
			yc = sin(yy)*emcs_tierod(i)
            Position ERSM Z=0 X=XC Y=YC  
          END DO
       END DO
C--
EndBlock


c----------------------------------------------------------------- Block ECGH --
c--
Block ECGH                                is air gap between endcap half wheels
c--
c-- Creates:
c-- + ECHC -- THE STAINLESS STEEL COVER FOR 1/2 OF THE EEMC.
c--
      Material  ECAL_Air
      Attribute ECGH   seen=0 colo=7                            !  LIGHTBLUE
      Shape     TRD1   dz=(emcs_zend-emcs_zorg)/2,
                dy =(emcs_gaphalf+emcs_cover)/2,
                dx1=emcs_zorg*tan_upp+dup,
                dx2=emcs_zend*tan_upp+dup
c--
c--                
      rth = emcs_gaphalf + emcs_cover
      xx=curr*tan_low-d2
      xleft = sqrt(xx*xx - rth*rth)
      yy=curr*tan_upp+dup
      xright = sqrt(yy*yy - rth*rth)
      secwid = yy - xx
      xx=curcl*tan_low-d2
      yleft = sqrt(xx*xx - rth*rth)
      yy=curcl*tan_upp+dup
      yright = sqrt(yy*yy - rth*rth)
      zwidth = yy - xx
      xx=(xleft+xright)/2
      yy=(yleft + yright)/2
      xc = yy - xx
      length = (xx+yy)/2
      yc = curcl - curr
      p = atan(xc/yc)/degrad
      rth = -(emcs_gaphalf + emcs_cover)/2
c--
      Create  ECHC
c--
      Position ECHC  X=+LENGTH Y=RTH
      Position ECHC  X=-LENGTH Y=RTH ALPHAZ=180
c--
EndBlock




c----------------------------------------------------------------- Block ECHC --
c--
Block ECHC                                            is steel endcap half cover
c--
      Material  ecal_steel
      Attribute ECHC      seen=1    colo=1              ! BLACK
c--
      Shape     TRAP   dz=(curcl-curr)/2,
	            thet=p,
                bl1=secwid/2,
                tl1=secwid/2,
                bl2=zwidth/2,
                tl2=zwidth/2,
                h1=emcs_cover/2,
                h2=emcs_cover/2,
                phi=0,  
                alp1=0,
                alp2=0
c--
EndBlock



c----------------------------------------------------------------- Block ESSP --
c--
Block ESSP                                        is stainless steel  back plate 
c--
      Material  ecal_steel
      Attribute ESSP   seen=1  colo=6 fill=1    
      Shape     CONS   dz=emcs_bckplate/2,
                       rmn1=zslice*tan_low-dd,
                       rmn2=(zslice+zwidth)*tan_low-dd,
                       rmx1=zslice*tan_upp+dup,
                       rmx2=(zslice+zwidth)*tan_upp+dup,
                       phi1=emcs_phimin,
                       phi2=emcs_phimax
c--
EndBlock




c----------------------------------------------------------------- Block EPSB --
c--
Block EPSB  IS A PROJECTILE STAINLESS STEEL BAR
C--
      Material  Ecal_Steel
      Attribute EPSB   seen=1  colo=6 FILL=1    
      Shape     TRAP   dz=(emcs_zend-emcs_zorg)/2,
	            thet=p,
                bl1=2.5/2,
                tl1=2.5/2,
                bl2=2.5/2,
                tl2=2.5/2,
                h1=2.0/2,
                h2=2.0/2,
                phi=0,
                alp1=0,
                alp2=0
c--
c--
EndBlock



c----------------------------------------------------------------- Block ERCM --
c--
Block ERCM                    is stainless steel tie rod in calorimeter sections
c--
      Material  Ecal_Steel
      Attribute ERSM     seen=1  colo=6 FILL=1    
c--
      Shape     TUBE   dz=zwidth/2,
                rmin=0,
                rmax=emcs_rtie
c--
c-- Looks like the tie rods are meant to engage the 1.525 cm diameter holes 
c-- piercing the ears of the smd spacer... 1.5 cm may be a better approximation
c-- here.
c--
c-- http://drupal.star.bnl.gov/star/system/files/smd_spacer_drawings.pdf
c--
EndBlock


c----------------------------------------------------------------- Block ERSM --
c--
Block ERSM                             is stainless steel tie rod in shower max
c--
      Material  Ecal_Steel
      Attribute ERSM       seen=1  colo=6 FILL=1    
c--
      Shape     TUBE dz=zwidth/2,
                rmin=0,
                rmax=emcs_rtie
c--
c-- see comments above
c--
EndBlock


c----------------------------------------------------------------- Block EMOD --
c--
Block EMOD   (fsect,lsect)  IS ONE MODULE  OF THE EM ENDCAP
c--
c-- Arguements: (do be defined prior to the creation of this block)
c--
c--   fsect -- first section to create
c--   lsect -- last section to create
c--
      Attribute EMOD      seen=1    colo=3  serial=FILLED         ! GREEN
      Material  ECAL_Air
      Shape     CONS   dz=zwidth/2,
                phi1=emcs_phimin/emcs_nsupsec,
                phi2=emcs_phimax/emcs_nsupsec,
                rmn1=zslice*tan_low-dd,
                rmn2=(zslice+zwidth)*tan_low-dd,
                rmx1=zslice*tan_upp+dup,
                rmx2=(zslice+zwidth)*tan_upp+dup
c--
c--  Running parameter 'section' contains the position of the current section
c--   it should not be modified in daughters, use 'current' variable instead.
c--   secwid is used in all 'cons' daughters to define dimensions.
c--
        section = zslice
        curr = zslice + zwidth/2
c--
c--
        DO i_section = fsect, lsect

        USE ESEC isect=i_section  
c--
        secwid  = esec_cell*esec_nlayer
c--
c--     Section 3 precedes the smd.  section 5 is the post shower.  in
c--     both cases these sections end with a scintillator layer and no
c--     radiator.
c--
        IF (I_SECTION = 3 | I_SECTION = 5) THEN   
           secwid  = secwid - radiator
        ELSE IF (I_SECTION = 4) THEN                     ! add one more radiator 
           secwid  = secwid - esec_cell + radiator
        ENDIF
c--  
        Prin1 i_section, section-curr+secwid/2
          ('+ ECVO isection=',I1,' zcenter=', F12.4)
c--
        Create AND Position ESEC z=section-curr+secwid/2
c--
        section = section + secwid
c--
      ENDDO! Loop over sections
c--
EndBlock


c----------------------------------------------------------------- Block ESEC --
c--
Block ESEC                                              is a single em section

      Material  ECAL_Air
      Attribute ESEC seen=1 colo=1 serial=filled  lsty=2
c--
      Shape     CONS  dz=secwid/2,  
                rmn1=(section)*tan_low-dd,
                rmn2=(section+secwid)*tan_low-dd,
                rmx1=(section)*tan_upp+dup,
                rmx2=(section+secwid)*tan_upp+dup
c--
      length = -secwid/2
      current = section
c--
      megatile = esec_scint+emcs_alincell+emcs_frplast+emcs_bkplast
c--
      gap = esec_cell - radiator - megatile
      Prin2 i_section,section
        (' ESEC:i_section,section',i3,f12.4)
c--
c--   Loop over all layers in this section
c--
      DO is = 1,esec_nlayer
c--
c--	    Define actual  cell thickness:         
        cell  = esec_cell
        plate = radiator
c--
        IF (is=nint(esec_nlayer) & (i_section = 3 | i_section = 5)) THEN
c--
           cell = megatile + gap
           plate=0
c--
        ELSE IF (i_section = 4 & is = 1) THEN    ! RADIATOR ONLY
c--
           cell = radiator  
c--
        ENDIF
c--
        Prin2 i_section,is,length,cell,current
          (' esec:i_section,is,length,cell,current  ',2i3,3f12.4)
C--
C--     This handles the special case in the section after the smd.
c--     this section begins with a lead radiator.  the previous section
c--     ended with a plastic scintillator
c--
      	IF (i_section = 4 & is = 1) THEN       ! radiator only
c--
c$$$           cell = radiator + .14
           cell = radiator + emcs_slop
                          ! ^^^^ probably the fiber router layer... but is this needed here?
c--
           Prin1 is, current + cell/2+esec_deltaz
              ( '  + ESEC radiator ilayer=',I2,' z=',F12.4 )
           Create AND Position ERAD z=length+(cell)/2+esec_deltaz
c--
           length  = length + cell
           current = current + cell
c--
c--     All other cases are standard radiator followed by scintillator
c--
        ELSE
c--
           cell = megatile
           IF (FILLED = 1) THEN
c--
              Create AND Position EMGT z=length+(gap+cell)/2+esec_deltaz
c--
              xx = current + (gap+cell)/2+esec_deltaz
              prin2 i_section,is,xx
                (' mega  i_section,is ',2i3,f10.4)
              Prin1 is, xx
                 ('  + ESEC megatile ilayer=',I2,' z=',F12.4)
c--
           ENDIF 
c--
           length  = length  + cell + gap
           current = current + cell + gap
c--
           IF (PLATE>0) THEN
c--
              cell = radiator
              Prin1 is, current + cell/2+esec_deltaz
                 ( '  + ESEC radiator ilayer=',I2,' z=',F12.4 )
              Create AND Position ERAD z=length+cell/2+esec_deltaz
c--
              length  = length  + cell
          	  current = current + cell
c--
           ENDIF
c--
         ENDIF
c--
      ENDDO
c--
c--
EndBlock




c----------------------------------------------------------------- Block EMGT --
c--
Block EMGT                                               is a 30 degree megatile
c--
      Material  ECAL_Air
      Attribute EMGT   seen=1  colo=1    lsty=2
c--
      Shape     CONS  dz=megatile/2,
                rmn1=(current)*tan_low-dd,  
                rmn2=(current+megatile)*tan_low-dd,
                rmx1=(current)*tan_upp+dup, 
                rmx2=(current+megatile)*tan_upp+dup

c--
c--   Original block of code tried to set different tracking parameters for
c--   preshower and megatiles.  However, it did not define seperate medium
c--   IDs.  The result was that the code set preshower-like cuts for all 
c--   layers.   --JCW 11/10/09
c--
        Call GSTPAR (ag_imed,'CUTGAM',0.00001)
        Call GSTPAR (ag_imed,'CUTELE',0.00001)
c--     Set absorber-type cuts
        Set EmCuts(EABS)
c--
      DO isec=1,nint(emcs_nslices)
c--
         myPhi = (emcs_nslices/2-isec+0.5)*dphi + esec_jiggle(is)
c--
         Create AND Position EPER alphaz=myPhi
c--
      END DO 
c--
EndBlock




c----------------------------------------------------------------- Block EPER --
c--
Block EPER               is a 5 degree slice of a 30 degree megatile (subsector)
c--
c--   Creates:
c--   + ETAR -- The pseudo-rapidity divivisions in the megatiles
c--
      Material  Polystyren
      Material  ECAL_Polystyren isvol=0
      Attribute EPER       seen=1  colo=1   lsty=1
c--
c--
c--
      Shape     CONS  dz=megatile/2, 
                phi1=emcs_phimin/emcs_nsector,
                phi2=emcs_phimax/emcs_nsector,
                rmn1=(current)*tan_low-dd,
                rmn2=(current+megatile)*tan_low-dd,
                rmx1=(current)*tan_upp+dup,
                rmx2=(current+megatile)*tan_upp+dup
c--
c--   Absorber-type cuts
      Set EmCuts(EABS)
c--
      curcl = current+megatile/2 
      DO ie = 1, nint(eetr_neta)
c--
        etabot  = eetr_etabin(ie)
        etatop  = eetr_etabin(ie+1)

        rbot=(curcl)*tanf(etabot)
        rtop=min((curcl)*tanf(etatop), ((current)*tan_upp+dup))
c--
        check rbot<rtop
c--
        xx=tan(pi*emcs_phimax/180.0/emcs_nsector)
        yy=cos(pi*emcs_phimax/180.0/emcs_nsector)

        Create and Position  ETAR    x=(rbot+rtop)/2  ort=yzx
        prin2 ie,etatop,etabot,rbot,rtop
          (' EPER : ie,etatop,etabot,rbot,rtop ',i3,4f12.4)
c--
      ENDDO
c--
EndBlock


c----------------------------------------------------------------- Block ETAR --
c--
c-- ETAR is a single cell of scintillator, including fiber router, plastic,
c-- etc...
c-- 
c-- local z is radially outward in star
c-- local y is the thickness of the layer
c--
Block ETAR is a single calorimeter cell, containing scintillator, fiber router, etc...
c--
      Material  ECAL_POLYSTYREN
      Attribute ETAR   seen=1  colo=4  lsty=1                         ! BLUE
c--
      Shape TRD1 dy=megatile/2 dz=(rtop-rbot)/2,
            dx1=rbot*xx-emcs_gapcel/yy,
            dx2=rtop*xx-emcs_gapcel/yy
c--
        Create AND Position EALP y=(-megatile+emcs_alincell)/2
      	g10 = esec_scint
      	Create AND Position ESCI y=(-megatile+g10)/2+emcs_alincell _
				                               +emcs_frplast
c--
EndBlock


c----------------------------------------------------------------- Block ESCI --
c--
Block ESCI                        is the active scintillator (polystyrene) layer  
c--
c--   Obtain the definition of polystyrene on this line, next line clones
      Material  Polystyren 
      Material  Ecal_scint   isvol=1
c--
      Attribute ESCI   seen=1   colo=7   fill=0    lsty=1     ! LIGHTBLUE
c--   local z goes along the radius, y is the thickness
      Shape     TRD1   dy=esec_scint/2,
                dz=(rtop-rbot)/2-emcs_gapcel


c--   The original block of cuts from the cvs file
      Call GSTPAR (ag_imed,'CUTGAM', 0.00008)
      Call GSTPAR (ag_imed,'CUTELE', 0.001)
      Call GSTPAR (ag_imed,'BCUTE',  0.0001)
      Call GSTPAR (ag_imed,'CUTNEU', 0.001)
      Call GSTPAR (ag_imed,'CUTHAD', 0.001)
      Call GSTPAR (ag_imed,'CUTMUO', 0.001)
c--   Set sensitive cuts
      Set EmCuts (ESEN)
c--   Define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.013)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-6)
c--
      HITS ESCI   BIRK:0:(0,10)  
c--
c--
EndBlock


c----------------------------------------------------------------- Block ERAD --
c--
Block ERAD                   is the lead radiator with stainless steel cladding
c--
c-- Creates:
c-- + ELED -- the business end of the calorimeter...
c--
      Material ECAL_STEEL
c--
      Attribute ERAD   seen=1  colo=6 fill=1    lsty=1        ! VIOLET
      Shape     CONS  dz=radiator/2, 
                rmn1=(current)*tan_low-dd,
                rmn2=(current+cell)*tan_low-dd,
                rmx1=(current)*tan_upp+dup,
                rmx2=(current+radiator)*tan_upp+dup
c--
      Create AND Position ELED     
c--
EndBlock


c----------------------------------------------------------------- Block ELED --
c--
Block ELED                                              is a lead absorber plate
c--
c--  Lead alloy used in the radiators
c--
      Component  Sn        A=118.710  Z=50  W=0.014
      Component  Ca        A=40.0780  Z=20  W=0.00075
      Component  Al        A=26.9815  Z=13  W=0.0003
      Component  Pb        A=207.190  Z=82  W=0.98495
c--   Define ECAL_PbAlloy.  ECAL prefix given in order to protect from other users
c--   utilizing the name.  --JCW 11/10/09
      Mixture    ECAL_PbAlloy   DENS=11.35    
c--
      Attribute ELED   seen=1 colo=4 fill=1 lsty=1
      Shape     TUBS  dz=emcs_pbplate/2,  
                rmin=(current)*tan_low,
                rmax=(current+emcs_pbplate)*tan_upp,


c--   The original block of cuts from the cvs file
      Call GSTPAR (ag_imed,'CUTGAM', 0.00008)
      Call GSTPAR (ag_imed,'CUTELE', 0.001)
      Call GSTPAR (ag_imed,'BCUTE',  0.0001)
      Call GSTPAR (ag_imed,'CUTNEU', 0.001)
      Call GSTPAR (ag_imed,'CUTHAD', 0.001)
      Call GSTPAR (ag_imed,'CUTMUO', 0.001)

      Set EmCuts(EABS)

c--
EndBlock
c--


c----------------------------------------------------------------- Block EFLP --
c--
Block EFLP                 is the aluminum (aluminium) front plate of the endcap
c--
      Material  ALUMINIUM
      Material  ECAL_ALUMINIUM isvol=0
      Attribute EFLP   seen=1  colo=3  fill=1   lsty=1                   ! GREEN
      Shape     CONS   dz=emcs_front/2,
                rmn1=68.813 rmn2=68.813,
                rmx1=(zslice)*tan_upp+dup,
                rmx2=(zslice+zwidth)*tan_upp+dup,
                phi1=emcs_phimin phi2=emcs_phimax
c--
      Set EmCuts(EABS)
EndBlock

c----------------------------------------------------------------- Block EALP --
c--
Block EALP                       is the thin aluminium plate in calorimeter cell
c--
c--
      Material  Aluminium
c--   Create a copy of aluminium for local use.  This is done to ensure the GsTPar
c--   calls below are local to this file.
      Material  ECAL_AluPlate isVol=0
      Attribute EALP seen=1 colo=1 lsty=1
c--
c--
      Shape     TRD1   dy=emcs_alincell/2  dz=(rtop-rbot)/2
c--
c--   Thin aluminium plate in each calorimeter cell.  The energy-loss
c--   fluctuations are restricted in this thin material.
c--
      CALL GsTPar (AG_IMED,'CUTGAM',0.00001)
      CALL GsTPar (AG_IMED,'CUTELE',0.00001)
      CALL GsTPar (AG_IMED,'LOSS',1.)
      CALL GsTPar (AG_IMED,'STRA',1.)
c--
      Set EmCuts(EABS)
EndBlock





c----------------------------------------------------------------- Block ESPL --
c--
Block ESPL                         is the logical volume containing an SMD plane
c--
      Material  ECAL_Air 
      Attribute ESPL   seen=1   colo=4   lsty=4
      Shape     TUBS   dz=emcs_gapsmd/3/2,
                rmin=section*tan_low-1.526,
                rmax=(section-secwid/2)*tan_upp+dup,
                phi1=emcs_phimin phi2=emcs_phimax
c--
      USE EMXG version=1
      msecwd = (emxg_sapex+emxg_f4)/2		
c--   ^^^^^^ what is this used for?  --jw
c--          looks like the g10 layer which we are retiring
c--
c--   loop over the six sectors in an endcap half wheel
c--	
      DO isec=1,6
         cut=1
         d3 = 75 - (isec-1)*30
c--
         IF (exse_sectype(isec)=0|(emcg_fillmode=1&(isec=6|isec=1))) THEN
            cut = 0
c        -- come back and build spacers --
	     ElseIF (exse_sectype(isec) = 1) then !   v
c--
            Create and Position EXSG alphaz=d3 ncopy=isec              kOnly='MANY'
c--
	     ElseIF (exse_sectype(isec) = 2) then               !   u
c--
            Create and Position EXSG alphaz=d3 ort=x-y-z ncopy=isec    kOnly='MANY'
c--
	     ElseIF (exse_sectype(isec) = 3) then               !  cut v
c--
            cut=2
            Create and Position EXSG alphaz=d3 ncopy=isec              kOnly='MANY'
c--
	     ElseIF (exse_sectype(isec) = 4) then               !  cut u 
c--
            cut=2
            Create and Position EXSG alphaz=d3 ort=x-y-z ncopy=isec    kOnly='MANY'
c--
         EndIF
c--
      EndDO! loop over six sectors in eemc half wheel
c--
c--   repeat the loop and add in the spacer layers
c--
      DO isec=1,6
         d3=75 - (isec-1)*30
         IF (exse_sectype(isec)=0|(emcg_fillmode=1&(isec=6|isec=1))) then                                                                               
            cut = 0         
c--                                                                 Do not kill neighbors
            Create and Position EXSG alphaz=d3 ncopy=isec           kOnly='MANY'
c           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c           potential side effect... may screw up the mapping
c           of the smd strips into the tables?
c
         EndIF
      EndDO
c--
EndBlock


c----------------------------------------------------------------- Block EXSG --
c--
Block EXSG   Is another logical volume... this one acutally creates the planes
c--
c-- Creates:
c-- + EHMS -- shower max strips
c-- + EFLS -- front cover for SMD planes
c-- + EBLS -- back cover for SMD planes
c--
      Attribute EXSG   seen=1   colo=7   serial=cut   lsty=3   ! MEH
      Material  ECAL_Air
      Shape     TUBS   dz=emcs_gapsmd/3/2,
                rmin=section*tan_low-1.526,
                rmax=(section-secwid/2)*tan_upp+dup,
                phi1=emcs_phimin/emcs_nsupsec-5,
                phi2=emcs_phimax/emcs_nsupsec+5
c--
      rbot = emxg_rin
      rtop = emxg_rout
c--
c--   Code to handle smd spacers
c--
      IF ( cut .eq. 0 ) THEN
	  Create and Position EXPS kONLY='MANY' ! disabled by Ilya
      ENDIF
c--
c--   Code to handle smd planes
c--
      IF (cut > 0) THEN
c--
c--     setup which plane we are utilizing
c--
        IF (cut = 1) THEN
           nstr = 288
        ELSE
           nstr = 285
        ENDIF

c--
c--    loop over all smd strips and place them w/in this smd plane
c--
    	DO istrip = 1,nstr
c--
          Call ecal_get_strip( section, cut, istrip, xc, yc, length )
c--
          IF (mod(istrip,2) != 0 ) THEN
             Create and Position EHMS  x=xc y=yc alphaz=-45 kOnly='ONLY'
	     Create and Position EBLS  x=xc y=yc z=(+esmd_apex/2+esmd_back_layer/2) alphaz=-45 kOnly='ONLY' 
          ELSE
             Create and Position EHMS  x=xc y=yc alphaz=-45 ort=x-y-z kOnly='ONLY'
	     Create and Position EFLS  x=xc y=yc z=(-esmd_apex/2-esmd_front_layer/2) alphaz=-45 ort=x-y-z kOnly='ONLY'
          ENDIF
c--
          Prin1 istrip, xc, yc, length
            ( 'SMD Plane: strip=',I3,' xc=',F5.1,' yc=,'F5.1,' length=',F5.1 )
c--
        ENDDO
c--
      ENDIF
c--
c--
*     dcut exsg z 0 0 10 0.1 0.1
*     dcut exsg y 0 10 -50 0.7 0.7
c--
EndBlock
c--
c--

c----------------------------------------------------------------- Block EHMS --
c--
Block EHMS                                     defines the triangular SMD strips
c--
      Material  Polystyren 
c--   to make sure that GsTPar calls below stay w/in the EHMS block. --JCW 11/10/09
      Material  ECAL_smdstrip isvol=1
      Attribute EHMS      seen=1    colo=2  serial=cut  lsty=1        ! red
c--
      Shape     TRD1 dx1=0 dx2=emxg_Sbase/2 dy=length/2 dz=emxg_Sapex/2
c--

c--   The original block of cuts from cvs file
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
c--   Define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.0130)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-6)
c--   Sensitive cuts
      Set EmCuts (ESEN)
      HITS EHMS     Birk:0:(0,10)  
c--
Endblock! EHMS
c-----------------------------------------------------------------------------

c---
c-- Several thin layers of material are applied to the front and back of the 
c--   Copy polystyren and name the copied material ECAL_smdstrip.  This is done
c-- SMD planes to provide structural support.  We combine these layers into
c-- a single effective volume, which is affixed to the base of the SMD
c-- strips.  As with the SMD strips, z along the depth, y is length
c--
c-- http://drupal.star.bnl.gov/STAR/system/files/SMD_module_stack.pdf
c--
c-- 1.19 mm G10
c-- 0.25 mm Fiberglass and epoxy
c-- 0.17 mm Aluminized mylar
c--
c-- Weight in mixture by mass = (depth)*(Area)
c--
c-- Weighted density is given by sum (density)_i * (depth)_i / sum (depth)_i
c--


c----------------------------------------------------------------- Block EFLS --
c--
Block EFLS               is the layer of material on the front of the SMD planes
c--
c--
      Component G10        A=18.017 Z=9.013 w=1.19*1.700/(1.19*1.700+0.25*1.530+0.17*1.390)
      Component Fiberglass A=19.103 Z=9.549 w=0.25*1.530/(1.19*1.700+0.25*1.530+0.17*1.390) 
      Component AlMylar    A=12.889 Z=6.465 w=0.17*1.390/(1.19*1.700+0.25*1.530+0.17*1.390) 
      Mixture   EFLS       dens=(1.19*1.7+0.25*1.53+0.17*1.39)/(1.19+0.25+0.17)

      Attribute EFLS seen=1 colo=22 lsty=1
      Shape     TRD1 dz=esmd_front_layer/2 dy=length/2 dx1=esmd_base/2 dx2=esmd_base/2 

      Set EmCuts(EABS)
c--
EndBlock! EFLS


c--
c-- see link above for documentation
c--
c-- 0.10 mm aluminized mylar
c-- 0.25 mm fiberglass and epoxy
c-- 1.50 mm WLS fiber router layer (polystyrene)
c-- 0.25 mm aluminum
c--


c----------------------------------------------------------------- Block EBLS --
c--
Block EBLS                is the layer of material on the back of the SMD planes
c--
      Component AlMylar    A=12.889 Z=6.465   w=0.10*1.390/(0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)  
      Component Fiberglass A=19.103 Z=9.549   w=0.25*1.530/(0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)  
      Component Polystyren A=11.154 Z=5.615   w=1.50*1.032/(0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)  
      Component Al         A=28.08  Z=14.00   w=0.25*2.699/(0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)  
      Mixture   EBLS       dens=(0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)/(0.10+0.25+1.50+0.25)
c--
      Attribute EFLS seen=1 colo=22 lsty=1
      Shape     TRD1 dz=esmd_back_layer/2 dy=length/2 dx1=esmd_base/2 dx2=esmd_base/2 

      Set EmCuts(EABS)
c--
EndBlock! EFLS


c----------------------------------------------------------------- Block EXPS --
c--
Block EXPS                   is the plastic spacer in the shower maximum section
c--
c--   Simple implementation of the spacer in the shwoer maximum detector.
c--   This implmentation neglects the ears and the source tube.
c--
c--      n.b.  There may be a side effect in the way this gets created...
c--            it could overwrite SMD strips which extend into this plane.
c--            Probably need to go with a different approach here.
c--
c--   Scanned Drawings:
c--   + http://drupal.star.bnl.gov/STAR/system/files/SMD_spacer_drawings.pdf
c--
c--     thickness is 1.2 cm, as given by detail B and C... but I do not want
c--     to do alot of complicated recoding of the geometry.  So I am limiting
c--     it to be the same width as a normal SMD volume.
c--
c--
c--  PVC used in the SMD spacer layers
c--
      Component H  A=1       Z=1   W=3.0*1.0/62.453
      Component C  A=12      Z=6   W=2.0*12.0/62.453
      Component Cl A=35.453  Z=17  W=1.0*35.453/62.453
      Mixture   PVC_Spacer   Dens=1.390*(1.20/1.00)

      Attribute EXPS   seen=1   colo=6    lsty=1    lwid=2
c--
c--   Spacer layers are extended by +/- 5 degrees into the adjacent sectors.
c--   The kONLY='Many' option at creation time should mean that conflicts
c--   in volume will be resolved in favor of the SMD strips.
c--
      Shape   TUBS   dz=1.0/2,
              rmin=(section)*Tan_Low-1.526,
              rmax=(section+msecwd)*Tan_Upp,
              phi1=emcs_PhiMin/emcs_Nsupsec,
              phi2=emcs_PhiMax/emcs_Nsupsec
c--
      Set EmCuts(EABS)
c--
EndBlock
c--
END
c----------------------------------------------------------------- End Module --

c------------------------------------------------------------------------------
c--                                           Helper subroutines and functions
c------------------------------------------------------------------------------
c--
c--
        Subroutine ecal_get_strip( section, cut, istrip, xcenter, ycenter, length )
c--                                in       in   in      out      out      out
          Implicit NONE
c--
          Real     section
          Integer  cut         ! 0=no plane  1=normal plane  2=cut plane
          Integer  istrip      ! strip index
          Real     xcenter     ! output
          Real     ycenter     ! output
          Real     length      ! output
c--
          Integer  nstrips     
          Real     rdel        ! shift in radius (?)
          Real     rth
          Real     ddn, ddup   
          Real     megatile, p
c--
          Real     xleft, yleft, xright, yright 
          Real     dxy, xx, yy
          Real     sqrt2, sqrt3
c--
c--       SMD data copied from data structures above
c--
          Real base, apex
          Data base, apex / 1.0, 0.7/ !cm
c--
          Real Rbot, Rtop
          Data Rbot, Rtop / 77.41, 213.922 /
c--
          Real EtaMin, EtaMax
          Data EtaMin, EtaMax / 1.086, 2.000 /
c--
          Real tan_theta_min, tan_theta_max
c--
          Real tanf, eta
          tanf(eta) = tan(2*atan(exp(-eta)))
c--
          tan_theta_min = tanf( EtaMax )
          tan_theta_max = tanf( EtaMin )
c--
          IF (cut    = 1) THEN                                                                                                       
             rdel    = 3.938                                                                                                         
             nstrips = 288                                                                                                           
          ELSE                                                                                                                    
             rdel    = -.475                                                                                                         
             nstrips = 285                                                                                                           
          ENDIF               
c--
          xcenter=0. 
          ycenter=0.
          length=0.
c--
          IF ( cut = 0 ) THEN
          RETURN
          ENDIF
c--
          sqrt2 = sqrt(2.0)
          sqrt3 = sqrt(3.0)
c--
          rth = .53*rdel        ! .53 --- tentatavily    jcw-- wtf?                                                               
          ddn = sqrt(3.0)*1.713 + rdel                                                                                                  
          ddup = .5*1.846 + 1.713             
          megatile = base + .01
c--
          p = .5*(istrip-1)*megatile + 41.3655  

          IF (p <= (.5*rbot*sqrt3 + rth)) THEN
          dxy     = 1.9375*sqrt2
          xleft  = .5*sqrt2*p*(sqrt3 + 1.) - dxy
          yleft  = .5*sqrt2*p*(sqrt3 - 1.) - dxy 
          yright = .5*sqrt2*(sqrt( rbot*rbot - p*p) - p)
          xright = sqrt2*p + yright
          ELSEIF ((.5*rbot*sqrt3  + rth) < p <= (.5*rtop + 1.5)) THEN
          dxy = 1.9375*sqrt2
          xleft = .5*sqrt2*p*(sqrt3 + 1.) - dxy
          yleft = .5*sqrt2*p*(sqrt3 - 1.) - dxy 
          dxy = rdel*sqrt2/sqrt3
          yright = .5*sqrt2*p*(1.- 1./sqrt3)
          xright = sqrt2*p - yright - dxy
          yright = -yright - dxy
          ELSEIF (p > (.5*rtop +1.5)) THEN
          yleft = (sqrt(rtop*rtop - p*p) - p)/sqrt2
          xleft = sqrt2*p + yleft
          dxy = rdel*sqrt2/sqrt3
          yright = .5*sqrt2*p*(1.- 1./sqrt3)
          xright = sqrt2*p - yright - dxy
          yright = -yright - dxy
          dxy = 0. 
c--
          IF ((.5*sqrt3*160.- ddn) < p <= (.5*sqrt3*160.+ ddup) ) THEN
          xcenter = .5*(sqrt3*160.+1.846)
          ycenter = xcenter - .5*sqrt3*1.713
          IF (p > ycenter) THEN
              dxy = .5*sqrt2*(2/sqrt3*rdel + .5*sqrt3*1.846 +_
              sqrt(1.713*1.713 - (p-xcenter)*(p-xcenter)))
          ELSE
              dxy = sqrt2/sqrt3*(p - .5*sqrt3* 160. + ddn)
          ENDIF
          ELSEIF ((.5*sqrt3*195.- ddn) < p <= (.5*sqrt3*195. + ddup) ) THEN
          xcenter = .5*(sqrt3*195.+1.846)
          ycenter = xcenter - .5*sqrt3*1.713
          IF (p > ycenter) THEN
             dxy = .5*sqrt2*(2/sqrt3*rdel + .5*sqrt3*1.846 +_
             sqrt(1.713*1.713 - (p-xcenter)*(p-xcenter)))
          ELSE
             dxy = sqrt2/sqrt3*(p - .5*sqrt3*195. + ddn)
          ENDIF
          ENDIF
             xright = xright + dxy
             yright = yright + dxy
          ENDIF

          dxy     =  section*tan_theta_max - rtop                                                                                                            
          xcenter = .5*(xright+xleft) + dxy                                                                                                            
          ycenter = .5*(yright+yleft)                                                                                                                  
          xx = .5*sqrt2*(xleft+yleft)                                                                                                               
          yy = .5*sqrt2*(xright+yright)                                                                                                             
          length = xx-yy                              
c--
c--          
          Return
c--
        End! Subroutine smd_strip
c--
* ----------------------------------------------------------------------------
* ECAL nice views: dcut ecvo x 1       10 -5  .5 .1
*                  draw emdi 105 0 160  2 13  .2 .1
*                  draw emdi 120 180 150  1 14  .12 .12
* ---------------------------------------------------------------------------



c-- examples of HITS
*      HITS EHMS     Birk:0:(0,10)  
*                     xx:16:SH(-250,250)  yy:16:(-250,250)  zz:16:(-350,350),
*                     px:16:(-100,100)    py:16:(-100,100)  pz:16:(-100,100),
*                     Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)  Step:16:(0,100),
*                     none:16:            Eloss:0:(0,10)
* 

c Version 1.1, W.J. Llope
c               - changed sensitive medium names...
c
c Version 2.0, R.R. Mehdiyev                                  16.04.97
c               - Support walls included
c               - intercell and intermodule gaps width updated
c               - G10 layers inserted
c Version 2.1, R.R. Mehdiyev                                  23.04.97
c               - Shower Max Detector geometry added          
c               - Variable eta grid step size introduced 
c Version 2.2, R.R. Mehdiyev                                  03.12.97
c               - Eta grid corrected 
c               - Several changes in volumes dimensions
c               - Material changes in SMD
c       
c Version 3.0, O. Rogachevsky                                 28.11.99
c               - New proposal for calorimeter SN 0401
c
c Version 4.1, O.Akio                                          3 Jan 01
c               - Include forward pion detectors
c
c Version 5.0, O. Rogachevsky                                 20.11.01
c               - FPD is eliminated in this version
c               - More closed to proposal description
c                 of calorimeter and SMD structure
c
c Version 6.0, 2009/11/09
c 		Jason Webb, Hal Spinka, Ilya Selyuzhenkov, Alice Bridgeman, Keith Krueger, Michael Betancourt
c 		Main changes:
c 		A) new SMD layers added (EXSG, EFLS, EBLS)
c 		B) Introduced sector overlaps
c 		C) All other changes are documentede here:
c	 	   http://drupal.star.bnl.gov/STAR/subsys/eemc/endcap-geometry-update-2009
c
c $Id: ecalgeo6.g,v 1.4 2010/09/09 17:08:45 jwebb Exp $
c $Log: ecalgeo6.g,v $
c Revision 1.4  2010/09/09 17:08:45  jwebb
c Removed stray comma (typo) from EMCS fill statement.
c
c Revision 1.3  2009/12/22 18:27:15  jwebb
c Corrected error in definition of 10 keV cuts for bcutm.
c
c Revision 1.2  2009/12/22 13:38:49  jwebb
c Added options to change the geant tracking cuts for electrons and photons in
c all volumes to 10, 30, 100 or 1000 keV.
c
c Revision 1.1  2009/11/16 22:07:53  jwebb
c Version 6.1 of the EEMC geometry.
c
c (1) Significant reorganization / documentation of the code.
c     e.g. SMD strip placement logic moved to a subroutine
c (2) Fixed several material / medium relationship problems
c (3) Fixed issue where SMD strips extruded their mother volumes
c (4) Expanded the SMD gap from 34mm to 36mm as built
c (5) Added SMD spacer layers (*)
c (6) Added material to front and back of SMD strips (*)
c (7) Defined and used PbAlloy and Steel mixtures, to better match
c     material in calorimeter.
c (8) Thickness of preshower layers reduced to 4.75mm as built
c
c Changes warrent a new source file.  ecalgeo.g retained for
c compatability with older tags used in previous MC productions.
c
