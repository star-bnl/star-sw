******************************************************************************
MODULE   CALAGEO is the temporary geometry of the BEMC in ALICE
Author   A.Pavlinov, WSU.
Created  October-12-2001
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*     working  variable
      real theta, tan_theta, r_cur
      integer i
*     CALA - ALIce Calorimeter
      Content CALA, SCIN, ABSL, LEDS

      Structure CALG {version,  RMIN, RMAX, Etacut, etaN, phiN, zCala,
                      nlayer, AbsorThk, ScintThk}

      Real      RKB2sc/0.013/, RKB3sc/9.6E-6/
*---- local definitions...
      real RMIN, RMAX, DZ    ! for shape 
* --- Important                Comments is obligatory 15-oct-2001
   fill CALG                 ! Barrel Calorimeter data
      Version  = 0.0         ! geometry version
      RMIN     = 450         ! inner radius 
      RMAX     = 550         ! outer radius 
      EtaCut   = 0.7         ! calorimeter rapidity cut
      etaN     = 224         ! #division in eta
      phiN     = 480         ! #division in eta
      zCala    = 500         ! half length in z
      nlayer   = 21          ! number layers in sampling
      AbsorThk = 0.25        ! absorber plate thickness  half thickness      
      ScintThk = 0.25        ! active scintillator plate half thickness
   Endfill
   USE    CALG
*
* ---------------------------------------------------------------------------
*     primary geometrical constant
      CALG_RMAX = CALG_RMIN + 2.*CALG_ScintThk*CALG_nlayer
     +                      + 2.*CALG_AbsorThk*(CALG_nlayer-1) 
*
      theta      = 2*atan(exp(-CALG_EtaCut))
      tan_theta  = tan(theta)
      CALG_zcala = CALG_RMAX / tan_theta
*
      create   CALA
      position CALA in CAVE

      prin0  CALG_Version;            (' CALA geo.  version   =',F7.1)
      prin0  CALG_RMin;               (' CALA inner radius    =',F7.1)
      prin0  CALG_RMax;               (' CALA outer radius    =',F7.1)
      prin0  CALG_EtaCut;             (' CALA eta   cut       =',F7.1)
      prin0  int(CALG_nlayer);        (' CALA #layers         =',I7)
*
* ---------------------------------------------------------------------------
*
block CALA  is EMC for ALICE
      Material  Air
      Medium    Standard
      attribute CALA  seen=0  colo=7
      SHAPE     TUBE  Rmin=CALG_RMIN Rmax=CALG_RMAX+40.,
                Dz=CALG_zCala  
*     Scintillator
      create   SCIN
      position SCIN
*
      r_cur  = r_cur + 10.
      create   LEDS
      position LEDS
endblock
*
block SCIN   sensetive scintillator
      Material  polystyren 
      Material  Cpolystyren   Isvol=1
      Medium    sens_sci
      attribute CALA  seen=1  colo=4
*
      SHAPE     TUBE  Rmin=CALG_RMIN Rmax=CALG_RMAX,
                Dz=CALG_zCala  
      Call CALAPAR(ag_imed,'ABSORBER')

      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',RKB2sc)
      Call GSTPAR (ag_imed,'BIRK3',RKB3sc)
*     Absorber 
      r_cur = CALG_RMIN  + 2.*CALG_ScintThk
      do i=1,int(CALG_nlayer-1)
        create   absl
        position absl
        r_cur = r_cur + 2.*(CALG_ScintThk+CALG_AbsorThk)
      enddo
*
      HITS SCIN ELOS:0:C(0,1000) BIRK:0:C(0,1000)
endblock
*
block ABSL    Absorber layer from LEAD
      Material  Lead
      Material  CLead Isvol=0
      Medium    Lead_emc
      Attribute ABSL seen=1  colo=1

      SHAPE     TUBE  Rmin=r_cur Rmax=r_cur+2.*CALG_AbsorThk,
                Dz=CALG_zCala  
      Call CALAPAR(ag_imed,'ABSORBER')
endblock
*
block LEDS    LEAD sensetive for analysing leakage energy
      Material  Lead
      Material  CLead1 Isvol=1
      Medium    Lead_sens
      Attribute LEDS seen=1  colo=5

      SHAPE     TUBE  Rmin=r_cur Rmax=r_cur+10.,
                Dz=CALG_zCala  

      HITS LEDS ELOS:0:C(0,1000)
endblock
*
      end

      subroutine  CALAPAR(imed,medium)
*
**   originate from subroutine CALBPAR(imed,medium) 
**   in CALB geometry in STAR - 19-oct-2001
*
      integer   imed
      character medium*(*)
*
      if      ( medium=='ABSORBER' ) then    
* --- cuts for EMC absorber and scintillator 
        Call GSTPAR (imed,'CUTGAM',0.00008)
        Call GSTPAR (imed,'CUTELE',0.001)
        Call GSTPAR (imed,'BCUTE' ,0.0001)
*      else if ( medium=='SENSITIVE' ) then   
* --- cuts for SMD light material and gas
*        Call GSTPAR (imed,'CUTGAM',0.00001)
*        Call GSTPAR (imed,'CUTELE',0.00001)    
      endif

*--- common cuts for hadrons and muons in CALA
      Call GSTPAR (imed,'CUTNEU',0.001)
      Call GSTPAR (imed,'CUTHAD',0.001)
      Call GSTPAR (imed,'CUTMUO',0.001)
*
      end

