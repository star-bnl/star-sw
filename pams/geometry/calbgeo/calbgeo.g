*******************************************************************************
MODULE  CALBGEO is the geometry of the Barrel EM Calorimeter in (aG)STAR      *
*                                                                             *
   Author    W. J. Llope, Rice University
   Created   November 17 1995
*
*	Revisions:
*	Version 1.1, W.J. Llope 17-Nov-96
*		- nml version...
*	Version 1.1, Pavel Nevski 25-Mar-96
*		- Modified and optimized for aGSTAR...
*	Version 2.0, W.J. Llope 16-Oct-96
*		- changed module gap from 2.0cm to 1.5 cm...
*       - changed gamma and e thresholds...
*		- removed G10 layers (by setting half-thickness to 0.0001cm)...
*       - now using 4 superlayers to allow 10/10 or 5/15 depth segmentations...
*		- changed 4mm scintillator to 5mm scintillator...
*		- added some more write statements, seen with 'debug on'...
*	Version 2.1, W.J. Llope 19-Oct-96
*		- fixed wrong scintillator thickness (bug inserted in previous update)...
*		- removed G10 layers (block CGTN) entirely - no longer in tree...
*		- moved g10 mixture definition from CGTN block to CSME block...
*		- double gas layer SMD (from old nml days) changed to single gas layer... 
*			(still 0.6cm thick total)
*		- CSMA renamed to CSML, prev. CSML block commented...
*			 (HITS still in CSML block)
*		- SMD aluminum front/back plates now 2mm thick...
*		- SMD electronics G10 now 2mm thick...
*		- changed sensitive medium names for clarity...
*  Version 2.2, W.J. Llope 10-Nov-96
*       - fixed energy response
*       - fixed energy response check mailing list
*
*******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
*WJL      Content   CALB,CHLV,CPHI,CSUP,CLAY,CSMD,CGTN,CSCI,CSMA,CSML,CSME,CBTW
      Content   CALB,CHLV,CPHI,CSUP,CLAY,CSMD,CSCI,CSML,CSME,CBTW
      Structure CALG { version,  Rmin, Rmax, Rcut, HLeng, Etacut, CrackWd,
                       FrontThk, BackThk,  ScintThk, AbsorThk, 
                       SmCovThk, SmGasThk, SmEleThk, g10LaThk, 
                       Nsuper,   Nsmd,     NsubLay(5)  }
      Real      RKB2sc/0.013/, RKB3sc/9.6E-3/
*---- local definitions...
      real      current_depth, current, layer_width, smd_width, tan_theta,
                cut_length, cut_radius, future_depth
      integer   layer,super,sub
*
* ------------------------------------------------------------------------------
*
   fill CALG              !  Barrel Calorimeter data
      Version  = 2.1         ! geometry version
      Rmin     = 222         ! inner radius 
      Rmax     = 263         ! outer radius
      HLeng    = 309.078     ! half outer length
      EtaCut   = 1.0         ! calorimeter rapidity cut
      CrackWd  = 0.75        ! half width of the crack between modules
      FrontThk = 1.000       ! front plate half thickness 
      BackThk  = 1.500       ! Module back plate half thicknes
      ScintThk = 0.250       ! active scintillator plate half thickness
      AbsorThk = 0.250       ! absorber plate thickness
      SmCovThk = 0.100       ! Shower Max front/back Cover half thickness
      SmGasThk = 0.300       ! SMD gas volume half thickness
      SmEleThk = 0.100       ! SMD electronics and Sources half thicknes
      g10LaThk = 0.080       ! G10 layer half thickness	(NOT USED BELOW)
      Nsuper   = 4           ! number of readout superlayer
      Nsmd     = 2           ! SMD positiond after this superlayer
      NsubLay  = {1,4,5,11}  ! number of layers in a superlayer
   Endfill
*
      USE    CALG
      create and position CALB in CAVE  AlphaZ=105
      prin1  calg_Version;            (' CALB geo. version    =',F7.1)
      prin1  calg_RMin;               (' CALB inner radius    =',F7.1)
      prin1  calg_RMax;               (' CALB outer radius    =',F7.1)
      prin1  calg_Rmax-current_depth; (' CALB outer clearance =',F7.1)
      prin1  2.0*calg_CrackWd;        (' CALB crack width     =',F7.1)
*
* ------------------------------------------------------------------------------
block CALB is  EMC Barrel envelope
      Material  Air
      Medium    Standard      
      attribute CALB  seen=0  colo=5
*
* cut calorimeter corners along the rapidity=1
      tan_theta  = tan(2*atan(exp(-calg_EtaCut)))
      cut_length = calg_Rmin/tan_theta
      cut_radius = calg_Hleng*tan_theta
      SHAPE     PCON  Phi1=0  Dphi=360  Nz=4,
                      zi  = {-calg_Hleng,-cut_length, cut_length,  calg_Hleng},
                      rmn = { cut_radius,  Calg_rmin,  Calg_Rmin,  cut_radius},
                      rmx = {  Calg_Rmax,  Calg_Rmax,  Calg_Rmax,  Calg_Rmax };
      create    CHLV
      Position  CHLV
      Position  CHLV  thetaZ=180
*
EndBlock
* ------------------------------------------------------------------------------
Block CHLV corresponds to double modules...
*
      shape     PCON  Phi1=0  Dphi=360  Nz=3,
                      zi  = { 0,          cut_length,  calg_Hleng},
                      rmn = { Calg_rmin,  Calg_Rmin,   cut_radius},
                      rmx = { Calg_Rmax,  Calg_Rmax,   Calg_Rmax };
      Create    CPHI
EndBlock
* ------------------------------------------------------------------------------
Block CPHI corresponds to a single module
      attribute CPHI  seen=0   colo=5
      Shape  Division Iaxis=2  Ndiv=60    " C0=105 - not supported by GEANT3 "
*
*WJL	these are half-widths!
*WJL      layer_width = calg_ScintThk + calg_AbsorThk + 2*calg_g10LaThk
      layer_width = calg_ScintThk + calg_AbsorThk 
      smd_width   = 2*calg_SmCovThk + calg_SmGasThk + calg_SmEleThk
*
      current_depth = calg_Rmin
      Create   CBTW   dx=calg_FrontThk
      Position CBTW   x =calg_Rmin+calg_FrontThk,
                      z =current_depth/tan_theta/2 
      current_depth = current_depth + 2*calg_FrontThk  
      layer = 0
      do super = 1,nint(calg_Nsuper)
         create and position CSUP
* place SMD
         Check  super==nint(calg_Nsmd)
         create and position CSMD  x=current_depth+smd_width,
                                   z=current_depth/tan_theta/2 
         current_depth = current_depth + 2*smd_width  
      end do
*
* Module Back Plate
      Create   CBTW   dx=calg_BackThk
      Position CBTW   x =current_depth+calg_BackThk,
                      z =current_depth/tan_theta/2 
      current_depth = current_depth + 2*calg_BackThk 
*
EndBlock
*-------------------------------------------------------------------------------
Block CSUP  is a super layer with few layers inside
      future_depth = current_depth+calg_NsubLay(super)*layer_width*2
*
      attribute CSUP  seen=0   colo=5
      shape     PCON  Phi1=-3  Dphi=6  Nz=3,
                      zi ={0, current_depth/tan_theta, future_depth/tan_theta},
                      rmn={ current_depth,    current_depth,    future_depth },
                      rmx={ future_depth,     future_depth,     future_depth };
      Call GSTPAR (ag_imed,'CUTGAM',0.001)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
*  
      Do sub = 1,nint(calg_NsubLay(super))
         layer = layer + 1
         create and position CLAY  x=current_depth+layer_width,
                                   z=current_depth/tan_theta/2 
         current_depth = current_depth + 2*layer_width 
      enddo      
*
EndBlock
*-------------------------------------------------------------------------------
Block CLAY  is a single layer with one absorber plate
      Material  Lead
      Attribute CLAY seen=1  colo=1
      SHAPE  BOX  dx = layer_width,
                  dy = (current_depth*TwoPi/60-calg_CrackWd)/2,
                  dz = current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.001)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
*WJL       Create and Position  CGTN  x=calg_AbsorThk
      Create and Position  CSCI  x=calg_AbsorThk
Endblock
*
* G10 is about 60% SiO2 and 40% epoxy
*WJL Block CGTN are g10 layers on both sides of the scintillator
*WJL       Component Si    A=28.08  Z=14   W=0.6*1*28./60.
*WJL       Component O     A=16     Z=8    W=0.6*2*16./60.
*WJL       Component C     A=12     Z=6    W=0.4*8*12./174.
*WJL       Component H     A=1      Z=1    W=0.4*14*1./174.
*WJL       Component O     A=16     Z=8    W=0.4*4*16./174.
*WJL       Mixture   g10   Dens=1.7
*WJL       attribute rest  seen=1  colo=5
*WJL       Shape     BOX   dx=2*calg_g10LaThk + calg_ScintThk
* scintillator
*WJL      Create and Position  CSCI 
*WJL EndBlock
*
Block CSCI a scintillator layer.
      Material  polystyren
      Medium    sens_sci     Isvol=1
      attribute rest  seen=1  colo=7
      Shape     BOX   dx=calg_ScintThk  
      Call GSTPAR (ag_imed,'CUTGAM',0.001)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
*	Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',RKB2sc)
      Call GSTPAR (ag_imed,'BIRK3',RKB3sc)
      HITS     CSUP  eta:0.05:(0,1)        y:1:(-13,13),
                     xx:16:H(-300,300)     yy:16:(-300,300)   zz:16:(-350,350),
                     px:16:(-100,100)      py:16:(-100,100)   pz:16:(-100,100),
                     Slen:16:(0,1.e4)      Tof:16:(0,1.e-6)   Step:16:(0,100),
                     none:16:              Birk:0:(0,10)
EndBlock
* ------------------------------------------------------------------------------
Block CSMD is the shower maximum detector
      Material  Aluminium
      Attribute CSMD seen=1  colo=6
      Shape BOX      dx = smd_width,
                     dy = (current_depth*TwoPi/60-calg_CrackWd)/2,
                     dz = current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.001)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
* SMD gas volume 
      current = -smd_width+2*calg_SmCovThk
      Create and Position  CSML   X = current+calg_SmGasThk
* SMD Electronics and Sources
      current = current+2*calg_SmGasThk
      Create and Position  CSME   X = current+calg_SmEleThk
Endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*WJL	This block was CSMA...
Block CSML is the sensitive argon layer of SMD
      Material  argon_gas
      Medium    sens_gas  Isvol=1
      attribute CSML      seen=1  colo=3
      Shape     BOX       dx=calg_SmGasThk 
      Call GSTPAR (ag_imed,'CUTGAM',0.001)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      HITS      CSML z:1.0:               y:1.0:,
                     xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
                     px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
                     Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100),
                     none:16:             Eloss:0:(0,10)
*WJL      Create    CSML
EndBlock
*WJL	This block is not necessary...
*WJL Block CSML is sensitive argon layer
*WJL       attribute CSML      seen=1   colo=3
*WJL       Shape     division  Iaxis=1  Ndiv=2
*WJL       HITS      CSML z:1.0:               y:1.0:,
*WJL                      xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
*WJL                      px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
*WJL                      Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100),
*WJL                      none:16:             Eloss:0:(0,10)
*WJL EndBlock
*
Block CSME are  SMD Electronics and Sources
*WJL	first define G10
      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
      Component O     A=16     Z=8    W=0.6*2*16./60.
      Component C     A=12     Z=6    W=0.4*8*12./174.
      Component H     A=1      Z=1    W=0.4*14*1./174.
      Component O     A=16     Z=8    W=0.4*4*16./174.
      Mixture   g10   Dens=1.7
      attribute CSME  seen=1  colo=4
      Shape     BOX   dx=calg_SmEleThk  
EndBlock
* -----------------------------------------------------------------------------
Block CBTW  is the  Module Front/Back Plates
      Material  Aluminium
      attribute CBTW  seen=1  colo=1
      Shape     BOX   dy = (current_depth*TwoPi/60-calg_CrackWd)/2,
                      dz = current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.001)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
EndBlock
* -----------------------------------------------------------------------------
      end

