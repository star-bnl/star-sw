******************************************************************************
MODULE  CALBGEO is the geometry of the Barrel EM Calorimeter in (aG)STAR     *
*                                                                            *
   Author    K. Shestermanov, IHEP. First version W. Llope 
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
*		- added some more write statements...
*
*       Version 2.3
*               - Modified by K. Shestermanov   10/22/96
*               - more (from today point of view) realistic SMD
*               - move scintillator plate in sandvich on first place
*               - add 21-th scintillator
*               - changed gamma and e threshold in SMD
*               - changed angle position CALB in CAVE to 75 degree. note#229 
*               - changed wire and strip width in SMD 
*               - changed inner and outer radius of EMC
*
*      Version 2.5
*               - modified by K. Shestermanov   02/10/98
*               - two layers SMD
*               - now using two super layer 1/21+20/21 depth segmentation.
*               - Scintillation layer placed between paper  
*                 Cellulose ro=0.35
*               - two type of strips in the SMD in eta direction
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
external etsphit
*
      Content CALB,CHLV,CPHI,CSUP,CPBP,CSCI,CSMD,CSMG,CSDA,CSMC,CSMB,CSME,
              CSHI,CBTW
      Structure CALG { version,  Rmin,     Etacut,   CrackWd,
                       FrontThk, CompThk,  AirThk,   BackThk,  SpaceThk, 
                       ScintThk, AbsorThk, AbPapThk, Sntchthk, g10SbThk,
                       SmAlfWdh, SmAlfThk, SmGasThk, SmGasWdh, SmGasRad,
                       SmAffWdh, SmAfbWdh, SmetaWdh, Seta1Wdh, Netfirst, 
                       Seta2Wdh, Netsecon, Set12Wdh, SphiWdh,  SphidWdh, 
                       NPhistr,  NSmdAlw,  Nsuper  , Nsmd,     NsubLay(2),
                       Nmodule(2), Shift }
      Real      RKB2sc/0.013/, RKB3sc/9.6E-6/
*---- local definitions...
      real      current_depth, current, layer_width, smd_width, tan_theta,
                cut_length, cut_radius, future_depth,c_dep,c_lead_dep, 
                eta_lenght, current_csda, h_eta1, h_eta2, h_phi1, h_phi2,
                sh_eta1,sh_eta2,sh_phi1,sh_phi2,Rmax,Hleng,Dphi
      integer   layer,super,sub,i,j,ii,nn
*
* ----------------------------------------------------------------------------
*
   fill CALG                 ! Barrel Calorimeter data
      Version  = 2.5         ! geometry version
      Rmin     = 223.5       ! inner radius 
      EtaCut   = 1.0         ! calorimeter rapidity cut
      CrackWd  = 0.655       ! half width of the crack between modules
      FrontThk = 0.9525      ! front plate half thickness 
      CompThk  = 0.9525      ! back plate half thickness
      AirThk   = 0.158       ! Air gap half thicness
      BackThk  = 1.5875      ! Module back plate half thicknes
      SpaceThk = 0.9525      ! Spacer back plate half thicknes
      ScintThk = 0.250       ! active scintillator plate half thickness
      AbsorThk = 0.250       ! absorber plate thickness halfpThickness
      AbPapThk = 0.005       ! absorber paper plate thickness half thicness
      SntchThk = 0.000       ! Smd front notch width
      g10SbThk = 0.115       ! G10 SMD front and back plate half thickness
      SmAlfWdh =11.2014      ! SMD Al front back plate with Ar half width
      SmAlfThk = 0.3893      ! SMD Al front back plate with Ar half thickness
      SmGasThk = 0.1359      ! SMD gas BOX volume half thickness
      SmGasWdh = 0.2946      ! SMD gas BOX volume half width
      SmGasRad = 0.2946      ! SMD gas volume TUBS radius
      SmAffWdh = 0.113       ! SMD Al front first (last) rib half width 
      SmAfbWdh = 0.1664      ! SMD Al back first (last) rib half width
      SmetaWdh = 0.9806      ! Eta=0 strip notch half width
      Seta1Wdh = 0.7277      ! strip#1-75 half witdh
      Netfirst = 75.         ! Number of strip in first part eta=0-0.5
      Seta2Wdh = 0.9398      ! strip#76-150 half witdh
      Netsecon = 75.         ! Number of strip in second part eta=0.5-1.0
      Set12Wdh = 0.04064     ! half distance between strips in eta
      SphiWdh  = 0.6680      ! strip#(1-NPhistr) in phi direction half witdh
      SphidWdh = 0.07874     ! half distance between strips in phi
      NPhistr  = 15          ! Number of the strip in phi direction
      NSmdAlw  = 30          ! Number SMD gaseus interval in tile
      Nsuper   = 2           ! number of readout superlayer
      Nsmd     = 5           ! SMD positioned after sandvich type layers EMC
      NsubLay  = {1,20}      ! number of layers in a superlayer
      Nmodule  = {60,60}     ! number of modules
      Shift    = 75          ! starting azimuth of the first module   
   Endfill
*
      USE    CALG
*
* ---------------------------------------------------------------------------
*                          geometrical constant
*
      layer_width = calg_ScintThk + calg_AbsorThk+2.*calg_AbPapThk
      smd_width=2.*calg_g10SbThk+2.*calg_SmAlfThk+2.*calg_AbPapThk
      Rmax=0.
      do i=1,nint(calg_Nsuper)
        Rmax+=(calg_NsubLay(i)-i+1)*layer_width*2+
               (smd_width+calg_scintThk+2.*calg_AbPapThk)*2.*(i-1)
      enddo
      Rmax+=calg_Rmin+2.*calg_FrontThk
      cut_radius=Rmax
      Rmax+=2.*(Calg_BackThk+calg_SpaceThk+Calg_CompThk+calg_AirThk)
      tan_theta  = tan(2*atan(exp(-calg_EtaCut)))
      cut_length = calg_Rmin/tan_theta
      Hleng = cut_radius/tan_theta  
      nn    = max(calg_Nmodule(1),calg_Nmodule(2))
      dphi  = 6*nn
*
      create and position CALB in CAVE  AlphaZ=calg_shift
      prin1  calg_Version;            (' CALB geo. version    =',F7.1)
      prin1  calg_RMin;               (' CALB inner radius    =',F7.1)
*
* ---------------------------------------------------------------------------
*
block CALB is  EMC Barrel envelope
      Material  Air
      Medium    Standard
      attribute CALB  seen=0  colo=7
*
      SHAPE     PCON  Phi1=0  Dphi=Dphi  Nz=4,
                      zi  = {-Hleng,      -cut_length, cut_length, Hleng},
                      rmn = { cut_radius,  Calg_rmin,  Calg_Rmin,  cut_radius},
                      rmx = { Rmax,        Rmax,       Rmax,       Rmax };

      if calg_Nmodule(1)>0 { ii=1; create and Position  CHLV;            }
      
      if calg_Nmodule(2)>0 { ii=2; create and Position  CHLV thetaZ=180; }
*
EndBlock
* -----------------------------------------------------------------------------
Block CHLV corresponds to double modules...
*
      shape     PCON  Phi1=0  Dphi=6*calg_Nmodule(ii)  Nz=3,
                      zi  = { 0,          cut_length,  Hleng},
                      rmn = { Calg_rmin,  Calg_Rmin,   cut_radius},
                      rmx = { Rmax,       Rmax,        Rmax };
      Create    CPHI
EndBlock
* -----------------------------------------------------------------------------
Block CPHI corresponds to a single module
      attribute CPHI  seen=0   colo=4
      Shape  Division Iaxis=2  Ndiv=calg_Nmodule(ii) " C0=105 - not supported "
*
      current_depth = calg_Rmin
      c_dep=current_depth
      Create   CBTW   dx=calg_FrontThk
      Position CBTW   x =calg_Rmin+calg_FrontThk,
                      z =current_depth/tan_theta/2 
*
      current_depth = current_depth + 2*calg_FrontThk  
      layer = 0
      do super = 1,nint(calg_Nsuper)
*
         create and position CSUP
*
      enddo
*                                  Module Back Plates
      Create   CBTW   dx=calg_CompThk
      Position CBTW   x =current_depth+calg_CompThk,
                      z =current_depth/tan_theta/2       
*
      c_dep=2.*calg_CompThk+2.*calg_AirThk
      Create   CBTW   dx=calg_BackThk
      Position CBTW x=current_depth+c_dep+calg_BackThk,
                    z =current_depth/tan_theta/2 
*
      c_dep=c_dep+2.*calg_BackThk
      Create   CBTW   dx=calg_SpaceThk
      Position CBTW x=current_depth+c_dep+calg_SpaceThk,
                    z =current_depth/tan_theta/2 
      c_dep=c_dep+2.*calg_SpaceThk
      current_depth=current_depth+c_dep 

*
EndBlock
*-----------------------------------------------------------------------------
Block CSUP  is a super layer with few layers inside
      future_depth=current_depth+(calg_NsubLay(super)-super+1)*layer_width*2+
                     (smd_width+calg_scintThk+2.*calg_AbPapThk)*2.*(super-1)
*Cellulose C6H10O5
      Component C  A=12.01   Z=6.    W=6./21.
      Component H  A=1.      Z=1.    W=10./21.
      Component O  A=16.     Z=8.    W=5./21.
      Mixture   Cellulose Dens=0.35 Isvol=1       
      attribute CSUP  seen=0   colo=1
      shape     PCON  Phi1=-3  Dphi=6  Nz=3,
                      zi ={0, current_depth/tan_theta, future_depth/tan_theta},
                      rmn={ current_depth,    current_depth,    future_depth },
                      rmx={ future_depth,     future_depth,     future_depth };
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)   
      Do sub = 1,nint(calg_NsubLay(super))
         layer = layer + 1
         if(layer.lt.nint(calg_NsubLay(1)+calg_NsubLay(2))) then
           Create   CSCI 
           Position CSCI  x=current_depth+calg_ScintThk+2.*calg_AbPapThk,
                          z=current_depth/tan_theta/2 
           Create   CPBP 
           c_lead_dep=2.*calg_ScintThk+4.*calg_AbPapThk
           Position CPBP x=current_depth+c_lead_dep+calg_AbsorThk,
                          z=current_depth/tan_theta/2 
           current_depth = current_depth + 2*layer_width
         else 
           Create   CSCI 
           Position CSCI  x=current_depth+calg_ScintThk+2.*calg_AbPapThk,
                          z=current_depth/tan_theta/2 
           current_depth = current_depth+c_lead_dep
         endif
*                                    place SMD
         Check  layer==nint(calg_Nsmd)
         create and position CSMD  x=current_depth+smd_width,
                                   z=current_depth/tan_theta/2 
         current_depth = current_depth + 2*smd_width  
*
      enddo      
EndBlock
*-----------------------------------------------------------------------------
Block CPBP
      Material  Lead
      Material  CLead Isvol=0
      Medium    Lead_emc
      Attribute CPBP seen=1  colo=1
      SHAPE  BOX  dx = calg_AbsorThk,
                  dy = current_depth*tan(TwoPi/120.)-calg_CrackWd,
                  dz = current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
Endblock
*
Block CSCI a scintillator layer.
      Material  polystyren
      Material  Cpolystyren   Isvol=1
      Medium    sens_sci
      attribute CSCI  seen=1  colo=4
      Shape     BOX   dx=calg_ScintThk,  
                      dy = current_depth*tan(TwoPi/120.)-calg_CrackWd,
                      dz = current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
* define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',RKB2sc)
      Call GSTPAR (ag_imed,'BIRK3',RKB3sc)
*
      HITS   CSUP  eta:0.05:(0,1)        y:1:(-13,13),
                   xx:16:H(-300,300)     yy:16:(-300,300)   zz:16:(-350,350),
                   px:16:(-100,100)      py:16:(-100,100)   pz:16:(-100,100),
                   Slen:16:(0,1.e4)      Tof:16:(0,1.e-6)   Step:16:(0,100),
                   none:16:              Birk:0:(0,10)
*
EndBlock
* 
Block CBTW  is the  Module Front Back Plate
      Material  Aluminium
      Material  EAluminium Isvol=1
      Medium Al_emc 
      attribute CBTW  seen=1  colo=6
      Shape     BOX   dy = current_depth*tan(TwoPi/120.)-calg_CrackWd,
                      dz = current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
EndBlock
* ----------------------------------------------------------------------------
Block CSMD is the shower maximum detector envelope
      current = -smd_width
*Cellulose C6H10O5
      Component C  A=12.01   Z=6.    W=6./21.
      Component H  A=1.      Z=1.    W=10./21.
      Component O  A=16.     Z=8.    W=5./21.
      Mixture   Cellulose Dens=0.35 Isvol=1       
      attribute CSMD  seen=1  colo=6
      Shape BOX dx=smd_width,
                dy=current_depth*tan(TwoPi/120.)-calg_CrackWd-calg_SntchThk,
                dz=current_depth/tan_theta/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
*
* front back G10 plate
      Create CSMG 
      Position CSMG  x = -smd_width+calg_g10SbThk+2.*calg_AbPapThk
      Position CSMG  x =  smd_width-calg_g10SbThk-2.*calg_AbPapThk
*
* front SMD Al block with sensitive gas volume
      current = current+2.*calg_g10SbThk+2.*calg_AbPapThk
      do j=1,4 
        current_csda=-current_depth/tan_theta/2
        if(j.eq.1) then 
          eta_lenght=calg_Netfirst*(calg_Seta1Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+calg_SmAlfThk,
                        z=current_csda+2.*calg_SmetaWdh+eta_lenght
        elseif(j.eq.2) then
          current_csda=current_csda+2.*(calg_SmetaWdh+eta_lenght)
          eta_lenght=calg_Netfirst*(calg_Seta2Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+calg_SmAlfThk,
                        z=current_csda+eta_lenght
        elseif(j.eq.3) then
          eta_lenght=calg_Netfirst*(calg_Seta1Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+3.*calg_SmAlfThk,
                        z=current_csda+2.*calg_SmetaWdh+eta_lenght,
                        thetaX=90 phiX=180
        elseif(j.eq.4) then
          current_csda=current_csda+2.*(calg_SmetaWdh+eta_lenght)
          eta_lenght=calg_Netfirst*(calg_Seta2Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+3.*calg_SmAlfThk,
                        z=current_csda+eta_lenght,
                        thetaX=90 phiX=180
        endif
      enddo
*
* SMD Al front fisrt (last) rib
      Create CSMC    
      Position CSMC   x = current+calg_SmAlfThk,
                      y = calg_SmAlfWdh+calg_SmAffWdh
      Position CSMC   x = current+calg_SmAlfThk,
                      y = -calg_SmAlfWdh-calg_SmAffWdh
*
* SMD Al back fisrt (last) rib
      Create CSMB      
      Position CSMB   x = current+3.*calg_SmAlfThk,
                      y = calg_SmAlfWdh+calg_SmAfbWdh
      Position CSMB   x = current+3.*calg_SmAlfThk,
                      y = -calg_SmAlfWdh-calg_SmAfbWdh
Endblock
*
*     G10 is about 60% SiO2 and 40% epoxy
Block CSMG is G10 front back plate
      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
      Component O     A=16     Z=8    W=0.6*2*16./60.
      Component C     A=12     Z=6    W=0.4*8*12./174.
      Component H     A=1      Z=1    W=0.4*14*1./174.
      Component O     A=16     Z=8    W=0.4*4*16./174.
      Mixture   g10   Dens=1.7 Isvol=0
      attribute CSMG seen=1 colo=1
      Shape BOX dx=calg_g10SbThk
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
      CAll GSTPAR (ag_imed,'LOSS',1.0)
      CAll GSTPAR (ag_imed,'DRAY',1.0)
      CAll GSTPAR (ag_imed,'STRA',1.0)
EndBlock
*
Block CSDA is Al block with sensitive gas volume
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSDA seen=1 colo=6 Serial=j      
      Shape BOX   dx = calg_SmAlfThk,
                  dy = calg_SmAlfWdh, 
                  dz = eta_lenght
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001) 
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
      CAll GSTPAR (ag_imed,'LOSS',1.0)
      CAll GSTPAR (ag_imed,'DRAY',1.0)
      CAll GSTPAR (ag_imed,'STRA',1.0)
      Create CSME
EndBlock
*
Block CSMC is the front first (last) Al rib 
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSMC seen=1 colo=6
      Shape BOX dx = calg_SmAlfThk,
                dy = calg_SmAffWdh
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
      CAll GSTPAR (ag_imed,'LOSS',1.0)
      CAll GSTPAR (ag_imed,'DRAY',1.0)
      CAll GSTPAR (ag_imed,'STRA',1.0)
EndBlock
*
Block CSMB is the back first (last) Al rib 
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSMB seen=1 colo=6
      Shape BOX dx = calg_SmAlfThk,
                dy = calg_SmAfbWdh
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
      CAll GSTPAR (ag_imed,'LOSS',1.0)
      CAll GSTPAR (ag_imed,'DRAY',1.0)
      CAll GSTPAR (ag_imed,'STRA',1.0)
EndBlock
*
Block CSME is the part of CSDA Al box with Ar/CO2 sensiteve gas 
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSME seen=1 colo=6
      Shape Division Iaxis=2 Ndiv = nint(calg_NSmdAlw)
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001) 
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
      CAll GSTPAR (ag_imed,'LOSS',1.0)
      CAll GSTPAR (ag_imed,'DRAY',1.0)
      CAll GSTPAR (ag_imed,'STRA',1.0)
* sensitive Ar/CO2 box 
      do i=1,2
          Create CSHI
        if(i.eq.1) then
          Position CSHI  x = -calg_SmAlfThk+calg_SmGasThk
        else
          Position CSHI  x = 2.*calg_SmGasThk-calg_SmAlfThk
        endif
      enddo
EndBlock
* 90% argon + 10% CO2
Block CSHI is a sensiteve Ar/CO2 box
      Component Ar A=39.95   Z=18.   W=0.9
      Component C  A=12.01   Z=6.    W=0.1*1*12.01/44.01
      Component O  A=16.     Z=8.    W=0.1*2*16./44.01
      Mixture   sens_gas Dens=0.0018015 Isvol=1        
      attribute CSHI      seen=1   colo=4
      if(i.eq.1) then
        Shape BOX dx = calg_SmGasThk,
                  dy = calg_SmGasWdh
      else
        Shape TUBS Rmin = 0  Rmax = calg_SmGasRad,
                   Dx = calg_SmGasWdh,
                   Phi1 = 270 phi2 = 450
      endif
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)    
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
      CAll GSTPAR (ag_imed,'LOSS',1.0)
      CAll GSTPAR (ag_imed,'DRAY',1.0)
      CAll GSTPAR (ag_imed,'STRA',1.0)
      h_eta1=2.*(calg_Seta1Wdh+calg_Set12Wdh)
      sh_eta1=calg_Netfirst*h_eta1
      h_eta2=2.*(calg_Seta2Wdh+calg_Set12Wdh) 
      sh_eta2=calg_Netsecon*h_eta2
      h_phi1=2.*(calg_SphiWdh+calg_SphidWdh)
      h_phi2=h_phi1
      sh_phi1=calg_NPhistr*h_phi1
      sh_phi2=sh_phi1
*
      HITS   CSDA    type=1:2:       eta:0.1:(0,1)   etsp:h_eta1:(0,sh_eta1),
                     xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
                     px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
                     Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100),
                     Eloss:0:(0,1)
*
      HITS   CSDA    type=2:2:       eta:0.1:(0,1)   etsp:h_eta2:(0,sh_eta2),
                     xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
                     px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
                     Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100),
                     Eloss:0:(0,1)
*
      HITS   CSDA    type=3:2:       eta:0.1:(0,1)   etsp:h_phi1:(0,sh_phi1),
                     xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
                     px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
                     Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100),
                     Eloss:0:(0,1)
*
      HITS   CSDA    type=4:2:       eta:0.1:(0,1)   etsp:h_phi2:(0,sh_phi2),
                     xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
                     px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
                     Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100),
                     Eloss:0:(0,1)
*
EndBlock
*
      end
******************************************************************************
                Subroutine  etsphit(j,Hit)
*
+CDE,Typing,GCBANK,GCVOLU,GCKINE,GCTRAK,AgCSTEP.
*
      integer j,iplane,ishape
      integer nlev_r,nlev_m,mycell,jmycell,jmypar
      character cname*4,mname*4
      data mname/'CSDA'/
      Real      Hit,xyzhm(3),xyzh(3),dxhw,dyhw,dzhw
      save nlev_r,nlev_m,dxhw,dyhw,dzhw
*
      nlev_r=nlevel-2
      mycell=lvolum(nlev_r) 
      jmycell=LQ(jvolum-mycell)
      call uhtoc(IQ(jmycell-4),4,cname,4)
      if(cname==mname) then
        ishape=Q(jmycell+2)
        if(ishape==1) then
          jmypar=LQ(jgpar-nlev_r)
          dxhw=Q(jmypar+1)
          dyhw=Q(jmypar+2)
          dzhw=Q(jmypar+3)
        endif
      else
        print *,'There are some problem with level volume in the CSDA'        
      endif
      nlev_m=nlevel
      call gdtom(xloc(1),xyzhm(1),1)
      hits(1)=hit
      iplane=hit
      nlevel=nlev_r
      call gmtod(xyzhm(1),xyzh(1),1)
      nlevel=nlev_m
      if(iplane.le.2) then
        hit=xyzh(3)+dzhw
      else
        hit=xyzh(2)+dyhw
      endif
*
      end







