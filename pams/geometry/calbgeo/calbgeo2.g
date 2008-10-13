******************************************************************************
MODULE  CALBGEO2 is the geometry of the Barrel EM Calorimeter
   Author    Maxim Potekhin BNL
   Created   December 12, 2006
* Based on CALBGEO1 -- tower map removed due to a full calorimeter
*
* $Id: calbgeo2.g,v 1.2 2008/10/13 02:57:18 perev Exp $
* $Log: calbgeo2.g,v $
* Revision 1.2  2008/10/13 02:57:18  perev
* S.Trentalange modifications
*
* Revision 1.1  2006/12/12 21:12:00  potekhin
* Since the CALB geo is pretty finalized, with full population,
* it makes sense to rationalize the internal logic a little and
* get rid of the population map (which effectively did not reflect
* reality in 2006 tags). The new file solves this problem.
*
* New modification made by S.Trentalange
* http://drupal.star.bnl.gov/STAR/blog-entry/trent/2008/sep/15/btwo-geometry-changes
* http://drupal.star.bnl.gov/STAR/blog-entry/trent/2008/aug/26/btow-bsmd-geometry
*
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
external etsphit
*
      Content CALB,CHLV,CPHI,CSZO,CSZU,CSUP,CPBP,CSCI,CSMD,CSMG,CSDA,CSMC,CSMB,CSME,
              CSHI,CBTW,CSPT,CSPB,CSTP,CSLG

      Structure CALG { version,  Rmin,     Etacut,   CrackWd,
                       FrontThk, CompThk,  AirThk,   BackThk,  SpaceThk, 
                       ScintThk(2),        AbsorThk, AbPapThk, g10SbThk,
                       SmAlfWdh, SmAlfThk, SmGasThk, SmGasWdh, SmGasRad,
                       SmAffWdh, SmAfbWdh, SmetaWdh, Seta1Wdh, Netfirst, 
                       Seta2Wdh, Netsecon, Set12Wdh, SphiWdh,  SphidWdh, 
                       NPhistr,  NSmdAlw,  Nsuper  , Nsmd,     NsubLay(2),
                       Nmodule(2), Shift(2), MaxModule, NetaT, Nsub,
                       NetaSMDp}
      Structure CALR { Rmin, Rprs, Rsmd1, Rsmd2, Rmax } 

      Real      RKB2sc/0.013/, RKB3sc/9.6E-6/
*---- local definitions...
      real      current_depth, current, layer_width(2), tan_theta,
                smd_width, smd_width1, smd_width2, smd_width3,
                cut_length, cut_radius, future_depth,c_dep,c_lead_dep, 
                eta_lenght, current_csda, h_eta1, h_eta2, h_phi1, h_phi2,
                sh_eta1,sh_eta2,sh_phi1,sh_phi2,Rmax,Hleng, Deta,
                angular_offset,
                DphiTot, DphiMod, DphiT, R1, R2, R3, R4, RR(2)
      real      zz0,zz1,zz2,sin_theta,cut_length2,Hleng2,angle,
                skindepth,skinoffset,skinoffset1,skinoffset2,skinthk
      real      strap_dx,strap_dy,strap_dz,strap_r
      real      slug_dx,slug_dy,slug_dz
      real      strap_spacing
      real      cut_radius2,calg_Rmin2,Rmax2,Rmax3

      integer   layer,super,sub,i,j,ii,nn,imod,nslug
*
* ---------------------------------------------------------------------------
*                          primary geometrical constant
*
   fill CALG                 ! Barrel Calorimeter data
      Version  = 3.0         ! geometry version
      Rmin     = 223.5       ! inner radius 
      EtaCut   = 1.0         ! calorimeter rapidity cut
      CrackWd  = 0.655       ! half width of the crack between modules
      FrontThk = 0.9525      ! front plate half thickness 
      CompThk  = 0.9525      ! back plate half thickness
      AirThk   = 0.158       ! Air gap half thicness
      BackThk  = 1.5875      ! Module back plate half thicknes
      SpaceThk = 0.9525      ! Spacer back plate half thicknes
      ScintThk = {0.3,0.25}  ! active scintillator plate half thickness
      AbsorThk = 0.250       ! absorber plate thickness halfpThickness
      AbPapThk = 0.005       ! absorber paper plate thickness half thickness
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
      Seta2Wdh = 0.9398      ! strip#76-150 half witdh
      Set12Wdh = 0.04064     ! half distance between strips in eta
      SphiWdh  = 0.6680      ! strip#(1-NPhistr) in phi direction half witdh
      SphidWdh = 0.07874     ! half distance between strips in phi
      NSmdAlw  = 30          ! Number SMD gaseus interval in tile
      Nsuper   = 2           ! number of readout superlayer
      Nsmd     = 5           ! SMD positioned after sandvich type layers EMC
      NsubLay  = {2,19}      ! number of layers in a superlayer
      MaxModule= 60          ! max number of moudle
      NetaT    = 20          ! Number of eta division for tower/preshower
      Nsub     = 2           ! Number of sub div. in phi for tower/preshower
      NetaSMDp = 10          ! Number of eta division in for SMD phi plane
      NPhistr  = 15          ! Number of the strip in phi direction
      Netfirst = 75.         ! Number of strip in first part eta=0-0.5
      Netsecon = 75.         ! Number of strip in second part eta=0.5-1.0
      Nmodule  = {60,60}     ! number of modules
      Shift    = {75,105}    ! starting azimuth of the first module   
   Endfill
*

   USE    CALG
*
* ---------------------------------------------------------------------------
*                          primary geometrical constant
*
      zz0=0.477              ! thickness of SS plate  eta=0
      zz1=0.692              ! thickness of total gap eta=0
      zz2=0.79375            ! thickness of SS plate  eta=1
      calg_EtaCut=0.9835     ! needs to be set elsewhere in program

      smd_width=2.*calg_g10SbThk+2.*calg_SmAlfThk+2.*calg_AbPapThk
      smd_width1=2.*calg_g10SbThk+2.*calg_AbPapThk
      smd_width2=smd_width1+calg_SmGasThk+calg_SmGasRad
      smd_width3=2.*smd_width-smd_width1-calg_SmGasThk-calg_SmGasRad
      R1=calg_Rmin+2.*calg_FrontThk
      R2=0.0
      do i=1,nint(calg_Nsuper)
        layer_width(i) = calg_ScintThk(i) + calg_AbsorThk+2.*calg_AbPapThk
        R2+=(calg_NsubLay(i)-i+1)*layer_width(i)*2.0
  	    RR(i)=R2 
         enddo
      R3=(calg_Nsuper*layer_width(1)+(calg_nsmd-calg_Nsuper)*layer_width(2))*2.
      R4=(smd_width+calg_scintThk(2)+2.*calg_AbPapThk)*2.0
      cut_radius=R1+R2+R4
      Rmax=cut_radius+2.*(Calg_BackThk+calg_SpaceThk+Calg_CompThk+calg_AirThk)
      angle      = 2*atan(exp(-calg_EtaCut))*180./3.14159
      tan_theta  = tan(2*atan(exp(-calg_EtaCut)))
      sin_theta  = sin(2*atan(exp(-calg_EtaCut)))
      cut_length = calg_Rmin/tan_theta
      Hleng   = cut_radius/tan_theta  
      cut_length2 = cut_length + zz2/sin_theta
      Hleng2 = Hleng + zz2/sin_theta
      nn      = max(calg_Nmodule(1),calg_Nmodule(2))
      Deta    = 1.0/calg_NetaT
      DphiMod = 360/calg_MaxModule
      DphiT   = DphiMod/calg_Nsub
      DphiTot = DphiMod*nn
*
      skinthk = 0.040                  ! thickness of aluminum cover
      skinthk = (skinthk/Rmax)*180./3.14159         !convert to Dphi
      skinoffset = (0.250/Rmax)*180./3.14159		!convert to Dphi
      skinoffset1= -3+skinoffset
      skinoffset2= +3-skinoffset
      Rmax2 = cut_radius +2.*0.9525+ 0.291
      calg_Rmin2 = calg_Rmin + 0.291
      cut_radius2= cut_radius +0.291
*
      strap_spacing = 17.06        ! 15 places along module z0=6.5295
      slug_dx       = 0.9525/2     ! half width of slug
      slug_dy       = 3.1750/2     ! half width of slug
      slug_dz       = 3.1750/2     ! half width of slug
      strap_dx      = (Rmax2-calg_Rmin2)/2     ! half width of strap
      strap_dy      = 0.2/2      ! half width of strap
      strap_dz      = 3.175/2        ! half width of strap
      strap_r       = (calg_Rmin2+Rmax2)/2 -0.291

      fill CALR               ! barrel EMC radiuses
      RMIN = R1               ! inner raduis of sensitive area
	  RPRS = R1+RR(1)/2.0     ! mean raduis of PRS
	  RSMD1= R1+R3+smd_width2 ! mean raduis of SMD
	  RSMD2= R1+R3+smd_width3 ! mean raduis of SMD
	  RMAX = cut_radius       ! outer raduis of sensitive area
      Rmax3 = Rmax/cos(3.14159*DphiMod/360) 

      Endfill
      USE CALR
*
      create and position CALB in CAVE
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
      SHAPE     PCON  Phi1=0  Dphi=360  Nz=4,
                      zi  = {-Hleng2,      -cut_length2, cut_length2, Hleng2},
                      rmn = { cut_radius,  Calg_rmin,  Calg_Rmin,  cut_radius},
                      rmx = { Rmax3,        Rmax3,       Rmax3,       Rmax3 };

*     write(*,*) 'CALB: Zi =', cut_length, Hleng
*     write(*,*) 'CALB: Rmi=', Calg_Rmin,  cut_radius
*     write(*,*) 'CALB: Rmx=', Rmax,       Rmax

      ii=1
      create and Position CHLV
      ii=2
      create and Position CHLV thetaZ=180
*
EndBlock
* -----------------------------------------------------------------------------
Block CHLV corresponds to double modules...
*
      shape     PCON  Phi1=calg_shift(ii) Dphi=DphiMod*calg_Nmodule(ii)  Nz=3,
                      zi  = { 0,          cut_length2,  Hleng2},
                      rmn = { Calg_rmin,  Calg_Rmin,   cut_radius},
                      rmx = { Rmax3,       Rmax3,        Rmax3 };

      Create  CPHI
      do imod=1,calg_Nmodule(ii)
*         write(*,*) 'CALB: ii=',ii
          angular_offset=calg_shift(ii)+3.0+6.0*(imod-1)
          Position CPHI AlphaZ=angular_offset Ncopy=imod;
      enddo
EndBlock
* -----------------------------------------------------------------------------
Block CPHI corresponds to a single module
      attribute CPHI  seen=1   colo=5
      Shape  PCON Phi1=-3.0 DPhi=6.0 Nz=3,
                      zi  = { 0,          cut_length2,  Hleng2},
                      rmn = { Calg_rmin,  Calg_Rmin,   cut_radius},
                      rmx = { Rmax3,       Rmax3,        Rmax3 };
*
      Create CSZO    
      Position CSZO  x = (Rmax+calg_Rmin)/2,
                     z = zz0/2,
                     AlphaX=90,
                     AlphaZ=-90
*
      Create CSZU
      Position CSZU  x = (cut_radius+calg_Rmin)/2,
                     z = (cut_length+Hleng+zz2/sin_theta)/2,
                     AlphaX=angle,
                     AlphaZ=-90
*
      Create CSPT
      Position CSPT
*
      Create CSPB
      Position CSPB
*
      current_depth = calg_Rmin
      c_dep=current_depth
      Create   CBTW   dx=calg_FrontThk
      Position CBTW   x =calg_Rmin+calg_FrontThk,
                      z =current_depth/tan_theta/2 +zz0/2

      do nslug = 0,13
      Create  CSLG
      Position CSLG   x =calg_Rmin+slug_dx,
       y = current_depth*tan(TwoPi/360*DphiT)-calg_CrackWd-slug_dy,
                      z = 16.5849+nslug*strap_spacing+zz0
      Create  CSLG
      Position CSLG   x =calg_Rmin+slug_dx,
       y = -1*current_depth*tan(TwoPi/360*DphiT)+calg_CrackWd+slug_dy,
                      z = 16.5849+nslug*strap_spacing+zz0

      Create CSTP
      Position CSTP   x=strap_r,
      y = +1*strap_r*tan(TwoPi/360*DphiT)-calg_CrackWd+strap_dy,
                      z = 16.5849+nslug*strap_spacing+zz0,
                      AlphaZ=+3
      Create CSTP
      Position CSTP   x=strap_r,
      y = -1*strap_r*tan(TwoPi/360*DphiT)+calg_CrackWd-strap_dy,
                      z = 16.5849+nslug*strap_spacing+zz0,
                      AlphaZ=-3
      enddo

      Create  CSLG
      Position CSLG   x =calg_Rmin+slug_dx,
       y = current_depth*tan(TwoPi/360*DphiT)-calg_CrackWd-slug_dy,
                      z =current_depth/tan_theta-zz0-3.679
      Create  CSLG
      Position CSLG   x =calg_Rmin+slug_dx,
       y = -1*current_depth*tan(TwoPi/360*DphiT)+calg_CrackWd+slug_dy,
                      z =current_depth/tan_theta-zz0-3.679
      Create CSTP
      Position CSTP   x=strap_r,
      y = +1*strap_r*tan(TwoPi/360*DphiT)-calg_CrackWd+strap_dy,
                      z =current_depth/tan_theta-zz0-3.679,
                      AlphaZ=+3
      Create CSTP
      Position CSTP   x=strap_r,
      y = -1*strap_r*tan(TwoPi/360*DphiT)+calg_CrackWd-strap_dy,
                      z =current_depth/tan_theta-zz0-3.679,
                      AlphaZ=-3


      current_depth = current_depth + 2*calg_FrontThk  

      layer = 0
      do super = 1,nint(calg_Nsuper)
         create and position CSUP
      enddo
*                                  Module Back Plates
      Create   CBTW   dx=calg_CompThk
      Position CBTW   x =current_depth+calg_CompThk,
                      z =current_depth/tan_theta/2 +zz0/2      
*
      c_dep=2.*calg_CompThk+2.*calg_AirThk
      Create   CBTW   dx=calg_BackThk
      Position CBTW x=current_depth+c_dep+calg_BackThk,
                    z =current_depth/tan_theta/2 +zz0/2
*
      c_dep=c_dep+2.*calg_BackThk
      Create   CBTW   dx=calg_SpaceThk
      Position CBTW x=current_depth+c_dep+calg_SpaceThk,
                    z =current_depth/tan_theta/2 +zz0/2
      c_dep=c_dep+2.*calg_SpaceThk
      current_depth=current_depth+c_dep 

*
EndBlock
*-----------------------------------------------------------------------------
Block CSTP
      Material  Iron
      Material  CIron Isvol=0
      Medium    Iron_emc
      Attribute CSTP seen=1  colo=1
      SHAPE  BOX  dx = strap_dx,
                  dy = strap_dy,
                  dz = strap_dz
      Call CALBPAR(ag_imed,'ABSORBER')
Endblock
*-----------------------------------------------------------------------------
Block CSPT is the side aluminum skin top 
      Material Aluminium
      Material CAluminium Isvol=0
      Medium Al_emc
      attribute CSPT seen=1 colo=6
      Shape  PCON Phi1=-2.94 DPhi=0.0045 Nz=3,
                      zi  = { 0,          cut_length,  Hleng},
                      rmn = { Calg_Rmin2,  Calg_Rmin2,   cut_radius2},
                      rmx = { Rmax2,       Rmax2,        Rmax2 };
      Call CALBPAR(ag_imed,'ABSORBER')
EndBlock
*-----------------------------------------------------------------------------
Block CSPB is the side aluminum skin bottom 
      Material Aluminium
      Material CAluminium Isvol=0
      Medium Al_emc
      attribute CSPB seen=1 colo=6
      Shape  PCON Phi1=+2.94 DPhi=0.0045 Nz=3,
                      zi  = { 0,          cut_length,  Hleng},
                      rmn = { Calg_Rmin2,  Calg_Rmin2,   cut_radius2},
                      rmx = { Rmax2,       Rmax2,        Rmax2 };
      Call CALBPAR(ag_imed,'ABSORBER')
EndBlock
*-----------------------------------------------------------------------------
Block CSLG
      Material  Iron
      Material  CIron Isvol=0
      Medium    Iron_emc
      Attribute CSLG seen=1  colo=1
      SHAPE  BOX  dx = slug_dx,
                  dy = slug_dy,
                  dz = slug_dz
      Call CALBPAR(ag_imed,'ABSORBER')
Endblock
*----------------------------------------------------------------------------
Block CSZO
      Material  Iron
      Material  CIron Isvol=0
      Medium    Iron_emc
      Attribute CSZO seen=1  colo=1
      Shape  TRD1  dx1 = calg_Rmin*tan(TwoPi/360*DphiT)-calg_CrackWd,
                   dx2 = Rmax*tan(TwoPi/360*DphiT)-calg_CrackWd,
                   dy  = zz0/2,
                   dz  = (Rmax - calg_Rmin)/2
      Call CALBPAR(ag_imed,'ABSORBER')
Endblock
*----------------------------------------------------------------------------
Block CSZU 
      Material  Iron
      Material  CIron Isvol=0
      Medium    Iron_emc
      Attribute CSZU seen=1  colo=1
      Shape  TRD1  dx1 = calg_Rmin*tan(TwoPi/360*DphiT)-calg_CrackWd,
                   dx2 = cut_radius*tan(TwoPi/360*DphiT)-calg_CrackWd,
                   dy  = zz2/2,
                   dz  = (cut_radius - calg_Rmin)/(2.*sin_theta)
      Call CALBPAR(ag_imed,'ABSORBER')
Endblock
*-----------------------------------------------------------------------------
Block CSUP  is a super layer with few layers inside
      future_depth=current_depth+
                 (calg_NsubLay(super)-super+1)*layer_width(super)*2+
                 (smd_width+calg_scintThk(super)+2.*calg_AbPapThk)*2*(super-1)
*Cellulose C6H10O5
      Component C  A=12.01   Z=6.    W=6./21.
      Component H  A=1.      Z=1.    W=10./21.
      Component O  A=16.     Z=8.    W=5./21.
      Mixture   Cellulose Dens=0.35 Isvol=1       
      attribute CSUP  seen=0   colo=1
      shape     PCON  Phi1=-3.0  Dphi=DphiMod  Nz=3,
                      zi ={zz1, current_depth/tan_theta, future_depth/tan_theta},
                      rmn={ current_depth,    current_depth,    future_depth },
                      rmx={ future_depth,     future_depth,     future_depth };
      Call CALBPAR(ag_imed,'ABSORBER')

      Do sub = 1,nint(calg_NsubLay(super))
         layer = layer + 1
         if(layer.lt.nint(calg_NsubLay(1)+calg_NsubLay(2))) then
           Create   CSCI 
           Position CSCI x=current_depth+calg_ScintThk(super)+2.*calg_AbPapThk,
                         z=current_depth/tan_theta/2 +zz1/2
           Create   CPBP 
           c_lead_dep=2.*calg_ScintThk(super)+4.*calg_AbPapThk
           Position CPBP x=current_depth+c_lead_dep+calg_AbsorThk,
                         z=current_depth/tan_theta/2 +zz1/2
           current_depth = current_depth + 2*layer_width(super)
         else 
           Create   CSCI 
           Position CSCI x=current_depth+calg_ScintThk(2)+2.*calg_AbPapThk,
                         z=current_depth/tan_theta/2 +zz1/2
           current_depth = current_depth+c_lead_dep
         endif
*                                    place SMD
         Check  layer==nint(calg_Nsmd)
         create and position CSMD  x=current_depth+smd_width,
                                   z=current_depth/tan_theta/2 +zz0/2 
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
                  dy = current_depth*tan(TwoPi/360*DphiT)-calg_CrackWd,
                  dz = current_depth/tan_theta/2 -zz1/2
      Call CALBPAR(ag_imed,'ABSORBER')

*      write(*,*) 'super,sub:',super,' ',sub,' Pb dx,dy,dz ',
     *calg_AbsorThk,current_depth*tan(TwoPi/360*DphiT)-calg_CrackWd,
     *current_depth/tan_theta/2

Endblock
*
Block CSCI a scintillator layer.
      Material  polystyren
      Material  Cpolystyren   Isvol=1
      Medium    sens_sci
      attribute CSCI  seen=1  colo=4
      Shape     BOX   dx=calg_ScintThk(super),  
                      dy = current_depth*tan(TwoPi/360*DphiT)-calg_CrackWd,
                      dz = current_depth/tan_theta/2 -zz1/2
      Call CALBPAR(ag_imed,'ABSORBER')

* define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',RKB2sc)
      Call GSTPAR (ag_imed,'BIRK3',RKB3sc)
*
      HITS   CSUP  eta:Deta:(0,1)        y:1:(-13,13)       Birk:0:(0,10)
*                  xx:16:H(-300,300)     yy:16:(-300,300)   zz:16:(-350,350),
*                  px:16:(-100,100)      py:16:(-100,100)   pz:16:(-100,100),
*                  Slen:16:(0,1.e4)      Tof:16:(0,1.e-6)   Step:16:(0,100),
*                  none:16:            
*
EndBlock
* 
Block CBTW  is the  Module Front Back Plate
      Material  Aluminium
      Material  EAluminium Isvol=1
      Medium Al_emc 
      attribute CBTW  seen=1  colo=6
      Shape     BOX   dy = current_depth*tan(TwoPi/360*DphiT)-calg_CrackWd,
                      dz = current_depth/tan_theta/2 -zz0/2
      Call CALBPAR(ag_imed,'ABSORBER')

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
                dy=current_depth*tan(TwoPi/120.)-calg_CrackWd,
                dz=current_depth/tan_theta/2 -zz0/2
      Call CALBPAR(ag_imed,'SENSITIVE')
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
                        z=current_csda+2.*calg_SmetaWdh+eta_lenght+zz0/2
        elseif(j.eq.2) then
          current_csda=current_csda+2.*(calg_SmetaWdh+eta_lenght)
          eta_lenght=calg_Netfirst*(calg_Seta2Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+calg_SmAlfThk,
                        z=current_csda+eta_lenght +zz0/2
        elseif(j.eq.3) then
          eta_lenght=calg_Netfirst*(calg_Seta1Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+3.*calg_SmAlfThk,
                        z=current_csda+2.*calg_SmetaWdh+eta_lenght+zz0/2,
                        thetaX=90 phiX=180
        elseif(j.eq.4) then
          current_csda=current_csda+2.*(calg_SmetaWdh+eta_lenght)
          eta_lenght=calg_Netfirst*(calg_Seta2Wdh+calg_Set12Wdh)
          create CSDA
          Position CSDA x=current+3.*calg_SmAlfThk,
                        z=current_csda+eta_lenght+zz0/2,
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
      Call CALBPAR(ag_imed,'SENSITIVE')

EndBlock
*
Block CSDA is Al block with sensitive gas volume
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSDA seen=1 colo=6 Serial=j      
      Shape BOX   dx = calg_SmAlfThk,
                  dy = calg_SmAlfWdh, 
                  dz = eta_lenght -zz0/2
      Call CALBPAR(ag_imed,'SENSITIVE')

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
      Call CALBPAR(ag_imed,'SENSITIVE')

EndBlock
*
Block CSMB is the back first (last) Al rib 
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSMB seen=1 colo=6
      Shape BOX dx = calg_SmAlfThk,
                dy = calg_SmAfbWdh
      Call CALBPAR(ag_imed,'SENSITIVE')

EndBlock
*
Block CSME is the part of CSDA Al box with Ar/CO2 sensiteve gas 
      Material Aluminium 
      Material CAluminium Isvol=0
      Medium Al_smd
      attribute CSME seen=1 colo=6
      Shape Division Iaxis=2 Ndiv = nint(calg_NSmdAlw)
      Call CALBPAR(ag_imed,'SENSITIVE')

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
      Mixture   sens_gas  Dens=0.0018015  Isvol=1        
      attribute CSHI      seen=1   colo=4
      if(i.eq.1) then
        Shape BOX  dx = calg_SmGasThk,
                   dy = calg_SmGasWdh
      else
        Shape TUBS Rmin = 0  Rmax = calg_SmGasRad,
                   Dx = calg_SmGasWdh,
                   Phi1 = 270 phi2 = 450
      endif
      call CALBPAR(ag_imed,'SENSITIVE')
      CAll GSTPAR (ag_imed,'STRA',1.0)
*
      h_eta1=2.*(calg_Seta1Wdh+calg_Set12Wdh)
      sh_eta1=calg_Netfirst*h_eta1
      h_eta2=2.*(calg_Seta2Wdh+calg_Set12Wdh) 
      sh_eta2=calg_Netsecon*h_eta2
      h_phi1=2.*(calg_SphiWdh+calg_SphidWdh)
      h_phi2=h_phi1
      sh_phi1=calg_NPhistr*h_phi1
      sh_phi2=sh_phi1
*
      HITS  CSDA type=1:2: eta:0.1:(0,1) etsp:h_eta1:(0,sh_eta1) Eloss:0:(0,1)
*                   xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
*                   px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
*                   Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100)

      HITS  CSDA type=2:2: eta:0.1:(0,1) etsp:h_eta2:(0,sh_eta2) Eloss:0:(0,1)
*                   xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
*                   px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
*                   Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100)

      HITS  CSDA type=3:2: eta:0.1:(0,1) etsp:h_phi1:(0,sh_phi1) Eloss:0:(0,1)
*                   xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
*                   px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
*                   Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100)

*
      HITS  CSDA type=4:2: eta:0.1:(0,1) etsp:h_phi2:(0,sh_phi2) Eloss:0:(0,1)
*                   xx:16:SHX(-300,300)  yy:16:(-300,300)   zz:16:(-350,350),
*                   px:16:(-100,100)     py:16:(-100,100)   pz:16:(-100,100),
*                   Slen:16:(0,1.e4)     Tof:16:(0,1.e-6)   Step:16:(0,100)

*
EndBlock
*
      end







