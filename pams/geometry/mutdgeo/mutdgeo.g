* $Id: mutdgeo.g,v 1.1 2006/03/21 23:49:01 potekhin Exp $
* $Log: mutdgeo.g,v $
* Revision 1.1  2006/03/21 23:49:01  potekhin
* We MUST add this geom to our system in order to account
* for an existing prototype of the Muon trigger system and
* accomodate its possible expansion into a full blown detector.
* First version in need of final touches.
*
*
*
********************************************************************************
Module  MUTDGEO is the geometry of the STAR muon trigger system
********************************************************************************
+CDE,AGECOM,GCUNIT.
   Author    Maxim Potekhin
   Created   21 March 2006

   Content   MUTD,MUSC,MTRA,MXTR,MMTC,MXSA

   Structure MTDG { version,Rmin, Rmax, dz, Length, Radii(2) }
   Structure MTRY { Height, Width, Length, WallThk, SupFullH, SupFullW,
                       SupLen,
                       SupBaseT, SupBaseW, SupArmT, CoolOutR, CoolInnR,
                       StripT  , FootInse, FootThk, Foot1Len, Foot2Thk,
                       Foot3Len}

   Structure MTBB { Slab1Len, Slab2Len, Slab1x,  
                    Slab2x,   SlabThck, SlabWid,  
                    ConvLen,  ConvWidM, ConvThck, 
                    PmtLen,   PmtMaxR,  PmtMinR, 
                    BaseLen,  BaseMaxR, BaseMinR, 
                    ElecThck, Wrap,     Shim   }
   Integer Ntray;
   Real BarPhi, xpos, ypos, zpos;
********************************************************************************
*
   Fill MTDG                ! Muon system basic dimensions
      version  = 1          ! version number
      Rmin     = 390.00     ! inner radius of the magnet system
      Rmax     = 435.00     ! outer radius of the magnet system
      dz       = 246.0      ! CTB/TOF tube half length
      Length   = 500.00     ! slightly longer than full length of the trays
      Radii    = {390.093, 420.093}          ! radii of trays
   Endfill

   Fill MTRY ! general tray stats        
      Height    =  8.89      ! tray height(8.89)
      Width     = 21.59      ! full tray width
      Length    = 241.62     ! full tray length(241.62)
      WallThk   =  0.13      ! tray wall thickness
      SupFullH  =  2.03      ! support height (radial)
      SupFullW  =  15.24     ! support full width with arms
      SupLen    = 215.9      ! support length
      SupBaseW  =  9.22      ! support base width
      SupBaseT  =  0.32      ! support base thickness  
      SupArmT   =  0.64      ! support arm  thickness
      CoolOutR  =  0.80      ! Cooling channel outer radius
      CoolInnR  =  0.48      ! Cooling channel inner radius
      StripT    =  0.08      ! Thickness of polyethylene strip on bottom
      FootInse  =  1.06      ! foot inset from tray edge
      FootThk   =  0.23      ! thickness of foot material
      Foot1len  =  1.68      ! length (in section) of first part of foot
      Foot2Thk  =  1.16      ! thickness of second foot section
      Foot3Len  =  2.16      ! length of third part of foot
   Endfill

   Fill MTBB ! barrel trigger stats
      Slab1Len  = 112.5      ! first slab (B) length
      Slab2Len  = 130.0      ! second slab (A)length 
      Slab1x    = 5.84       ! first slab (B) x position
      Slab2x    = 2.67       ! second slab (A) x position
      SlabThck  = 1.0        ! scintillator slab thicknesses
      SlabWid   = 21.0       ! scintillator slab width
      ConvLen   = 8.5        ! optical converter length
      ConvWidM  = 4.0        ! optical convertor min width
      ConvThck  = 0.92       ! optical convertor thickness
      PmtLen    = 5.0        ! PMT length
      PmtMaxR   = 2.0        ! PMT max radius
      PmtMinR   = 1.84       ! PMT min radius
      BaseLen   = 4.0        ! Base length
      BaseMaxR  = 2.13       ! Base max radius
      baseMinR  = 1.0        ! Base min radius
      ElecThck  = 0.25       ! readout electronics thickness
      Wrap      = 0.13       ! thickness of Tyvek + black plastic
      Shim      = 0.26       ! thickness of shim to position slat 2
   Endfill

*
       Use    MTDG
       Use    MTRY
       Use    MTBB

       create and position MUTD in Cave

* --------------------------------------------------------------------------
Block MUTD is the muon detector mother
       material  Air
       medium    Standard
       Attribute MAGP     seen=1  colo=1

       Shape     TUBE  Rmin=MTDG_Rmin  Rmax=MTDG_Rmax  dz=MTDG_Length/2

* the number of slabs is already hardocoded in magp, so why bother:
       BarPhi = 360.0/30.0;

       Create    MUSC

       Position  MUSC AlphaZ=BarPhi/2.0
* x=MTDG_Radii(Ntray)-MTDG_Rmin 

EndBlock
* ---------------------------------------------------------------------------
Block MUSC is a sector of MUON Trigger Barrel Scintillators
      Attribute MUSC      seen=1   colo=1

      Shape     Tubs      phi1 = -3.0 phi2 = 3.0

      do Ntray=1,2
          Create and Position MTRA   X = MTDG_Radii(Ntray)+(MTRY_SupFullH+MTRY_height+MTRY_StripT)/2.0
      enddo

EndBlock
* ------------------------------------------------------------------------------
*     remember that volume attributes are inherited, no need to redefine serial
*
Block MTRA is one full tray plus supporting structure for CTB/TOF
      Attribute MTRA      seen=1   colo=2

*      shape box dx=(mtry_supfullh+mtry_height+mtry_stript)/2, dy=mtry_width/2

      Shape     BOX       dx=(MTRY_SupFullH+MTRY_height+MTRY_StripT)/2, dy=MTRY_Width/2
      Create and Position MXTR     X=(MTRY_SupFullH+MTRY_StripT)/2,
                                   z=(MTDG_dz-MTRY_length)/2
*      Create and Position BUND     X=-(MTRY_height+MTRY_StripT)/2,
*                                   z=(MTDG_dz-MTRY_SupLen)/2 

EndBlock
*------------------------------------------------------------------------------
*
Block MXTR  is a Main TRay covering box for CTB or TOF
      Attribute  MXTR     seen=1   colo=2
      Material   Aluminium

      Shape      BOX      dx=MTRY_height/2,
                          dz=MTRY_length/2  
      Create and Position MMTC
EndBlock
*------------------------------------------------------------------------------
*
Block MMTC  is  the Main Tray Cavity filled with the details for CTB
      Attribute  MMTC     seen=1   colo=5
      Material   Air
      Shape      BOX      dx=MTRY_height/2-MTRY_WallThk,  
                          dy=MTRY_Width/2-MTRY_WallThk,
                          dz=MTRY_Length/2-MTRY_WallThk
*
*---- inner slab + readout
      zpos  =  (MTRY_length-MTBB_Slab1Len)/2-MTRY_WallThk-MTBB_Wrap
      xpos  =  -MTRY_Height/2+MTBB_Slab1x
      Create and Position MXSA  dx=MTBB_SlabThck/2 ,
                                dy=MTBB_SlabWid/2 ,
                                dz=MTBB_Slab1Len/2,
                                X=xpos  Z=zpos
*---- outer slab + readout
      zpos  =  (MTRY_length-MTBB_Slab2Len)/2-MTRY_WallThk-MTBB_Wrap-MTBB_Shim
      xpos  =  -MTRY_Height/2+MTBB_Slab2x
      Create and Position MXSA dx=MTBB_SlabThck/2,
                               dy=MTBB_SlabWid/2,
                               dz=MTBB_Slab2Len/2,
                               X=xpos Z=-zpos
EndBlock
*------------------------------------------------------------------------------
Block MXSA  is  the active trigger scintillator SLAB for ctb 
      Attribute MXSA      seen=1   colo=3
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX    dx=0  dy=0 dz=0
*
*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
      HITS    MXSA   X:.01:S   Y:.01:   Z:.01:,
                     Ptot:18:(0,100)    cx:10:   cy:10:   cz:10:,
                     Sleng:.1:(0,500)   ToF:16:(0,1.e-6) Step:.01:,      
                     Eloss:16:(0,0.01) 
EndBlock



* ---
End
