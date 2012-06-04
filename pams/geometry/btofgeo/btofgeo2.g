* $Id: btofgeo2.g,v 1.15 2011/02/28 15:33:40 jwebb Exp $
*
* btofgeo2.g is the geometry to contain TOFp+r and the CTB
* $Log: btofgeo2.g,v $
* Revision 1.15  2011/02/28 15:33:40  jwebb
* Cosmetic changes to comments needed for AgML translation.
*
* Revision 1.14  2010/06/23 19:09:49  jwebb
* Resolved minor bug in the cooling tubes for the tof.  The inner radii of
* the cooling tubes are passed to the block creating the water volume, but
* the shape operator indicated that it should inherit its parameters from
* the mother volume.  This resulted in the outer radius of the water volume
* being set equal to the outer radius of the cooling tube.
*
* Revision 1.13  2005/08/04 23:39:24  potekhin
* A very old bug, with BLEM having zero sizes and being
* neve properly sized. Since this volume effectively
* didn't exist, for that reason, we are now removing
* it altogether, to avoid annoying error messages
* when running VMC software
*
* Revision 1.12  2005/05/23 14:56:15  llope
* removed unneeded Dens=0.282 statements from honeycomb Material definitions
*
* Revision 1.11  2004/07/08 01:44:09  potekhin
* Restoring previosu version since I decided that Frank's
* correction should go into a separate file number 3
*
* Revision 1.9  2004/03/16 18:10:23  llope
* fixed posit1 pieces and added comments for readabilityd
*
* Revision 1.8  2004/03/12 20:32:29  llope
* only added print statement to confirm "choice" selection at run-time
*
* Revision 1.7  2004/02/25 19:21:33  llope
* fine-tuning TOFp and TOFr for Run-IV - use choice=7 for these
*
* Revision 1.6  2004/02/12 18:59:56  llope
* modifications for run4 MRPC positioning inside TOFr-prime
*
* Revision 1.6 2004/02/12 12:00:00 llope modifications for run4
*
*
*******************************************************************************
Module  BTOFGEO2 is the Geometry of Barrel Trigger / Time Of Flight system 
*******************************************************************************

  Author     W.J. Llope 
  Created    29 December 1999
*---Version 3------------------------------------------------------------------
* modified   29 Dec  1999, WJL- many changes to implement actual "TOFp Detector" 
*       better HVSys cell geometry....
*       slat length now 20cm...
*       big changes to divisioning scheme for phi segmentation!!! 
*       slat positioning based on AutoCAD file...
*       foam-support plastic angles included...
*       FEE dimensioning updated, FEE posns from AutoCAD, including lemo connectors...
* modified   ~9 Apr  2000  PN?- rearranged phi segmentation scheme
* modified   27 Apr  2000  FG- removed posit2 from BTOG structure
*                          FG- increased version number to 3
*                          FG- default TOF_choice is now 4 (single-tray TOF)
*            04 May  2000  FG- removed the (unused) BMTM block
*            27 Jul  2000  FG- TOFp is now on the EAST side, position 32
*            22 Nov  2000  PN- tof geometry versions 1 and 2 preserved
*                              in tofgeo1, version 3 goes in btofgeo2
*            08 Feb  2001  FG- final positions of the slats and FEE boards
*            20 Feb  2001  FG- fixed bug in outer CTB slab dimensioning,
*                              water in TOFp cooling loops
*            03 Apr  2001  GE- add BRTC and BRMD for TOFr
*            28 May  2002  GE- Change the Geometry of TOFr(S.L. Huang)
*            10 Jun  2002  GE- add BRSG BUPC BRFE on geometry & change the 
*            sensitive volume to gas layers
*            Oct/Nov 2002  SY/SH/FG - many, many updates
*            26 Nov  2002  FG - posit2 reintroduced. TOFr is now at
*                              pos.23. New TOF_choices are 5 (single
*                              TOFp+single TOFr tray) and 6 (full TOFr
*                              system)
*            04 Dec  2002 FG - modr structure changed. Position arrays
*                             introduced for the TOFr geometry. Common
*                             material definitions moved to the top.
*            12 Feb  2004 WJL- modifications for run-4 TOFr' geometry
*            23 Feb  2004 WJL- fine-tuning of run-4 geometry
*                                 use choice==7 for run-4
*                                 BTOG_posit1b reflects move of TOFp in run-IV
*
*
*******************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
*   List of GEANT volumes: note BLEM was removed 20050804 due to the ancient bug
*   which assigned null size to it, so it doesn't matter anyway
      Content   BTOF,BTOH,BSEC,BTRA,BUND,BTFT,BASE,BARM,BANG,
                BWAT,BCOV,BXTR,BMTC,BTTC,BMAA,BMTD,
                BASS,BXSA,BCSB,BCCV,BFEE,
                BCPM,BCSK,BTSK,BZEL,BCEL,BCEB,BCON,BPLA,
                BCOO,BRAI,BPIP,BPIQ,
                BRTC,BRMD,BRHC,BRCB,BRMY,BRGR,BROG,
                BRDT,BRSG,BRIG,BRWG,
                BRFE,BUPC,BGMT

*
*   Data Base interface staff:
      Structure BTOG { Version, Rmin, Rmax, dz, choice, posit1(2), posit2 }
*
      Structure TRAY { Height, Width, Length, WallThk, SupFullH, SupFullW,
                       SupLen,
                       SupBaseT, SupBaseW, SupArmT, CoolOutR, CoolInnR,
                       StripT  , FootInse, FootThk, Foot1Len, Foot2Thk,
                       Foot3Len}
*
      Structure CTBB { Slab1Len, Slab2Len, Slab1x,  
                       Slab2x,   SlabThck, SlabWid,  
                       ConvLen,  ConvWidM, ConvThck, 
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, 
                       ElecThck, Wrap,     Shim   }
*
      Structure TOFF { BoxWidth, SlatLen, 
                       Slat01z,  Slat02z,  Slat03z,  Slat04z,  Slat05z, 
                       Slat06z,  Slat07z,  Slat08z,  Slat09z,  Slat10z, 
                       SlatThck, SlatWid,  SlatAng,
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, SockLen,
                       CellWid,  CellHgt,
                       ElecHgt,  ElecThck, ElecWid,  ElecLen,
                       Elec01z,  Elec02z,  Elec03z,  Elec04z,  Elec05z, 
                       Elec06z,  Elec07z,  Elec08z,  Elec09z,  Elec10z, 
                       RailThck, RailWid,
                       CoolInnR, CoolOutR } 
*
      Structure MODR { Height, Width, Length,Center, mrpcX(33), 
                       mrpcZ(33), mrpcA(33),
                       HCHgt,  HCWid,  HCLen,  PCBHgt, PCBWid, PCBLen,
                       MYHgt,  MYWid,  MYLen,  GRHgt,  GRWid,  GRLen,
                       OGHgt,  OGWid,  OGLen,  IGHgt,  IGWid,  IGLen,
                       SPRMin, SPRMax, SPLen,  WGRMin, WGRMax, WGLen,
                       FEEH,   HBWid,  NGap }
*
      Structure MOD4 { Height, Width, Length, Center, mrpcX(32), 
                       mrpcZ(32), mrpcA(32),
                       HCHgt,  HCWid,  HCLen,  PCBHgt, PCBWid, PCBLen,
                       MYHgt,  MYWid,  MYLen,  GRHgt,  GRWid,  GRLen,
                       OGHgt,  OGWid,  OGLen,  IGHgt,  IGWid,  IGLen,
                       SPRMin, SPRMax, SPLen,  WGRMin, WGRMax, WGLen,
                       FEEH,   HBWid,  NGap, TrayEdgeZ }
*
      real      support_arm_width,  support_arm_Xpos,  support_arm_Ypos,
                support_aile_width, support_aile_Ypos, 
                xpos, ypos, zpos, totlen, sublen, subcen, x0, z0,
                Y, Z, DTHgt
      integer   i,is, choice, tof, iwid, igap
*
* -------------------------------------------------------------------------
*
      Fill BTOG ! Barrel Trigger, CTB/TOF Basic dimensions 
         Version   = 5         ! geometry version
         Rmin      = 207.80    ! minimum CTB/TOF system radius (as built)
         Rmax      = 219.5     ! maximum CTB/TOF system radius
         dz        = 246.0     ! CTB/TOF tube half length
         choice    = 7         ! 1=CTB, 2=Full-TOFp, 3=25% TOFp, 4=1 tray-TOFp, 
                               ! 5=1 tray-TOFr, 6=Full-TOFr, 7=TOFp+TOFrp Run-IV
         posit1    = {32,33}   ! TOFp tray position: (0) choice 4 or 5 -> run-2,3 posn, 
                               !                     (1) choice 7 -> run-4 posn
         posit2    = 23        ! TOFr tray position for choice 5 
*
      Fill TRAY ! general tray stats        
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
*
      Fill CTBB ! barrel trigger stats
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
*
      Fill TOFF ! time of flight stats
         BoxWidth  = 21.0875  ! width of the 5w box (BMTD)
         SlatLen   = 20.0     ! slat length
         Slat01z   =  104.938 ! 5_wide_slat Z position for row 1 from AutoCAD
         Slat02z   =   84.060 ! 4_wide_slat Z position for row 2 from AutoCAD
         Slat03z   =   62.860 ! 4_wide_slat Z position for row 3 from AutoCAD
         Slat04z   =   41.254 ! 4_wide_slat Z position for row 4 from AutoCAD
         Slat05z   =   18.966 ! 4_wide_slat Z position for row 5 from AutoCAD
         Slat06z   =   -3.954 ! 4_wide_slat Z position for row 6 from AutoCAD
         Slat07z   =  -27.528 ! 4_wide_slat Z position for row 7 from AutoCAD
         Slat08z   =  -51.254 ! 4_wide_slat Z position for row 8 from AutoCAD
         Slat09z   =  -75.634 ! 4_wide_slat Z position for row 9 from AutoCAD
         Slat10z   = -100.683 ! 4_wide_slat Z position for row 10 from AutoCAD
         SlatThck  = 2.0      ! scintillator slab thicknesses
         SlatWid   = 3.81     ! scintillator slab width (4.0)
         SlatAng   = 11.5     ! slat assy. angle w.r.t. tray
         PmtLen    = 5.0      ! PMT length
         PmtMaxR   = 1.91     ! PMT max radius
         PmtMinR   = 1.8      ! PMT min radius
         SockLen   = 1.0      ! thickness of socket
         BaseLen   = 5.0      ! Base length
         BaseMaxR  = 1.91     ! Base max radius
         baseMinR  = 1.8      ! Base min radius  
         CellWid   = 3.1      ! Cell width 
         CellHgt   = 1.6      ! Cell height
         ElecHgt   = 3.0      ! FEE Board height in tray... (rails/loop too).
         ElecThck  = 0.17     ! FEE Board thickness (67 mils)
         ElecWid   = 20.3     ! FEE Board width (was 21)
         ElecLen   = 5.715    ! FEE Board length (was 16)
         Elec01z   = 105.610  ! FEE Z position for row 1 from AutoCAD
         Elec02z   =  84.573  ! FEE Z position for row 2 from AutoCAD
         Elec03z   =  63.224  ! FEE Z position for row 3 from AutoCAD
         Elec04z   =  41.667  ! FEE Z position for row 4 from AutoCAD
         Elec05z   =  19.379  ! FEE Z position for row 5 from AutoCAD
         Elec06z   =  -3.542  ! FEE Z position for row 6 from AutoCAD
         Elec07z   = -27.165  ! FEE Z position for row 7 from AutoCAD
         Elec08z   = -50.841  ! FEE Z position for row 8 from AutoCAD
         Elec09z   = -75.170  ! FEE Z position for row 9 from AutoCAD
         Elec10z   = -100.270 ! FEE Z position for row 10 from AutoCAD
         RailThck  = 0.2      ! Cooling loop rail thickness
         RailWid   = 1.5      ! Cooling loop rail width
         CoolOutR  = 0.635    ! Cooling loop pipe outer radius, 0.5in/2
         CoolInnR  = 0.561    ! Cooling loop pipe inner radius, (0.5in-0.058in)/2
*
      Fill MODR ! RUN3 MRPC TOF Module dimensions and positions
         Height    = 1.95     ! Module height (r)
         Width     = 21.2     ! Module width (phi)
         Length    = 9.4      ! Module length (z)
         Center    = 0.35     ! Module detector center in (phi)
         mrpcX = { 4.76, 1.21, 4.99, 1.46, 5.19, 1.71, 4.98, 1.54, 2.69, 3.39,
                   3.25, 3.49, 3.33, 3.54, 3.54, 3.57, 3.48, 3.51, 3.19, 3.19,
                   3.19, 3.19, 3.19, 3.43, 3.43, 3.43, 3.43, 3.43, 3.43, 3.43,
                   3.43, 3.43, 3.43 } ! mrpc Xpositions
         mrpcZ = {  4.50,  10.48,  16.83,  22.64,  29.25,  34.89,
                   41.68,  47.19,  53.76,  60.18,  66.53,  72.95,
                   79.42,  85.97,  92.59,  99.28, 106.00, 112.84,
                  119.60, 126.66, 133.80, 141.06, 148.43, 156.09,
                  163.71, 171.46, 179.35, 187.38, 195.56, 203.84,
                  212.33, 220.94, 229.7 } ! mrpc Zpositions
         mrpcA = {  1.20,  2.84,  4.48,  6.12,  7.74,  9.36,
                   10.97,  7.60, 14.30, 22.30, 22.30, 22.30,
                   22.30, 22.30, 22.30, 22.30, 22.30, 22.30,
                   33.00, 33.00, 33.00, 33.00, 33.00, 37.58,
                   37.58, 37.58, 37.58, 37.58, 37.58, 37.58,
                   37.58, 37.58, 37.58 } ! mrpc angles
         HCHgt  =  0.466  ! HC->Height (r)
         HCWid  = 20.8    !  HC->Width (phi)
         HCLen  =  8.4    !   HC->Length (z)
         PCBHgt =  0.15   ! PCB->Height (r)
         PCBWid = 21.0    !  PCB->Width  (phi)
         PCBLen =  9.4    !   PCB->Length (z) *PCBLen =  9.4(real)
         MYHgt  =  0.035  ! MYlar->Height
         MYWid  = 21.2    !  MYlar->Width
         MYLen  =  8.4    !   MYlar->Length
         GRHgt  =  0.013  ! GRaphite->Height
         GRWid  = 20.2    !  GRaphite->Width
         GRLen  =  7.4    !   GRaphite->Length
         OGHgt  =  0.11   ! Outer Glass->Height
         OGWid  = 20.6    !  Outer Glass->Width
         OGLen  =  7.8    !   Outer Glass->Length
         IGHgt  =  0.054  ! Inner Glass->Height
         IGWid  = 20.0    !  Inner Glass->Width
         IGLen  =  6.1    !   Inner Glass->Length
         SPRMin =  0.     ! SeParator Tube->RMin
         SPRMax =  0.011  !  SeParator Tube->RMax
         SPLen  =  7.8    !   SeParator Tube->Length
         WGRMin =  0.     ! Wedge Tube: RMin
         WGRMax =  0.15   !  Wedge Tube->RMax
         WGLen  = 10.0    !   Wedge Tube->Length
         FEEH   =  0.15   ! tofr fee pcb thickness
         HBWid  =  0.635  ! the slim honeycomb support box width
         NGap   = 6       ! Number of gaps in MRPC
      EndFill
*         NPad   = 6       ! Number of pads within a MRPC
* 
      Fill MOD4   ! RUN4 MRPC TOF Module dimensions and positions 
         Height    = 1.95     ! Module height (r)
         Width     = 21.2     ! Module width (phi)
         Length    = 9.4      ! Module length (z)
         Center    = 0.35     ! Module detector center (phi)
         mrpcZ = { 5.83, 11.97, 18.17, 24.03, 30.55, 36.22, 42.54, 48.87, 
                  55.19, 61.53, 67.86, 74.24, 80.68, 87.08, 93.64, 100.36, 
                  107.19,114.05,121.02,128.08,135.24,142.51,149.88,157.35, 
                  164.93,172.63,180.43,188.35,196.38,204.52,212.79,221.17 } ! mrpc Zposns
         mrpcX = { 1.5, 2.83, 4.16, 1.55, 4.73, 1.88, 2.56, 3.06, 
                   3.3, 3.41, 3.32, 3.2, 3.07, 2.73, 2.58, 2.64, 
                   2.74, 2.74, 2.76, 2.76, 2.76, 2.76, 2.76, 2.76, 
                   2.76, 2.76, 2.76, 2.76, 2.76, 2.76, 2.76, 2.76 } ! mrpc Xposns
         mrpcA = { 6., 6., 6., 6., 0., 12., 12., 16., 
                  16., 19., 19., 19., 19., 24., 24., 25., 
                  27., 29., 30., 30., 30., 30., 30., 30., 
                  30., 30., 30., 30., 30., 30., 30., 30. } ! mrpc angles
         HCHgt  =  0.466  ! HC->Height (r)
         HCWid  = 20.8    !  HC->Width (phi)
         HCLen  =  8.4    !   HC->Length (z)
         PCBHgt =  0.15   ! PCB->Height (r)
         PCBWid = 21.0    !  PCB->Width  (phi)
         PCBLen =  9.4    !   PCB->Length (z) *PCBLen =  9.4(real)
         MYHgt  =  0.035  ! MYlar->Height
         MYWid  = 21.2    !  MYlar->Width
         MYLen  =  8.4    !   MYlar->Length
         GRHgt  =  0.013  ! GRaphite->Height
         GRWid  = 20.2    !  GRaphite->Width
         GRLen  =  7.4    !   GRaphite->Length
         OGHgt  =  0.11   ! Outer Glass->Height
         OGWid  = 20.6    !  Outer Glass->Width
         OGLen  =  7.8    !   Outer Glass->Length
         IGHgt  =  0.054  ! Inner Glass->Height
         IGWid  = 20.0    !  Inner Glass->Width
         IGLen  =  6.1    !   Inner Glass->Length
         SPRMin =  0.     ! SeParator Tube->RMin
         SPRMax =  0.011  !  SeParator Tube->RMax
         SPLen  =  7.8    !   SeParator Tube->Length
         WGRMin =  0.     ! Wedge Tube: RMin
         WGRMax =  0.15   !  Wedge Tube->RMax
         WGLen  = 10.0    !   Wedge Tube->Length
         FEEH   =  0.15   ! tofr fee pcb thickness
         HBWid  =  0.635  ! the slim honeycomb support box width
         NGap   = 6       ! Number of gaps in MRPC
         TrayEdgeZ = (3.0*2.54)+1.0   ! tray posn along rail wrt TPC centerplane (Z)
      EndFill
*         NPad   = 6       ! Number of pads within a MRPC
*
      USE   BTOG
      USE   TRAY
      USE   CTBB
      USE   TOFF
      USE   MODR
      USE   MOD4
*
* ---
* G10 material definition for PCBs
      Component Si   A=28.08  Z=14   W=0.6*1*28./60.   
      Component O    A=16     Z=8    W=0.6*2*16./60.   
      Component C    A=12     Z=6    W=0.4*8*12./174.  
      Component H    A=1      Z=1    W=0.4*14*1./174.
      Component O    A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10  Dens=1.7
* ---
* RPCgas material for TOFr gas
      Component H    A=1      Z=1    W=0.90*2*1./102.  + 0. + 0.05*10*1./58.
      Component C    A=12     Z=6    W=0.90*2*12./102. + 0. + 0.05*4*12./58.
      Component F    A=19     Z=9    W=0.90*4*19./102. + 0.05*6*19./146. + 0.
      Component S    A=32     Z=16   W=0.              + 0.05*1*32./146. + 0.
      Mixture   RPCgas  Dens=4.55E-3
* ---
      Component  Al        A=27  Z=13   W=0.0105
      Component  N         A=14  Z=7    W=0.7395
      Component  Adhesive  A=9   Z=4.5  W=0.2500
      Mixture    HoneyComb Dens=0.282
* ---
*
      Create and Position BTOF in Cave
*
*******************************************************************************
*     Rotate the +z half of the tube so that the phi divisions run clockwise
*     for each half of the detector when viewed from outside. The direction of 
*     increasing z is then toward eta=0 in each half.
*
Block BTOF is the whole CTF system envelope 
      Attribute BTOF      seen=1  colo=1
      Material  Air
      Medium    Standard
      Shape     Tube      rmin=btog_Rmin  Rmax=btog_Rmax  dz=btog_dz

	  if (btog_choice == 7) print *,' TOF: btog_choice=7: This is the Run-IV geometry...'

      choice = 1                                     ! ctb
      if (btog_choice == 2) choice=btog_choice       ! full tofp
      if (btog_choice == 6) choice=btog_choice       ! full tofr
!      print *,' Positioning West Barrel, choice=',choice
      Create and Position BTOH  z=+btog_dz/2    alphay=180   ! West barrel
      choice=btog_choice                   
!      print *,' Positioning East Barrel, choice=',choice
      Create and Position BTOH  z=-btog_dz/2                 ! East barrel
EndBlock
*
*------------------------------------------------------------------------------
*  BSEC only is needed to model a system that contains both TOF and CTB
*  In the event that z symmetry is broken, two copies of BTOH/BSEC
*  with DIFFERENT names are automatically created by AGI 
*
Block BTOH is a half of trigger system (west-east)
      Attribute BTOH      seen=0  colo=1  serial=choice
      Shape     Tube      dz=btog_dz/2

      ! tof=0 means ctb, tof=1 means TOFp, tof=2 means TOFr, tof=3 means TOFr'
      do is=1,60
         tof=0		                                !// all CTB for choice=1                     
         if (choice==2)                      tof=1	!// all TOFp
         if (choice==3 & 46<=is&is<=60)      tof=1	!// big TOFp patch, rest CTB
         if (choice==4 & is==btog_posit1(1)) tof=1	!// Run-2 (one TOFp tray)
         if (choice==5 & is==btog_posit1(1)) tof=1	!// Run-3 (one TOFp tray
         if (choice==5 & is==btog_posit2)    tof=2	!      and one TOFr tray)
         if (choice==6)                      tof=2	!// all TOFr
         if (choice==7 & is==btog_posit1(2)) tof=1	!// Run-4 (one TOFp tray moved 1 slot
         if (choice==7 & is==btog_posit2)    tof=3	!      and one TOFrp tray)
!         print *,' Positioning Tray, choice,is,tof=',choice,is,tof
         Create and Position BSEC  alphaz = 102+6*is
      enddo
EndBlock
*
*------------------------------------------------------------------------------
Block BSEC is a sector of CTB/TOF Trigger Barrel Scintillators
      Attribute BSEC      seen=0   colo=1  serial=tof  
      Shape     Tubs      phi1 = -3.0 phi2 = 3.0
      Create and Position BTRA   X = _ 
             btog_Rmin+(tray_SupFullH+tray_height+tray_StripT)/2
      if(tof==2) then
         Create and Position BRFE   X = _
             btog_Rmin+tray_SupFullH+tray_StripT+tray_height+modr_feeh/2,
                                 z=(btog_dz-tray_length)/2
      endif
EndBlock
*
*------------------------------------------------------------------------------
*     remember that volume attributes are inherited, no need to redefine serial
*
Block BTRA is one full tray plus supporting structure for CTB/TOF
      Attribute BTRA      seen=1   colo=2
      Shape     BOX       dx=(tray_SupFullH+tray_height+tray_StripT)/2,
                          dy=tray_Width/2
      Create and Position BXTR     X=(tray_SupFullH+tray_StripT)/2,
                                   z=(btog_dz-tray_length)/2
      Create and Position BUND     X=-(tray_height+tray_StripT)/2,
                                   z=(btog_dz-tray_SupLen)/2 
EndBlock
*
*------------------------------------------------------------------------------
*
Block BRFE is the FEE of tofr (run-3)
      Attribute BRFE seen=0   colo=3
      Material  G10
      Shape     BOX       dx=modr_feeh/2,
                          dy=tray_Width/2,
                          dz=tray_Length/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block BXTR  is a Main TRay covering box for CTB or TOF
      Attribute  BXTR     seen=0   colo=2
      Material   Aluminium

      Shape      BOX      dx=tray_height/2,
                          dz=tray_length/2  
      if (tof==1) then
         Create and Position BTTC
      else if (tof>=2) then
         Create and Position BRTC
         Create and Position BUPC  X=(tray_Height-tray_WallThk)/2
      else
         Create and Position BMTC
      endif 
EndBlock
*
*------------------------------------------------------------------------------
*
Block BMTC  is  the Main Tray Cavity filled with the details for CTB
      Attribute  BMTC     seen=-1   colo=5
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
*
*---- inner slab + readout
      zpos  =  (tray_length-ctbb_Slab1Len)/2-tray_WallThk-ctbb_Wrap
      xpos  =  -tray_Height/2+ctbb_Slab1x
      Create and Position BXSA  dx=ctbb_SlabThck/2 ,
                                dy=ctbb_SlabWid/2 ,
                                dz=ctbb_Slab1Len/2,
                                X=xpos  Z=zpos
      zpos = zpos - (ctbb_Slab1Len + ctbb_ConvLen)/2
      Create and Position BCCV  X=xpos  Z=zpos,
                     Dx1=ctbb_SlabThck/2  Dx2=ctbb_SlabThck/2,
                     Dy1=ctbb_ConvWidM/2  Dy2=ctbb_SlabWid/2  dz=ctbb_ConvLen/2
      zpos = zpos - (ctbb_ConvLen + ctbb_PmtLen)/2
      Create and Position BCPM  X=xpos  Z=zpos,
                     Rmin=ctbb_PmtMinR  Rmax=ctbb_PmtMaxR  Dz=ctbb_PmtLen/2
      zpos = zpos - (ctbb_PmtLen + ctbb_BaseLen)/2
      Create and Position BCSK  X=xpos  Z=zpos,
                     Rmin=ctbb_BaseMinR  Rmax=ctbb_BaseMaxR   Dz=ctbb_BaseLen/2
      Create and Position BZEL  X=xpos  Z=zpos dx=ctbb_ElecThck/2, 
                               dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
*
*---- outer slab + readout
      zpos  =  (tray_length-ctbb_Slab2Len)/2-tray_WallThk-ctbb_Wrap-ctbb_Shim
      xpos  =  -tray_Height/2+ctbb_Slab2x
      Create and Position BXSA dx=ctbb_SlabThck/2,
                               dy=ctbb_SlabWid/2,
                               dz=ctbb_Slab2Len/2,
                               X=xpos Z=-zpos
      zpos = zpos - (ctbb_Slab2len + ctbb_ConvLen)/2
                 Position BCCV  X=xpos  Z=-zpos  alphax=180,
                     Dx1=ctbb_SlabThck/2  Dx2=ctbb_SlabThck/2,
                     Dy1=ctbb_ConvWidM/2  Dy2=ctbb_SlabWid/2  dz=ctbb_ConvLen/2
      zpos = zpos - (ctbb_ConvLen + ctbb_PmtLen)/2
                 Position BCPM  X=xpos  Z=-zpos,
                     Rmin=ctbb_PmtMinR  Rmax=ctbb_PmtMaxR  Dz=ctbb_PmtLen/2
      zpos = zpos - (ctbb_PmtLen + ctbb_BaseLen)/2
                 Position BCSK  X=xpos  Z=-zpos,
                     Rmin=ctbb_BaseMinR  Rmax=ctbb_BaseMaxR   Dz=ctbb_BaseLen/2
                 Position BZEL  X=xpos  Z=-zpos dx=ctbb_ElecThck/2,  
                                dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2

EndBlock
*
*-------------------------------------------------------------------------------
Block BTTC  is  the Main Tray Cavity filled with the details for TOFp
      Attribute  BTTC      seen=0  colo=3
      Component  C         A=12 Z=6 W=1
      Component  H2        A=1  Z=1 W=2
      Mixture    LastAFoam Dens=0.048
      Shape      BOX       dx=tray_height/2-tray_WallThk,  
                           dy=tray_Width/2-tray_WallThk,
                           dz=tray_Length/2-tray_WallThk
*---- the 4wide mother box...
      sublen             = ((toff_Slat02z+15.5)-(toff_Slat10z-15.5))
      subcen             = (toff_Slat02z+15.5)-sublen/2.

      iwid=4
      Create and Position BMAA  X=0   Z=subcen konly='MANY'
*---- the 5wide mother box...
      iwid=5
      Create and Position BMAA  X=0.0 Z=toff_Slat01z konly='MANY'

*---- interior cooling rails and tubing....
      Create and Position  BCOO X=0 Y=0 dx=0 dy=0 dz=0
*---- front end electronics boards
      Create    BFEE       dx=toff_ElecThck/2, 
                           dy=toff_ElecWid/2, 
                           dz=toff_ElecLen/2
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec01z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec02z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec03z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec04z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec05z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec06z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec07z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec08z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec09z-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elec10z-toff_ElecLen/2 
*---- plastic angles...
      Create    BPLA
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec01z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec02z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec03z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec04z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec05z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec06z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec07z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec08z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec09z+3.0
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elec10z+3.0
EndBlock
*------------------------------------------------------------------------------
*
Block BUPC is the up pcb cover of tofr
      Attribute BUPC seen=0   colo=3
      Material  G10
      Shape     BOX       dx=tray_WallThk/2,
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk     
EndBlock
*
*------------------------------------------------------------------------------
*
Block BRTC is the Main Tray Cavity filled with the details for TOFr (run3 or run4)
      Attribute  BRTC     seen=0   colo=5
      Material   HoneyComb 
      Shape      BOX      dx=tray_Height/2-tray_WallThk,
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
     Create and Position BGMT konly='MANY'

*---- create and position TOFr modules
* 
* Change the new geometry to honeycomb support of long slim box by
* add mixture gas box in the tray box iterior

*      Create and Position BGMT

      Create BRMD

	if (tof==2) then
      z0 = tray_Length/2 - 0.05
*      x0 = -(btog_Rmin+tray_SupFullH+tray_StripT+tray_Height/2) - 1.5
      x0 = -3.66
      do i=1,33
            Position BRMD  X=x0+modr_mrpcX(i) ,
                           Z=z0-modr_mrpcZ(i) ,
                           alphay=modr_mrpcA(i)
      enddo
	elseif (tof==3) then
      z0 = tray_Length/2 - 0.05 - mod4_TrayEdgeZ
      x0 = -3.66
      do i=1,32
         if (i.le.20.or.(i.ge.25.and.i.le.28)) then
            Position BRMD  X=x0+mod4_mrpcX(i) ,
                           Z=z0-mod4_mrpcZ(i) ,
                           alphay=mod4_mrpcA(i)
         endif
      enddo
    else 

	endif
*
EndBlock
*
*------------------------------------------------------------------------------
*
Block BGMT is the mixture gas box in tray that change the hc box into slim
      Attribute BGMT  seen=0   colo=2
      Material  RPCgas
      Shape     BOX  dx=tray_Height/2-tray_WallThk,
                     dy=tray_Width/2-tray_WallThk-modr_HBWid,
                     dz=tray_Length/2-tray_WallThk
EndBlock
*
*------------------------------------------------------------------------------
*
Block BMAA is a b1ox for a 4wide AND 5wide phi column of TOF Scintillators
      Attribute BMAA  seen=0   colo=2
      if (iwid==4) then
*  ----  the 4wide mother box...
         Shape  BOX    dx=tray_height/2-tray_WallThk,  
                       dy=0.8*toff_BoxWidth/2,
                       dz=sublen/2. 
      else
*---- the 5wide mother box...
         Shape  BOX    dx=tray_height/2-tray_WallThk,  
                       dy=toff_BoxWidth/2,
                       dz=15.5 
      endif
      Create    BMTD  " dont need to positition it, because this is division" 
EndBlock
*
* - - *
Block BMTD is a 5wide phi column of TOF Scintillators
      Attribute BMTD      seen=1   colo=1
      Shape     division  Iaxis=2  Ndiv=iwid  
      Create BASS
      if (iwid==5) then
        Position BASS X=-1.7                       alphay=5.0 konly='MANY'
      else
        Position BASS X=-0.4 Z=toff_Slat02z-subcen alphay=10.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat03z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat04z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat05z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat06z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat07z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat08z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat09z-subcen alphay=11.0 konly='MANY'
        Position BASS X=-0.2 Z=toff_Slat10z-subcen alphay=11.0 konly='MANY'
     endif
EndBlock
* - - *
*
*******************************************************************************
*******************************************************************************
Block BASS is a single TOF Slat Assembly (slat+PMT+base)
      Attribute BASS seen=0 colo=6
      totlen = toff_SlatLen+toff_PmtLen+toff_BaseLen
      Shape BOX dx=toff_PmtMaxR, 
                dy=(tray_Width/2-tray_WallThk)/5.,
                dz=totlen/2.
      zpos = -(totlen-toff_SlatLen)/2
      Create and Position BCSB  Z=zpos
      zpos = zpos + (toff_SlatLen+toff_PmtLen)/2
      Create and Position BCPM  X=0                Z=zpos,
                                Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR,  
                                Dz=toff_PmtLen/2
      zpos = zpos + toff_PmtLen/2.
      Create and Position BTSK  X=0                Z=zpos+(toff_SockLen/2),
                                Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR,  
                                Dz=toff_SockLen/2
      Create and Position BCEL  X=0                Z=zpos+(toff_ElecThck/2),
                                Rmin=0             Rmax=toff_PmtMinR,
                                Dz=toff_ElecThck/2
      zpos = zpos + toff_BaseLen/2
      Create BCEB      dx=toff_ElecThck/2,
                       dy=toff_CellWid/2,
                       dz=toff_BaseLen/2
      Position BCEB X=toff_CellHgt/2 Z=zpos
      Position BCEB X=-toff_CellHgt/2 Z=zpos
      zpos = zpos + toff_BaseLen/2 - 0.6
      Create and Position BCON X=0 Y=0 Z=zpos,
                    dx=(toff_CellHgt-toff_ElecThck)/2,
                    dy=1.25,
                    dz=0.6
EndBlock
*
*------------------------------------------------------------------------------
Block BXSA  is  the active trigger scintillator SLAB for ctb 
      Attribute BXSA      seen=0   colo=3
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX    dx=0  dy=0 dz=0
*
*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
      HITS    BXSA   X:.01:S   Y:.01:   Z:.01:,
                     Ptot:18:(0,100)    cx:10:   cy:10:   cz:10:,
                     Sleng:.1:(0,500)   ToF:16:(0,1.e-6) Step:.01:,      
                     Eloss:16:(0,0.01) 
EndBlock
*
*------------------------------------------------------------------------------
Block BCSB  is  the active trigger scintillator SLAB for tof
      Attribute BCSB      seen=1   colo=7
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX    dx=toff_SlatThck/2  dy=toff_SlatWid/2  dz=toff_SlatLen/2 
*
*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
      HITS    BCSB   X:.01:S   Y:.01:   Z:.01:,
                     Ptot:18:(0,100)    cx:10:   cy:10:   cz:10:,
                     Sleng:.1:(0,500)   ToF:16:(0,1.e-6)  Step:.01:,
                     Eloss:16:(0,0.01) 
EndBlock
*
*------------------------------------------------------------------------------
Block BCCV  is  a  Ctb optical ConVerter
      Attribute BCCV      seen=0   colo=3
      Material polystyren 
      Shape   TRD2   Dx1=0   Dx2=0   Dy1=0   Dy2=0  dz=0
EndBlock
Block BCSK  is a CTB Linear Base tube
      Attribute BCSK      seen=0   colo=2
      Material  polystyren 
      Shape   TUBE   Rmin=0  Rmax=0   Dz=0
EndBlock
Block BZEL  is a Ctb PM electronics
      Attribute BZEL      seen=0   colo=6
      Material silicon
      Shape   BOX    dx=0  dy=0  dz=0
EndBlock
Block BCPM  is a PhotoMultiplier Tube (same for CTB and TOF)
      Attribute BCPM      seen=0   colo=1
      Material polystyren 
      Shape   TUBE   Rmin=0  Rmax=0  Dz=0
EndBlock
*
Block BTSK  is the outer shell of a TOF CW Base 
      Attribute BTSK      seen=0   colo=7
      Material  polystyren
      Shape   TUBE   Rmin=0  Rmax=0   Dz=0
EndBlock
Block BCEL is a circular G10 board in the CW Base for TOF
      Attribute BCEL seen=0   colo=3
      Material G10
      Shape     TUBE Rmin=0  Rmax=0   Dz=0
EndBlock
Block BCEB is a square G10 board in the CW Base for TOF
      Attribute BCEL seen=0   colo=3
      Material  G10
      Shape     BOX  dx=0  dy=0  dz=0
EndBlock
*
Block BPLA is the plastic angle pieces that hold the upper foam supports...
      Attribute BPLA      seen=0   colo=4
      Material  polystyren 
      Shape     BOX       dx=0 dy=0 dz=0
      Create and Position BCON  x=0 y=0 z=(-0.5*2.54)/2,
                                dx=0.08*2.54/2,
                                dy=tray_Width/2-tray_WallThk-0.5,
                                dz=0.5*2.54/2
      Position  BCON            x=(-0.08*2.54 - 0.25*2.54)/2,
                                y=0,
                                z=(-0.08*2.54)/2,
                                dx=0.25*2.54/2,
                                dy=tray_Width/2-tray_WallThk-2.0,
                                dz=0.08*2.54/2
EndBlock
Block BCON is a generic plastic block for various connectors, foam-support-angles, etc......
      Attribute BCON      seen=0   colo=6
      Material  polystyren 
      Shape     BOX       dx=0 dy=0 dz=0
EndBlock
*
* -- removing blem --
Block BFEE is a G10 FrontEndElectronics board for TOF
      Attribute BFEE seen=0   colo=3
      Material  G10
      Shape     BOX  dx=0  dy=0  dz=0
*      Create    BLEM
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=-7.0 z=2 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=-3.5 z=2 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=0.   z=2 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=3.5  z=2 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=7    z=2 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=-7.0 z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=-3.5 z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=0.   z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=3.5  z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=7    z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=-6.0 z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=-2.5 z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=1.   z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=4.5  z=-2 alphax=180 
*      Position  BLEM x=toff_ElecThck+(0.7/2) y=8.   z=-2 alphax=180 
EndBlock
*Block BLEM is a Lemo connector on the FEE boards
*      Attribute BLEM seen=0   colo=3
*      Shape     BOX   dx=0 dy=0 dz=0
**      Create    BRAI  dx=0.9/2    dy=0.7/2    dz=0.7/2
**      Create    BPIP  Rmin=0.62/2 Rmax=0.68/2 dz=1.0/2
**      Position  BRAI  x=0            y=0 z=0
**      Position  BPIP  x=(0.9-0.72)/2 y=0 z=(0.7+1.0)/2
*      Create and Position    BPIP  x=(0.9-0.72)/2 y=0 z=(0.8+1.0)/2,
*                                   Rmin=0.62/2 Rmax=0.68/2 dz=2.0/2
*EndBlock
*
*******************************************************************************
*******************************************************************************
Block BCOO  are the cooling rails/loops
      Attribute BCOO  seen=0  colo=2
      Shape     BOX   dx=0 dy=0 dz=0
      Create    BRAI  dx=toff_RailThck/2,
                      dy=toff_RailWid/2,
                      dz=tray_Length/2-tray_WallThk
      Position  BRAI  X=toff_ElecHgt-toff_RailThck,
                      Y= (tray_width/2-toff_RailWid/2-tray_WallThk),
                      konly='MANY'
      Position  BRAI  X=toff_ElecHgt-toff_RailWid/2-toff_RailThck/2,
                      Y= (tray_width/2-toff_RailThck/2-tray_WallThk),
                      alphaz=90,
                      konly='MANY'
      Position  BRAI  X=toff_ElecHgt-toff_RailThck,
                      Y=-(tray_width/2-toff_RailWid/2-tray_WallThk),
                      konly='MANY'
      Position  BRAI  X=toff_ElecHgt-toff_RailWid/2-toff_RailThck/2,
                      Y=-(tray_width/2-toff_RailThck/2-tray_WallThk),
                      alphaz=90,
                      konly='MANY'
      Create    BPIP  Rmin=toff_CoolInnR Rmax=toff_CoolOutR,
                      dz=tray_Length/2-tray_WallThk
      Position  BPIP  X=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR,
             Y= (tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR),
             konly='MANY'
      Position  BPIP  X=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR,
             Y=-(tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR),
             konly='MANY'
      Create    BPIQ  Rmin=toff_CoolInnR Rmax=toff_CoolOutR,
                      dz=tray_width/2-tray_WallThk-2.*toff_CoolOutR-toff_RailThck
      Position  BPIQ  X=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR Y=0.0,
             Z=tray_Length/2-tray_WallThk-toff_RailThck-toff_CoolOutR,
             alphaX=90,
             konly='MANY'
      Create BWAT Rmin=0 Rmax=toff_CoolInnR dz=tray_Length/2-tray_WallThk
      Position BWAT   X=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR,
             Y=(tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR)
      Position BWAT  X=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR,
             Y=-(tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR)
      Create BWAT Rmin=0 Rmax=toff_CoolInnR,
                  dz=tray_width/2-tray_WallThk-2.*toff_CoolOutR-toff_RailThck
      Position BWAT  X=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR Y=0.0,
             Z=tray_Length/2-tray_WallThk-toff_RailThck-toff_CoolOutR,
             alphaX=90
EndBlock
Block BRAI  is the Rail for the cooling loop
      Attribute BRAI   seen=0  colo=7
      Material  Aluminium
      Shape     BOX    dx=0.0  dy=0.0  dz=0.0 
EndBlock
Block BPIP  is the Long Pipe for the cooling loop
      Attribute BPIP seen=0  colo=7
      Material  Aluminium
      Shape     TUBE Rmin=0  Rmax=0  Dz=0
EndBlock
Block BPIQ  is the Short Pipe for the cooling loop
      Attribute BPIQ seen=0  colo=7
      Material  Aluminium
      Shape     TUBE Rmin=0  Rmax=0  Dz=0
EndBlock
*
*
*******************************************************************************
*******************************************************************************
Block BUND   is  Undercarriage support tray - same both for CTB and TOF 
      Attribute BUND      seen=0   colo=1   serial=0
      Shape  BOX dx=tray_SupFullH/2  dy=tray_Width/2  dz=tray_SupLen/2
      Material   Aluminium
                 xpos = (tray_SupFullH - tray_FootThk)/2
                 ypos = (tray_width - tray_Foot1Len)/2 - tray_FootInse 
      Create and Position  BTFT   X = xpos,
                 Y = -ypos dx = tray_FootThk/2 dy = tray_Foot1Len/2
                 Position  BTFT   X = xpos,
                 Y = +ypos dx = tray_FootThk/2 dy = tray_Foot1Len/2
                 xpos = (tray_SupFullH - tray_Foot2Thk)/2
                 ypos = ypos - (tray_Foot1Len + tray_FootThk)/2 
                 Position  BTFT   X = xpos,
                 Y = -ypos dx = tray_Foot2Thk/2 dy = tray_FootThk/2
                 Position  BTFT   X = xpos,
                 Y = +ypos dx = tray_Foot2Thk/2 dy = tray_FootThk/2
                 xpos = (tray_SupFullH + tray_FootThk)/2 - tray_Foot2Thk 
                 ypos = ypos - (tray_FootThk + tray_Foot3Len)/2
                 Position  BTFT   X = xpos,
                 Y = -ypos dx = tray_FootThk/2 dy = tray_Foot3Len/2
                 Position  BTFT   X = xpos,
                 Y = +ypos dx = tray_FootThk/2 dy = tray_Foot3Len/2
      support_aile_width = ( tray_SupFullH-tray_SupArmT )/tan(60*DegRad) 
      support_arm_width  = ( tray_SupFullW-tray_SupBaseW)/2-support_aile_width
      support_aile_Ypos  = ( tray_SupBaseW+Support_Aile_width)/2
      support_arm_Xpos   = ( tray_SupFullH-tray_SupArmT )/2
      support_arm_Ypos   = ( tray_SupFullW-Support_Arm_width)/2
      Material   Aluminium
      Create and Position  BASE   X=(-tray_SupFullH+tray_SupBaseT)/2
      Create and Position  BARM   X=support_arm_Xpos     Y=-support_arm_Ypos
                 Position  BARM   X=support_arm_Xpos     Y=+support_arm_Ypos
      Create and Position  BANG   Y=-support_aile_Ypos
                 Position  BANG   Y=+support_aile_Ypos   thetaX=270
      Create and Position  BCOV   X=-tray_SupFullH/2+tray_CoolOutR
EndBlock
Block BTFT  is the Foot structure    ( Material  Aluminium )
      Attribute BTFT      seen=0   colo=2
      Shape     BOX    dx = 0.0  dy = 0.0  dz = 0.0 
EndBlock
Block BARM  is  a TPC cooling structure arm             ( Material  Aluminium )
      Attribute BARM      seen=0   colo=2
      Shape     BOX    Dx=tray_SupArmT/2   DY=support_arm_width/2
EndBlock
Block BANG  is  an angled part of TPC cooling structure ( Aile )
      Attribute BANG   seen=0   colo=2
      Shape     PARA   dx=tray_SupArmT/2   Dy=support_aile_width/2,
                       Alph=-60   thet=0   phi=0
EndBlock
Block BASE  is  a bottom of TPC coolant structure       
      Attribute BASE   seen=0   colo=2
      Shape     BOX    Dx=tray_SupBaseT/2  Dy=tray_SupBaseW/2
EndBlock
Block BCOV  is  a whole TPC cooling channel
      Attribute BCOV   seen=0   colo=2
      Shape     TUBE   Rmin=0   Rmax=tray_CoolOutR
      Create and Position BWAT Rmin=0  Rmax=tray_CoolInnR
EndBlock
Block BWAT  is  TPC cooling water
      Attribute BWAT   seen=0   colo=3
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Shape     TUBE   Rmin=0  Rmax=0
EndBlock

*------------------------------------------------------------------------------
* the frame of MPRC module:
*
*        HoneyComb      BRHC
*        Double-side tape is not included
*        PCB            BRCB
*        Anode(Electrode) copper-pad is not included so far
*        Mylar          BRMY
*        Graphite       BRGR
*        Outer-Glass    BROG
*        Separator      BRSG
*     /--Inner-Glass    BRIG
*  n +
*     \--Separator
*        Outer-Glass
*        Graphite
*        Mylar
*        Cathode(Electrode) copper-pad is not included so far
*        PCB
*        Double-side tape is not included
*        HoneyComb
*   +    Wedges         BRWG
*
*  Note:
*      1. PCB, short for "Printed Circuit Board
*      2. Separator(BRSP) and Wedge(BRWG) are not precisely positioned
*
*------------------------------------------------------------------------------
Block BRMD  is a six channel module for TOFr
      Attribute  BRMD     seen=0   colo=6
      Material   RPCgas
      Shape      BOX      dx=modr_Height/2,
                          dy=modr_Width/2,
                          dz=modr_Length/2
      Create     BRHC
      Create     BRCB
      Create     BRMY
      Create     BRGR
      Create     BROG
      DTHgt = (modr_IGHgt+modr_SPRMax*2)*(modr_NGap-1)+modr_SPRMax*2
      Create     BRDT  dx=DTHgt/2., dy=modr_IGWid/2., dz=modr_IGLen/2.,
                       konly='MANY'
      Create     BRWG

      XPOS = modr_Height/2.
      Y = 0.
      Z = modr_Center

*
*- Single-direction positioning from bottom only
*
      Position   BRHC   X=XPOS-modr_HCHgt/2 Z=modr_Center
      XPOS = XPOS - modr_HCHgt

      Position   BRCB   X=XPOS-modr_PCBHgt/2 Z=0.
      XPOS = XPOS - modr_PCBHgt

      Position   BRMY   X=XPOS-modr_MYHgt/2  Z=modr_Center
      XPOS = XPOS - modr_MYHgt

      Position   BRGR   X=XPOS-modr_GRHgt/2
      XPOS = XPOS - modr_GRHgt

      Position   BROG   X=XPOS-modr_OGHgt/2
      XPOS = XPOS - modr_OGHgt

      Position   BRDT   X=XPOS-DTHgt/2
      XPOS = XPOS - DTHgt

      Position   BROG   X=XPOS-modr_OGHgt/2
      XPOS = XPOS - modr_OGHgt
                 
      Position   BRGR   X=XPOS-modr_GRHgt/2
      XPOS = XPOS - modr_GRHgt

      Position   BRMY   X=XPOS-modr_MYHgt/2
      XPOS = XPOS - modr_MYHgt
                 
      Position   BRCB   X=XPOS-modr_PCBHgt/2, Z=0.
      XPOS = XPOS - modr_PCBHgt

      Position   BRHC   X=XPOS-modr_HCHgt/2, Z=modr_Center
      XPOS = XPOS - modr_HCHgt

EndBlock
 
*------------------------------------------------------------------------------
Block BRHC  is the HoneyComb in the TOFr module
      Attribute  BRHC      seen=1 colo=1
      Material   HoneyComb
      Shape      BOX       dx = modr_HCHgt/2,
                           dy = modr_HCWid/2,
                           dz = modr_HCLen/2
EndBlock

*------------------------------------------------------------------------------
Block BRCB  is the PCB in the TOFr module
      Attribute  BRCB   seen=1 colo=3
      Material   G10
      Shape      BOX    dx = modr_PCBHgt/2,
                        dy = modr_PCBWid/2,
                        dz = modr_PCBLen/2
EndBlock

*------------------------------------------------------------------------------
Block BRMY  is the MYlar in the TOFr module
      Attribute  BRMY   seen=0 colo=1
      Material   MYLAR
      Shape      BOX    dx = modr_MYHgt/2,
                        dy = modr_MYWid/2,
                        dz = modr_MYLen/2
EndBlock

*------------------------------------------------------------------------------
Block BRGR  is the GRaphite in the TOFr module
      Attribute  BRGR   seen=0 colo=1
      Material   Carbon
      Shape      BOX    dx = modr_GRHgt/2,
                        dy = modr_GRWid/2,
                        dz = modr_GrLen/2
EndBlock

*------------------------------------------------------------------------------
Block BROG  is the Outer Glass in the TOFr module
      Attribute  BROG   seen=1 colo=7
      Component  Si     A=28  Z=14  W=1.
      Component  O      A=16  Z=8   W=2.
      Mixture    Glass  Dens=2.2
      Shape      BOX    dx = modr_OGHgt/2,
                        dy = modr_OGWid/2,
                        dz = modr_OGLen/2
EndBlock

*------------------------------------------------------------------------------
* This is the sensitive part, consisting of gas, excluding volumes(glass and
* separators) inside. The responses (amplitude, efficiency, and time
* resolution) depends on the position.
*
Block BRDT  is the middle part (including innner glass and gas)in the MRPC
      Attribute  BRDT
      Shape      BOX
      Create     BRIG  konly='MANY'
      Create     BRSG  konly='MANY'
      XPOS = DTHgt/2.
      Y    = 0.
      Z    = 0.
      Do igap=1,modr_NGap-1
         Position   BRSG   X= XPOS - modr_SPRMax
         XPOS = XPOS - modr_SPRMax*2.

         Position   BRIG   X= XPOS - modr_IGHgt/2
         XPOS = XPOS - modr_IGHgt
      Enddo

      Position   BRSG   X= XPOS - modr_SPRMax
      XPOS = XPOS - modr_SPRMax*2.

*-Divsion of BRDT into Pad-Rooms(BRPR)
*
*      CREATE BRPR

*
*  hit is defined in BRDT instead of BTPR, because a hit may yield signal
*   in two adjacent pads
*
*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
*      HITS    BRDT   X:.01:HS   Y:.01:   Z:.01:,
*                     Ptot:18:(0,100),
*                     Sleng:.1:(0,500)   ToF:16:(0,1.e-7)  Step:.01:,
*                     Eloss:16:(0,1.e-6) 
EndBlock


*------------------------------------------------------------------------------
*  It includes both anode and chathode, plus room between them, so it is the
*  basic unit area to collect signal
*
*Block BRPR is the pad in the TOFr module
*      Attribute  BRPR
**      Shape      Division  IAxis=3  NDiv=modr_NPad
*      Shape      Box    dy = modr_PadWid,
*                        dz = modr_PadLen, konly='MANY'
*EndBlock


*------------------------------------------------------------------------------
Block BRIG  is the Inner Glass in the TOFr module
      Attribute  BRIG   seen=1 colo=7
      Material   Glass
      Shape      BOX    dx = modr_IGHgt/2,
                        dy = modr_IGWid/2,
                        dz = modr_IGLen/2
EndBlock

*------------------------------------------------------------------------------
*Block BRSP  is the SeParator in the TOFr module
*      Attribute  BRSP 
*      Component  O    A=16     Z=8    W=1
*      Component  N    A=14     Z=7    W=1
*      Component  C    A=12     Z=6    W=6
*      Component  H    A=1      Z=1    W=11
*      Mixture    Nylon  Dens = 1.14
*      Shape      TUBE   rmin = modr_SPRMin,
*                        rmax = modr_SPRMax,
*                        dz   = modr_SPLen/2
*EndBlock
*------------------------------------------------------------------------------
Block BRSG  is the sensitive gas layer in the TOFr module
      Attribute BRSG seen=0   colo=5
      Material  RPCgas
      Medium    sensitive IsVol=1
      Shape     BOX    dx = modr_IGHgt/2,
                        dy = modr_IGWid/2,
                        dz = modr_IGLen/2
      
      HITS    BRSG   X:.01:HS   Y:.01:   Z:.01:,
                     Ptot:18:(0,100),
                     Sleng:.1:(0,500)   ToF:16:(0,1.e-7)  Step:.01:,
                     Eloss:16:(0,1.e-6)
EndBlock
*------------------------------------------------------------------------------
Block BRWG  is the WedGe(support) in the TOFr module
      Attribute  BRWG seen=0
      Component  O    A=16     Z=8    W=2
      Component  C    A=12     Z=6    W=5
      Component  H    A=1      Z=1    W=8
      Mixture    Lucite Dens = 1.18
      Shape      TUBE   rmin = modr_WGRMin,
                        rmax = modr_WGRMax,
                        dz   = modr_WGLen/2
EndBlock


* ----------------------------------------------------------------------------
   end


