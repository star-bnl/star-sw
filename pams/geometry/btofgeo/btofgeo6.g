* $Id: btofgeo6.g,v 1.11 2011/02/28 15:33:40 jwebb Exp $
*
* btofgeo2.g is the geometry to contain TOFp+r and the CTB
* $Log: btofgeo6.g,v $
* Revision 1.11  2011/02/28 15:33:40  jwebb
* Cosmetic changes to comments needed for AgML translation.
*
* Revision 1.10  2010/12/17 20:00:21  jwebb
*
* Reverted to previous version of btofgeo6 code.  Fixes will be applied now
* in btofgeo7.
*
* Revision 1.8  2010/06/23 19:09:49  jwebb
* Resolved minor bug in the cooling tubes for the tof.  The inner radii of
* the cooling tubes are passed to the block creating the water volume, but
* the shape operator indicated that it should inherit its parameters from
* the mother volume.  This resulted in the outer radius of the water volume
* being set equal to the outer radius of the cooling tube.
*
* Revision 1.7  2009/08/18 17:26:17  perev
* F.Geurts TOF for run 9
*
* Revision 1.6  2009/01/03 23:03:33  perev
* BtofConfig=6 in 2008a,2009
*
* Revision 1.5  2008/12/05 23:45:40  perev
* BRMD is seen now
*
* Revision 1.4  2008/10/16 02:47:27  perev
* change the tray selection to be the final tray design. Xin
*
* Revision 1.3  2008/07/08 19:36:22  perev
* phi alignment XinDong
*
* Revision 1.2  2008/01/21 01:12:15  perev
* TOF weight corrected(XinDong)
*
* Revision 1.1  2007/11/07 21:26:11  perev
* btofgeo6 added by X.Dong
*
* Revision 1.2  2007/02/16 22:56:01  potekhin
* Code improvements by Xin, aimed at better readability,
* better code structure and elimination of hardcoded values.
*
* Revision 1.4  2005/08/04 23:37:37  potekhin
* Jing must have forgotten to define a non-zero
* size for the volume "BLEM", which was already fixed
* in the previous version of the code but is missing
* in this one. Since this source was never used in prod,
* there will be no effect on the data.
*
* Revision 1.3  2005/06/01 21:16:00  llope
* includes jings bugfixes and updates for run-5
*
* Revision 1.2  2005/05/23 14:56:16  llope
* removed unneeded Dens=0.282 statements from honeycomb Material definitions
*
* Revision 1.1  2005/04/11 17:46:18  potekhin
* Latest file from Xin should be put in this separate source,
* to 100% ensure prior versions aren't broken
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
Module  BTOFGEO6 is the Geometry of Barrel Trigger / Time Of Flight system 
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
*            12 Feb  2004 WJL- modifications for run-4 TOFr' geometry'
*            23 Feb  2004 WJL- fine-tuning of run-4 geometry
*                                 use choice==7 for run-4
*                                 BTOG_posit1b reflects move of TOFp in run-IV
*
*
*******************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
*   List of GEANT volumes produce here:
      Content   BTOF,BTOH,BSEC,BTRA,BUND,BTFT,BASE,BARM,BANG,
                BWAT,BCOV,BXTR,BMTC,BTTC,BMAA,BMTD,
                BASS,BXSA,BCSB,BCCV,BFEE,BLEM,
                BCPM,BCSK,BTSK,BZEL,BCEL,BCEB,BCON,BPLA,
                BCOO,BRAI,BPIP,BPIQ,
                BRTC,BRMD,BRHC,BRCB,BRMY,BRGR,BROG,
                BRDT,BRSG,BRIG,BRWG,
                BRFE,BUPC,BTFE,BGMT

*
*   Data Base interface staff:
      Structure BTOG { Version, Rmin, Rmax, dz, X0, Z0, choice,
                       posit1(2), posit2, posit3, posit4(5), dphi1(5),posit5(120)}
*
      Structure TRAY { Height, Width, Length, WallThk, SupFullH, SupFullW,
                       SupLen,
                       SupBaseT, SupBaseW, SupArmT, CoolOutR, CoolInnR,
                       StripT  , FootInse, FootThk, Foot1Len, Foot2Thk,
                       Foot3Len, TopThk,   TopH,    CoverThk, CoverH,
                       CoverL,   FEEH1,    FEEH2,   FEEW,     FEEL,
                       FEEThk}
*
      Structure CTBB { Slab1Len, Slab2Len, Slab1x,  
                       Slab2x,   SlabThck, SlabWid,  
                       ConvLen,  ConvWidM, ConvThck, 
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, 
                       ElecThck, Wrap,     Shim   }
*
      Structure TOFF { BoxWidth, SlatLen,  Slat5Z,
                       Slatz(10), Slatx(10), SlatAy(10),
                       SlatThck, SlatWid,  SlatAng,
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, SockLen,
                       CellWid,  CellHgt,
                       ElecHgt,  ElecThck, ElecWid,  ElecLen,
                       Elecz(10), PlasPos,
                       RailThck, RailWid,
                       CoolInnR, CoolOutR,
                       BconYLen, BconZLen,
                       BconPLdx, BconPLdz,
                       BLEMPosX, BLEMPosY(15), BLEMPosZ1, BLEMPosZ2,
                       BLEMLenX, BLEMLenY, BLEMLenZ,
                       BPIPPosX, BPIPPosY, BPIPPosZ,
                       BPIPRmin, BPIPRmax, BPIPLenZ } 
*
      Structure MODR { Height, Width, Length,Center, mrpcX(33), 
                       mrpcZ(33), mrpcA(33), X0Offset,
                       HCHgt,  HCWid,  HCLen,  PCBHgt, PCBWid, PCBLen,
                       MYHgt,  MYWid,  MYLen,  GRHgt,  GRWid,  GRLen,
                       OGHgt,  OGWid,  OGLen,  IGHgt,  IGWid,  IGLen,
                       SPRMin, SPRMax, SPLen,  WGRMin, WGRMax, WGLen,
                       FEEH,   HBWid,  NGap }
*
      Structure MOD4 { Height, Width, Length, Center, mrpcX(32), 
                       mrpcZ(32), mrpcA(32), X0Offset,
                       HCHgt,  HCWid,  HCLen,  PCBHgt, PCBWid, PCBLen,
                       MYHgt,  MYWid,  MYLen,  GRHgt,  GRWid,  GRLen,
                       OGHgt,  OGWid,  OGLen,  IGHgt,  IGWid,  IGLen,
                       SPRMin, SPRMax, SPLen,  WGRMin, WGRMax, WGLen,
                       FEEH,   HBWid,  NGap, TrayEdgeZ }

      Structure MOD5 { Height, Width, Length, Center, mrpcX(32), 
                       mrpcZ(32), mrpcA(32), X0Offset,
                       HCHgt,  HCWid,  HCLen,  PCBHgt, PCBWid, PCBLen,
                       MYHgt,  MYWid,  MYLen,  GRHgt,  GRWid,  GRLen,
                       OGHgt,  OGWid,  OGLen,  IGHgt,  IGWid,  IGLen,
                       SPRMin, SPRMax, SPLen,  WGRMin, WGRMax, WGLen,
                       FEEH,   HBWid,  NGap, TrayEdgeZ }
      Structure MOD7 { Height, Width, Length, Center, mrpcX(32), 
                       mrpcZ(32), mrpcA(32), X0Offset,
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
      integer   i,is, choice, tof, iwid, igap, islat
*
* -------------------------------------------------------------------------
*
      Fill BTOG ! Barrel Trigger, CTB/TOF Basic dimensions 
         Version   = 6         ! geometry version
         Rmin      = 207.80    ! minimum CTB/TOF system radius (as built)
         Rmax      = 221.00    ! maximum CTB/TOF system radius
         dz        = 246.0     ! CTB/TOF tube half length
         X0        = 0.0       ! radial global offset 
         Z0        = 0.0       ! z distance of tray edge to TPC centralplane (previous TrayEdgeZ moved here)
         choice    = 11        ! 1=CTB, 2=Full-TOFp, 3=25% TOFp, 4=1 tray-TOFp, 
*                              ! 5=1 tray-TOFr, 6=Full-TOFr, 7=TOFp+TOFrp Run-IV
*                              ! 8= Run-V 1 tray-TOFr, 9= Run-VI, 10= Run-VII
         posit1    = {32,33}   ! TOFp tray position (0) choice 4 or 5 -> run-2,3 posn
*                              !                    (1) choice 7 -> run-4 posn
         posit2    = 23        ! TOFr tray position for choice 5 -> run-4 posn
	 posit3    = 33        ! TOFr tray position for choice 8,9,10 -> run-5,6,7 posn
         posit4    = {16,17,18,19,20}  			! TOFr8 tray positions for choice 11,12 -> run 8 east and west trays
         dphi1     = {0.24, 0.17, 0.15, 0.08, 0.02} 	! TOFr8 tray phi alignment parameter

         posit5    = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                      1, 1, 0, 0, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                      1, 0, 0, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                      1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                      1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1}   ! TOF run-9 tray map (west=1-60, east=61-120)
*
      Fill TRAY ! general tray stats        
         Height    = 11.43      ! tray height(8.128+3.302)
         Width     = 21.59      ! full tray width
         Length    = 240.60     ! full tray length(240.60)
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
         TopThk    =  0.23      ! thickness of the tray top (not the WallThk)
         TopH      =  1.75      ! height of top edges 
         CoverThk  =  0.13      ! cover thickness
         CoverH    =  3.302     ! cover height (from tray top to cover top)
         CoverL    = 234.96     ! cover length (shorter than tray)
         FEEH1     =   0.0      ! distance of TINO/TCPU to tray top
         FEEH2     =   1.35     ! distance of TDIG/TTRG to tray top
         FEEW      =  20.83     ! TINO/TDIG width
         FEEL      = 231.72     ! TINO/TDIG length
         FEEThk    =   0.24     ! TINO/TDIG thickness
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
         Slat5Z    = 15.5     ! 5_wide mother box z length/2.
         Slatz     = { 104.938, 84.060, 62.860, 41.254, 18.966,
                       -3.954, -27.528, -51.254, -75.634, -100.683 } ! (5)4_wide_slat Z position for 10 rows
         Slatx     = { -1.7, -0.4, -0.2, -0.2, -0.2,
                       -0.2, -0.2, -0.2, -0.2, -0.2 } ! Slat assembly X pos
         SlatAy    = { 5.0, 10.0, 11.0, 11.0, 11.0,
                       11.0, 11.0, 11.0, 11.0, 11.0 } ! Slat assembly alphay
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
         Elecz     = { 105.610, 84.573, 63.224, 41.667, 19.379,
                       -3.542, -27.165, -50.841, -75.170, -100.270} ! FEE Z position for 10 rows
         PlasPos   = 3.0      ! Plastic angles (BPLA) Z center offset
         RailThck  = 0.2      ! Cooling loop rail thickness
         RailWid   = 1.5      ! Cooling loop rail width
         CoolOutR  = 0.635    ! Cooling loop pipe outer radius, 0.5in/2
         CoolInnR  = 0.561    ! Cooling loop pipe inner radius, (0.5in-0.058in)/2
         BconYLen  = 2.50     ! BCON Y length for TOFp Slat assembly
         BconZLen  = 1.20     ! BCON Z length for TOFp Slat assembly
         BconPLdx  = 0.203    ! BCON dx for BPLA = 0.08*2.54 
         BconPLdz  = 0.635    ! BCON dz for BPLA = 0.25*2.54
         BLEMPosX  = 0.52     ! BLEM position X in BFEE = ElecThck + 0.7/2.0
         BLEMPosY  = { -7.0, -3.5, 0.0, 3.5, 7.0,
                       -7.0, -3.5, 0.0, 3.5, 7.0,
                       -6.0, -2.5, 1.0, 4.5, 8.0 }  ! BLEM position Y in BFEE
         BLEMPosZ1 = 2.0      ! BLEM position Z1 in BFEE
         BLEMPosZ2 = -2.0     ! BLEM position Z2 in BFEE
         BLEMLenX  = 0.86     ! BLEM X length
         BLEMLenY  = 0.68     ! BLEM Y length
         BLEMLenZ  = 3.80     ! BLEM Z length
         BPIPPosX  = 0.09     ! BPIP position X in BLEM
         BPIPPosY  = 0.00     ! BPIP position Y in BLEM
         BPIPPosZ  = 0.90     ! BPIP posision Z in BLEM
         BPIPRmin  = 0.31     ! BPIP Rmin
         BPIPRmax  = 0.34     ! BPIP Rmax
         BPIPLenZ  = 2.00     ! BPIP Z length
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
         X0Offset = -3.66 ! Tray inner surface to the center  
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
         X0Offset = -3.66 ! Tray inner surface to the center  
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

      Fill MOD5   ! RUN5 MRPC TOF Module dimensions and positions 
         Height    = 1.95     ! Module height (r)
         Width     = 21.2     ! Module width (phi)
         Length    = 9.4      ! Module length (z)
         Center    = 0.35     ! Module detector center (phi)
         mrpcZ = {7.08, 13.23, 19.23, 25.48, 33.27, 39.48, 45.67, 51.51,
                  59.66, 65.98, 71.73, 78.09, 86.36, 92.02, 98.7, 104.28,
                  112.45,118.79,125.14,131.51,139.6,145.96,152.32,158.68,
			      167.41,173.78,180.18,186.6,195.53,202.01,208.52,215.06 } ! mrpc Zposns
         mrpcX = { 1.61, 4.44, 1.61, 5.49, 2.0, 3.54, 5.02, 2.01,
                   3.36, 5.02, 2.0, 3.56, 5.02, 2.0, 5.02, 2.0,
                   2.63, 3.09, 3.6, 4.11, 3.83, 4.06, 4.21, 4.34,
				   3.85, 3.85, 3.85, 3.85, 3.72, 3.72, 3.72, 3.72} ! mrpc Xposns
         mrpcA = { 0.,  0.,  0.,  0.,  6., 6., 6., 6.,
                   6., 6., 6., 6., 6., 6., 6., 6., 
				   16., 16., 16., 16., 19., 19., 19., 19., 
				   22., 22., 22., 22., 24., 24., 24., 24. } ! mrpc angles
         X0Offset = -3.90 ! Tray inner surface to the center - new value from Jing
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
*         TrayEdgeZ = 6.0*2.54+1.0   ! tray posn along rail wrt TPC centerplane (Z)
*         TrayEdgeZ = 18.737   ! tray posn along rail wrt TPC centerplane (Z)
         TrayEdgeZ = 13.25  !tray posn along rail wrt TPC centerplane (Z).Liuj puts this new value 
      EndFill

      Fill MOD7   ! RUN7++ MRPC TOF Module dimensions and positions 
         Height    = 1.95     ! Module height (r)
         Width     = 21.2     ! Module width (phi)
         Length    = 9.4      ! Module length (z)
         Center    = 0.35     ! Module detector center (phi)
         mrpcZ = {5.43, 11.42, 17.71, 23.56, 29.96, 35.66, 42.71, 49.04,
                  55.35, 61.65, 67.99, 74.42, 80.91, 87.44, 93.93, 100.53,
                  107.30,114.17,121.22,128.29,135.52,142.80,150.19,157.72,
			      165.34,173.07,180.92,188.93,197.02,205.24,213.61,222.08 } ! mrpc Zposns
         mrpcX = { 3.94, 1.48, 3.94, 1.48, 3.94, 1.48, 3.01, 3.54,
                   3.79, 3.86, 3.84, 3.89, 3.89, 3.84, 3.51, 3.29,
                   3.29, 3.29, 3.50, 3.50, 3.60, 3.60, 3.60, 3.60,
				   3.60, 3.60, 3.60, 3.60, 3.60, 3.60, 3.60, 3.60} ! mrpc Xposns
         mrpcA = { 0.,  0.,  0.,  0.,  0., 0., 16., 16.,
                   20., 22., 22., 22., 22., 22., 26., 26.,
				   26., 26., 30., 30., 32., 32., 32., 32., 
				   32., 32., 32., 32., 32., 32., 32., 32.} ! mrpc angles
         X0Offset = -3.87 ! Tray inner surface to the center -tray_Height/2+tray_WallThk
         HCHgt  =  0.4    ! HC->Height (r)
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
         OGHgt  =  0.07   ! Outer Glass->Height
         OGWid  = 20.7    !  Outer Glass->Width
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
         TrayEdgeZ = 0.50  !tray posn along rail wrt TPC centerplane
      EndFill

*         NPad   = 6       ! Number of pads within a MRPC
*
      USE   BTOG
      USE   TRAY
      USE   CTBB
      USE   TOFF
      USE   MODR
      USE   MOD4
      USE   MOD5
      USE   MOD7
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
      Mixture    HoneyComb Dens=0.73
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
      Shape     Tube      rmin=btog_Rmin+btog_X0  Rmax=btog_Rmax+btog_X0  dz=btog_dz+btog_Z0

      print *,'BTOF choice = ',btog_choice
	  if (btog_choice ==  7) print *,' TOF: btog_choice=7: This is the Run-IV geometry...'
	  if (btog_choice ==  8) print *,' TOF: btog_choice=8: This is the Run-V geometry...'
	  if (btog_choice ==  9) print *,' TOF: btog_choice=9: This is the Run-VI geometry...'
	  if (btog_choice == 10) print *,' TOF: btog_choice=10: This is the Run-VII geometry...'
	  if (btog_choice == 11) print *,' TOF: btog_choice=11: This is the Run-VIII geometry...'
	  if (btog_choice == 12) print *,' TOF: btog_choice=12: This is the Run-IX geometry...'

      choice = 1                           ! ctb
      if (btog_choice == 2)  choice= 2     ! full tofp
      if (btog_choice == 6)  choice= 6     ! full tofr
      if (btog_choice == 12) choice=12     ! run-9 selection (west)
      print *,' Positioning West Barrel, choice=',choice
      Create and Position BTOH  z=+btog_dz/2+btog_Z0   alphay=180   ! West barrel
      choice = btog_choice                   
      if (btog_choice == 12) choice=-12    ! run-9 selection (east)
      print *,' Positioning East Barrel, choice=',choice
      Create and Position BTOH  z=-btog_dz/2-btog_Z0                 ! East barrel
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

      ! tof=0 means ctb, tof=1 means TOFp, tof=2 means TOFr, tof=3 means TOFr', tof=4 means TOFr5 '
      ! tof=5 means TOFr6 tray (not active), tof=6 means TOFr7 tray, tof=7 means TOFr8++ tray
      ! tof=-1 means no tray (Run 9)
      do is=1,60
         tof=0		                                !// all CTB for choice=1                     
         if (choice==2)                       tof=1	!// all TOFp
         if (choice==3 & 46<=is&is<=60)       tof=1	!// big TOFp patch, rest CTB
         if (choice==4 & is==btog_posit1(1))  tof=1	!// Run-2 (one TOFp tray)
         if (choice==5 & is==btog_posit1(1))  tof=1	!// Run-3 (one TOFp tray
         if (choice==5 & is==btog_posit2)     tof=2	!      and one TOFr tray)
         if (choice==6)                       tof=7	!// all TOFr
         if (choice==7 & is==btog_posit1(2))  tof=1	!// Run-4 (one TOFp tray moved 1 slot
         if (choice==7 & is==btog_posit2)     tof=3	!      and one TOFrp tray)
	 if (choice==8  & is==btog_posit3)    tof=4  	!// Run-5 (one TOFr5 tray)
	 if (choice==9  & is==btog_posit3)    tof=5  	!// Run-6 (one TOFr6 tray)
	 if (choice==10 & is==btog_posit3)    tof=6  	!// Run-7 (one TOFr7 tray)
         if (choice==11 & is==btog_posit4(1)) tof=7 	!// Run-8 (5 TOFr8 trays)
         if (choice==11 & is==btog_posit4(2)) tof=7 	!// Run-8 (5 TOFr8 trays)
         if (choice==11 & is==btog_posit4(3)) tof=7	!// Run-8 (5 TOFr8 trays)
         if (choice==11 & is==btog_posit4(4)) tof=7 	!// Run-8 (5 TOFr8 trays)
         if (choice==11 & is==btog_posit4(5)) tof=7 	!// Run-8 (5 TOFr8 trays)

         if (choice==12  & btog_posit5(is)   ==1) tof= 7 !// Run-9:TOFr8 tray (west)
         if (choice==12  & btog_posit5(is)   ==0) tof=-1 !// Run-9: no tray (west)
         if (choice==-12 & btog_posit5(is+60)==1) tof= 7 !// Run-9: TOFr8 tray (east)
         if (choice==-12 & btog_posit5(is+60)==0) tof=-1 !// Run-9: no tray (east)

*         print *,' Positioning Tray, choice,is,tof=',choice,is,tof
*         Create and Position BSEC  alphaz = 102+6*is
* Since Run 8, start to implement phi alignment parameters for trays
         if (choice==11) then
            if (is==btog_posit4(1)) then
               Create and Position BSEC alphaz = 102+6*is+btog_dphi1(1)
            else if (is==btog_posit4(2)) then
               Create and Position BSEC alphaz = 102+6*is+btog_dphi1(2)
            else if (is==btog_posit4(3)) then
               Create and Position BSEC alphaz = 102+6*is+btog_dphi1(3)
            else if (is==btog_posit4(4)) then
               Create and Position BSEC alphaz = 102+6*is+btog_dphi1(4)
            else if (is==btog_posit4(5)) then
               Create and Position BSEC alphaz = 102+6*is+btog_dphi1(5)
            else
               Create and Position BSEC alphaz = 102+6*is
            endif
         else
            Create and Position BSEC alphaz = 102+6*is
         endif
      enddo
EndBlock
*
*------------------------------------------------------------------------------
Block BSEC is a sector of CTB/TOF Trigger Barrel Scintillators
      Attribute BSEC      seen=0   colo=1  serial=tof  
      Shape     Tubs      phi1 = -3.0 phi2 = 3.0
      Create and Position BTRA   X = _ 
             btog_Rmin+(tray_SupFullH+tray_height+tray_StripT)/2+btog_X0
*      print *,' position BTRA ...'
      if(tof==2) then
         Create and Position BRFE   X = _
             btog_Rmin+tray_SupFullH+tray_StripT+tray_height+modr_feeh/2+btog_X0,
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
* --- skip creating a tray if tof==-1
      if(tof!=-1) then
       Create and Position BXTR     X=(tray_SupFullH+tray_StripT)/2,
                                   z=(btog_dz-tray_length)/2
*      print *,' position BXTR ...'
      endif
* --- always create the support structure
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
         Create and Position BRTC  X=-(tray_CoverH+tray_TopThk-tray_WallThk)/2
*         print *, ' position BRTC ...'
         Create and Position BUPC  X=(tray_Height-tray_CoverH)/2-tray_CoverThk/2
*         print *, ' position BUPC ...'
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
      sublen             = ((toff_Slatz(2)+toff_Slat5Z)-(toff_Slatz(10)-toff_Slat5Z))
      subcen             = (toff_Slatz(2)+toff_Slat5Z)-sublen/2.

      iwid=4
      Create and Position BMAA  X=0   Z=subcen konly='MANY'
*---- the 5wide mother box...
      iwid=5
      Create and Position BMAA  X=0.0 Z=toff_Slatz(1) konly='MANY'

*---- interior cooling rails and tubing....
      Create and Position  BCOO X=0 Y=0 dx=0 dy=0 dz=0
*---- front end electronics boards
      Create    BFEE       dx=toff_ElecThck/2, 
                           dy=toff_ElecWid/2, 
                           dz=toff_ElecLen/2
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(1)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(2)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(3)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(4)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(5)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(6)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(7)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(8)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(9)-toff_ElecLen/2 
      Position  BFEE  X=toff_ElecHgt Z=toff_Elecz(10)-toff_ElecLen/2 
*---- plastic angles...
      Create    BPLA
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(1)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(2)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(3)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(4)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(5)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(6)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(7)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(8)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(9)+toff_PlasPos
      Position  BPLA  X=toff_ElecHgt Y=0 Z=toff_Elecz(10)+toff_PlasPos
EndBlock
*------------------------------------------------------------------------------
*
Block BUPC is the upper cover of tofr
      Attribute BUPC seen=0   colo=3
      Material  Air
      Shape     BOX       dx=(tray_CoverH-tray_CoverThk)/2,
                          dy=tray_Width/2-tray_CoverThk,
                          dz=tray_Length/2
      
      Create BTFE

      Position BTFE  X=-(tray_CoverH-tray_CoverThk)/2+tray_FEEH1+tray_FEEThk/2
      Position BTFE  X=-(tray_CoverH-tray_CoverThk)/2+tray_FEEH2+tray_FEEThk/2
EndBlock
*------------------------------------------------------------------------------
*
Block BTFE is the TINO/TDIG boards
      Attribute BTFE seen=0   colo=3
      Material   G10
      Shape      BOX      dx=tray_FEEThk/2,
                          dy=tray_FEEW/2,
                          dz=tray_FEEL/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block BRTC is the Main Tray Cavity filled with the details for TOFr (run3 or run4)
      Attribute  BRTC     seen=0   colo=5
      Material   HoneyComb 
      Shape      BOX      dx=(tray_Height-tray_CoverH-tray_WallThk-tray_TopThk)/2,
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
*      Create and Position BGMT konly='MANY'
      Create and Position BGMT
*      print *,'position BGMT ...'
EndBlock
*
*------------------------------------------------------------------------------
*
Block BGMT is the mixture gas box in tray that change the hc box into slim
      Attribute BGMT  seen=0   colo=2
      Material  RPCgas
      Shape     BOX  dx=(tray_Height-tray_CoverH-tray_TopThk-tray_WallThk)/2,
                     dy=tray_Width/2-tray_WallThk,
                     dz=tray_Length/2-tray_WallThk
*---- create and position TOFr modules
* 
* Change the new geometry to honeycomb support of long slim box by
* add mixture gas box in the tray box iterior


      Create BRMD

	if (tof==2) then
      z0 = tray_Length/2 - 0.05
*      x0 = -(btog_Rmin+tray_SupFullH+tray_StripT+tray_Height/2) - 1.5
      x0 = modr_X0Offset
      do i=1,33
            Position BRMD  X=x0+modr_mrpcX(i) ,
                           Z=z0-modr_mrpcZ(i) ,
                           alphay=modr_mrpcA(i)
      enddo
	elseif (tof==3) then
      z0 = tray_Length/2 - 0.05 - mod4_TrayEdgeZ
      x0 = mod4_X0Offset
      do i=1,32
         if (i.le.20.or.(i.ge.25.and.i.le.28)) then
            Position BRMD  X=x0+mod4_mrpcX(i) ,
                           Z=z0-mod4_mrpcZ(i) ,
                           alphay=mod4_mrpcA(i)
         endif
      enddo
    elseif (tof==4) then
      z0 = tray_Length/2 - 0.05 - mod5_TrayEdgeZ
      x0 = mod5_X0Offset
      do i=1,32
         Position BRMD  X=x0+mod5_mrpcX(i) ,
                        Z=z0-mod5_mrpcZ(i) ,
                        alphay=mod5_mrpcA(i)
      enddo
    elseif (tof==5) then
*      z0 = tray_Length/2 - 0.05 - mod6_TrayEdgeZ
*      x0 = -3.90
*      do i=1,32
*         Position BRMD  X=x0+mod6_mrpcX(i) ,
*                        Z=z0-mod6_mrpcZ(i) ,
*                        alphay=mod6_mrpcA(i)
*      enddo
* Run-7
    elseif (tof==6) then
      z0 = tray_Length/2 - mod7_TrayEdgeZ
      x0 = -tray_Height/2 + tray_WallThk
      do i=1,32
         Position BRMD  X=x0+mod7_mrpcX(i) ,
                        Z=z0-mod7_mrpcZ(i) ,
                        alphay=mod7_mrpcA(i)
      enddo

* Run-8, module positions are the same as Run-7
    elseif (tof==7) then
      z0 = tray_Length/2 - tray_WallThk
      x0 = -(tray_Height-tray_CoverH-tray_WallThk-tray_TopThk)/2
      do i=1,32
         Position BRMD  X=x0+mod7_mrpcX(i) ,
                        Z=z0-mod7_mrpcZ(i) ,
                        alphay=mod7_mrpcA(i)
      enddo


	endif
*
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
                       dz=toff_Slat5Z
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
        Position BASS X=toff_Slatx(1) alphay=toff_SlatAy(1) konly='MANY'
      else
        do islat=2,10
            Position BASS X=toff_Slatx(islat) Z=toff_Slatz(islat)-subcen,
                          alphay=toff_SlatAy(islat) konly='MANY'
        enddo
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
                    dy=toff_BconYLen/2,
                    dz=toff_BconZLen/2
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
      Create and Position BCON  x=0 y=0 z=-toff_BconPLdz,
                                dx=toff_BconPLdx/2,
                                dy=tray_Width/2-tray_WallThk-0.5,
                                dz=toff_BconPLdz
      Position  BCON            x=(-toff_BconPLdx - toff_BconPLdz)/2,
                                y=0,
                                z=(-toff_BconPLdx)/2,
                                dx=toff_BconPLdz/2,
                                dy=tray_Width/2-tray_WallThk-2.0,
                                dz=toff_BconPLdx/2
EndBlock
Block BCON is a generic plastic block for various connectors, foam-support-angles, etc......
      Attribute BCON      seen=0   colo=6
      Material  polystyren 
      Shape     BOX       dx=0 dy=0 dz=0
EndBlock
*
Block BFEE is a G10 FrontEndElectronics board for TOF
      Attribute BFEE seen=0   colo=3
      Material  G10
      Shape     BOX  dx=0  dy=0  dz=0
      Create    BLEM
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(1)  z=toff_BLEMPosZ1 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(2)  z=toff_BLEMPosZ1 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(3)  z=toff_BLEMPosZ1 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(4)  z=toff_BLEMPosZ1 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(5)  z=toff_BLEMPosZ1 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(6)  z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(7)  z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(8)  z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(9)  z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(10) z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(11) z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(12) z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(13) z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(14) z=toff_BLEMPosZ2 alphax=180 
      Position  BLEM x=toff_BLEMPosX y=toff_BLEMPosY(15) z=toff_BLEMPosZ2 alphax=180 
EndBlock
Block BLEM is a Lemo connector on the FEE boards
      Attribute BLEM seen=0   colo=3
      Shape     BOX   dx=toff_BLEMLenX/2,
                      dy=toff_BLEMLenY/2,
                      dz=toff_BLEMLenZ/2
**      Create    BRAI  dx=0.9/2    dy=0.7/2    dz=0.7/2
**      Create    BPIP  Rmin=0.62/2 Rmax=0.68/2 dz=1.0/2
**      Position  BRAI  x=0            y=0 z=0
**      Position  BPIP  x=(0.9-0.72)/2 y=0 z=(0.7+1.0)/2
      Create and Position    BPIP  x=toff_BPIPPosX y=0 z=toff_BPIPPosZ,
                                   Rmin=toff_BPIPRmin Rmax=toff_BPIPRmax dz=toff_BPIPLenZ/2
EndBlock
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
*      1. PCB, short for Printed Circuit Board
*      2. Separator(BRSP) and Wedge(BRWG) are not precisely positioned
*
*------------------------------------------------------------------------------
Block BRMD  is a six channel module for TOFr
      Attribute  BRMD     seen=1   colo=6
      Material   RPCgas
      Shape      BOX      dx=mod7_Height/2,
                          dy=mod7_Width/2,
                          dz=mod7_Length/2
      Create     BRHC
      Create     BRCB
      Create     BRMY
      Create     BRGR
      Create     BROG
      DTHgt = (mod7_IGHgt+mod7_SPRMax*2)*(mod7_NGap-1)+mod7_SPRMax*2
      Create     BRDT  dx=DTHgt/2., dy=mod7_IGWid/2., dz=mod7_IGLen/2.,
                       konly='MANY'
      Create     BRWG

      XPOS = mod7_Height/2.
      Y = 0.
      Z = mod7_Center

*
*- Single-direction positioning from bottom only
*
      Position   BRHC   X=XPOS-mod7_HCHgt/2 Z=mod7_Center
      XPOS = XPOS - mod7_HCHgt

      Position   BRCB   X=XPOS-mod7_PCBHgt/2 Z=0.
      XPOS = XPOS - mod7_PCBHgt

      Position   BRMY   X=XPOS-mod7_MYHgt/2  Z=mod7_Center
      XPOS = XPOS - mod7_MYHgt

      Position   BRGR   X=XPOS-mod7_GRHgt/2
      XPOS = XPOS - mod7_GRHgt

      Position   BROG   X=XPOS-mod7_OGHgt/2
      XPOS = XPOS - mod7_OGHgt

      Position   BRDT   X=XPOS-DTHgt/2
      XPOS = XPOS - DTHgt

      Position   BROG   X=XPOS-mod7_OGHgt/2
      XPOS = XPOS - mod7_OGHgt
                 
      Position   BRGR   X=XPOS-mod7_GRHgt/2
      XPOS = XPOS - mod7_GRHgt

      Position   BRMY   X=XPOS-mod7_MYHgt/2
      XPOS = XPOS - mod7_MYHgt
                 
      Position   BRCB   X=XPOS-mod7_PCBHgt/2, Z=0.
      XPOS = XPOS - mod7_PCBHgt

      Position   BRHC   X=XPOS-mod7_HCHgt/2, Z=mod7_Center
      XPOS = XPOS - mod7_HCHgt

EndBlock
 
*------------------------------------------------------------------------------
Block BRHC  is the HoneyComb in the TOFr module
      Attribute  BRHC      seen=1 colo=1
      Material   HoneyComb
      Shape      BOX       dx = mod7_HCHgt/2,
                           dy = mod7_HCWid/2,
                           dz = mod7_HCLen/2
EndBlock

*------------------------------------------------------------------------------
Block BRCB  is the PCB in the TOFr module
      Attribute  BRCB   seen=1 colo=3
      Material   G10
      Shape      BOX    dx = mod7_PCBHgt/2,
                        dy = mod7_PCBWid/2,
                        dz = mod7_PCBLen/2
EndBlock

*------------------------------------------------------------------------------
Block BRMY  is the MYlar in the TOFr module
      Attribute  BRMY   seen=0 colo=1
      Material   MYLAR
      Shape      BOX    dx = mod7_MYHgt/2,
                        dy = mod7_MYWid/2,
                        dz = mod7_MYLen/2
EndBlock

*------------------------------------------------------------------------------
Block BRGR  is the GRaphite in the TOFr module
      Attribute  BRGR   seen=0 colo=1
      Material   Carbon
      Shape      BOX    dx = mod7_GRHgt/2,
                        dy = mod7_GRWid/2,
                        dz = mod7_GrLen/2
EndBlock

*------------------------------------------------------------------------------
Block BROG  is the Outer Glass in the TOFr module
      Attribute  BROG   seen=1 colo=7
      Component  Si     A=28  Z=14  W=1.
      Component  O      A=16  Z=8   W=2.
      Mixture    Glass  Dens=2.5
      Shape      BOX    dx = mod7_OGHgt/2,
                        dy = mod7_OGWid/2,
                        dz = mod7_OGLen/2
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
      Do igap=1,mod7_NGap-1
         Position   BRSG   X= XPOS - mod7_SPRMax
         XPOS = XPOS - mod7_SPRMax*2.

         Position   BRIG   X= XPOS - mod7_IGHgt/2
         XPOS = XPOS - mod7_IGHgt
      Enddo

      Position   BRSG   X= XPOS - mod7_SPRMax
      XPOS = XPOS - mod7_SPRMax*2.

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
**      Shape      Division  IAxis=3  NDiv=mod7_NPad
*      Shape      Box    dy = mod7_PadWid,
*                        dz = mod7_PadLen, konly='MANY'
*EndBlock


*------------------------------------------------------------------------------
Block BRIG  is the Inner Glass in the TOFr module
      Attribute  BRIG   seen=1 colo=7
      Material   Glass
      Shape      BOX    dx = mod7_IGHgt/2,
                        dy = mod7_IGWid/2,
                        dz = mod7_IGLen/2
EndBlock

*------------------------------------------------------------------------------
*Block BRSP  is the SeParator in the TOFr module
*      Attribute  BRSP 
*      Component  O    A=16     Z=8    W=1
*      Component  N    A=14     Z=7    W=1
*      Component  C    A=12     Z=6    W=6
*      Component  H    A=1      Z=1    W=11
*      Mixture    Nylon  Dens = 1.14
*      Shape      TUBE   rmin = mod7_SPRMin,
*                        rmax = mod7_SPRMax,
*                        dz   = mod7_SPLen/2
*EndBlock
*------------------------------------------------------------------------------
Block BRSG  is the sensitive gas layer in the TOFr module
      Attribute BRSG seen=0   colo=5
      Material  RPCgas
      Medium    sensitive IsVol=1
      Shape     BOX    dx = mod7_IGHgt/2,
                        dy = mod7_IGWid/2,
                        dz = mod7_IGLen/2
      
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
      Shape      TUBE   rmin = mod7_WGRMin,
                        rmax = mod7_WGRMax,
                        dz   = mod7_WGLen/2
EndBlock


* ----------------------------------------------------------------------------
   end


