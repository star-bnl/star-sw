* $Id: vpddgeo2.g,v 1.3 2008/11/19 04:08:38 perev Exp $
* $Log: vpddgeo2.g,v $
* Revision 1.3  2008/11/19 04:08:38  perev
* updates to the corrected(vp) starsim
*
* Revision 1.2  2007/02/16 22:54:32  potekhin
* Code improvements by Xin, aimed at a better code structure,
* readability and removal of hardcoded values.
*
* Revision 1.2  2004/07/08 01:52:50  potekhin
* Need to properly name the module here (different from
* the previosu version)
*
* Revision 1.1  2004/07/08 01:49:27  potekhin
* Need a new source to properly version recent
* changes by Frank
*
* Revision 1.14  2004/07/06 20:47:53  geurts
* *** empty log message ***
*
* Revision 1.13  2004/03/24 23:32:47  potekhin
* Moved the numerical data introduced by Bill Llope in
* geometry.g, into that source file, similar to the pipegeo
* and other code, to make it self-contained.
*
* Revision 1.12  2004/03/24 19:42:39  llope
* extremely trivial change: stupid typo in a print statement for debugging. fixed.
*
* Revision 1.11  2004/03/24 18:24:07  llope
* Zposns increased by 4" w.r.t. the run-3 posns, radiator changed to quartz to plastic scintillator
*
* Revision 1.10  2004/03/12 20:51:27  llope
* only added a print statement to confirm Z-positioning during run-time
*
* Revision 1.9  2002/11/26 17:45:45  geurts
* Updated the pVPD mothervolume positions for the 2002/2003 run.
* Removed obsolete vpdg_zpos.
*
* Revision 1.8  2002/11/26 17:32:00  geurts
* The pVPD mothervolumes have been moved to the best estimate for the
* 2000/2001 run. The two mothervolumes are now asymetrically
* placed. Positioning of the I-Beams had to be changed accordingly so
* the absolute position of the two I-Beams remained the same.
*
* Revision 1.7  2001/09/13 15:59:47  geurts
* pVPD mothervolumes moved by 36inch
*
* Revision 1.6  2001/03/14 01:42:35  nevski
* add few konly=MANY to make I-beam real
*
* Revision 1.5  2001/03/14 01:29:55  nevski
* more detailed VPD description
*
* Revision 1.4  2000/08/21 23:57:25  geurts
* updated dimensions of the I-beam
*
* Revision 1.3  2000/08/08 00:11:25  nevski
* more realistic VPD project
*
*
*************************************************************************
module  VPDDGEO2  is the StartDet and pipe support hardware
Author  W.J. Llope 
Created 21 June 2000
*
* Notes:
*   - Use IBchoice=0  to switch off the definition of the IBeam.
*
* modified:
*   13 Sept 2001  FG - moved both pVPD mothervolumes 36" (91.44cm) outward.
*
************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
     CONTENT   VPDD,VPBP, VPBO, VPBA, VPBB, VPBX, 
               VFEE, VLEM, VPIP,
               VPFP, VPFA, VPFB, VPFC, VPBI, VPST,
               VPSC, VPSA, VPSB, VPSV, VPSW,
               VRNG, VSEC, VSED, VDET, VXST, VCNV, 
               VPMT, VRAD, VPCL, VPCF,
               VPCV, VPCH, VDTI,
               IBEM,IBEH,IBEV,IBEW,
               IBSA,IBSB,IBSC,IBSD,IBSE,IBSF,IBSG,IBSH,
               IBAA,IBAB,IBAC,          IBSS,
                         IBBC,
                         IBCC
*

     Structure VPDV { version,  vpdConfig }
     Structure VPDG { version,  zposEast, zposWest, rmin,    rmax,
                      BPwidth,  BPlength, BPthick,
                      BXheight, BXlength, BXwidth,  BXthick, BXzposC,
                      FPwidth,  FPheight, FPthick,
		              FPAwidth,	FPAhght, FPChght,
                      FPHhght, FPHwidth, FPHthick,
                      STthick,  STheight, STangle, STdiagsz,
                      SCwidth, SCheight, SClength, SCthick,
		              CLheight, CLwidth, CLlength, CLthick,
                      DETlen,   DETrad,   DETfront,
                      ConvThk,  RadiThk,  EleLeng,  DrLayer,
                      NumPMT,   PMTwall,  PMTrad,   PMTlen,
                      IBchoice, IBPosYc,  IBPosZc,  IBLeng,  IBthickH,
                      IBthickV, IBheight, IBwidth, 
                      IBwlen,   IBwhghtF, IBwhghtB,
                      EWShift,  UDShift,  BoltShift }
     Structure VPDH { version,   zposEast, zposWest, rmin,   rmax, length,
                      detrad,    detlen,   pmtrad,   pmtlen,
                      detwall,   detfront, leadthick, scintthick,
                      ring1_ndet, ring1_rad, ring1_phi0, ring1_dphi, ring1_kproj,
                      ring2_ndet, ring2_rad, ring2_phi0, ring2_dphi, ring2_kproj,
                      ring3_ndet, ring3_rad, ring3_phi0, ring3_dphi, ring3_kproj,
                      EWShift,  UDShift,  BoltShift }
     Structure VPDS { version, IBSAZc,  IBSAYc,  IBSAXc,
                      IBSBZc,  IBSBYc,  IBSBXc,
                      IBSCZc,  IBSCYc,  IBSCXc,
                      IBSDZc1, IBSDZc2, IBSDYc1, IBSDYc2,  IBSDXc,
                      IBSEZc1, IBSEZc2, IBSEYc,  IBSEXc,
                      IBSFZc,  IBSFYc,  IBSFXc,
                      IBSGZc1, IBSGZc2, IBSGZc3, IBSGYc,  IBSGXc,
                      IBSHZc1, IBSHZc2, IBSHYc,  IBSHXc1, IBSHXc2, 
                      BSALenX, BSALenY, BSALenZ,
                      BAALenZ, BSBLenY, BSCLenX, BSCLenY,
                      BSCLenZ, BACLenZ, BSDLenX,
                      BSELenY, BSELenZ,
                      BSFRmax, BSFLenZ, BSSLenZ,
                      BSGRmax, BSGLenZ1, BSGLenZ2, BSGLenZ3,
                      BSHLenZ,
                      ElecWid, ElecThck, ElecLen,
                      VFEEPosX, VFEEPosY, VFEEPosZ,
                      VLEMPosX(15),  VLEMPosY,  VLEMPosZ(15),
                      VLEMLenX,  VLEMLenY,  VLEMLenZ,
                      VPIPPosX,  VPIPPosY,  VPIPPosZ,
                      VPIPRmin,  VPIPRmax,  VPIPLenZ }
*
     real ybase, ytop, convlength, detangle, strutheight, ydispl
     real xloc, yloc,zloc,zpos,tempos
     real zpose, zposw, EWshift, UDshift, BoltShift, locang, phiang, detzint
     integer isec
     integer kDetStyle, kIBeamStyle
*
* ----------------------------------------------------------------------
*
     Fill VPDV  ! VPD configuration control
        version   =    2      ! default version (set in geometry.g)
        vpdConfig =    1      ! default config  (set in geometry.g)
     EndFill

     FILL VPDG  ! pVPD basic dimensions
        version   =    1      ! geometry version
        zposEast  =  550.0000 ! Z position East
        zposWest  =  550.0000 ! Z position West
        rmin      =    6.35   ! mothervolume rmin		(2.5inch)
        rmax      =   31.27375! mothervolume rmin		(12.3125inch)
        BPwidth   =   30.48   ! baseplate width 		(12inch)
        BPlength  =   46.355  ! baseplate length		(18.5inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    5.08   ! FEE box height			(2inch)
        BXlength  =   15.24   ! FEE box length			(6inch)
        BXwidth   =   25.4    ! FEE box width			(10inch)
        BXthick   =    0.635  ! FEE box plate thickness		(0.25inch) 
        BXzposC   =    0.635  ! FEE central z pos (+/-??)	(0.25inch)
        FPwidth   =    7.62   ! frontplate width		(3inch)
        FPheight  =   25.45842! frontplate height		(10.023inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPAwidth  =    6.63956! frontpl.part A width            (2.614inch)
        FPAhght   =    8.35405! frontpl.part A height           (3.289inch)
        FPChght   =   12.573  ! frontpl.part C height           (4.95inch)
        FPHhght   =    3.81   ! frontpl. hook height            (1.5inch)
        FPHwidth  =    5.08   ! frontpl. hook width             (2inch)
        FPHthick  =    0.635  ! frontpl. hook thickness         (0.25inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   23.1775 ! strut height			(9.125 inch)
        STangle   =   25.     ! strut angle (degr)
        STdiagsz  =    5.08   ! diagnoal strut size             (2inch)
        SCwidth   =    7.62   ! strut clamp width               (3inch)
        SCheight  =    3.81   ! strut clamp height              (1.5inch)
        SClength  =    3.81   ! strut clamp length              (1.5inch)
        SCthick   =    0.635  ! strut clamp thickness           (0.25inch)
        CLheight  =    3.4925 ! clamp height			(1.375inch)
        CLwidth   =    3.81   ! clamp width			(1.5inch)
        CLlength  =   41.275  ! clamp length			(16.25inch)
        CLthick   =    0.3175 ! clamp plate thickness           (0.125inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.81   ! PMT assembly radius		(1.5inch)
        DETfront  =    1.016  ! PMT ass. frontplate thickness	(0.4inch)
        ConvThk   =    1.     ! Converter layer thickness
        RadiThk   =    1.     ! Radiator layer thickness      
        EleLeng   =   15.0    ! electronics mount length
        DrLayer   =    6.     ! layer radial width
        NumPMT    =    3      ! number of PMT in layer
        PMTwall   =    0.1    ! PMT wall thickness
        PMTrad    =    2.54   ! PMT and detector radius
        PMTlen    =    8.0    ! PMT tube length
        IBchoice  =    1      ! active/de-activate (0) ibeam
        IBPosYc   =  -16.51   ! IBeam central Ylocation		(6.5inch)
        IBPosZc   =  530.477  ! IBeam central Zposition
        IBLeng    =  294.894  ! IBeam length                    (116.1inch)
        IBthickH  =    0.7366 ! IBeam horiz. plate thickness	(0.29inch)
        IBthickV  =    0.4318 ! IBeam vert. plate thickness	(0.17inch)
        IBheight  =   10.16   ! IBeam height			(4inch)
        IBwidth   =    7.62   ! IBeam width			(3inch)
        IBwlen    =   22.86   ! IBeam vert. front piece         (9inch)   
        IBwhghtF  =    4.4489 ! IBeam vert. front piece height
        IBwhghtB  =    8.3097 ! IBeam vert. front piece height
        EWShift   =    0.0    ! east west z shift
        UDShift   =    0.0    ! up down y shift
        BoltShift =    0.0    ! Bolt additional y shift
     Endfill
*
     FILL VPDG  ! pVPD basic dimensions
        version   =    2      ! geometry version
        zposEast  =  561.2638 ! Z position East
        zposWest  =  563.1688 ! Z position West
        rmin      =    6.35   ! mothervolume rmin		(2.5inch)
        rmax      =   31.27375! mothervolume rmin		(12.3125inch)
        BPwidth   =   30.48   ! baseplate width 		(12inch)
        BPlength  =   46.355  ! baseplate length		(18.5inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    5.08   ! FEE box height			(2inch)
        BXlength  =   15.24   ! FEE box length			(6inch)
        BXwidth   =   25.4    ! FEE box width			(10inch)
        BXthick   =    0.635  ! FEE box plate thickness		(0.25inch) 
        BXzposC   =    0.635  ! FEE central z pos 	(0.25inch)
        FPwidth   =    7.62   ! frontplate width		(3inch)
        FPheight  =   25.45842! frontplate height		(10.023inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPAwidth  =    6.63956! frontpl.part A width            (2.614inch)
        FPAhght   =    8.35405! frontpl.part A height           (3.289inch)
        FPChght   =   12.573  ! frontpl.part C height           (4.95inch)
        FPHhght   =    3.81   ! frontpl. hook height            (1.5inch)
        FPHwidth  =    5.08   ! frontpl. hook width             (2inch)
        FPHthick  =    0.635  ! frontpl. hook thickness         (0.25inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   23.1775 ! strut height			(9.125 inch)
        STangle   =   25.     ! strut angle (degr)
        STdiagsz  =    5.08   ! diagnoal strut size             (2inch)
        SCwidth   =    7.62   ! strut clamp width               (3inch)
        SCheight  =    3.81   ! strut clamp height              (1.5inch)
        SClength  =    3.81   ! strut clamp length              (1.5inch)
        SCthick   =    0.635  ! strut clamp thickness           (0.25inch)
        CLheight  =    3.4925 ! clamp height			(1.375inch)
        CLwidth   =    3.81   ! clamp width			(1.5inch)
        CLlength  =   41.275  ! clamp length			(16.25inch)
        CLthick   =    0.3175 ! clamp plate thickness           (0.125inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.81   ! PMT assembly radius		(1.5inch)
        DETfront  =    1.016  ! PMT ass. frontplate thickness	(0.4inch)
        ConvThk   =    1.     ! Converter layer thickness
        RadiThk   =    1.     ! Radiator layer thickness      
        EleLeng   =   15.0    ! electronics mount length
        DrLayer   =    6.     ! layer radial width
        NumPMT    =    3      ! number of PMT in layer
        PMTwall   =    0.1    ! PMT wall thickness
        PMTrad    =    2.54   ! PMT and detector radius
        PMTlen    =    8.0    ! PMT tube length
        IBchoice  =    1      ! active/de-activate (0) ibeam
        IBPosYc   =  -16.51   ! IBeam central Ylocation		(6.5inch)
        IBPosZc   =  530.477  ! IBeam central Zposition
        IBLeng    =  294.894  ! IBeam length                    (116.1inch)
        IBthickH  =    0.7366 ! IBeam horiz. plate thickness	(0.29inch)
        IBthickV  =    0.4318 ! IBeam vert. plate thickness	(0.17inch)
        IBheight  =   10.16   ! IBeam height			(4inch)
        IBwidth   =    7.62   ! IBeam width			(3inch)
        IBwlen    =   22.86   ! IBeam vert. front piece         (9inch)   
        IBwhghtF  =    4.4489 ! IBeam vert. front piece height
        IBwhghtB  =    8.3097 ! IBeam vert. front piece height
        EWShift   =    0.0    ! east west z shift
        UDShift   =    0.0    ! up down y shift
        BoltShift =    0.0    ! Bolt additional y shift
     Endfill
*
     FILL VPDG  ! pVPD basic dimensions
        version   =    3      ! geometry version
        zposEast  =  564.4388 ! Z position East
        zposWest  =  563.4069 ! Z position West
        rmin      =    6.35   ! mothervolume rmin		(2.5inch)
        rmax      =   31.27375! mothervolume rmin		(12.3125inch)
        BPwidth   =   30.48   ! baseplate width 		(12inch)
        BPlength  =   46.355  ! baseplate length		(18.5inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    5.08   ! FEE box height			(2inch)
        BXlength  =   15.24   ! FEE box length			(6inch)
        BXwidth   =   25.4    ! FEE box width			(10inch)
        BXthick   =    0.635  ! FEE box plate thickness		(0.25inch) 
        BXzposC   =    0.635  ! FEE central z pos 	(0.25inch)
        FPwidth   =    7.62   ! frontplate width		(3inch)
        FPheight  =   25.45842! frontplate height		(10.023inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPAwidth  =    6.63956! frontpl.part A width            (2.614inch)
        FPAhght   =    8.35405! frontpl.part A height           (3.289inch)
        FPChght   =   12.573  ! frontpl.part C height           (4.95inch)
        FPHhght   =    3.81   ! frontpl. hook height            (1.5inch)
        FPHwidth  =    5.08   ! frontpl. hook width             (2inch)
        FPHthick  =    0.635  ! frontpl. hook thickness         (0.25inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   23.1775 ! strut height			(9.125 inch)
        STangle   =   25.     ! strut angle (degr)
        STdiagsz  =    5.08   ! diagnoal strut size             (2inch)
        SCwidth   =    7.62   ! strut clamp width               (3inch)
        SCheight  =    3.81   ! strut clamp height              (1.5inch)
        SClength  =    3.81   ! strut clamp length              (1.5inch)
        SCthick   =    0.635  ! strut clamp thickness           (0.25inch)
        CLheight  =    3.4925 ! clamp height			(1.375inch)
        CLwidth   =    3.81   ! clamp width			(1.5inch)
        CLlength  =   41.275  ! clamp length			(16.25inch)
        CLthick   =    0.3175 ! clamp plate thickness           (0.125inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.81   ! PMT assembly radius		(1.5inch)
        DETfront  =    1.016  ! PMT ass. frontplate thickness	(0.4inch)
        ConvThk   =    1.     ! Converter layer thickness
        RadiThk   =    1.     ! Radiator layer thickness      
        EleLeng   =   15.0    ! electronics mount length
        DrLayer   =    6.     ! layer radial width
        NumPMT    =    3      ! number of PMT in layer
        PMTwall   =    0.1    ! PMT wall thickness
        PMTrad    =    2.54   ! PMT and detector radius
        PMTlen    =    8.0    ! PMT tube length
        IBchoice  =    1      ! active/de-activate (0) ibeam
        IBPosYc   =  -16.51   ! IBeam central Ylocation		(6.5inch)
        IBPosZc   =  530.477  ! IBeam central Zposition
        IBLeng    =  294.894  ! IBeam length                    (116.1inch)
        IBthickH  =    0.7366 ! IBeam horiz. plate thickness	(0.29inch)
        IBthickV  =    0.4318 ! IBeam vert. plate thickness	(0.17inch)
        IBheight  =   10.16   ! IBeam height			(4inch)
        IBwidth   =    7.62   ! IBeam width			(3inch)
        IBwlen    =   22.86   ! IBeam vert. front piece         (9inch)   
        IBwhghtF  =    4.4489 ! IBeam vert. front piece height
        IBwhghtB  =    8.3097 ! IBeam vert. front piece height
        EWShift   =    0.0    ! east west z shift
        UDShift   =    0.0    ! up down y shift
        BoltShift =    0.0    ! Bolt additional y shift
     Endfill
*
*---- Version 4 is the star default setup for Y2004X,A,B....
     FILL VPDG  ! pVPD basic dimensions
        version   =    4      ! geometry version
        zposEast  = 574.5688  ! Z position East 
        zposWest  = 573.5669  ! Z position West 
        rmin      =    6.35   ! mothervolume rmin		(2.5inch)
        rmax      =   31.27375! mothervolume rmin		(12.3125inch)
        BPwidth   =   30.48   ! baseplate width 		(12inch)
        BPlength  =   46.355  ! baseplate length		(18.5inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    5.08   ! FEE box height			(2inch)
        BXlength  =   15.24   ! FEE box length			(6inch)
        BXwidth   =   25.4    ! FEE box width			(10inch)
        BXthick   =    0.635  ! FEE box plate thickness		(0.25inch)
        BXzposC   =    0.635  ! FEE central z pos 	(0.25inch)
        FPwidth   =    7.62   ! frontplate width		(3inch)
        FPheight  =   25.45842! frontplate height		(10.023inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPAwidth  =    6.63956! frontpl.part A width            (2.614inch)
        FPAhght   =    8.35405! frontpl.part A height           (3.289inch)
        FPChght   =   12.573  ! frontpl.part C height           (4.95inch)
        FPHhght   =    3.81   ! frontpl. hook height            (1.5inch)
        FPHwidth  =    5.08   ! frontpl. hook width             (2inch)
        FPHthick  =    0.635  ! frontpl. hook thickness         (0.25inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   23.1775 ! strut height			(9.125 inch)
        STangle   =   25.     ! strut angle (degr)
        STdiagsz  =    5.08   ! diagnoal strut size             (2inch)
        SCwidth   =    7.62   ! strut clamp width               (3inch)
        SCheight  =    3.81   ! strut clamp height              (1.5inch)
        SClength  =    3.81   ! strut clamp length              (1.5inch)
        SCthick   =    0.635  ! strut clamp thickness           (0.25inch)
        CLheight  =    3.4925 ! clamp height			(1.375inch)
        CLwidth   =    3.81   ! clamp width			(1.5inch)
        CLlength  =   41.275  ! clamp length			(16.25inch)
        CLthick   =    0.3175 ! clamp plate thickness           (0.125inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.81   ! PMT assembly radius		(1.5inch)
        DETfront  =    1.016  ! PMT ass. frontplate thickness	(0.4inch)
        ConvThk   =    1.     ! Converter layer thickness
        RadiThk   =    1.     ! Radiator layer thickness      
        EleLeng   =   15.0    ! electronics mount length
        DrLayer   =    6.     ! layer radial width
        NumPMT    =    3      ! number of PMT in layer
        PMTwall   =    0.1    ! PMT wall thickness 
        PMTrad    =    2.54   ! PMT and detector radius
        PMTlen    =    8.0    ! PMT tube length
        IBchoice  =    1      ! active/de-activate (0) ibeam
        IBPosYc   =  -16.51   ! IBeam central Ylocation		  (6.5inch)
        IBPosZc   =  530.477  ! IBeam central Zposition  
        IBLeng    =  294.894  ! IBeam length                (116.1inch) 
        IBthickH  =    0.7366 ! IBeam horiz. plate thickness (0.29inch)
        IBthickV  =    0.4318 ! IBeam vert. plate thickness	 (0.17inch)
        IBheight  =   10.16   ! IBeam height		      	    (4inch)
        IBwidth   =    7.62   ! IBeam width			            (3inch)
        IBwlen    =   22.86   ! IBeam vert. front piece         (9inch)   
        IBwhghtF  =    4.4489 ! IBeam vert. front piece height
        IBwhghtB  =    8.3097 ! IBeam vert. front piece height
        EWShift   =    0.0    ! east west z shift
        UDShift   =    0.0    ! up down y shift
        BoltShift =    0.0    ! Bolt additional y shift
     Endfill

*---- Version 5/6 includes all Run-4/5 geometry plus add'l support structure for Y2004Y/Y2005 (WJL)
*
* note that there is a 3/4" shift between the Zpositions of the [I-beam+supportstructure] on the east and west...
*  the shift is 3/4", east items farther from Z=0 than west items
* for run<5, this pvpd is posn'd wrt to the ibeam, hence the pvpd Zposns aren't symmetric in |Z|
* for run=5, the pvpd is posn'd wrt to the downstream faces of the magnet steel...
*
* in this version of the vpddgeo code: 
*   for pvpd this offset is handled explictly in zposEast/West variables, for historical/back-compatibility reasons...
*   for pipe support structure this is handled in code with variable EWshift=0.75*2.54 for simplicity...
*
     FILL VPDG  ! pVPD basic dimensions
        version   =    5      ! geometry version    WJL ...used by Y2004Y
        zposEast  = 570.0     ! Z position East 	WJL see next, plus add 3/4in to take care of E/W offset (was 574.5688)
        zposWest  = 568.1     ! Z position West 	WJL via CADD: 362.2+(194.3-11.6)+2.54*(18.25/2)=568.08  (was 573.5669)
        rmin      =    6.35   ! mothervolume rmin		(2.5inch)
        rmax      =   31.27375! mothervolume rmin		(12.3125inch)
        BPwidth   =   30.48   ! baseplate width 		(12inch)
        BPlength  =   46.355  ! baseplate length		(18.5inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    5.08   ! FEE box height			(2inch)
        BXlength  =   15.24   ! FEE box length			(6inch)
        BXwidth   =   25.4    ! FEE box width			(10inch)
        BXthick   =    0.127  ! FEE box plate thickness		WJL should be 50mils!! MODIFIED (was 0.635)
        BXzposC   =    0.635  ! FEE central z pos 		(0.25inch)
        FPwidth   =    7.62   ! frontplate width		(3inch)
        FPheight  =   25.45842! frontplate height		(10.023inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPAwidth  =    6.63956! frontpl.part A width            (2.614inch)
        FPAhght   =    8.35405! frontpl.part A height           (3.289inch)
        FPChght   =   12.573  ! frontpl.part C height           (4.95inch)
        FPHhght   =    3.81   ! frontpl. hook height            (1.5inch)
        FPHwidth  =    5.08   ! frontpl. hook width             (2inch)
        FPHthick  =    0.635  ! frontpl. hook thickness         (0.25inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   23.1775 ! strut height			(9.125 inch)
        STangle   =   25.     ! strut angle (degr)
        STdiagsz  =    5.08   ! diagnoal strut size             (2inch)
        SCwidth   =    7.62   ! strut clamp width               (3inch)
        SCheight  =    3.81   ! strut clamp height              (1.5inch)
        SClength  =    3.81   ! strut clamp length              (1.5inch)
        SCthick   =    0.635  ! strut clamp thickness           (0.25inch)
        CLheight  =    3.4925 ! clamp height			(1.375inch)
        CLwidth   =    3.81   ! clamp width			(1.5inch)
        CLlength  =   41.275  ! clamp length			(16.25inch)
        CLthick   =    0.3175 ! clamp plate thickness           (0.125inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.81   ! PMT assembly radius		(1.5inch)
        DETfront  =    1.016  ! PMT ass. frontplate thickness	(0.4inch)
        ConvThk   =    1.     ! Converter layer thickness
        RadiThk   =    1.     ! Radiator layer thickness      
        EleLeng   =   15.0    ! electronics mount length
        DrLayer   =    6.     ! layer radial width
        NumPMT    =    3      ! number of PMT in layer
        PMTwall   =    0.1    ! PMT wall thickness 
        PMTrad    =    2.54   ! PMT and detector radius
        PMTlen    =    8.0    ! PMT tube length
        IBchoice  =    1      ! active/de-activate (0) ibeam
        IBPosYc   =  -16.51   ! IBeam central Ypos	(6.5inch)
        IBPosZc   =  540.45   ! IBeam central Zpos	WJL should be 362.2+(12.125*2.54)+(IBLeng/2)=540.45 MODIFIED, NOTE EAST is 3/4in farther, add that offset in the code!!!!
        IBLeng    =  294.894  ! IBeam length                (116.1inch) 
        IBthickH  =    0.7366 ! IBeam horiz. plate thickness (0.29inch)
        IBthickV  =    0.4318 ! IBeam vert. plate thickness	 (0.17inch)
        IBheight  =   10.16   ! IBeam height		      	    (4inch)
        IBwidth   =    7.62   ! IBeam width			            (3inch)
        IBwlen    =   22.86   ! IBeam vert. front piece         (9inch)   
        IBwhghtF  =    4.4489 ! IBeam vert. front piece height
        IBwhghtB  =    8.3097 ! IBeam vert. front piece height
        EWShift   =    1.905  ! east west z shift (0.75*2.54)
        UDShift   =    0.0    ! up down y shift
        BoltShift =    0.0    ! Bolt additional y shift
    Endfill
     FILL VPDG  ! pVPD basic dimensions
        version   =    6      ! geometry version    WJL ...used by Y2005, Y2006A, Y2006B
        zposEast  = 583.806   ! Z position East 	WJL from newer CADD: 220.720in+(18.25in/2)=229.845in  
        zposWest  = 583.488   ! Z position West 	WJL from newer CADD: 220.595in+(18.25in/2)=229.720in
        rmin      =    6.35   ! mothervolume rmin		(2.5inch)
        rmax      =   31.27375! mothervolume rmin		(12.3125inch)
        BPwidth   =   30.48   ! baseplate width 		(12inch)
        BPlength  =   46.355  ! baseplate length		(18.5inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    5.08   ! FEE box height			(2inch)
        BXlength  =   15.24   ! FEE box length			(6inch)
        BXwidth   =   25.4    ! FEE box width			(10inch)
        BXthick   =    0.127  ! FEE box plate thickness		WJL should be 50mils!! MODIFIED (was 0.635)
        BXzposC   =    0.635  ! FEE central z pos 		(0.25inch)
        FPwidth   =    7.62   ! frontplate width		(3inch)
        FPheight  =   25.45842! frontplate height		(10.023inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPAwidth  =    6.63956! frontpl.part A width            (2.614inch)
        FPAhght   =    8.35405! frontpl.part A height           (3.289inch)
        FPChght   =   12.573  ! frontpl.part C height           (4.95inch)
        FPHhght   =    3.81   ! frontpl. hook height            (1.5inch)
        FPHwidth  =    5.08   ! frontpl. hook width             (2inch)
        FPHthick  =    0.635  ! frontpl. hook thickness         (0.25inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   23.1775 ! strut height			(9.125 inch)
        STangle   =   25.     ! strut angle (degr)
        STdiagsz  =    5.08   ! diagnoal strut size             (2inch)
        SCwidth   =    7.62   ! strut clamp width               (3inch)
        SCheight  =    3.81   ! strut clamp height              (1.5inch)
        SClength  =    3.81   ! strut clamp length              (1.5inch)
        SCthick   =    0.635  ! strut clamp thickness           (0.25inch)
        CLheight  =    3.4925 ! clamp height			(1.375inch)
        CLwidth   =    3.81   ! clamp width			(1.5inch)
        CLlength  =   41.275  ! clamp length			(16.25inch)
        CLthick   =    0.3175 ! clamp plate thickness           (0.125inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.81   ! PMT assembly radius		(1.5inch)
        DETfront  =    1.016  ! PMT ass. frontplate thickness	(0.4inch)
        ConvThk   =    1.     ! Converter layer thickness
        RadiThk   =    1.     ! Radiator layer thickness      
        EleLeng   =   15.0    ! electronics mount length
        DrLayer   =    6.     ! layer radial width
        NumPMT    =    3      ! number of PMT in layer
        PMTwall   =    0.1    ! PMT wall thickness 
        PMTrad    =    2.54   ! PMT and detector radius
        PMTlen    =    8.0    ! PMT tube length
        IBchoice  =    1      ! active/de-activate (0) ibeam
        IBPosYc   =  -16.51   ! IBeam central Ypos	(6.5inch)
        IBPosZc   =  540.45   ! IBeam central Zpos	WJL should be 362.2+(12.125*2.54)+(IBLeng/2)=540.45 MODIFIED, NOTE EAST is 3/4in farther, add that offset in the code!!!!
        IBLeng    =  294.894  ! IBeam length                (116.1inch) 
        IBthickH  =    0.7366 ! IBeam horiz. plate thickness (0.29inch)
        IBthickV  =    0.4318 ! IBeam vert. plate thickness	 (0.17inch)
        IBheight  =   10.16   ! IBeam height		      	    (4inch)
        IBwidth   =    7.62   ! IBeam width			            (3inch)
        IBwlen    =   22.86   ! IBeam vert. front piece         (9inch)   
        IBwhghtF  =    4.4489 ! IBeam vert. front piece height
        IBwhghtB  =    8.3097 ! IBeam vert. front piece height
        EWShift   =    1.905  ! east west z shift (0.75*2.54)
        UDShift   =    0.0    ! up down y shift
        BoltShift =    0.0    ! Bolt additional y shift
     Endfill

     FILL VPDH  ! upVPD basic dimensions
        version   =    7       ! geometry version	WJL ...used by Y2006A --- and future 2007 run
        zposEast  =  571.45    ! Z position East	(more or less arbitrary)
        zposWest  =  571.45    ! Z position West	(more or less arbitrary)
        rmin      =    6.6675  ! mothervolume rmin	(2 5/8in)
        rmax      =   16.51    ! mothervolume rmax	(6 1/2in)
        length    =   32.      ! mothervolume length
        detrad    =    2.54    ! det assy cylnder radius (2in diameter)
        detlen    =   20.32    ! det assy cylinder radius (8in)
        pmtrad    =    1.905   ! pmt cylinder radius (1.5in diameter)
        pmtlen    =    1.905   ! pmt cylinder length (1.5in)
        detwall   =    0.127   ! det assy cylinder wall thickness (50mils)
        detfront  =    0.3175  ! det assy front and back plate thicknesses (1/8in)
        leadthick =    1.0     ! lead converter layer thickness (1cm)
        scintthick=    1.0     !   scintillator layer thickness (1cm)
        ring1_ndet =  10       ! upVPD number of tubes in ring 1
        ring1_rad  =  10.16    ! upVPD geo parameter (4in)
        ring1_phi0 = -49.      ! upVPD geo parameter
        ring1_dphi =  31.      ! upVPD geo parameter
        ring1_kproj=   0       ! upVPD geo parameter
        ring2_ndet =   9       ! upVPD number of tubes in ring 2
        ring2_rad  =  14.2875  ! upVPD geo parameter (5 5/8in)
        ring2_phi0 = -33.5     ! upVPD geo parameter
        ring2_dphi =  31.      ! upVPD geo parameter
        ring2_kproj=   0       ! upVPD geo parameter
        ring3_ndet =   0       ! upVPD number of tubes in ring 3
        ring3_rad  =  99.      ! upVPD geo parameter (5 5/8in)
        ring3_phi0 =  99.      ! upVPD geo parameter
        ring3_dphi =  99.      ! upVPD geo parameter
        ring3_kproj=   0       ! upVPD geo parameter
        EWShift   =    1.905   ! east west z shift (0.75*2.54)
        UDShift   =    0.0     ! up down y shift
        BoltShift =    0.0     ! Bolt additional y shift
     Endfill
     FILL VPDH  ! upVPD basic dimensions
        version   =    8       ! geometry version	WJL ...used by Y2006B
        zposEast  =  571.45    ! Z position East	(more or less arbitrary)
        zposWest  =  571.45    ! Z position West	(more or less arbitrary)
        rmin      =    6.6675  ! mothervolume rmin	(2 5/8in)
        rmax      =   16.51    ! mothervolume rmax	(6 1/2in)
        length    =   32.      ! mothervolume length
        detrad    =    2.54    ! det assy cylnder radius (2in diameter)
        detlen    =   20.32    ! det assy cylinder radius (8in)
        pmtrad    =    1.905   ! pmt cylinder radius (1.5in diameter)
        pmtlen    =    1.905   ! pmt cylinder length (1.5in)
        detwall   =    0.127   ! det assy cylinder wall thickness (50mils)
        detfront  =    0.3175  ! det assy front and back plate thicknesses (1/8in)
        leadthick =    1.0     ! lead converter layer thickness (1cm)
        scintthick=    1.0     !   scintillator layer thickness (1cm)
        ring1_ndet =  11       ! upVPD number of tubes in ring 1
        ring1_rad  =  9.5      ! upVPD geo parameter      (3.74in)
        ring1_phi0 = -73.6364  ! upVPD geo parameter      (360/11)
        ring1_dphi =  32.7272  ! upVPD geo parameter (-90+(360/11/2))
        ring1_kproj=   0       ! upVPD geo parameter
        ring2_ndet =  11       ! upVPD number of tubes in ring 2
        ring2_rad  =  13.7     ! upVPD geo parameter      (5.394in)
        ring2_phi0 = -90       ! upVPD geo parameter 
        ring2_dphi =  32.7272  ! upVPD geo parameter      (360/11)
        ring2_kproj=   0       ! upVPD geo parameter
        ring3_ndet =   0       ! upVPD number of tubes in ring 3
        ring3_rad  =  99.      ! upVPD geo parameter (5 5/8in)
        ring3_phi0 =  99.      ! upVPD geo parameter
        ring3_dphi =  99.      ! upVPD geo parameter
        ring3_kproj=   0       ! upVPD geo parameter
        EWShift   =    1.905   ! east west z shift (0.75*2.54)
        UDShift   =   -5.715   ! up down y shift
        BoltShift =    1.0     ! Bolt additional y shift
     Endfill
     FILL VPDH  ! upVPD basic dimensions
        version   =    9       ! geometry version	WJL ...used by Y2006C
        zposEast  =  571.45    ! Z position East	(more or less arbitrary)
        zposWest  =  571.45    ! Z position West	(more or less arbitrary)
        rmin      =    6.6675  ! mothervolume rmin	(2 5/8in)
        rmax      =   19.685   ! mothervolume rmax  (7 3/4in)
        length    =   32.      ! mothervolume length
        detrad    =    2.54    ! det assy cylnder radius (2in diameter)
        detlen    =   20.32    ! det assy cylinder radius (8in)
        pmtrad    =    1.905   ! pmt cylinder radius (1.5in diameter)
        pmtlen    =    1.905   ! pmt cylinder length (1.5in)
        detwall   =    0.127   ! det assy cylinder wall thickness (50mils)
        detfront  =    0.3175  ! det assy front and back plate thicknesses (1/8in)
        leadthick =    1.0     ! lead converter layer thickness (1cm)
        scintthick=    1.0     !   scintillator layer thickness (1cm)
        ring1_ndet =  11       ! upVPD number of tubes in ring 1
        ring1_rad  =  9.5      ! upVPD geo parameter      (3.74in)
        ring1_phi0 = -73.6364  ! upVPD geo parameter      (360/11)
        ring1_dphi =  32.7272  ! upVPD geo parameter (-90+(360/11/2))
        ring1_kproj=   0       ! upVPD geo parameter
        ring2_ndet =  11       ! upVPD number of tubes in ring 2
        ring2_rad  =  13.7     ! upVPD geo parameter      (5.394in)
        ring2_phi0 = -90       ! upVPD geo parameter 
        ring2_dphi =  32.7272  ! upVPD geo parameter      (360/11)
        ring2_kproj=   0       ! upVPD geo parameter
        ring3_ndet =  11       ! upVPD number of tubes in ring 3
        ring3_rad  =  16.8     ! upVPD geo parameter
        ring3_phi0 = -73.6364  ! upVPD geo parameter
        ring3_dphi =  32.7272  ! upVPD geo parameter
        ring3_kproj=   0       ! upVPD geo parameter
        EWShift   =    1.905   ! east west z shift (0.75*2.54)
        UDShift   =   -8.255   ! up down y shift
        BoltShift =    1.5     ! Bolt additional y shift
     Endfill
     FILL VPDH  ! upVPD basic dimensions
        version   =   10       ! geometry version	WJL ...used by Y2006D
        zposEast  =  571.45    ! Z position East	(more or less arbitrary)
        zposWest  =  571.45    ! Z position West	(more or less arbitrary)
        rmin      =    6.6675  ! mothervolume rmin	(2 5/8in)
        rmax      =   19.685   ! mothervolume rmax  (7 3/4in)
        length    =   32.      ! mothervolume length
        detrad    =    2.54    ! det assy cylnder radius (2in diameter)
        detlen    =   20.32    ! det assy cylinder radius (8in)
        pmtrad    =    1.905   ! pmt cylinder radius (1.5in diameter)
        pmtlen    =    1.905   ! pmt cylinder length (1.5in)
        detwall   =    0.127   ! det assy cylinder wall thickness (50mils)
        detfront  =    0.3175  ! det assy front and back plate thicknesses (1/8in)
        leadthick =    1.0     ! lead converter layer thickness (1cm)
        scintthick=    1.0     !   scintillator layer thickness (1cm)
        ring1_ndet =  11       ! upVPD number of tubes in ring 1
        ring1_rad  =  9.5      ! upVPD geo parameter      (3.74in)
        ring1_phi0 = -73.6364  ! upVPD geo parameter      (360/11)
        ring1_dphi =  32.7272  ! upVPD geo parameter (-90+(360/11/2))
        ring1_kproj=   1       ! upVPD geo parameter
        ring2_ndet =  11       ! upVPD number of tubes in ring 2
        ring2_rad  =  13.7     ! upVPD geo parameter      (5.394in)
        ring2_phi0 = -90       ! upVPD geo parameter 
        ring2_dphi =  32.7272  ! upVPD geo parameter      (360/11)
        ring2_kproj=   1       ! upVPD geo parameter
        ring3_ndet =  11       ! upVPD number of tubes in ring 3
        ring3_rad  =  16.8     ! upVPD geo parameter
        ring3_phi0 = -73.6364  ! upVPD geo parameter
        ring3_dphi =  32.7272  ! upVPD geo parameter
        ring3_kproj=   1       ! upVPD geo parameter
        EWShift   =    1.905   ! east west z shift (0.75*2.54)
        UDShift   =   -8.255   ! up down y shift
        BoltShift =    1.5     ! Bolt additional y shift
     Endfill

*--- pVPD: pipe-support additional hardware defined...
     FILL VPDS  ! pipe-support material
        version = 1          ! first version in Year 2007
        IBSAZc  = 692.91     ! IBSA position Z center
        IBSAYc  = -60.10     ! IBSA position Y center
        IBSAXc  = 24.11      ! IBSA position X center
        IBSBZc  = 631.20     ! IBSB position Z center
        IBSBYc  = -69.50     ! IBSB position Y center = (Yc + UDshift)
        IBSBXc  = 24.11      ! IBSB position X center
        IBSCZc  = 581.10     ! IBSC position Z center
        IBSCYc  = -23.60     ! IBSC position Y center = (Yc + UDshift)
        IBSCXc  = 0.00       ! IBSC position X center
        IBSDZc1 = 397.70     ! IBSD position Z center 1
        IBSDZc2 = 442.80     ! IBSD position Z center 2
        IBSDYc1 = -11.01     ! IBSD position Y center 1 = (Yc+UDshift)
        IBSDYc2 = -7.20      ! IBSD position Y center 2 
        IBSDXc  = 0.00       ! IBSD position X center
        IBSEZc1 = 397.70     ! IBSE position Z center 1
        IBSEZc2 = 442.80     ! IBSE position Z center 2
        IBSEYc  = -13.44     ! IBSE position Y center 
        IBSEXc  = 6.99       ! IBSE position X center
        IBSFZc  = 411.70     ! IBSF position Z center
        IBSFYc  = -16.51     ! IBSF position Y center = (Yc + UDshift)
        IBSFXc  = 0.00       ! IBSF position X center
        IBSGZc1 = 397.70     ! IBSE position Z center 1
        IBSGZc2 = 442.80     ! IBSE position Z center 2
        IBSGZc3 = 2.54       ! IBSG position Z center offset
        IBSGYc  = -17.57     ! IBSG position Y center = (Yc+UDshift+BoltShift)
        IBSGXc  = 0.00       ! IBSG position X center
        IBSHZc1 = 397.70     ! IBSH position Z center 1
        IBSHZc2 = 442.80     ! IBSH position Z center 2
        IBSHYc  = 0.00       ! IBSH position Y center
        IBSHXc1 = 6.35       ! IBSH position X center 1
        IBSHXc2 = 7.62       ! IBSH position X center 2
        BSALenX = 7.62       ! IBSA X length
        BSALenY = 84.00      ! IBSA Y length
        BSALenZ = 7.62       ! IBSA Z length
        BAALenZ = 0.95       ! IBAA Z length
        BSBLenY = 142.4      ! IBSB Y length
        BSCLenX = 40.64      ! IBSC X length
        BSCLenY = 4.01       ! IBSC Y length
        BSCLenZ = 10.16      ! IBSC Z length
        BACLenZ = 0.64       ! IBAC Z length
        BSDLenX = 20.32      ! IBSD X length
        BSELenY = 8.26       ! IBSE Y length
        BSELenZ = 6.35       ! IBSE Z length
        BSFRmax = 0.95       ! IBSF Rmax
        BSFLenZ = 182.88     ! IBSF Z length
        BSSLenZ = 7.62       ! IBSS Z length
        BSGRmax = 0.64       ! IBSG Rmax
        BSGLenZ1 = 10.16     ! IBSG Z length 1 for vpdConfig<8
        BSGLenZ2 = 12.70     ! IBSG Z length 2 for vpdConfig=8
        BSGLenZ3 = 15.24     ! IBSG Z length 3 for vpdConfig else
        BSHLenZ  = 5.08      ! IBSH Z length
        ElecWid  = 20.3      ! Electronic box width
        ElecThck = 0.17      ! Electronic box thickness
        ElecLen  = 5.10      ! Electronic box length
        VFEEPosX = 0.31      ! VFEE position X center
        VFEEPosY = 0.45      ! VFEE position Y center
        VFEEPosZ = -4.75     ! VFEE position Z center
        VLEMPosX = { -7.0, -3.5, 0.0, 3.5, 7.0,
                     -7.0, -3.5, 0.0, 3.5, 7.0,
                     -6.0, -2.5, 1.0, 4.5, 8.0 }  ! VLEM position X center
        VLEMPosY = 0.52      ! VLEM position Y center
        VLEMPosZ = { 2.0, 2.0, 2.0, 2.0, 2.0,
                     -2.0, -2.0, -2.0, -2.0, -2.0,
                     -2.0, -2.0, -2.0, -2.0, -2.0 } ! VLEM position Z center
        VLEMLenX = 0.86      ! VLEM X length
        VLEMLenY = 0.68      ! VLEM Y length
        VLEMLenZ = 3.8       ! VLEM Z length
        VPIPPosX = 0.09      ! VPIP position X center
        VPIPPosY = 0.0       ! VPIP position Y center
        VPIPPosZ = 0.9       ! VPIP position Z center
        VPIPRmin = 0.31      ! VPIP Rmin
        VPIPRmax = 0.34      ! VPIP Rmax
        VPIPLenZ = 2.0       ! VPIP Z length
     Endfill

     USE  VPDV
     USE  VPDS

*---- the detectors...
      print *,' pVPD: VPDV_vpdConfig =',VPDV_vpdConfig
      if (VPDV_vpdConfig<=4) then
       print *,' pVPD: Using the VPDG_ Geometry for the STAR-standard pVPD detector...'
       kDetStyle = 0
       USE VPDG version=VPDV_vpdConfig;
       zposE     = vpdg_zposEast
       zposW     = vpdg_zposWest
       EWshift   = vpdg_EWShift
       UDshift   = vpdg_UDShift
       BoltShift = vpdg_BoltShift
      elseif (VPDV_vpdConfig==5.or.VPDV_vpdConfig==6) then
       print *,' pVPD: Using the VPDG_ Geometry for the improved pVPD & pipe-support structure...'
       kDetStyle = 1
       USE VPDG version=VPDV_vpdConfig;
       zposE     = vpdg_zposEast
       zposW     = vpdg_zposWest
       EWshift   = vpdg_EWShift
       UDshift   = vpdg_UDShift
       BoltShift = vpdg_BoltShift
      elseif (VPDV_vpdConfig>=7) then
       print *,'upVPD: Using the VPDG_ & VPDH_ Geometries for the Upgraded pVPD (upPVD) detector, W/ Ibeam lowering...'
       kDetStyle = 2
       USE VPDG version=6;
       USE VPDH version=VPDV_vpdConfig;
       zposE     = vpdh_zposEast
       zposW     = vpdh_zposWest
       EWshift   = vpdh_EWShift
       UDshift   = vpdh_UDShift
       BoltShift = vpdh_BoltShift
      endif
*      print *,' pVPD: Zpositions East and West:',zposE,' &',zposW,' cm'	
*      if (VPDV_vpdConfig==10) then
*        print *,'upVPD: Detector assemblies have faces pointing at X=Y=Z=0...'
*      else
*        print *,'upVPD: Detector assemblies have long axes parallel to Z-axis ...'
*      endif

      Create VPDD
      zpos = zposW
      Position VPDD in Cave   z=+zpos            Konly='Many'
      zpos = zposE
      Position VPDD in Cave   z=-zpos ThetaZ=180 Konly='Many'

      kIBeamStyle=0
      if (VPDG_IBchoice!=0) then
        kIBeamStyle=1
        if (VPDV_vpdConfig>4) then
          kIBeamStyle=2
          if (VPDV_vpdConfig>=8) then        
           kIBeamStyle=3
          end if
        endif
      endif

      if (kIBeamStyle>0) then
*- - - - the 4" I-Beam only (STAR default)
       Create and Position IBEM in Cave z=+(vpdg_IBPosZc) y=vpdg_IBposYc+UDshift
                  Position IBEM in Cave z=-(vpdg_IBPosZc+EWshift) y=vpdg_IBposYc+UDshift ThetaZ=180
*       print *,' pVPD: pipe-support I-Beam defined...'
*       print *,' IBEM West Z =',vpdg_IBPosZc
*       print *,' IBEM East Z =',-(vpdg_IBPosZc+EWshift)
      if (kIBeamStyle>1) then
*       print *,' pVPD: pipe-support additional hardware defined...'
*- - - - vertical pcs of 3" Al Angle on balcony...
       Create and Position IBSA in Cave z=vpds_IBSAZc y=vpds_IBSAYc x=vpds_IBSAXc 
                  Position IBSA in Cave z=vpds_IBSAZc y=vpds_IBSAYc x=-vpds_IBSAXc,
                         ORT=-X-YZ
                  Position IBSA in Cave z=-(vpds_IBSAZc+EWshift) y=vpds_IBSAYc x=vpds_IBSAXc, 
                         ORT=-X-YZ 
                  Position IBSA in Cave z=-(vpds_IBSAZc+EWshift) y=vpds_IBSAYc x=-vpds_IBSAXc, 
                         ORT=-X-Y-Z 
*- - - - diagonal pcs of 3" Al Angle from balcony to I-beam...
       Create and Position IBSB in Cave z=vpds_IBSBZc      	 y=vpds_IBSBYc+UDshift x= vpds_IBSBXc,
                                        AlphaX=-45 
                  Position IBSB in Cave z=vpds_IBSBZc 		 y=vpds_IBSBYc+UDshift x=-vpds_IBSBXc,
                                        AlphaY= 90 AlphaX=-45  
                  Position IBSB in Cave z=-(vpds_IBSBZc+EWshift) y=vpds_IBSBYc+UDshift x= vpds_IBSBXc, 
		                        AlphaY= -90 AlphaX= 45   
                  Position IBSB in Cave z=-(vpds_IBSBZc+EWshift) y=vpds_IBSBYc+UDshift x=-vpds_IBSBXc,
		                        AlphaY= 180 AlphaX= 45  
*- - - - horizontal pcs of 4" channel+endcaps that connect diagonals and underside of I-beam...
       Create and Position IBSC in Cave z=vpds_IBSCZc y=vpds_IBSCYc+UDshift
                  Position IBSC in Cave z=-(vpds_IBSCZc+EWshift) y=vpds_IBSCYc+UDshift,
                                        ThetaZ=180
*- - - - the horizontal plates on top of I-beam that each hold 2 pipe-support brackets...
       Create and Position IBSD in Cave z=vpds_IBSDZc1 x=0 y=vpds_IBSDYc1+UDshift
                  Position IBSD in Cave z=vpds_IBSDZc2 x=0 y=vpds_IBSDYc1+UDshift
                  Position IBSD in Cave z=-(vpds_IBSDZc1+EWshift) x=0 y=vpds_IBSDYc1+UDshift,
                                        ThetaZ=180
                  Position IBSD in Cave z=-(vpds_IBSDZc2+EWshift) x=0 y=vpds_IBSDYc1+UDshift,
                                        ThetaZ=180
                  Position IBSD in Cave z=vpds_IBSDZc1 x=0 y=vpds_IBSDYc2
                  Position IBSD in Cave z=vpds_IBSDZc2 x=0 y=vpds_IBSDYc2
                  Position IBSD in Cave z=-(vpds_IBSDZc1+EWshift) x=0 y=vpds_IBSDYc2,
                                        ThetaZ=180
                  Position IBSD in Cave z=-(vpds_IBSDZc2+EWshift) x=0 y=vpds_IBSDYc2,
                                        ThetaZ=180
*- - - - the pipe-support brackets...
       tempos = (vpdg_IBposYc+vpdg_IBthickH+(vpdg_IBheight/2.))
       Create and Position IBSE in Cave z=vpds_IBSEZc1 x=+vpds_IBSEXc y=vpds_IBSEYc
                  Position IBSE in Cave z=vpds_IBSEZc1 x=-vpds_IBSEXc y=vpds_IBSEYc
                  Position IBSE in Cave z=vpds_IBSEZc2 x=+vpds_IBSEXc y=vpds_IBSEYc
                  Position IBSE in Cave z=vpds_IBSEZc2 x=-vpds_IBSEXc y=vpds_IBSEYc
                  Position IBSE in Cave z=-(vpds_IBSEZc1+EWshift) x=+vpds_IBSEXc y=vpds_IBSEYc
                  Position IBSE in Cave z=-(vpds_IBSEZc1+EWshift) x=-vpds_IBSEXc y=vpds_IBSEYc 
                  Position IBSE in Cave z=-(vpds_IBSEZc2+EWshift) x=+vpds_IBSEXc y=vpds_IBSEYc 
                  Position IBSE in Cave z=-(vpds_IBSEZc2+EWshift) x=-vpds_IBSEXc y=vpds_IBSEYc
*- - - - the long threaded rods for X-support of the IBeam
* note this cuts through the IBEM mother, so two little pieces 'inside the ibeam' not defined!
* need to place two small stubs inside ibem! (note draw's o.k., only see this w/ agpmater!!!)'
       Create and Position IBSF in Cave  z=vpds_IBSFZc y=vpdg_IBposYc+UDshift alphay=90
                  Position IBSF in Cave  z=-(vpds_IBSFZc+EWshift) y=vpdg_IBposYc+UDshift alphay=90
       Create and Position IBSS in IBEM z=+(vpds_IBSFZc)-(vpdg_IBPosZc) y=0 alphay=90
                  Position IBSS in IBEM z=-(vpds_IBSFZc+EWshift)+(vpdg_IBPosZc) y=0 alphay=90
*- - - - the bolts in the pipe-support brackets...
       Create and Position IBSG in Cave z=vpds_IBSGZc1-vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc1-vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc1+vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc1+vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc2-vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc2-vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc2+vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=vpds_IBSGZc2+vpds_IBSGZc3,
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc1-vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc1-vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc1+vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc1+vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc2-vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc2-vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc2+vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=+vpds_IBSGXc alphax=90
                  Position IBSG in Cave z=-(vpds_IBSGZc2+vpds_IBSGZc3+EWshift),
                                        y=UDshift+BoltShift+vpds_IBSGYc,
                                        x=-vpds_IBSGXc alphax=90
       Create and Position IBSH in Cave z=+vpds_IBSHZc1 x=+vpds_IBSHXc1 alphay=90
                  Position IBSH in Cave z=+vpds_IBSHZc1 x=-vpds_IBSHXc1 alphay=90
                  Position IBSH in Cave z=-(vpds_IBSHZc1+EWshift) x=+vpds_IBSHXc1 alphay=90
                  Position IBSH in Cave z=-(vpds_IBSHZc1+EWshift) x=-vpds_IBSHXc1 alphay=90
                  Position IBSH in Cave z=+vpds_IBSHZc2 x=+vpds_IBSHXc2 alphay=90
                  Position IBSH in Cave z=+vpds_IBSHZc2 x=-vpds_IBSHXc2 alphay=90
                  Position IBSH in Cave z=-(vpds_IBSHZc2+EWshift) x=+vpds_IBSHXc2 alphay=90
                  Position IBSH in Cave z=-(vpds_IBSHZc2+EWshift) x=-vpds_IBSHXc2 alphay=90
        endif     ! end check kIBeamStyle>=2
       else       !
*        print *,' pVPD: pipe-support structure not defined...'
       endif      ! end check kIBeamStyle>=1 (equiv to star-standard IBchoice>0)

*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
*= = end of main routine... now start Blocks...
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

*=======================================================================
Block VPDD  is the whole VPPD assembly
     Material  Air
     Medium    Standard
     Attribute VPDD seen=1 colo=7 serial=zpos
	 if (kDetStyle<2) then
      Shape TUBE rmin=vpdg_rmin rmax=vpdg_rmax dz=vpdg_BPlength/2
     else
      Shape TUBE rmin=vpdh_rmin rmax=vpdh_rmax dz=vpdh_length/2
     endif

*---- start pVPD....
	 if (kDetStyle<2) then
*
* Mount the baseplate under the Ibeam
     ybase= vpdg_IBposYc - (vpdg_IBheight + vpdg_BPthick)/2
     ytop = ybase + vpdg_BPthick/2+ vpdg_FPheight + vpdg_FPChght 
     Create and Position VPBP y=ybase   " the Base Plate "
*
* Hooks between ground plate and front/back plates
     Create VPBO
      xloc=(vpdg_BPwidth-vpdg_FPwidth)/2
      yloc=ybase+(vpdg_BPthick+vpdg_FPHhght)/2
      zloc=(vpdg_BPlength-2*vpdg_FPthick-vpdg_FPHhght)/2
     Position VPBO  x= xloc y=yloc z= zloc ThetaZ=180
     Position VPBO  x= xloc y=yloc z=-zloc
     Position VPBO  x=-xloc y=yloc z= zloc ThetaZ=180
     Position VPBO  x=-xloc y=yloc z=-zloc
*
* Create and place the front and backplate structures
     Create VPFA
      yloc=ytop-vpdg_FPAhght/2
      zloc=(vpdg_BPlength - vpdg_FPthick)/2
     Position VPFA y=yloc z= zloc
     Position VPFA y=yloc z=-zloc
     Create VPFB
      xloc=(vpdg_FPAwidth+(vpdg_BPwidth-vpdg_FPAwidth-2*vpdg_FPwidth)/2)/2
      yloc=(ytop - (vpdg_FPChght+vpdg_FPAhght)/4)
      zloc=(vpdg_BPlength - vpdg_FPthick)/2
     Position VPFB  x= xloc y=yloc z= zloc alphaZ=90
     Position VPFB  x= xloc y=yloc z=-zloc alphaZ=90
     Position VPFB  x=-xloc y=yloc z= zloc alphaZ=90 alphax=180
     Position VPFB  x=-xloc y=yloc z=-zloc alphaZ=90 alphax=180
     Create VPFC
      xloc=(vpdg_BPwidth-vpdg_FPwidth)/2
      yloc=(ytop-(vpdg_FPChght+vpdg_FPwidth/2)/2)
      zloc=(vpdg_BPlength - vpdg_FPthick)/2
     Position VPFC  x= xloc y=yloc z= zloc alphaZ=90
     Position VPFC  x= xloc y=yloc z=-zloc alphaZ=90
     Position VPFC  x=-xloc y=yloc z= zloc alphaZ=90 alphax=180
     Position VPFC  x=-xloc y=yloc z=-zloc alphaZ=90 alphax=180
     Create VPFP
      xloc=(vpdg_BPwidth-vpdg_FPwidth)/2
      yloc=(ybase+vpdg_BPthick/2+ vpdg_FPheight/2)
      zloc=(vpdg_BPlength-vpdg_FPthick)/2
    Position VPFP x= xloc y=yloc z= zloc
    Position VPFP x= xloc y=yloc z=-zloc
    Position VPFP x=-xloc y=yloc z= zloc
    Position VPFP x=-xloc y=yloc z=-zloc
*
* Strut structures between the front and backplates
     strutheight = vpdg_STdiagsz/cos(vpdg_STangle*degrad) _
                   + vpdg_BPlength*tan(vpdg_STangle*degrad)
     Create VPST
      xloc=(vpdg_BPwidth+vpdg_STthick)/2
      yloc=ytop-vpdg_FPwidth-strutheight/2
     Position VPST x= xloc y=yloc
     Position VPST x=-xloc y=yloc
     Create VPSC
      xloc=(vpdg_BPwidth-vpdg_SCwidth)/2
      yloc=ytop-vpdg_FPwidth-strutheight+vpdg_SCheight/2
      zloc=(vpdg_BPlength+vpdg_SClength)/2
     Position VPSC x= xloc y=yloc z= zloc alphax=180.
     Position VPSC x=-xloc y=yloc z= zloc alphax=180. alphaz=180.
      yloc=ytop-vpdg_FPwidth-vpdg_SCheight/2
     Position VPSC x= xloc y=yloc z=-zloc
     Position VPSC x=-xloc y=yloc z=-zloc alphaz=180.
*
* FEE Box volume
*WJL  note for run-4&5, the FEE box is on the side, not
*WJL  the bottom as in run-2&3
     if (VPDV_vpdConfig < 4) then
*       print *,' pVPD: FEE box is below the bottom plate... (runs 2,3)'
       Create and Position VPBX x=-(vpdg_BPwidth-vpdg_BXwidth)/2,
                                y=(ybase-vpdg_BPthick/2-vpdg_BXheight/2)
     else
*       print *,' pVPD: FEE box is on a side strut... (runs 4,5)'
       Create and Position VPBX x=-(vpdg_BPwidth+2.*vpdg_STthick+vpdg_BXheight)/2.,
                                y=-4 ThetaX=150 AlphaZ=90
     endif
*
     endif 
*---- end check on kDetStyle<2 (pVPD hardware)...
*
* a ring to section in phi for placement of detector assys....
*WJL this ring is sectioned for pVPD, but is not for upVPD....
*
     Create and Position VRNG  Konly='Many'

Endblock
*
* ======================================================================
*
Block VPBP is the Base Plate
     Material  Aluminium
     Attribute VPBP seen=1 colo=5 fill=6
     Shape BOX  dx=vpdg_BPwidth/2 dy=vpdg_BPthick/2
Endblock
*
Block VPBO is container for the hook
     Attribute VPBO seen=0
     Shape BOX dx=vpdg_FPHwidth/2  dy=vpdg_FPHhght/2 dz=vpdg_FPHhght/2
     Create and Position VPBA z=-(vpdg_FPHhght-vpdg_FPHthick)/2 
     Create and Position VPBB y=-(vpdg_FPHhght-vpdg_FPHthick)/2,
                              z=vpdg_FPHthick/2 
Endblock
*
Block VPBA is the part of the hook that mounts to the front/back plate
     Material  Aluminium
     Attribute VPBA seen=1 colo=5 fill=6
     Shape BOX dx=vpdg_FPHwidth/2 dy=vpdg_FPHhght/2 dz=vpdg_FPHthick/2
Endblock
*
Block VPBB is the part of the hook that mounts to the base plate
     Material  Aluminium
     Attribute VPBB seen=1 colo=5 fill=6
     Shape BOX dx=vpdg_FPHwidth/2 dy=vpdg_FPHthick/2,
               dz=(vpdg_FPHhght-vpdg_FPHthick)/2
Endblock
*
*----------------------------------------------------------------------
Block VPFP is a single rectangular piece of the frontpanel
     Material  polystyren
     Attribute VPFP seen=1 colo=3 fill=6
     Shape BOX dx=vpdg_FPwidth/2,
               dy=vpdg_FPheight/2,
               dz=vpdg_FPthick/2
Endblock
*
Block VPFA is the central upper part of the frontplate
     Material  polystyren
     Attribute VPFA seen=1 colo=3 fill=6
     Shape BOX dx=vpdg_FPAwidth/2,
		dy=vpdg_FPAhght/2,
		dz=vpdg_FPthick/2
Endblock
*
Block VPFB is the middle upper part of the frontplate
     Material  polystyren
     Attribute VPFB seen=1 colo=3 fill=6
     Shape TRAP dz=vpdg_FPthick/2  thet=0. phi=0.,
		H1=(vpdg_BPwidth -vpdg_FPAwidth-2*vpdg_FPwidth)/4,
		H2=(vpdg_BPwidth -vpdg_FPAwidth-2*vpdg_FPwidth)/4,
		TL1=vpdg_FPAhght/2,
		TL2=vpdg_FPAhght/2,
		BL1=vpdg_FPChght/2 BL2=vpdg_FPChght/2,
                ALP1=atan(vpdg_FPAhght /(vpdg_BPwidth _
                     -vpdg_FPAwidth-2*vpdg_FPwidth)/2)*raddeg,
                ALP2=atan(vpdg_FPAhght /(vpdg_BPwidth _
                     -vpdg_FPAwidth-2*vpdg_FPwidth)/2)*raddeg
Endblock
*
Block VPFC is the outer upper part of the frontplate
     Material  polystyren
     Attribute VPFC seen=1 colo=3 fill=6
     Shape TRAP dz=vpdg_FPthick/2  thet=0. phi=0.,
		H1=vpdg_FPwidth/2 BL1=(vpdg_FPChght-vpdg_FPwidth)/2,
                H2=vpdg_FPwidth/2 BL2=(vpdg_FPChght-vpdg_FPwidth)/2,
		TL1=vpdg_FPChght/2  TL2=vpdg_FPChght/2,
		ALP1=atan(.5)*raddeg ALP2=atan(.5)*raddeg
Endblock
*
*----------------------------------------------------------------------
Block VPST is the strut  volume
     Attribute VPST seen=0
     Shape BOX dx=vpdg_STthick/2 dy= strutheight/2,
               dz=(vpdg_BPlength+2*vpdg_SClength)/2
     Create and Position VPSV
     Create VPSW 
     ydispl=(vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad))+vpdg_SCheight/2)/2
     Position VPSW ORT=yzx  z=-(vpdg_BPlength+vpdg_SClength)/2,
                            y=strutheight/2-ydispl  AlphaZ=180. 
     Position VPSW ORT=yzx  z=(vpdg_BPlength+vpdg_SClength)/2,
                            y=-(strutheight/2-ydispl) AlphaY=180.
Endblock
*
Block VPSV is the actual strut between front and backplates
     Material  Aluminium
     Attribute VPSV seen=1 colo=3 fill=6
     Shape TRAP dz=vpdg_BPlength/2  thet=vpdg_STangle  phi=270.,
                H1=vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad)),
                BL1=vpdg_STthick/2   TL1=vpdg_STthick/2 ALP1=0.,
                H2=vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad)),
                BL2=vpdg_STthick/2   TL2=vpdg_STthick/2 ALP2=0.  
Endblock
*
Block VPSW is a tiny piece of aluminium that belongs to the strut
     Material Aluminium
     Attribute VPSW seen=1 colo=3 fill=6
     Shape TRAP dz=vpdg_STthick/2  thet=0. phi=0.,
                H1=vpdg_SClength/2 H2=vpdg_SClength/2,
                BL1=vpdg_SCheight/2 BL2=vpdg_SCheight/2,
                TL1=vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad)),
                TL2=vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad)),
                ALP1=atan((vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad))_
                     -vpdg_SCheight/2)/vpdg_SClength)*raddeg,
                ALP2=atan((vpdg_STdiagsz/(2*cos(vpdg_STangle*degrad))_
                     -vpdg_SCheight/2)/vpdg_SClength)*raddeg
Endblock
*
Block VPSC is a clamp that holds the strut
     Shape BOX dx=vpdg_SCwidth/2,
               dy=vpdg_SCheight/2 ,
               dz=vpdg_SClength/2
     Create and Position VPSA x=-vpdg_SCthick/2,
                              z=(vpdg_SClength-vpdg_SCthick)/2
     Create and Position VPSB x=(vpdg_SCwidth-vpdg_SCthick)/2
Endblock
*
Block VPSA is part of a strut clamp that holds to the frontplate
     Material Aluminium
     Attribute VPSA seen=1 colo=3 fill=6
     Shape BOX dx=(vpdg_SCwidth-vpdg_SCthick)/2 dy=vpdg_SCheight/2,
               dz=vpdg_SCthick/2
Endblock
*
Block VPSB is part of a strut clamp that holds to the strut
     Material Aluminium
     Attribute VPSB seen=1 colo=3 fill=6
     Shape BOX dx=vpdg_SCthick/2 dy=vpdg_SCheight/2 dz=vpdg_SClength/2
Endblock
*
*
*----------------------------------------------------------------------
Block VPBX is the FEE box
     Material  Aluminium
     Attribute VPBX seen=1 colo=2
     Shape BOX  dx=vpdg_BXwidth/2 dy=vpdg_BXheight/2 dz=vpdg_BXlength/2
     Create and Position VPBI
Endblock
*
Block VPBI is the empty space inside of the FEE box
     Material  Air
     Attribute VPBI seen=0
     Shape BOX  dx=(vpdg_BXwidth -vpdg_BXthick)/2,
                dy=(vpdg_BXheight-vpdg_BXthick)/2,
                dz=(vpdg_BXlength-vpdg_BXthick)/2
*     Create VFEE  dx=vpds_ElecWid/2 dy=vpds_ElecThck/2 dz=vpds_ElecLen/2
     Create and Position VFEE x=vpds_VFEEPosX y=vpds_VFEEPosY,
                   z=vpds_VFEEPosZ
Endblock
*
* Note: the following blocks are copied from the TOFp FEE,
*       both detectors use identical FEE boards.
Block VFEE is the FEE inside the box
      Attribute VFEE seen=1   colo=3
      Component Si   A=28.08  Z=14   W=0.6*1*28./60.
      Component O    A=16     Z=8    W=0.6*2*16./60.
      Component C    A=12     Z=6    W=0.4*8*12./174.
      Component H    A=1      Z=1    W=0.4*14*1./174.
      Component O    A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10  Dens=1.7
*fg      Shape     BOX  dx=0 dy=0 dz=0
      Shape     BOX   dx=vpds_ElecWid/2 dy=vpds_ElecThck/2 dz=vpds_ElecLen/2
      Create    VLEM
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(1) z=vpds_VLEMPosZ(1) 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(2) z=vpds_VLEMPosZ(2) 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(3) z=vpds_VLEMPosZ(3) 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(4) z=vpds_VLEMPosZ(4) 
*      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(5) z=vpds_VLEMPosZ(5) 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(6) z=vpds_VLEMPosZ(6) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(7) z=vpds_VLEMPosZ(7) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(8) z=vpds_VLEMPosZ(8) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(9) z=vpds_VLEMPosZ(9) alphax=180 
*      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(10) z=vpds_VLEMPosZ(10) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(11) z=vpds_VLEMPosZ(11) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(12) z=vpds_VLEMPosZ(12) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(13) z=vpds_VLEMPosZ(13) alphax=180 
      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(14) z=vpds_VLEMPosZ(14) alphax=180 
*      Position  VLEM y=vpds_VLEMPosY x=vpds_VLEMPosX(15) z=vpds_VLEMPosZ(15) alphax=180 
EndBlock
*
Block VLEM is a Lemo connector on the FEE boards
      Attribute VLEM seen=1   colo=3
*fg      Shape     BOX   dx=0 dy=0 dz=0
      Shape     BOX    dx=vpds_VLEMLenX/2.,
                       dy=vpds_VLEMLenY/2.,
                       dz=vpds_VLEMLenZ/2.
      Create and Position    VPIP  x=vpds_VPIPPosX y=0 z=vpds_VPIPPosZ,
                                   Rmin=vpds_VPIPRmin Rmax=vpds_VPIPRmax dz=vpds_VPIPLenZ/2
EndBlock
*
Block VPIP  is the Long Pipe
      Attribute VPIP seen=1  colo=7
      Material  Aluminium
      Shape     TUBE Rmin=0  Rmax=0  Dz=0
EndBlock
*
*======================================================================
*
Block VRNG  is a single pVPD Ring or the entire upVPD
     if (kDetStyle<2) then
       Shape TUBE rmin=vpdg_Rmin,
                  rmax=vpdg_Rmin+2*vpdg_DETrad+vpdg_CLheight ,
                  dz=vpdg_CLlength/2
        do isec=1,vpdg_NumPMT
          detangle=-30+isec*(360/vpdg_NumPMT)
          Create and Position VSEC alphaz=detangle  Konly='Many'
        enddo
    elseif (kDetStyle==2) then 
       Shape TUBE rmin=vpdh_rmin rmax=vpdh_rmax dz=vpdh_length/2  
       Create VDET
       detzint = -(VPDH_detlen/2.0)+VPDH_detfront+(VPDH_leadthick+vpdh_scintthick)
*       print *,'upVPD: detzint = ',detzint
*---- upVPD ring #1
       locang=0.
       if (VPDH_ring1_kproj>0) then
         locang=degrad*0.
         locang=ATAN((VPDH_ring1_rad)/(0.5*VPDH_zposWest+0.5*VPDH_zposEast+detzint))
*         print *,'upVPD: locang1 = ',locang/degrad
       endif
       do isec = 0,VPDH_ring1_ndet-1
        phiang = degrad*(VPDH_ring1_phi0 + isec*VPDH_ring1_dphi)
        xloc   = (VPDH_ring1_rad+0.5*VPDH_detlen*SIN(locang))*COS(phiang)
        yloc   = (VPDH_ring1_rad+0.5*VPDH_detlen*SIN(locang))*SIN(phiang)
        Position VDET x=xloc y=yloc,
                      ThetaX=90.+(COS(phiang)*locang) ThetaY=90.+(SIN(phiang)*locang),
                      Konly='Many'
       end do
*---- upVPD ring #2
       locang=0.
       if (VPDH_ring2_kproj>0) then
         locang=degrad*4.
         locang=ATAN((VPDH_ring2_rad)/(0.5*VPDH_zposWest+0.5*VPDH_zposEast+detzint))
*         print *,'upVPD: locang2 = ',locang/degrad
       endif
       do isec = 0,VPDH_ring2_ndet-1
        phiang = degrad*(VPDH_ring2_phi0 + isec*VPDH_ring2_dphi)
        xloc   = (VPDH_ring2_rad+0.5*VPDH_detlen*SIN(locang))*COS(phiang)
        yloc   = (VPDH_ring2_rad+0.5*VPDH_detlen*SIN(locang))*SIN(phiang)
        Position VDET x=xloc y=yloc,
                      ThetaX=90.+(COS(phiang)*locang) ThetaY=90.+(SIN(phiang)*locang),
                      Konly='Many'
       end do
*---- upVPD ring #3
       locang=0.
*       print *,'VPDH_ring3_kproj =',VPDH_ring3_kproj
       if (VPDH_ring3_kproj>0) then
         locang=degrad*8.
         locang=ATAN((VPDH_ring3_rad)/(0.5*VPDH_zposWest+0.5*VPDH_zposEast+detzint))
*         print *,'upVPD: locang3 = ',locang/degrad
       endif
       do isec = 0,VPDH_ring3_ndet-1
        phiang = degrad*(VPDH_ring3_phi0 + isec*VPDH_ring3_dphi)
        xloc   = (VPDH_ring3_rad+0.5*VPDH_detlen*SIN(locang))*COS(phiang)
        yloc   = (VPDH_ring3_rad+0.5*VPDH_detlen*SIN(locang))*SIN(phiang)
        Position VDET x=xloc y=yloc,
                      ThetaX=90.+(COS(phiang)*locang) ThetaY=90.+(SIN(phiang)*locang),
                      Konly='Many'
       end do
    endif
Endblock
*
Block VSEC  is one pVPD sector with all stuff inside
     Attribute VSEC seen=1 colo=5 serial=isec
     Shape  Tubs phi1=-360/(2*vpdg_NumPMT) phi2=+360/(2*vpdg_NumPMT)
     ydispl=vpdg_DETrad - sqrt(vpdg_DETrad**2 - _
            (vpdg_CLwidth/2-vpdg_CLthick)**2)
     Create and Position VDET  x=vpdg_Rmin+vpdg_DETrad 

*WJL fixed bug (added alphaz=180 for isec=1,2)....
     if (isec==3) then
       Create and Position VPCL  x=vpdg_Rmin+vpdg_DETrad ,
                      y=(vpdg_DETrad+vpdg_CLheight/2-ydispl)
     else
       Create and Position VPCL  x=vpdg_Rmin+vpdg_DETrad ,
                      y=-(vpdg_DETrad+vpdg_CLheight/2-ydispl) alphaz=180
     endif

* these cables are for fun only
     Create VXST
     Position VXST  x=vpdg_Rmin+vpdg_DETrad+0.4  y=+1.5,
                    z=vpdg_DETlen/2+1.0
     Position VXST  x=vpdg_Rmin+vpdg_DETrad-0.4  y=-1.5,
                    z=vpdg_DETlen/2+1.0
EndBlock
*
Block VDET  is a single detector (Radiator+converter and PMT+electroncs)
     if (kDetStyle<2) then
      Material  IRON
      Attribute VDET   seen=1  colo=1 serial=0
      SHAPE     TUBE   Rmin=0  Rmax=vpdg_DETrad  Dz=vpdg_DETlen/2
      Create and Position VDTI z=+vpdg_DETfront
     elseif (kDetStyle==2) then
      Material  Aluminium
      Attribute VDET   seen=1  colo=1 serial=0
      SHAPE     TUBE   Rmin=0  Rmax=vpdh_detrad  Dz=vpdh_detlen/2
      Create and Position VDTI z=0
     endif
endblock
*
Block VDTI is inner part of the single detector
     Material Air
     Attribute VDTI seen=1 colo=1
     if (kDetStyle<2) then
       Shape TUBE rmin=0 rmax=vpdg_PMTrad,
                  dz=vpdg_DETlen/2-vpdg_DETfront  
       convlength=vpdg_ConvThk+vpdg_RadiThk
       Create and position VCNV z=-(vpdg_DETlen-convlength)/2
       Create and position VPMT z=-(vpdg_DETlen/2-convlength-vpdg_PMTlen/2)
     elseif (kDetStyle==2) then
       Shape TUBE rmin=0 rmax=vpdh_detrad-vpdh_detwall ,
                  dz=(vpdh_detlen/2.)-(2.0*vpdh_detfront)
	   Create and Position VCNV x=0 ,
                  z=-(vpdh_detlen/2.0)+vpdh_detfront+(vpdh_leadthick+vpdh_scintthick)
     endif
Endblock
*
Block VCNV  is converter layer (radiator included)
     Material  Lead 
     Attribute VCNV   seen=1   colo=2
     if (kDetStyle<2) then
       SHAPE TUBE dz=vpdg_ConvThk/2+vpdg_RadiThk/2
       Create and Position VRAD z=vpdg_ConvThk/2
     elseif (kDetStyle==2) then
       SHAPE TUBE rmin=0 rmax=vpdh_pmtrad ,
                  dz=(vpdh_leadthick+vpdh_scintthick)/2.
       Create and Position VRAD z=vpdh_leadthick/2.
     end if
EndBlock
*
Block VRAD is light-producing layer (scintillator or quartz)
*---- radiator is Quartz.....
*     Component Si    A=28.09   Z=14  W=1
*     Component O2    A=16      Z=8   W=2
*     Mixture   SiO2  Dens=2.65
*---- radiator is Scintillator (pVPD)....
     Material  Polystyren
*
     Medium    sensitive   IsVol=1
     Attribute VRAD seen=1 colo=3
     if (kDetStyle<2) then
      Shape     TUBE   dz=vpdg_RadiThk/2
      HITS      VRAD   x:.1:S     y:.1:   z:.1:   cx:10:   cy:10:   cz:10:,
                       Step:.01:          Slen:.1:(0,600)   Ptot:18:(0,100),
                       Tof:18:(0,1.e-6)   Eloss:16:(0,.01)
     elseif (kDetStyle==2) then
      Shape     TUBE   dz=vpdh_scintthick/2.
      HITS      VRAD   x:.1:S     y:.1:   z:.1:   cx:10:   cy:10:   cz:10:,
                       Step:.01:          Slen:.1:(0,600)   Ptot:18:(0,100),
                       Tof:18:(0,1.e-6)   Eloss:16:(0,.01)
     endif
EndBlock
*
Block VPMT is the PMT inner volume
     Material  Vacuum
     Attribute VPMT   seen=1   colo=7
     Shape     TUBE   Rmax=vpdg_PMTrad-vpdg_PMTwall,
                      Dz=vpdg_PMTlen/2-vpdg_PMTwall 
EndBlock
*
Block VXST are PMT output cables (just to look nicer)
     attribute VXST   seen=1   colo=7   serial=0
     shape     TUBE   rmin=0   rmax=0.3   dz=1.0
Endblock
*
Block VPCL is the boat clamp
     Attribute VPCL seen=1 colo=6 serial=0     
     Shape BOX dx=vpdg_CLwidth/2 dy=vpdg_CLheight/2 dz=vpdg_CLlength/2
     Create VPCF
     Position VPCF z=+(vpdg_CLlength-vpdg_CLthick)/2
     Position VPCF z=-(vpdg_CLlength-vpdg_CLthick)/2
     Create and Position VPCH
     Create VPCV
     Position VPCV x=(vpdg_CLwidth-vpdg_CLthick)/2,
                   y=-vpdg_CLthick/2 - (vpdg_CLheight/2-vpdg_CLthick/2)/2
     Position VPCV x=-(vpdg_CLwidth-vpdg_CLthick)/2,
                   y=-vpdg_CLthick/2 - (vpdg_CLheight/2-vpdg_CLthick/2)/2
Endblock
*
Block VPCF is the front plate of the boat clamp
    Material Aluminium
    Attribute VPCF seen=1 colo=6 fill=6
     Shape BOX dx=vpdg_CLwidth/2 dy=vpdg_CLheight/2 dz=vpdg_CLthick/2
Endblock
*
Block VPCH is the horizontal plate of the boat clamp
    Material Aluminium
    Attribute VPCH seen=1 colo=6 fill=6
     Shape BOX dx=vpdg_CLwidth/2 dy=vpdg_CLthick/2,
               dz=(vpdg_CLlength-2*vpdg_CLthick)/2
Endblock
*
Block VPCV is the vertical plate of the boat clamp
    Material Aluminium
    Attribute VPCV seen=1 colo=6 fill=6
     Shape BOX dx=vpdg_CLthick/2,
               dy=(vpdg_CLheight/2-vpdg_CLthick/2)/2,
	       dz=(vpdg_CLlength-2*vpdg_CLthick)/2
Endblock
*

*
*=====================================================================-
*
Block IBEM is the IBeam structure beneath the Bell reducer cone
     Material  Air
     Attribute IBEM seen=0  colo=2
     Shape     BOX  dx=vpdg_IBwidth/2 dy=vpdg_IBheight/2 _
                                     dz=vpdg_IBleng/2     
     Create and Position IBEH  dy=vpdg_IBthickH/2,
                               y=+(vpdg_IBheight-vpdg_IBthickH)/2
     Create and Position IBEH  dy=vpdg_IBthickH/2,
                               dz=(vpdg_IBleng-vpdg_IBwlen)/2,
                               z=+vpdg_IBwlen/2,
                               y=-(vpdg_IBheight-vpdg_IBthickH)/2
     Create and Position IBEV z=+vpdg_IBwlen/2
     Create and Position IBEW z=-(vpdg_IBleng-vpdg_IBwlen)/2,
          y= (vpdg_IBheight/2-vpdg_IBthickH-(vpdg_IBwhghtF+vpdg_IBwhghtB)/4),
          ORT=yzx AlphaZ=180
EndBlock
Block IBEH is a horizontal IBeam plate
     Material Aluminium
     Attribute IBEH seen=1 colo=2 fill=7
     Shape BOX dx=0 dy=0 dz=0
EndBlock
Block IBEV is a vertical IBeam plate
     Material  Aluminium
     Attribute IBEV seen=1 colo=2 fill=7
     Shape BOX dx=vpdg_IBthickV/2 dy=(vpdg_IBheight-2*vpdg_IBthickH)/2,
               dz=(vpdg_IBleng-vpdg_IBwlen)/2
EndBlock
Block IBEW is the first part of the IBeam plate
     Material Aluminium
     Attribute IBEW seen=1 colo=2 fill=7
     Shape TRAP dz=vpdg_IBthickV/2 thet=0. phi=0.,
                H1=vpdg_IBwlen/2 H2=vpdg_IBwlen/2,
                BL1=vpdg_IBwhghtF/2 BL2=vpdg_IBwhghtF/2,
                TL1=vpdg_IBwhghtB/2 TL2=vpdg_IBwhghtB/2,
                ALP1=atan((vpdg_IBwhghtB-vpdg_IBwhghtF)/vpdg_IBwlen/2)*raddeg,
                ALP2=atan((vpdg_IBwhghtB-vpdg_IBwhghtF)/vpdg_IBwlen/2)*raddeg
EndBlock
*
Block IBSA is the vertical post on the balcony (Envelope)
     Material  Air
     Attribute IBSA seen=0 colo=5
     Shape     BOX  dx=vpds_BSALenX/2. dy=vpds_BSALenY/2. dz=vpds_BSALenZ/2.
     Create and Position IBAA x=0 z=-(vpds_BSALenX-vpds_BAALenZ)/2.
                Position IBAA x=-(vpds_BSALenX-vpds_BAALenZ)/2. z=0 AlphaY=90
EndBlock
Block IBAA is the vertical post on the balcony (Aluminum)
     Material  Aluminium
     Attribute IBAA seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BSALenX/2. dy=vpds_BSALenY/2. dz=vpds_BAALenZ/2.
EndBlock
*
Block IBSB is the diagonal post from the balcony (Envelope)
     Material  Air
     Attribute IBSB seen=0 colo=5
     Shape     BOX  dx=vpds_BSALenX/2. dy=vpds_BSBLenY/2. dz=vpds_BSALenZ/2.
     Create and Position IBAB x=0 z=(vpds_BSALenX-vpds_BAALenZ)/2.
                Position IBAB x=-(vpds_BSALenX-vpds_BAALenZ)/2. z=0 AlphaY=90
EndBlock
Block IBAB is the diagonal post from the balcony (Aluminum)
     Material  Aluminium
     Attribute IBAB seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BSALenX/2. dy=vpds_BSBLenY/2. dz=vpds_BAALenZ/2.
EndBlock
*
Block IBSC is the cross post below the I-Beam (Envelope)
     Material  Air
     Attribute IBSC seen=0 colo=7
     Shape     BOX  dx=vpds_BSCLenX/2. dy=vpds_BSCLenY/2. dz=vpds_BSCLenZ/2.
     Create and Position IBAC x=0 y=0 z=+(vpds_BSCLenZ-vpds_BACLenZ)/2.
                Position IBAC x=0 y=0 z=-(vpds_BSCLenZ-vpds_BACLenZ)/2.
	 Create and Position IBBC x=0 y=(vpds_BSCLenY-vpds_BACLenZ)/2. z=0
     Create and Position IBCC x=+(vpds_BSCLenX-vpds_BACLenZ)/2. y=0 z=0
     Create and Position IBCC x=-(vpds_BSCLenX-vpds_BACLenZ)/2. y=0 z=0
EndBlock
Block IBAC is vertical parts of the cross post below the I-Beam (Aluminum)
     Material  Aluminium
     Attribute IBAC seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BSCLenX/2.-vpds_BACLenZ dy=vpds_BSCLenY/2 dz=vpds_BACLenZ/2.
EndBlock
Block IBBC is the horizontal part of the cross post below the I-Beam (Aluminum)
     Material  Aluminium
     Attribute IBBC seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BSCLenX/2.-vpds_BACLenZ dy=vpds_BACLenZ/2.,
                    dz=vpds_BSCLenZ/2.
EndBlock
Block IBCC is the end caps on the cross post below the I-Beam (Aluminum)
     Material  Aluminium
     Attribute IBCC seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BACLenZ/2. dy=vpds_BSCLenY/2. dz=vpds_BSCLenZ/2.
EndBlock
*
Block IBSD are the horizontal plates that hold the pipe-support brackets (Aluminum)
     Material Aluminium
     Attribute IBSD seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BSDLenX/2. dy=vpds_BACLenZ/2 dz=vpds_BSALenX/2.
EndBlock
Block IBSE are the vertical parts of the pipe-support brackets (Aluminum)
     Material  Aluminium
     Attribute IBSE seen=1 colo=1 fill=0
     Shape     BOX  dx=vpds_BAALenZ/2. dy=vpds_BSELenY/2 dz=vpds_BSELenZ/2.
EndBlock
*
Block IBSF are the long threaded rods for X-support of the I-beam
     Material  Iron
     Attribute IBSF seen=1 colo=4 
     Shape     TUBE rmin=0 rmax=vpds_BSFRmax dz=vpds_BSFLenZ/2.
EndBlock
Block IBSS are the long threaded rods for X-support of the I-beam, short stubs inside IBEM!
     Material  Iron
     Attribute IBSS seen=0 colo=4
     Shape     TUBE rmin=0 rmax=vpds_BSFRmax dz=vpds_BSSLenZ/2.
EndBlock
Block IBSG are the vertical bolts to the pipe-support brackets
     Material  Iron
     Attribute IBSG seen=1 colo=4 
     if (vpdv_vpdConfig<8) then
      Shape     TUBE rmin=0 rmax=vpds_BSGRmax dz=vpds_BSGLenZ1/2.
     elseif (vpdv_vpdConfig==8) then
      Shape     TUBE rmin=0 rmax=vpds_BSGRmax dz=vpds_BSGLenZ2/2.
     else 
      Shape     TUBE rmin=0 rmax=vpds_BSGRmax dz=vpds_BSGLenZ3/2.
     endif
EndBlock
Block IBSH are the cross-bolts from the pipe-support brackets to the pipe
     Material  Iron
     Attribute IBSH seen=1 colo=4 
     Shape     TUBE rmin=0 rmax=vpds_BSGRmax dz=vpds_BSHLenZ/2.
EndBlock

*=====================================================================-
*
     END
