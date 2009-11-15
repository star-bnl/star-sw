* $Id: vpddgeo1.g,v 1.3 2008/11/30 01:30:59 perev Exp $
* $Log: vpddgeo1.g,v $
* Revision 1.3  2008/11/30 01:30:59  perev
* modifs for extending alpha,theta,phi,ort commandas
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
module  VPDDGEO1  is the Pseudo Vertex Position Detector of STAR
Author  Frank Geurts
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
               IBEM,IBEH,IBEV,IBEW
*

     Structure VPDV { version,  int vpdConfig}
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
                      IBwlen,   IBwhghtF, IBwhghtB}
*
     real ybase, ytop, convlength, detangle, strutheight, ydispl
     real ElecThck, ElecWid, ElecLen, xloc, yloc,zloc, zpos
     integer isec
*
* ----------------------------------------------------------------------
*
     Fill VPDV  ! VPD configuration control
        version   =    1      ! version
        vpdConfig =    1      ! vpd configuration
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
        PMTrad    =    2.54   ! PMT and detector radius		(1inch)
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
        PMTrad    =    2.54   ! PMT and detector radius		(1inch)
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
        PMTrad    =    2.54   ! PMT and detector radius		(1inch)
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
     Endfill
*
     FILL VPDG  ! pVPD basic dimensions
        version   =    4      ! geometry version
        zposEast  =  574.5688 ! Z position East
        zposWest  =  573.5669 ! Z position West
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
        PMTrad    =    2.54   ! PMT and detector radius		(1inch)
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
     Endfill

*
** decide on the version
     USE  VPDV
     USE  VPDG version=VPDV_vpdConfig;  
**
     Create VPDD

     print *,'pVPD: Zpositions West and East:',vpdg_zposWest,' &',vpdg_zposEast,' cm'	

     zpos = vpdg_zposWest
     Position VPDD in Cave   z=+zpos            Konly='Many'
     zpos = vpdg_zposEast
     Position VPDD in Cave   z=-zpos ThetaZ=180 Konly='Many'

     if (vpdg_IBchoice != 0) then
       Position IBEM in Cave z=+vpdg_IBPosZc y=vpdg_IBposYc
       Position IBEM in Cave z=-vpdg_IBPosZc y=vpdg_IBposYc ThetaZ=180
     endif
*
*=======================================================================
Block VPDD  is the whole VPPD assembly
     Material  Air
     Medium    Standard
     Attribute VPDD seen=0 serial=zpos
     Shape TUBE rmin=vpdg_rmin rmax=vpdg_rmax dz=vpdg_BPlength/2
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
     Position VPFB  x= xloc y=yloc z= zloc ORT=Y-XZ 
     Position VPFB  x= xloc y=yloc z=-zloc ORT=Y-XZ
     Position VPFB  x=-xloc y=yloc z= zloc ORT=YXZ
     Position VPFB  x=-xloc y=yloc z=-zloc ORT=YXZ
     Create VPFC
      xloc=(vpdg_BPwidth-vpdg_FPwidth)/2
      yloc=(ytop-(vpdg_FPChght+vpdg_FPwidth/2)/2)
      zloc=(vpdg_BPlength - vpdg_FPthick)/2
     Position VPFC  x= xloc y=yloc z= zloc ORT=Y-XZ
     Position VPFC  x= xloc y=yloc z=-zloc ORT=Y-XZ
     Position VPFC  x=-xloc y=yloc z= zloc ORT=YXZ
     Position VPFC  x=-xloc y=yloc z=-zloc ORT=YXZ
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
     Create and Position VPBX x=-(vpdg_BPwidth-vpdg_BXwidth)/2,
                              y=(ybase-vpdg_BPthick/2-vpdg_BXheight/2)
*
* Ring of detectors
     Create and Position VRNG  Konly='Many'
*
     if (vpdg_IBchoice != 0) then
       print *,'pVPD: I-Beam support is activated...'
       Create and Position IBEM z=-zpos+vpdg_IBPosZc y=vpdg_IBposYc
     endif
*
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
     ElecWid=20.3
     ElecThck=0.17
     ElecLen=5.1
*     Create VFEE  dx=ElecWid/2 dy=ElecThck/2 dz=ElecLen/2
     Create and Position VFEE x=-(vpdg_BXwidth-vpdg_BXthick-ElecWid)/2+2.54,
                   y=(vpdg_BXheight-vpdg_BXthick)/2-1.77,
                   z=-(vpdg_BXlength-vpdg_BXthick-ElecLen)/2
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
      Shape     BOX   dx=ElecWid/2 dy=ElecThck/2 dz=ElecLen/2
      Create    VLEM
       ElecThck= 0.17 
      Position  VLEM y=ElecThck+(0.7/2) x=-7.0 z=2 
      Position  VLEM y=ElecThck+(0.7/2) x=-3.5 z=2 
      Position  VLEM y=ElecThck+(0.7/2) x=0.   z=2 
      Position  VLEM y=ElecThck+(0.7/2) x=3.5  z=2 
*      Position  VLEM y=ElecThck+(0.7/2) x=7    z=2 
      Position  VLEM y=ElecThck+(0.7/2) x=-7.0 z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=-3.5 z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=0.   z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=3.5  z=-2 alphax=180 
*      Position  VLEM y=ElecThck+(0.7/2) x=7    z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=-6.0 z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=-2.5 z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=1.   z=-2 alphax=180 
      Position  VLEM y=ElecThck+(0.7/2) x=4.5  z=-2 alphax=180 
*      Position  VLEM y=ElecThck+(0.7/2) x=8.   z=-2 alphax=180 
EndBlock
*
Block VLEM is a Lemo connector on the FEE boards
      Attribute VLEM seen=1   colo=3
*fg      Shape     BOX   dx=0 dy=0 dz=0
      Shape     BOX    dx=(0.68/2 + (0.9-0.72)/2),
                       dy=(0.68/2),
                       dz=(2.0/2 + (0.8+1.0)/2)
      Create and Position    VPIP  x=(0.9-0.72)/2 y=0 z=(0.8+1.0)/2,
                                   Rmin=0.62/2 Rmax=0.68/2 dz=2.0/2
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
Block  VRNG  is a single VPD Ring
     Shape TUBE rmin=vpdg_Rmin,
                rmax=vpdg_Rmin+2*vpdg_DETrad+vpdg_CLheight ,
                dz=vpdg_CLlength/2
     do isec=1,vpdg_NumPMT
       detangle=-30+isec*(360/vpdg_NumPMT)
       Create and Position VSEC alphaz=detangle  Konly='Many'
     enddo
Endblock
*
Block VSEC  is one VPD sector with all stuff inside
     Attribute VSEC seen=1 colo=5 serial=isec
     Shape  Tubs phi1=-360/(2*vpdg_NumPMT) phi2=+360/(2*vpdg_NumPMT)
     ydispl=vpdg_DETrad - sqrt(vpdg_DETrad**2 - _
            (vpdg_CLwidth/2-vpdg_CLthick)**2)
     Create and Position VDET  x=vpdg_Rmin+vpdg_DETrad 

     if (isec==3) then
     Create and Position VPCL  x=vpdg_Rmin+vpdg_DETrad ,
                    y=(vpdg_DETrad+vpdg_CLheight/2-ydispl)
     else
     Create and Position VPCL  x=vpdg_Rmin+vpdg_DETrad ,
                    y=-(vpdg_DETrad+vpdg_CLheight/2-ydispl)
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
     Material  IRON
     Attribute VDET   seen=1  colo=7 serial=0
     SHAPE     TUBE   Rmin=0  Rmax=vpdg_DETrad  Dz=vpdg_DETlen/2
     Create and Position VDTI z=+vpdg_DETfront
endblock
*
Block VDTI is inner part of the single detector
     Material Air
     Attribute VDTI seen=0
     Shape TUBE rmin=0 rmax=vpdg_PMTrad,
                dz=vpdg_DETlen/2-vpdg_DETfront
     
     convlength=vpdg_ConvThk+vpdg_RadiThk
     Create and position VCNV z=-(vpdg_DETlen-convlength)/2
     Create and position VPMT z=-(vpdg_DETlen/2-convlength-vpdg_PMTlen/2)
Endblock
*
Block VCNV  is converter layer (radiator included)
     Material  Lead 
     Attribute VCNV   seen=1   colo=2
     SHAPE     TUBE   dz=vpdg_ConvThk/2+vpdg_RadiThk/2
     create and position VRAD  z=vpdg_ConvThk/2
EndBlock
*
Block VRAD  is Cerenkov Radiator layer
*
*---- radiator is Quartz.....
*     Component Si    A=28.09   Z=14  W=1
*     Component O2    A=16      Z=8   W=2
*     Mixture   SiO2  Dens=2.65
*
*---- radiator is Scintillator (pVPD)....
     Material  Polystyren
*
     Medium    sensitive   IsVol=1
     Attribute VRAD   seen=1   colo=3
     Shape     TUBE   dz=vpdg_RadiThk/2
     HITS      VRAD   x:.1:S     y:.1:   z:.1:   cx:10:   cy:10:   cz:10:,
                      Step:.01:          Slen:.1:(0,600)   Ptot:18:(0,100),
                      Tof:18:(0,1.e-6)   Eloss:16:(0,.01)
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
     Attribute IBEM      seen=0  colo=2
     
     Shape    BOX  dx=vpdg_IBwidth/2 dy=vpdg_IBheight/2 _
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
*
Block IBEH is a horizontal IBeam plate
     Material Aluminium
     Attribute IBEH seen=1 colo=2 fill=6
     Shape BOX dx=0 dy=0 dz=0
EndBlock
*
Block IBEV is a vertical IBeam plate
     Material Aluminium
     Attribute IBEV seen=1 colo=2 fill=6
     Shape BOX dx=vpdg_IBthickV/2 dy=(vpdg_IBheight-2*vpdg_IBthickH)/2,
               dz=(vpdg_IBleng-vpdg_IBwlen)/2
EndBlock
*
Block IBEW is the first part of the IBeam plate
     Material Aluminium
     Attribute IBEW seen=1 colo=2 fill=6
     Shape TRAP dz=vpdg_IBthickV/2 thet=0. phi=0.,
                H1=vpdg_IBwlen/2 H2=vpdg_IBwlen/2,
                BL1=vpdg_IBwhghtF/2 BL2=vpdg_IBwhghtF/2,
                TL1=vpdg_IBwhghtB/2 TL2=vpdg_IBwhghtB/2,
                ALP1=atan((vpdg_IBwhghtB-vpdg_IBwhghtF)/vpdg_IBwlen/2)*raddeg,
                ALP2=atan((vpdg_IBwhghtB-vpdg_IBwhghtF)/vpdg_IBwlen/2)*raddeg
EndBlock
*
*=====================================================================-
*
     END
