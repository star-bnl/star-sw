* $Id: vpddgeo.g,v 1.4 2000/08/21 23:57:25 geurts Exp $
* $Log: vpddgeo.g,v $
* Revision 1.4  2000/08/21 23:57:25  geurts
* updated dimensions of the I-beam
*
* Revision 1.3  2000/08/08 00:11:25  nevski
* more realistic VPD project
*
*
*************************************************************************
module  VPDDGEO  is the Pseudo Vertex Position Detector of STAR
Author  Frank Geurts
Created 21 June 2000
*
* Notes:
*   - cf. SN0416 for more details on pVPD simulations.
*   - Use IBchoice=0  to switch off the definition of the IBeam.
*   - the IBeam is part of the VPDD volume but its z dimensions exceed
*     it.
*   - the exact frontplate x dimension is still to be decided and
*     not part of the VPDG structure.
*   - the struts dimensions are more or less fixed waiting for their
*     final dimensions. They are not part of the VPDG structure yet.
*   - The dimensions and location of the FEE-Box may change.
*
*
************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
     CONTENT   VPDD,VPBP, VPBX, VPFP, VPFT, VPBI, VPST,
               VRNG, VSEC, VDET, VXST, VCNV, VPMT, VRAD, VPCL, VDTI,
               IBEM,IBEH,IBEV
*
     Structure VPDG { version,  zpos,     rmin,    rmax,
                      BPwidth,  BPlength, BPthick,
                      BXheight, BXlength, BXwidth,  BXzposC,
                      FPwidth,  FPheight, FPthick,  FPrmin,  FPrmax,
                      STthick,  STheight, CLheight, CLwidth, CLlength,
                      DETlen,   DETrad,   DETfront,
                      ConvThk,  RadiThk,  EleLeng,  DrLayer,
                      NumPMT,   PMTwall,  PMTrad,   PMTlen,
                      IBchoice, IBPosYc,  IBPosZc,  IBLeng,  IBthickH,
                      IBthickV, IBheight, IBwidth}
*
     real    ybase, angle, convlength, fpxsize
*
* ----------------------------------------------------------------------
*
     FILL VPDG  ! pVPD basic dimensions
        version   =    2.     ! geometry version
        zpos      =  500.     ! Z position of pVPD-volume along beam axis
        rmin      =   10.16   ! mothervolume rmin		(4inch)
        rmax      =   38.1    ! mothervolume rmin		(15inch)
        BPwidth   =   35.56   ! baseplate height		(14inch)
        BPlength  =   45.72   ! baseplate length		(18inch)
        BPthick   =    0.635  ! baseplate thickness		(0.25inch)
        BXheight  =    7.62   ! FEE box height			(3inch)
        BXlength  =   25.4    ! FEE box length			(10inch)
        BXwidth   =   10.16   ! FEE box width (????)		(4inch)
        BXzposC   =    0.     ! FEE central z pos (???)
        FPwidth   =   35.56   ! frontplate width		(14inch)
        FPheight  =   21.59   ! frontplate height		(8.5inch)
        FPthick   =    2.54   ! frontplate thickness		(1inch)
        FPrmin    =   10.16   ! frontplate min. radius		(4inch)
        FPrmax    =   17.78   ! frontplate max. radius		(7inch)
        STthick   =    0.635  ! strut thickness			(0.25inch)
        STheight  =   20.518  ! strut height			(7plus inch)
        CLheight  =    0.3175 ! clamp height			(1/8inch)
        CLwidth   =    3.81   ! clamp width			(1/5inch)
        CLlength  =   40.64   ! clamp length			(16inch)
        DETlen    =   33.02   ! PMT assembly length		(13inch)
        DETrad    =    3.683  ! PMT assembly radius		(1.45inch)
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
     Endfill
*
     USE  VPDG  
*
     Create VPDD
     Position VPDD in Cave   z=+vpdg_zpos
     Position VPDD in Cave   z=-vpdg_zpos   ThetaZ=180

     if (vpdg_IBchoice != 0) then
       Position IBEM in Cave z=+vpdg_IBPosZc y=vpdg_IBposYc
       Position IBEM in Cave z=-vpdg_IBPosZc y=vpdg_IBposYc ThetaZ=180
     endif
*
*=======================================================================
Block VPDD  is the whole VPPD assembly
     Material  Air
     Medium    Standard
     Attribute VPDD seen=0 colo=5
     Shape TUBE rmin=vpdg_rmin rmax=vpdg_rmax dz=vpdg_BPlength/2
*
* Mount the baseplate under the Ibeam
     ybase= vpdg_IBposYc - vpdg_IBheight/2 - vpdg_BPthick/2
     Create and Position VPBP y=ybase   " the Base Plate "
*
* Front and backplate volumes
     fpxsize=vpdg_FPrmax-vpdg_FPrmin
     Create VPFT 
     Position VPFT z=+(vpdg_BPlength - vpdg_FPthick)/2
     Position VPFT z=-(vpdg_BPlength - vpdg_FPthick)/2
     Create VPFP
     Position VPFP x= (vpdg_BPwidth-fpxsize)/2 ,
                   y= (ybase + vpdg_BPthick/2+ vpdg_FPheight/2) ,
                   z= (vpdg_BPlength - vpdg_FPthick)/2
     Position VPFP x=-(vpdg_BPwidth-fpxsize)/2 ,
                   y= (ybase + vpdg_BPthick/2+ vpdg_FPheight/2) ,
                   z= (vpdg_BPlength - vpdg_FPthick)/2
     Position VPFP x= (vpdg_BPwidth-fpxsize)/2 ,
                   y= (ybase + vpdg_BPthick/2+ vpdg_FPheight/2) ,
                   z=-(vpdg_BPlength-vpdg_FPthick)/2
     Position VPFP x=-(vpdg_BPwidth-fpxsize)/2 ,
                   y= (ybase + vpdg_BPthick/2+ vpdg_FPheight/2) ,
                   z=-(vpdg_BPlength-vpdg_FPthick)/2
*
* Struts between the front and backplates
     Create VPST
     Position VPST x=+(vpdg_BPwidth+vpdg_STthick)/2 ,
                   y= (ybase + vpdg_BPthick/2 + vpdg_STheight/2)
     Position VPST x=-(vpdg_BPwidth+vpdg_STthick)/2 ,
                   y= (ybase + vpdg_BPthick/2 + vpdg_STheight/2)
*
* FEE Box volume
     Create and Position VPBX x=(vpdg_BPwidth - vpdg_BXwidth)/2,
                              y=(ybase - vpdg_BPthick/2 - vpdg_BXheight/2)
*
* Ring of detectors
     Create and Position VRNG  Konly='Many'
*
     if (vpdg_IBchoice != 0) then
       print *,'vpddgeo: I-Beam support is activated'
       Create and Position IBEM z=-vpdg_zpos+vpdg_IBPosZc   y=vpdg_IBposYc
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
*----------------------------------------------------------------------
Block VPFP is a single rectangular piece of the frontpanel
*
* Note: put dx in vpdg_structure (values will change)
*
     Material  polystyren
     Attribute VPFP seen=1 colo=3 fill=6
     Shape BOX dx=fpxsize/2,
               dy=vpdg_FPheight/2,
               dz=vpdg_FPthick/2
Endblock
*
Block VPFT is the upper part of the frontplate
     Material  polystyren
     Attribute VPFT seen=1 colo=3 fill=6
     Shape TUBS rmin=vpdg_FPrmin,
                rmax=vpdg_FPrmax,
                phi1=0 phi2=180,
                dz=vpdg_FPthick/2
Endblock
*
*----------------------------------------------------------------------
Block VPST is the strut between front and backplates
     Material  polystyren
     Attribute VPST seen=1 colo=3 fill=6
*
* Note: these values will change ... 
*       (and are NOT part of the vpdg_structure)
*
     angle=atan(7*2.54/vpdg_BPlength)
     Shape TRAP dz=vpdg_BPlength/2  thet=angle/degrad  phi=270.,
                H1=2.54/cos(degrad*angle) BL1=2.54/8 TL1=2.54/8 ALP1=0.,
                H2=2.54/cos(degrad*angle) BL2=2.54/8 TL2=2.54/8 ALP2=0.  
Endblock
*
*----------------------------------------------------------------------
Block VPBX is the FEE box
     Material  polystyren
     Attribute VPBX seen=1 colo=2
     Shape BOX  dx=vpdg_BXwidth/2 dy=vpdg_BXheight/2 dz=vpdg_BXlength/2
     Create and Position VPBI
Endblock
*
Block VPBI is the empty space inside of the FEE box
     Material  Air
     Attribute VPBI seen=0
     Shape BOX  dx=(vpdg_BXwidth-0.635)/2 dy=(vpdg_BXheight-0.635)/2 ,
                dz=(vpdg_BXlength-0.635)/2
Endblock
*
*======================================================================
*
Block  VRNG  is a single VPD Ring
     Shape TUBE rmin=vpdg_Rmin,
                rmax=vpdg_Rmin+2*vpdg_DETrad+vpdg_CLheight ,
                dz=vpdg_CLlength/2
     Create VSEC
Endblock
*
Block VSEC  is one VPD sector with all stuff inside
     Shape  DIVISION iaxis=2  ndiv=vpdg_NumPMT C0=30
     Create and Position VDET  x=vpdg_Rmin+vpdg_DETrad 
     Create and Position VPCL  x=vpdg_Rmin+2*vpdg_DETrad+vpdg_CLheight/2
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
     Attribute VDET   seen=1  colo=7
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
     Component Si    A=28.09   Z=14  W=1
     Component O2    A=16      Z=8   W=2
     Mixture   SiO2  Dens=2.65
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
     attribute VXST   seen=1   colo=7
     shape     TUBE   rmin=0   rmax=0.3   dz=1.0
Endblock
*
Block VPCL is the boat clamp
* extend the clamp definition to more detail
     Material Aluminium
     Attribute VCL seen=1 colo=6 fill=6
     Shape BOX dx=vpdg_CLheight/2 dy=vpdg_CLwidth/2 dz=vpdg_CLlength/2

Endblock
*
*=====================================================================-
*
Block IBEM is the IBeam structure beneath the Bell reducer cone
     Material  Air
     Attribute IBEM      seen=0  colo=2
     
     Shape    BOX  dx=vpdg_IBwidth/2 dy=vpdg_IBheight/2 _
                                     dz=vpdg_IBleng/2
     
     Create IBEH
     Position IBEH  y=+(vpdg_IBheight-vpdg_IBthickH)/2
     Position IBEH  y=-(vpdg_IBheight-vpdg_IBthickH)/2
     Create and Position IBEV
EndBlock
*
Block IBEH is a horizontal IBeam plate
     Material Aluminium
     Attribute IBEH seen=1 colo=2 fill=6
     Shape BOX dy=vpdg_IBthickH/2
EndBlock
*
Block IBEV is a vertical IBeam plate
     Material Aluminium
     Attribute IBEV seen=1 colo=2 fill=6
     Shape BOX dx=vpdg_IBthickV/2 dy=(vpdg_IBheight-2*vpdg_IBthickH)/2
EndBlock
*
*=====================================================================-
*
     END