* $Id: fgtdgeo2.g,v 1.1 2008/09/25 03:05:44 perev Exp $
* $Log: fgtdgeo2.g,v $
* Revision 1.1  2008/09/25 03:05:44  perev
* upgr16 (Jan)
*
* Revision 1.4  2007/11/13 21:29:45  perev
* material ALKAP fixed
*
* Revision 1.3  2007/02/23 21:16:37  potekhin
* Replacing a dummy version (a placeholder) of the FGT (formerly known
* as IGT) code with a piece that supposedly works, and has 6 GEM disks.
* All the structures and volumes were renames to comply with the new
* naming convention (i.e. FGT, not IGT anymore).
*
* Revision 1.2  2007/02/13 20:41:03  potekhin
* Remove the hit declaration just to make sure
* there is no confusion downstream -- this version
* if for material balance puprpose only.
*
* Revision 1.1  2007/02/08 00:01:47  potekhin
* First cut of the "new" FGT which is identical to the previously
* used IGT. Will soon be replaced by a newer version.
*
*
* FGTDGEO - Forward GEM Tracking Detector Geometry
*
* Describes a few disks of triple GEM detectors for a possible STAR upgrade
* Distinct from, and alternative to, the previosuly proposed
* LAGRGE GEM surfaces; formerly used to reside in the IGT branch.
*
* Inner and outer radii as well as position along the Z axis are variable
* parameters as well as the geometry, foil thicknesses, and spacing of the 
* triple GEM detectors.
*
* First design 2006/05/01 - D.K. Hasell
*
******************************************************************************
Module FGTDGEO2 is the geometry of the forward GEM tracking detector, version UPGR16
  Created  6/4/2008
  Author   Jan Balewski

* The single triple GEM assembly in GEANT as follows, in this order:
* Front support disk:
  * 0.25mm FR4, rho=1.91 g/cm3, composition as  G10
  * 5.00mm Nomex,0.048 g/cm3
  * 0.25mm FR4, rho=1.91 g/cm3
* Equivalent to 3GEMs+HV layer + readout layer:
  * 3.0mm ArC02_70_30  (active volume)
  * 0.2mm Cu, rho=1.16 g/cm3  (stretch ~13 x and reduced density)
  * 0.3mm Kapton, rho=1.02g/cm3  ( use 0.7 density to account for holes)
  * 0.2mm FR4, rho=1.19g/cm3, strech factor=1.6 ???
  * 5.8mm ArC02_70_30  (passive drift volume)
  * 2 rings of FR4 at inner/outer radii, 1 cm wide, thickness=9.5 cm
  * 4 bars at quadrant boundaries ???? 
  * the above values are old , only approximate
* Total depth=15.0mm

*********************************************************************
+CDE,AGECOM,GCUNIT.

* Declare variables used in the code.

      real centerZ     ! center of complete assembly along Z axis
      real lengthZ      ! length of complete assembly along Z axis
      real thickGG       ! thickness in Z of a single tripple-GEM disk
      real Rin         ! inner radius a komponent
      real Rout        ! outer radius a komponent
      real zsum        ! accumulates Z position for set of components
      integer disk    ! index for counting over the GEM disks
      integer quad    ! index for counting quadrants in one GEM disk
      integer cable   ! index for counting cables for one GEM disk
      integer cabseg   ! index for counting cable segment
      real x1,x2,x3  ! working variables 
      real zD,z1,z2  ! working variables 
 

* Declare the blocks to be defined in the code.
      Content   FGMO, FGGD, FGSC, FGFP, FGFC, FGFA, FGFL, FGOC,FGXR,
     + FGGF,FGGN,FGGC,FGGK,FGGP,FGGR,FGOR,FGOA,FGOU,FGCT,FGRS,
     + FGAP,FGAL,FGAS,FGAB, FGSU,FGSV,FGXC,FGXD,FGXE
* volumes added/changed by Jan:
* FGFP - front plate carbon nomex honeycomb
* FGFC - front plate carbon fiber
* FGFA - front plate inner Alu ring
* FGFL - front plate lip of the inner Alu ring
* FGOC - outer cylinder , carbon fiber
* FGOR - outer rails , Alu
* FGOA - outer Alu ring
* FGOU - FGT  utility cables, mix
* FGXR - mock interface Alu ring to SSD 
* FGXC - mock supporting C ring in SSD 
* FGXD - mock supporting C ring in IST 
* FGXE - mock supporting C ring in HFT 
* FGGD - tripple-GEM disk , FR4 reduced to inner + outer spacer rings
* FGGF - FR4 front foil of  tripple-GEM
* FGGN - Nomex front support of  tripple-GEM
* FGSC - active volume of tripple-GEM
* FGGC - Cu layer of  tripple-GEM
* FGGK - Kapton layer of  tripple-GEM
* FGGP - passive gas layer of  tripple-GEM
* FGGR - FR4 readout layer of  tripple-GEM
* FGCT - G10 Tube for IFC resitor chain 
* FGRS - Resistor Strip for IFC resitor chain 
* FGAP - APV-chips encapsulation volume
* FGAL - APV-chips large end-board
* FGAS - APV-chips smale end-board
* FGAB - APV-chips balde-boards, there are 2 
* FGSU - SSD utility cables-long, mix 
* FGSV - SSD utility cables-tube , mix

* Declare the structure FGTG which defines the detector geometry.
      integer mxDisk
      parameter (mxDisk=12) !stupid, should be used below as param,Jan
      Structure FGTG { Config, RI, RO, Zstart, Zdisk(12), SR, RThk,
     +  nDisk,GR2,GR3,FRArout,FRArin,FRAthk, FPthk, FPCthk,OCthk,OClen,
     +  FRALrin,
     +  GGSCthk, GGFR4thk, GGNthk, GGCUthk, GGKAthk, GGPAthk, GGRLthk,
     +  APVx,APVy,APVz,APVang,nQuad, APVd,APVy2, APVdy, 
     +  FGORrad, FGORang,  FGOAdelR, FGOAdelZ,  
     +  FGOUdelR,FGOUang,FGOUdelA, nCables,
     +  IFCrin, IFCTUBrin, IFCTUBrout, IFClenZ, IFCresAng, IFCTUBdelA,
     +  IFCResDelA, IFCResDelR

        }

* ----------------------------------------------------------------------
 Fill FGTG  ! Inner GEM Tracker Geometry data
      Config  =   2 ! Version

      GR2=0.3 ! outer gap between FGT disk and skin
      GR3=2.3 !  outer artificial gap beyond skin for services
      FRArout=22.45 ! outer radius of the front alu ring  
      FRArin=17.45 ! inner radius of the front alu ring  
      FRALrin=22.20 ! inner radius of the lip of  front alu ring  
      FRAthk=0.5 !  thickness  of the front alu ring  
      FPthk=2.0  ! overall thickness of the front plate 
      FPCthk=0.1 !  thickness of the front plate carbon layer 
      OCthk=0.3 !  thickness of the outer carbon fiber cylinder
      OClen=167.72 !  length of the outer carbon fiber cylinder
      FGORrad=0.3 ! radius of the rails
      FGORang=26. ! tilt of the rails
      FGOAdelR=1.0 ! R-thickness  of the outer alu ring  
      FGOAdelZ=5.0 ! thickness  of the outer alu ring  

      nCables=3    ! number of cable boundles for FGT   
      FGOUang=10.   !  tilt of the cables
      FGOUdelA=0.67 ! angular span of the cables for 1 disk
      FGOUdelR=0.5 !  R-thickness  of the FGT cables
       
      nDisk =8    ! # of disks
      RI     = 10.5 ! inner radii for physical GEM volume. 
      RO     = 38.1 ! outer radii for physical GEM volume. 
      Zstart  =   62.98  ! starting position along Z axis
      Zdisk = { 5.4, 15.4, 25.4, 35.4, 45.4, 55.4, 105., 155. 151, 152,153,154}  ! Z positions of GEM front face

      GGSCthk=0.3 ! thickness of active gas volume
      GGFR4thk=0.025 ! thickness of FR4 layer in GEM disk
      GGNthk=0.5 ! thickness of Nomex layer in GEM disk
      GGCUthk=0.02 ! thickness of combined Cu layer in GEM disk
      GGKAthk=0.03 ! thickness of combined Kapton layer in GEM disk
      GGPAthk=0.58 ! thickness of combined passive gas layer in GEM disk
      GGRLthk=0.02 ! thickness of FR4 readout layer in GEM disk
      SR      =   1.0   ! radial size for inner & outer spacers

* approximate APV chip mass, at the edge of quadrant boundary
      nQuad=4   ! # of quadrants per GEm disk    
      APVx=26. ! length  of APV assembley (at two quadrants boundary)
      APVy=6 ! width of APV assembley 
      APVz=6. ! length  of APV assembley
      APVang=-15. ! tilt of quadrant boundary, deg
      APVd=0.25 ! thickness of PC boards ,
*                 adjusted to yield 700g total for 1 FGT disk (4 assembly) 
      APVy2=3.0 ! Y-width of short end-board
      APVdy=1.2 ! separation between blade-boards

* IFC resitor chain, one for EAST+WEST
      IFCrin=46.1 ! inner radii of IFC
      IFCTUBrin=1.17 ! inner radii of carbon pipe
      IFCTUBrout=1.27 ! outer radii of carbon pipe
      IFClenZ=400 ! len of carbin pipe 
      IFCresAng=106. ! phi location of resitr chain
      IFCTUBdelA=11.3 ! angular separation between carbon pipes
      IFCResDelA=2.48 ! angular width of resitor strip
      IFCResDelR=0.13 ! thickness of resitor strip
   
   EndFill

*****************************************************************

      USE FGTG  config=2
      print * ,'**********FGT UPGR16 geometry, config=',FGTG_Config
      if(  FGTG_Config.le.0) then 
        print *, ' no FGT disks, just West Cone'
        FGTG_nDisk=0
      else  if(  FGTG_Config.eq.1) then 
        print *, ' 6 FGT disks, no additional 3 FGT test disks'
        FGTG_nDisk=6
      else  
        print *, ' 6 FGT disks and  3 FGT test disks' 
      endif
* Calculate some parameters from the input variables.


* Kapton   C22-H10-N2-O4
      Component C   A=12   Z=6   W=22.0*12.0/366.0
      Component H   A=1    Z=1   W=10.0*1.0/366.0
      Component N   A=14   Z=7   W=2.0*14.0/366.0
      Component O   A=16   Z=8   W=4.0*16.0/366.0
      Mixture   Kapton_dens1   Dens=1.02

* FR4=G10 is about 60% SiO2 and 40% epoxy
      Component Si  A=28.08  Z=14   W=0.6*1*28./60.
      Component O   A=16     Z=8    W=0.6*2*16./60.
      Component C   A=12     Z=6    W=0.4*8*12./174.
      Component H   A=1      Z=1    W=0.4*14*1./174.
      Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   FR4    Dens=1.80

* FR4=G10 is about 60% SiO2 and 40% epoxy
      Component Si  A=28.08  Z=14   W=0.6*1*28./60.
      Component O   A=16     Z=8    W=0.6*2*16./60.
      Component C   A=12     Z=6    W=0.4*8*12./174.
      Component H   A=1      Z=1    W=0.4*14*1./174.
      Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   FR4_dens1    Dens=1.12

* FR4+5%Cu 
      Component Si  A=28.08  Z=14   W=0.6*1*28./60.
      Component O   A=16     Z=8    W=0.6*2*16./60.
      Component C   A=12     Z=6    W=0.4*8*12./174.
      Component H   A=1      Z=1    W=0.4*14*1./174.
      Component O   A=16     Z=8    W=0.4*4*16./174.
      component Cu  A=63.5   Z=29   w=0.05 !educated guess
      Mixture   FR4Cu    Dens=1.8

*  Nomex - PN  8/3/96 following I.Sacreda note
     component C      A=12  Z=6  W=5
     component H      A=1   Z=1  W=8
     component O      A=16  Z=8  W=2
     mixture   Nomex  Dens=0.032

* Ar-C02  gass mixture,  70%-30%
      Component Ar A=39.95   Z=18.   W=0.7
      Component C  A=12.01   Z=6.    W=0.3*1*12.01/44.01
      Component O  A=16.     Z=8.    W=0.3*2*16./44.01
      Mixture   ArCO2_70_30  Dens=0.0018015 Isvol=1

* PVC material
      Component C   A=12  Z=6  W=2
      Component H   A=1   Z=1  W=3
      Component CL  A=35  Z=17 W=1
      Mixture   PVC  Dens=1.38

* SSD  cables & isolation mix 
      Component C   A=12  Z=6  W=2
      Component H   A=1   Z=1  W=3
      Component CL  A=35  Z=17 W=1
      component Cu  A=63.5   Z=29   w=0.20
      Mixture   Cable_SSD  Dens=1.6

* FGT  cables & isolation mix 
      Component C   A=12  Z=6  W=2
      Component H   A=1   Z=1  W=3
      Component CL  A=35  Z=17 W=1
      component Cu  A=63.5   Z=29   w=2.0
      Component AL  A=27  Z=13  W=0.12
      Mixture   Cable_FGT  Dens=3.2

* Alumnia - ceramic ued for resistors
      Component AL  A=27  Z=13  W=2
      Component O   A=16   Z=8  W=3
      Mixture   Alumina  Dens=3.90

* Definition for Carbon fiber
      Component C A=12 Z=6 W=1
      Mixture   CFiber Dens=1.713

* Definition for dilluted copper
      component Cu      A=63.540  Z=29  w=1
      Mixture   Copper_dens1 Dens=1.16


      if ( FGTG_nDisk>mxDisk) then
          print *, 'nDisk=',FGTG_nDisk,' is too large, fix the code,JB'
           call exit(1)
       endif

* Calculate some secondary dimensions
      thickGG=2*FGTG_GGFR4thk +FGTG_GGNthk +FGTG_GGCUthk +FGTG_GGKAthk
     +   +FGTG_GGPAthk  +FGTG_GGSCthk+ FGTG_GGRLthk
      print *,'thickGG=',thickGG
      Rin = FGTG_RI
      Rout = FGTG_RO+ FGTG_OCthk +FGTG_GR2
      lengthZ = FGTG_FPthk+FGTG_OClen

* Calculate centre of assembly.
      centerZ = FGTG_Zstart + lengthZ / 2.0
     
* SSD simplified mounting Alu ring 
      Create   FGXR
      Position FGXR in CAVE z=57 kOnly='MANY'
* SSD simplified supporting Carbon ring
      Create   FGXC
      Position FGXC in CAVE z=51.0 kOnly='MANY'
* SSD cables in front of FGT
      Create   FGSV
      Position FGSV in CAVE z=58. kOnly='MANY'
* IST simplified supporting Carbon ring
      Create   FGXD
      Position FGXD in CAVE z=24.0 kOnly='MANY'

* HFT simplified supporting Carbon ring
      Create   FGXE
      Position FGXE in CAVE z=11.0 kOnly='MANY'


* TPC resistor chain in IFC
      Create   FGCT
      x1=FGTG_IFCrin-FGTG_IFCTUBrout  ! r-distance
      x2=(FGTG_IFCresAng-FGTG_IFCTUBdelA/2.0)/180.*3.1416 ! titl angle in rad
      Create and Position FGCT z=0. x=x1*cos(x2) y=x1*sin(x2)  kOnly='MANY'    
      x2=(FGTG_IFCresAng+FGTG_IFCTUBdelA/2.0)/180.*3.1416 ! titl angle in rad
      Create and Position FGCT z=0. x=x1*cos(x2) y=x1*sin(x2)  kOnly='MANY'    

      Create   FGRS
      Create and Position FGRS  alphaZ=FGTG_IFCresAng kOnly='MANY'

* real FGT

      Create   FGMO
      Position FGMO in CAVE z=centerZ kOnly='MANY'

* -------------------------------------------------------------
Block FGMO is the mother volume for the whole FGT assembly
      Material  Air
      Attribute FGMO  Seen=0  colo=6
      Shape     TUBE Rmin=Rin Rmax=Rout+FGTG_GR3 Dz=lengthZ/2.0
 
* assembly enclosure
      Create and Position FGFP z=-lengthZ/2.0+FGTG_FPthk/2.0
      Create and Position FGFA z=-lengthZ/2.0+FGTG_FRAthk/2.0
      Create and Position FGFL z=-lengthZ/2.0+0.5*FGTG_FRAthk+FGTG_FPthk/2.0
      Create and Position FGOC z=+FGTG_FPthk/2.0
      Create and Position FGOA z=+lengthZ/2.0-FGTG_FGOAdelZ/2.0

* material inside enclosure
* rails
      x1=Rout+FGTG_FGORrad  ! r-distance
      x2=FGTG_FGORang/180.*3.1416 ! titl angle in rad
      Create and Position FGOR z=FGTG_FPthk/2.0 x=x1*cos(x2) y=x1*sin(x2)     
      Create and Position FGOR z=FGTG_FPthk/2.0 x=-x1*cos(x2) y=-x1*sin(x2)    

* Loop over the regular triple-GEM disks.
 
      do disk=1,FGTG_nDisk
         ! print *, 'do disk=',disk,  FGTG_Config
         zD=-lengthZ/2.0 + thickGG/2.0 + FGTG_Zdisk(disk) ! Z of this disk
         Create and Position FGGD z=zD

        ! add APV chips material, WARN: uses 'zsum' from FGGD
        x3=rin+FGTG_APVx/2. ! Rxy of the APV-chip center
        do quad=1,FGTG_nQuad
            x2=FGTG_APVang+quad*360./FGTG_nQuad ! titl angle
            Create and Position FGAP z=zD+zsum+FGTG_APVz/2.0 alphaZ=x2
     +      x=x3*cos(x2/180.*3.1416) y=x3*sin(x2/180.*3.1416)
        enddo


* FGT utility lines start at beginning of every disk: 
      do cable=1,FGTG_nCables
          x2=90+FGTG_FGOUang + cable*90 !center angle
          x3=x2+disk*FGTG_FGOUdelA*3.0 ! final phi orientation, space them apart
          z1=FGTG_OClen/2.0-zD ! cable length
      !  print *, 'disk=',disk, 'z1=',z1
          x1=FGTG_RO+FGTG_GR2+FGTG_OCthk !' inner radii
          Create and Position FGOU Rmin=x1 Rmax=x1+FGTG_FGOUdelR
     +    dZ=z1/2.0
     +        z=(FGTG_OClen-z1)/2.0  alphaZ=x3
     +        phi1=0 phi2=FGTG_FGOUdelA
         enddo ! end of cables
      enddo ! end of regular disks

* add one extra FGT sensitive volume in front, requested by Les
      if(  FGTG_Config.gt.1) then 
      Create and Position FGSC z=-lengthZ/2.0+4.0 Rmin=2.5
     +              Rmax=FGTG_RO Dz=FGTG_GGSCthk/2.0
      endif

* add SSD utility lines, continue engle 
        do cable=1,FGTG_nCables
           x1=FGTG_RO+FGTG_GR2+FGTG_OCthk+0.1 !' outer radii
           Create and Position FGSU Rmin=x1 Rmax=x1+1.5 dZ=FGTG_OClen/2.0
     +        z=0  alphaZ=80+FGTG_FGOUang+cable*90 
     +        phi1=30 phi2=45 
         enddo ! end of cables
 endblock

* ---------------------------------------------------------------------
Block FGGD is the mother volume of the individual tripple-GEM disks
      Material FR4
      Attribute FGGD  Seen=1  colo=7
      Shape TUBE Rmin=FGTG_RI Rmax=FGTG_RO Dz=thickGG/2.0

* assemble layres of one tripple-GEM disk
        zsum = -thickGG/2.0      ! Start at the front face.
        rin=FGTG_RI
        rout=FGTG_RO

        Create and Position FGGF z=zsum+ FGTG_GGFR4thk/2.0
        zsum=zsum+FGTG_GGFR4thk

        Create and Position FGGN z=zsum+ FGTG_GGNthk/2.0
        zsum=zsum+FGTG_GGNthk
 
        Create and Position FGGF z=zsum+ FGTG_GGFR4thk/2.0
        zsum=zsum+FGTG_GGFR4thk

        rin=FGTG_RI+FGTG_SR ! change Rin/Rout for the volumes below
        rout=FGTG_RO-FGTG_SR
        Create and Position FGSC z=zsum+FGTG_GGSCthk/2.0 Rmin=rin Rmax=rout Dz=FGTG_GGSCthk/2.0
        zsum=zsum+FGTG_GGSCthk

        Create and Position FGGC z=zsum+ FGTG_GGCUthk/2.0
        zsum=zsum+FGTG_GGCUthk

        Create and Position FGGK z=zsum+ FGTG_GGKAthk/2.0
        zsum=zsum+FGTG_GGKAthk
  
        Create and Position FGGP z=zsum+ FGTG_GGPAthk/2.0
        zsum=zsum+FGTG_GGPAthk

        Create and Position FGGR z=zsum+ FGTG_GGRLthk/2.0
        zsum=zsum+FGTG_GGRLthk

endblock


* ------------------------------------------------------------------------
Block FGGF describes the FR4 layer of 3GEM
      Material FR4 
      Attribute FGGF  Seen=1 colo=4
      Shape TUBE Rmin=rin Rmax=rout Dz=FGTG_GGFR4thk/2.0
endblock


* ----------------------------------------------------------------------
Block FGFP front plate, heonecomb carbon
      Material   Nomex
      Attribute FGFP  Seen=1  colo=1
      Shape TUBE Rmin=FGTG_FRArout Rmax=Rout Dz=FGTG_FPthk/2.0
      Create and Position FGFC z=-FGTG_FPthk/2.0 +FGTG_FPCthk/2.0
      Create and Position FGFC z=+FGTG_FPthk/2.0 -FGTG_FPCthk/2.0
endblock

* ----------------------------------------------------------------------
Block FGGN tripple-GEM support plate, heonecomb nomex
      Material  Nomex
      Attribute FGGN  Seen=1  colo=1
      Shape TUBE Rmin=Rin Rmax=Rout Dz=FGTG_GGNthk/2.0
endblock

* ----------------------------------------------------------------------
Block FGGC  combined Cu-layer in  tripple-GEM 
      Material  Copper_dens1
      Attribute FGGC  Seen=1  colo=3
      Shape TUBE Rmin=Rin Rmax=Rout Dz=FGTG_GGCUthk/2.0
endblock

* ----------------------------------------------------------------------
Block FGGK  combined Kapton layer in  tripple-GEM 
      Material  Kapton_dens1
      Attribute FGGK  Seen=1  colo=4
      Shape TUBE Rmin=Rin Rmax=Rout Dz=FGTG_GGKAthk/2.0
endblock

* ----------------------------------------------------------------------
Block FGGP  combined passive gas  layer in  tripple-GEM 
      Material  ArCO2_70_30
      Attribute FGGP  Seen=1  colo=5
      Shape TUBE Rmin=Rin Rmax=Rout Dz=FGTG_GGPAthk/2.0
endblock

* ----------------------------------------------------------------------
Block FGGR  FR4 readout layer in  tripple-GEM 
      Material  FR4_dens1
      Attribute FGGR  Seen=1  colo=4
      Shape TUBE Rmin=Rin Rmax=Rout Dz=FGTG_GGRLthk/2.0
endblock

* ---------------------------------------------------------------
Block FGFC front plate carbon fiber
      Material CFiber
      Attribute FGFC  Seen=1  colo=3
      Shape TUBE Rmin=FGTG_FRArout Rmax=Rout Dz=FGTG_FPCthk/2.0
endblock

* ---------------------------------------------------------------
Block FGCT G10 Tube for IFC resitor chain 
      Material FR4
      Attribute FGCT  Seen=1  colo=3
      Shape TUBE Rmin=FGTG_IFCTUBrin Rmax=FGTG_IFCTUBrout DZ=FGTG_IFClenZ/2.0
endblock

* ---------------------------------------------------------------
Block FGFA front plate inner Alu ring
      Material Aluminium
      Attribute FGFA  Seen=1  colo=1
      Shape TUBE Rmin=FGTG_FRArin Rmax=FGTG_FRArout Dz=FGTG_FRAthk/2.0
endblock

* ---------------------------------------------------------------
Block FGFL front plate lip of the  inner Alu ring
      Material Aluminium
      Attribute FGFL  Seen=1  colo=1
      Shape TUBE Rmin=FGTG_FRALrin Rmax=FGTG_FRArout  Dz=(FGTG_FPthk-FGTG_FRAthk)/2.0
endblock

* ---------------------------------------------------------------
Block FGOR  outer rails , Alu
      Material Aluminium
      Attribute FGOR  Seen=1  colo=2
      Shape TUBE Rmin=0 Rmax=FGTG_FGORrad Dz=FGTG_OClen/2.0
endblock

* ---------------------------------------------------------------
Block FGOA  outer  Alu ring
      Material Aluminium
      Attribute FGFC  Seen=1  colo=1
      Shape TUBE Rmin=Rout Rmax=Rout+FGTG_FGOAdelR Dz=FGTG_FGOAdelZ/2.0
endblock

* ------------------------------
Block FGOU  FGT utility  lines, all mixed and averaged
      Material  Cable_FGT
      Attribute FGOU  Seen=1  colo=1
      Shape TUBS Rmin=0 Rmax=0 dz=0 phi1=0 phi2=0
endblock

* ------------------------------
Block FGSU  SSD  utility lines, all mixed and averaged
      Material  Cable_SSD 
      Attribute FGSU  Seen=1  colo=7
      Shape TUBS Rmin=0 Rmax=0 dz=0 phi1=0 phi2=0
endblock

* ------------------------------
Block FGSV  SSD  utility -tube, all mixed and averaged
      Material  Cable_SSD 
      Attribute FGSU  Seen=1  colo=7
      Shape TUBE Rmin=36.10 Rmax=36.27 dZ=5.0
endblock

* ---------------------------------------------------------------

Block FGRS Resistor Strip for IFC resitor chain 
       Material Alumina
      Attribute FGRS  Seen=1  colo=1 ! was 3
      Shape TUBS Rmin=FGTG_IFCrin-FGTG_IFCResDelR-FGTG_IFCTUBrout
     +   Rmax=FGTG_IFCrin-FGTG_IFCTUBrout
     +   dz=FGTG_IFClenZ/2.0
     +   phi1=-FGTG_IFCResDelA/2.0  phi2=FGTG_IFCResDelA/2.0
endblock


* ---------------------------------------------------------------
Block FGXR SSD interface Alu ring 
      Material Aluminium
      Attribute FGXR  Seen=1  colo=1
      Shape TUBE Rmin=28.0 Rmax=36.0 Dz=0.7/2.0
endblock

* ---------------------------------------------------------------
Block FGXC SSD supporting carbon ring
      Material CARBON
      Attribute FGXC  Seen=1  colo=1
      Shape TUBE Rmin=2.5 Rmax=21.0 Dz=0.1/2.0
endblock

* ---------------------------------------------------------------
Block FGXD IST supporting carbon ring
      Material CARBON
      Attribute FGXD  Seen=1  colo=1
      Shape TUBE Rmin=2.5 Rmax=13.0 Dz=0.1/2.0
endblock

* ---------------------------------------------------------------
Block FGXE HFT supporting carbon ring
      Material CARBON
      Attribute FGXE  Seen=1  colo=1
      Shape TUBE Rmin=2.5 Rmax=8.0 Dz=0.1/2.0
endblock


* ---------------------------------------------------------------
Block FGAP  APV-chips block encapsulation volume
      Material air
      Attribute FGAP  Seen=0  colo=6
      Shape BOX dX=FGTG_APVx/2.0  dY=FGTG_APVy/2.0  dZ=FGTG_APVz/2.0  
      * add PC boards  
      Create and Position FGAS x=(-FGTG_APVx + FGTG_APVd)/2.0 y=0 z=0
      Create and Position FGAL x=( FGTG_APVx - FGTG_APVd)/2.0 y=0 z=0
      Create and Position FGAB x=0 y=-FGTG_APVdy/2.0 z=0
      Create and Position FGAB x=0 y=+FGTG_APVdy/2.0 z=0
endblock


* ---------------------------------------------------------------
Block FGAL  APV- large end-board made of FR4
      Material FR4Cu
      Attribute FGAL  Seen=1  colo=7
      Shape BOX dX=FGTG_APVd/2.0  dY=FGTG_APVy/2.0  dZ=FGTG_APVz/2.0 
endblock

* ---------------------------------------------------------------
Block FGAS  APV- small end-board made of FR4
      Material FR4Cu
      Attribute FGAS  Seen=1  colo=7
      Shape BOX dX=FGTG_APVd/2.0  dY=FGTG_APVy2/2.0  dZ=FGTG_APVz/2.0 
endblock

* ---------------------------------------------------------------
Block FGAB  APV- blade-board made of FR4
      Material FR4Cu
      Attribute FGAB  Seen=1  colo=7
      Shape BOX dX=FGTG_APVx/2.0-FGTG_APVd  dY=FGTG_APVd/2.0  dZ=FGTG_APVz/2.0 
endblock

* ---------------------------------------------------------------
Block FGOC  outer cylinder, carbon fiber FGTG_OCthk
      Material CFiber
      Attribute FGOC  Seen=1  colo=3 
      Shape TUBE Rmin=FGTG_RO+FGTG_GR2 Rmax=FGTG_RO+FGTG_GR2+FGTG_OCthk
     +           Dz=FGTG_OClen/2.0
endblock

* -------------------------------------------------------------------
Block FGSC describes the sensitive area
      Material  ArCO2_70_30  
      Material  Sensitive  Isvol=1
      Attribute FGSC  Seen=1 colo=6 
      Shape TUBE Rmin=0 Rmax=0 Dz=0 

      HITS    FGSC   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
endblock

* ----------------------------------------------------------------------
      END

    
