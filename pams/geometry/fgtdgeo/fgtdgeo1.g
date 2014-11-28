* $Id: fgtdgeo1.g,v 1.4 2007/11/13 21:29:45 perev Exp $
* $Log: fgtdgeo1.g,v $
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
Module FGTDGEO1 is the geometry of the forward GEM tracking detector
  Created  2/7/07
  Author   TUP Tea
******************************************************************************
+CDE,AGECOM,GCUNIT.

* Declare variables used in the code.

      real center                  ! center of complete assembly along Z axis
      real length                  ! length of complete assembly along Z axis
      real thick                   ! thickness in Z of a single GEM disk
      real rmn                     ! used to determine the minimum radius
      real rmx                     ! used to determine the maximum radius
      real rin                     ! inner radius for each GEM detector
      real rout                    ! outer radius for each GEM detector
      real zsum                    ! accumulates Z position for layers in GEM
      real thk                     ! half thickness of layer in each GEM

      integer disk                 ! index for counting over the GEM disks
      integer layer                ! index fro counting over the layers in GEM

* Declare the blocks to be defined in the code.
      Content   FGMO, FGDO, FGFO, FGIS, FGOS, FGRL, FGSC

      Structure FGTV {version,  int Config}

* Declare the structure FGTG which defines the detector geometry.
      Structure FGTG { Config, RI(6), RO(6), Zstart, Z(6), FThk(6), SThk(6), SR, RThk }

* -----------------------------------------------------------------------------
*      RI      = {  7.5,  7.5,  7.5, 16.5, 16.5, 16.5 }    ! inner radii for each GEM. ORG{3, 7.5, 12, 16.5, 21, 25.5} 

   Fill FGTV          ! FGT geometry version
      version =  1    ! geometry version - dummy
      Config  =  1    ! config
   EndFill

   Fill FGTG  ! Inner GEM Tracker Geometry data
      Config  =   1                                       ! Version
      RI      = {  7.5,  7.5,  7.5,  7.5,  7.5,  7.5 }    ! inner radii for each GEM. ORG{3, 7.5, 12, 16.5, 21, 25.5} 
      RO      = { 43.0, 43.0, 43.0, 43.0, 43.0, 43.0 }    ! outer radii for each GEM.
      Zstart  =   60.0                                    ! starting position along Z axis
      Z       = { 0.0, 18.0, 36.0, 54.0, 72.0, 90.0}      ! Z positions for GEM front face
      FThk    = { 0.05, 0.05, 0.05, 0.05, 0.05, 0.05 }    ! foil thicknesses inside GEM
      SThk    = { 0.4, 0.3, 0.3, 0.3, 0.3, 0.4 }          ! support/spacing thicknesses
      SR      =   1.0                                     ! radial size for support
      RThk    =   0.3                                     ! readout plane thickness
   EndFill

*******************************************************************************

* Calculate some parameters from the input variables.

      USE FGTV
      USE FGTG  config=FGTV_Config;
      write(*,*) 'FGTD Version:',FGTV_Config

* use aluminized mylar mixture instead of kapton
      Component C5  A=12    Z=6  W=5
      Component H4  A=1     Z=1  W=4
      Component O2  A=16    Z=8  W=2
      Component Al  A=27    Z=13 W=0.2302
      Mixture  ALKAP  Dens=1.432
*     G10 is about 60% SiO2 and 40% epoxy
      Component Si  A=28.08  Z=14   W=0.6*1*28./60.
      Component O   A=16     Z=8    W=0.6*2*16./60.
      Component C   A=12     Z=6    W=0.4*8*12./174.
      Component H   A=1      Z=1    W=0.4*14*1./174.
      Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10    Dens=1.7


* Loop over the layers in a GEM detector and accumulate the total thickness.

      thick = 0.0

      do layer = 1, 4
         thick = thick + FGTG_FThk(layer) + FGTG_SThk(layer)
      enddo

      thick = thick + FGTG_RThk

* Loop over the GEM disks and determine the min and max radius and max Z.

      rmn = 10000.0
      rmx = - rmn
      length = 0.0

      do disk = 1, 6

         if( rmn .gt. FGTG_RI(disk) ) then
            rmn = FGTG_RI(disk)
         endif

         if( rmx .lt. FGTG_RO(disk) ) then
            rmx = FGTG_RO(disk)
         endif

         if( length .lt. FGTG_Z(disk) ) then
            length = FGTG_Z(disk)
         endif
      enddo

      length = length + thick

* Calculate centre of assembly.

      center = FGTG_Zstart + length / 2.0

      Create   FGMO
      Position FGMO in CAVE z=+center kOnly='MANY'

* -----------------------------------------------------------------------------
Block FGMO is the mother volume for the FGTD
      Material  Air
      Attribute FGMO  Seen=0  colo=6

      Shape     TUBE Rmin=rmn Rmax=rmx Dz=length/2.0

* Loop over the four GEM disks.

      do disk=1,6
         rin  = FGTG_RI(disk)
         rout = FGTG_RO(disk)
         Create and Position FGDO z=-length/2.0 + thick/2.0 + FGTG_Z(disk)
      enddo

endblock

* -----------------------------------------------------------------------------
Block FGDO is the mother volume of the individual GEM disks
      Component Ar A=39.95   Z=18.   W=0.9
      Component C  A=12.01   Z=6.    W=0.1*1*12.01/44.01
      Component O  A=16.     Z=8.    W=0.1*2*16./44.01
      Mixture   Ar_mix  Dens=0.0018015

      Attribute FGDO  Seen=0  colo=6

      Shape TUBE Rmin=rin Rmax=rout Dz=thick/2.0

* Loop over the layers inside the GEM.

      zsum = -thick/2.0                 ! Start at the front face.

      do layer = 1, 4

* Create GEM foil.

         thk = FGTG_FThk(layer)/2.0     ! half thickness of foil
         zsum = zsum + thk              ! position of foil centre

         Create and Position FGFO z=zsum 

         zsum = zsum + thk              ! position of foil rear face

* Create inner and outer spacer/support.

         thk = FGTG_SThk(layer)/2.0     ! half thickness of spacer
         zsum = zsum + thk              ! position of spacer centre

         Create and Position FGIS z=zsum 
         Create and Position FGOS z=zsum 

* First gap is active volume -> create it.

         if( layer .eq. 1 ) then
            Create and Position FGSC z=zsum
         endif

         zsum = zsum + thk              ! position of spacer rear face

      enddo

* Create readout layer as last layer.

      thk = FGTG_RThk/2.0               ! half thickness of readout layer
      zsum = zsum + thk                 ! position of readout layer centre

      Create and Position FGRL z=zsum

endblock

* -----------------------------------------------------------------------------
Block FGFO describes the GEM foils
      Material ALKAP
      Attribute FGFO  Seen=1 colo=4

      Shape TUBE Rmin=rin Rmax=rout Dz=thk

endblock

* -----------------------------------------------------------------------------
Block FGIS describes the inner support or spacer
      Material G10
      Attribute FGIS  Seen=1  colo=3

      Shape TUBE Rmin=rin Rmax=rin+FGTG_SR Dz=thk

endblock

* -----------------------------------------------------------------------------
Block FGOS describes the outer support or spacer
      Material G10
      Attribute FGOS  Seen=1  colo=3

      Shape TUBE Rmin=rout-FGTG_SR Rmax=rout Dz=thk

endblock

* -----------------------------------------------------------------------------
Block FGRL describes the readout layer
      Material G10
      Attribute FGRL  Seen=1  colo=3

      Shape TUBE Rmin=rin Rmax=rout Dz=thk

endblock

* -----------------------------------------------------------------------------
Block FGSC describes the sensitive area
      Component Ar A=39.95   Z=18.   W=0.9
      Component C  A=12.01   Z=6.    W=0.1*1*12.01/44.01
      Component O  A=16.     Z=8.    W=0.1*2*16./44.01
      Mixture   Ar_mix  Dens=0.0018015 Isvol=1

      Material  Sensitive  Isvol=1
      Attribute FGSC  Seen=1 colo=6

      Shape TUBE Rmin=rin+FGTG_SR Rmax=rout-FGTG_SR Dz=FGTG_SThk(1)/2.0;

      HITS    FGSC   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 

endblock

* -----------------------------------------------------------------------------

      END

