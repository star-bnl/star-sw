* $Id: fstdgeo.g,v 1.1 2004/12/07 00:41:00 potekhin Exp $
* $Log: fstdgeo.g,v $
* Revision 1.1  2004/12/07 00:41:00  potekhin
* We need a directory subtree for the Forward Tracker.
* It has been planned for a long time.
*
*
******************************************************************************
Module FSTDGEO is the geometry of the forward silicon tracker pixel detector
  Created  12/06/04
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real angle,center,depth, anglePos,angleCorr,raddeg
      integer nl,ly,nSector

      Content   FSMO, FDMO
*
      Structure FSTG {Layer,
                      RminS,     RmaxS,
                      Rmin,      Rmax,
                      Zmin,      Zmax,
                      SensorThk, PassiveThk, ActiveThk,
                      Z1,Z2,Z3,Z4}
*
* -----------------------------------------------------------------------------
*
   Fill FSTG                   ! Forward detector data
      Layer      =  1          ! layer index
      RminS      =  7.0        ! inner radius of sensitive layer
      RmaxS      = 19.0        ! outer radius of sensitive layer
      Rmin       =  7.0        ! inner radius of all of the detector
      Rmax       = 22.0        ! outer radius of all of the detector
*
      Zmin       = 28.0        ! Z-start  of the barrel comprising the three pancakes
      Zmax       = 37.0150     ! Z-finish of the barrel comprising the three pancakes
*
      SensorThk  =  0.0150     ! Total   silicon Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0050     ! Active  silicon Thickness
      Z1         =  0.0        ! 1st face nominal z
      Z2         =  3.0        ! 2nd face nominal z
      Z3         =  6.0        ! 3rd face nominal z
      Z4         =  9.0        ! 4th face nominal z
   EndFill
*
******************************************************

      USE      FSTG
*
      raddeg=3.14159265/180.0

      center=(FSTG_Zmax+FSTG_Zmin)/2.0
      depth =(FSTG_Zmax-FSTG_Zmin)

      Create   FSMO
      Position FSMO in CAVE z=+center
      Position FSMO in CAVE z=-center
* -----------------------------------------------------------------------------
Block FSMO is the mother of the FSTD detector volumes
      Material  Air
      Attribute FSMO  Seen=1  colo=6

      Shape TUBE Rmin=FSTG_Rmin Rmax=FSTG_Rmax Dz=depth/2.0

      Create   FDMO
      Position FDMO z=FSTG_Z1-depth/2.0+FSTG_SensorThk/2.
      Position FDMO z=FSTG_Z2-depth/2.0+FSTG_SensorThk/2.
      Position FDMO z=FSTG_Z3-depth/2.0+FSTG_SensorThk/2.
      Position FDMO z=FSTG_Z4-depth/2.0+FSTG_SensorThk/2.
endblock

* -----------------------------------------------------------------------------
Block FDMO is the mother of an individual disk
      Material  Air
      Attribute FDMO  Seen=1  colo=5

      Shape TUBE Rmin=FSTG_Rmin Rmax=FSTG_Rmax Dz=FSTG_SensorThk/2.0

endblock

*

      END

