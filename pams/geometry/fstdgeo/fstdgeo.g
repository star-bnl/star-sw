* $Id: fstdgeo.g,v 1.2 2005/01/04 23:47:48 potekhin Exp $
* $Log: fstdgeo.g,v $
* Revision 1.2  2005/01/04 23:47:48  potekhin
* A waypoint check-in:
* implemented a full fledged double wafer in each layer,
* and the AlN thermal plate.
*
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

      real center,      depth
      real WedgeOffset, WedgeDx1, WedgeDx2, WedgeLength
      real PlateOffset, PlateDx1, PlateDx2
      real SensorOffset,SensorDx1,SensorDx2,SensorLength
      real raddeg,start,angle,    TanHalfAngle

      integer nl, ly

      Content   FSMO, FDMO, FDMS, FDMW, FDSW, FDTP, FDSA, FDSP

      Structure FSTG {Layer,     Nsec,
                      RminS,     RmaxS,     Rmin,      Rmax,       Zmin,      Zmax,
                      WedgeThk,  SensorThk, PassiveThk, ActiveThk,
                      AlNThk, AlNLength,    Z(4)}
*
* -----------------------------------------------------------------------------
*
   Fill FSTG                   ! Forward Silicon Tracker Geometry data
      Layer      =  1          ! layer index (reserved)
      Nsec       = 21          ! number of sectors in a wheel

      RminS      =  7.0        ! inner radius of sensitive wedge
      RmaxS      = 19.0        ! outer radius of sensitive wedge

      Rmin       =  7.0        ! inner radius of all of the detector
      Rmax       = 22.5        ! outer radius of all of the detector

      Zmin       = 28.0        ! Z-start  of the barrel comprising the three pancakes
      Zmax       = 38.0        ! Z-finish of the barrel comprising the three pancakes


      WedgeThk   =  0.2286     ! Includes two layers
      SensorThk  =  0.0300     ! Total   silicon thickness, includes passive and active
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0200     ! Active  silicon Thickness

      AlNThk     =  0.0762     ! AlN Thickness
      AlNLength  =  3.0        ! AlN length along radius

      Z          =  {0.0, 3.0, 6.0, 9.0} ! nominal Zs of the faces
   EndFill
*
******************************************************

      USE      FSTG
*
      raddeg=3.14159265/180.0

      angle=360.0/FSTG_Nsec ! opening angle of the sector
      TanHalfAngle=tan(angle*raddeg/2.0)

      SensorLength= FSTG_RmaxS - FSTG_RminS

      SensorDx1   = FSTG_RminS*TanHalfAngle
      SensorDx2   = FSTG_RmaxS*TanHalfAngle

      WedgeLength = SensorLength+FSTG_AlNLength
      SensorOffset= -0.5*(WedgeLength-SensorLength)


      WedgeOffset = FSTG_Rmin  + 0.5*WedgeLength

      WedgeDx1    = FSTG_Rmin*TanHalfAngle
      WedgeDx2    = (FSTG_RmaxS+FSTG_AlNLength)*TanHalfAngle

      PlateOffset = 0.5*(WedgeLength - FSTG_AlNLength)

      PlateDx1    = FSTG_RmaxS*TanHalfAngle
      PlateDx2    = (FSTG_RmaxS+FSTG_AlNLength)*TanHalfAngle

*      write(*,*) 'ISTB sector angle: ',angle

* Top sector has to be at 12 o'clock. Angular offset --
* Keep this blank for now
      start=0.0

      center=(FSTG_Zmax+FSTG_Zmin)/2.0
      depth =(FSTG_Zmax-FSTG_Zmin)

      Create   FSMO
      Position FSMO in CAVE z=+center
      Position FSMO in CAVE z=-center
* -----------------------------------------------------------------------------
Block FSMO is the mother of the FSTD detector volumes
      Material  Air
      Attribute FSMO  Seen=1  colo=6

      Shape    TUBE Rmin=FSTG_Rmin Rmax=FSTG_Rmax Dz=depth/2.0

      Create   FDMO

      do nl=1,4
       Position FDMO z=FSTG_Z(nl)-depth/2.0+FSTG_WedgeThk/2.0
      enddo

*      Position FDMO z=FSTG_Z2-depth/2.0+FSTG_WedgeThk/2.0
*      Position FDMO z=FSTG_Z3-depth/2.0+FSTG_WedgeThk/2.0
*      Position FDMO z=FSTG_Z4-depth/2.0+FSTG_WedgeThk/2.0

endblock

* -----------------------------------------------------------------------------
Block FDMO is the mother of an individual two-layer disk
      Material  Air
      Attribute FDMO  Seen=1  colo=5

      Shape TUBE Rmin=FSTG_Rmin Rmax=FSTG_Rmax Dz=FSTG_WedgeThk/2.0

      Create FDMS
endblock

*
* -----------------------------------------------------------------------------
Block FDMS is the sector within an individual disk
      Shape  Division   Iaxis=2   Ndiv=21 c0=start

      Create   FDMW
      Position FDMW x=WedgeOffset ORT=YZX
endblock
* -----------------------------------------------------------------------------
Block FDMW is the mother wedge, housing plate, sensor  and chips
      Attribute FDMW  Seen=1  colo=4

      Shape TRD1 dx1=WedgeDx1 dx2=WedgeDx2 dy=FSTG_WedgeThk/2.0 dz=WedgeLength/2.0

      Create   FDSW
      Position FDSW x=0.0 y=-FSTG_WedgeThk/2.0+FSTG_SensorThk/2.0 z=SensorOffset
      Position FDSW x=0.0 y=+FSTG_WedgeThk/2.0-FSTG_SensorThk/2.0 z=SensorOffset AlphaZ=180

      Create   FDTP
      Position FDTP z=PlateOffset

endblock
* -----------------------------------------------------------------------------
Block FDSW is the Silicon wafer mother
      Attribute FDSW  Seen=1  colo=3

      Shape TRD1 dx1=SensorDx1 dx2=SensorDx2 dy=FSTG_SensorThk/2.0 dz=SensorLength/2.0

      Create and Position FDSA y=-(FSTG_SensorThk-FSTG_ActiveThk)/2.0
      Create and Position FDSP y=+(FSTG_SensorThk-FSTG_PassiveThk)/2.0

endblock
* -----------------------------------------------------------------------------
Block FDSP is the Passive part of the Silicon wafer
      Material  Silicon  
      Attribute FDSW  Seen=1  colo=3

      Shape TRD1 dx1=SensorDx1 dx2=SensorDx2 dy=FSTG_PassiveThk/2.0 dz=SensorLength/2.0


endblock
* -----------------------------------------------------------------------------
Block FDSA is the Active part of the Silicon wafer
      Material  Silicon  
      Attribute FDSW  Seen=1  colo=3

      Shape TRD1 dx1=SensorDx1 dx2=SensorDx2 dy=FSTG_ActiveThk/2.0 dz=SensorLength/2.0

endblock
* -----------------------------------------------------------------------------
Block FDTP is the AlN Thermal Plate

      Component Al   A=27  Z=13  W=1
      Component N    A=14  Z=7   W=1
      Mixture   AlN  Dens=3.30

      Shape TRD1 dx1=PlateDx1 dx2=PlateDx2 dy=FSTG_AlNThk/2.0 dz=FSTG_AlNLength/2.0

endblock
* -----------------------------------------------------------------------------


      END

