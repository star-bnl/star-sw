* $Id: fgtdgeo.g,v 1.1 2005/02/19 04:12:20 potekhin Exp $
* $Log: fgtdgeo.g,v $
* Revision 1.1  2005/02/19 04:12:20  potekhin
* Waypoint check-in: created the first cut with
* simplified geometry of the Forward GEM tracker.
* The rings and sectors are in place, but layers
* aren't yet.
*
******************************************************************************
Module FGTDGEO is the geometry of the forward GEM tracking detector
  Created  02/18/05
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.

      real raddeg, center, dR, r1, r2, dPhi
      integer ixRing,ixSec, Ns

      Content   FGMO,FGRN,FGSC

      Structure FGTG {Version, Nring, Thk, Zend, G10Thk, ArThk, KaptonThk, Rmin, Rmax, Nsectors(5)}
*
* -----------------------------------------------------------------------------
*
   Fill FGTG                   ! Forward GEM Tracker Geometry data
      Version    =  1          ! version of the tracker
      Nring      =  5          ! number of rings
      Thk        =   2.05      ! Thickness
      Zend       = 268.0       ! where is ends
      G10Thk     =   0.5       ! G10 thickness
      ArThk      =   1.0       ! Argon thickness
      KaptonThk  =   0.05      ! Kapton thickness

      Rmin       =  75.0       ! inner radius
      Rmax       = 200.0       ! outer radius

      Nsectors   =  {24,30,36,42,50} ! sectors in a ring
   EndFill

*
******************************************************

      USE FGTG

      raddeg=3.14159265/180.0

      dR = (FGTG_Rmax-FGTG_Rmin)/FGTG_Nring
*      angle=360.0/FSTG_Nsec ! opening angle of the sector

      center = FGTG_Zend-FGTG_Thk/2.0

      Create   FGMO
      Position FGMO in CAVE z=+center
* -----------------------------------------------------------------------------
Block FGMO is the mother of the Forward GEM detector
      Material  Air
      Attribute FGMO  Seen=1  colo=6

      Shape     TUBE Rmin=FGTG_Rmin Rmax=FGTG_Rmax Dz=FGTG_Thk/2.0

      do ixRing=1,FGTG_Nring
         r1=FGTG_Rmin+(ixRing-1)*dR; r2=r1+dR
         Ns=FGTG_Nsectors(ixRing)
         dPhi=360.0/Ns
         Create and Position FGRN
      enddo

endblock
* -----------------------------------------------------------------------------
Block FGRN is a Ring in the GEM detector
      Material  Air
      Attribute FGRN  Seen=1  colo=1

      Shape     TUBE Rmin=r1 Rmax=r2 Dz=FGTG_Thk/2.0

      Create FGSC
      do ixSec=1,Ns
        Position FGSC alphaZ=(ixSec-1)*dPhi
      enddo

endblock
* -----------------------------------------------------------------------------
Block FGSC is a Sector of a Ring in the GEM detector
      Material  Air
      Attribute FGRN  Seen=1  colo=2

      Shape     TUBS phi1=0 phi2=dPhi

endblock

* -----------------------------------------------------------------------------


      END

