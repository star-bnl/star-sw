* $Id: fgtdgeo.g,v 1.7 2007/11/13 21:30:03 perev Exp $
* $Log: fgtdgeo.g,v $
* Revision 1.7  2007/11/13 21:30:03  perev
* material ALKAP fixed
*
* Revision 1.6  2005/04/19 19:39:22  potekhin
* Minor naming convention fix, in the attribute name
*
* Revision 1.5  2005/04/18 23:26:54  potekhin
* As Kai pointed out, need to change the FGAR
* hist defintition to something else, and FGSC fits
* teh convention
*
* Revision 1.4  2005/04/15 14:53:14  potekhin
* Add hit description to the FGT detector
*
* Revision 1.3  2005/03/31 18:34:25  potekhin
* Declare the sector of the ring a sensitive volume,
* and leave comment to that effect
*
* Revision 1.2  2005/02/19 05:14:27  potekhin
* Roughly, it's done according to Bernd's powerpoint specs.
* Things they might want to add:
* a) rotation around Z by 90 (orientation not fixed in the
* design I assume)
* b) Improve kapton deifnition (immaterial)
* c) Add hits (ask Kai)
*
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

      Content   FGMO,FGAM, FGRN,FGSC,FGGT,FGKP

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


     Create FGGT
     Position FGGT z=FGTG_Thk/2-FGTG_G10Thk/2
     Position FGGT z=-FGTG_Thk/2+FGTG_G10Thk/2

     Create and Position FGKP z=FGTG_Thk/2-FGTG_G10Thk-FGTG_KaptonThk/2
     Create and Position FGAM z=FGTG_Thk/2-FGTG_G10Thk-FGTG_KaptonThk-FGTG_ArThk/2


endblock
* -----------------------------------------------------------------------------
Block FGAM is the mother of the Argon volume
      Material  Air
      Attribute FGAM  Seen=1  colo=6

      Shape     TUBE Rmin=FGTG_Rmin Rmax=FGTG_Rmax Dz=FGTG_ArThk/2.0
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

      Shape     TUBE Rmin=r1 Rmax=r2 Dz=FGTG_ArThk/2.0

      Create FGSC
      do ixSec=1,Ns
        Position FGSC alphaZ=(ixSec-1)*dPhi
      enddo

endblock
* -----------------------------------------------------------------------------
* Sensitive volume in this model. 90%Ar+10%CO2
*
Block FGSC is an Ar Sector of a Ring in the GEM detector
      Component Ar A=39.95   Z=18.   W=0.9
      Component C  A=12.01   Z=6.    W=0.1*1*12.01/44.01
      Component O  A=16.     Z=8.    W=0.1*2*16./44.01
      Mixture   Ar_mix  Dens=0.0018015  Isvol=1        

      Material  Sensitive  Isvol=1

      Attribute FGSC  Seen=1  colo=2

      Shape     TUBS phi1=0 phi2=dPhi

      call      GSTPAR (%Imed,'STRA',1.)

      HITS    FGSC   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 

endblock
* -----------------------------------------------------------------------------
Block FGGT is the G10 plate
*     G10 is about 60% SiO2 and 40% epoxy
      Component Si  A=28.08  Z=14   W=0.6*1*28./60.
      Component O   A=16     Z=8    W=0.6*2*16./60.
      Component C   A=12     Z=6    W=0.4*8*12./174.
      Component H   A=1      Z=1    W=0.4*14*1./174.
      Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10    Dens=1.7

      Attribute FGGT  Seen=1  colo=3

      Shape     TUBE  Rmin=FGTG_Rmin Rmax=FGTG_Rmax Dz=FGTG_G10Thk/2.0

endblock
* -----------------------------------------------------------------------------
Block FGKP is the Kapton layer
* use aluminized mylar mixture instead of kapton
      Component C5  A=12    Z=6  W=5
      Component H4  A=1     Z=1  W=4
      Component O2  A=16    Z=8  W=2
      Component Al  A=27    Z=13 W=0.2302
      Mixture  ALKAP  Dens=1.432

      Attribute FGKP  Seen=1  colo=4

      Shape     TUBE  Rmin=FGTG_Rmin Rmax=FGTG_Rmax Dz=FGTG_KaptonThk/2.0

endblock


* -----------------------------------------------------------------------------


      END

