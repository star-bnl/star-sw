* $Id: ftrogeo.g,v 1.1 2004/07/23 01:34:41 potekhin Exp $
* $Log: ftrogeo.g,v $
* Revision 1.1  2004/07/23 01:34:41  potekhin
* A first cut of the FTPC readout barrel geometry,
* with a basic end flange and inner struts
*
******************************************************************************
Module FTROGEO is the geometry of the readout structure of the FTPC
  Created  07/22/04
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real Zpos, raddeg, holeAngle, strutAngle, anglePos, angle
      real strutRad
      integer nHole,nStrut

      Content  FTMO,FTOF,FTOH,FSTL
*
      Structure FTRG {Version, N,
                               ftpcZ, ftpcLen, length,
                               Rin,   Rout,
                               ofRin,  ofRout,  ofRthk, ofZ, ofNholes, ofHoleRad, ofHoleR,
                               strutLen, strutHgt, strutWth, strutThk}
*
* -----------------------------------------------------------------------------
*
   Fill FTRG                   ! FTPC readout data
      Version    =  1          ! version
      N          =  5          ! number of sectors
      ftpcZ      = 150.0       ! FTPC face from the interaction point
      ftpcLen    = 119.0       ! FTPC overall length
      length     = 80.0        ! RO barrel length
      Rin        = 20.0        ! innermost radius of envelope
      Rout       = 30.0        ! outermost radius of envelope
      ofRin      = 22.0        ! innermost radius of the outer flange
      ofRout     = 30.0        ! outermost radius of the outer flange
      ofRthk     = 0.15        ! thickness of the outer flange
      ofZ        = 79.0        ! Z of the outer flange inner face
      ofNholes   = 15.0        ! number of holes the outer flange
      ofHoleRad  = 26.0        ! radius at which the holes the outer flange are located
      ofHoleR    = 2.5         ! radius of the holes the outer flange
      strutLen   = 50.0        ! length of the strut
      strutHgt   = 5.0         ! length of the strut
      strutWth   = 2.0         ! width of the strut
      strutThk   = 0.25        ! thickness of the strut material

   EndFill

******************************************************
      USE      FTRG
*
      raddeg     = 3.14159265/180.0
      holeAngle  = 360.0/FTRG_ofNholes
      strutAngle = 360.0/FTRG_N
      strutRad   = FTRG_ofRin+FTRG_strutThk/2.0

      Zpos       = FTRG_ftpcZ+FTRG_ftpcLen+FTRG_length/2.0;



      Create   FTMO
      Position FTMO in CAVE z=Zpos;
* -----------------------------------------------------------------------------
Block FTMO is the mother of the single FTPC RO barrel
      Material  Air
      Attribute FTMO  Seen=1  colo=6

      Shape TUBE Rmin=FTRG_Rin Rmax=FTRG_Rout Dz=FTRG_length/2.0

      Create FTOF
      Position FTOF z=FTRG_ofZ - FTRG_length/2.0 + FTRG_ofRthk;

      Create FSTL


      do nStrut=1,FTRG_N
         angle=(nStrut-1)*strutAngle
	write(*,*) angle
         
         anglePos = angle*raddeg
         Position FSTL x=strutRad*cos(anglePos) y=strutRad*sin(anglePos) _
         z=FTRG_ofZ-FTRG_length/2.0-FTRG_strutLen/2.0 AlphaZ=90+angle
*                 AlphaZ=angle
      enddo
endblock
* -----------------------------------------------------------------------------
Block FTOF is the outer flange
      Material Aluminium
      Attribute FTOF  Seen=1  colo=2

      Shape TUBE Rmin=FTRG_ofRin Rmax=FTRG_ofRout Dz=FTRG_ofRthk/2.0

* drill the hole:
      Create FTOH
      do nHole=1,FTRG_ofNholes
         angle=(nHole-1)*holeAngle
         anglePos = angle*raddeg
         Position FTOH x=FTRG_ofHoleRad*cos(anglePos) y=FTRG_ofHoleRad*sin(anglePos)
      enddo
endblock
* -----------------------------------------------------------------------------
Block FTOH is a hole the outer flange
      Material Air
      Attribute FTOF  Seen=1  colo=2

      Shape TUBE Rmin=0.0 Rmax=FTRG_ofHoleR Dz=FTRG_ofRthk/2.0

endblock
* -----------------------------------------------------------------------------
Block FSTL is the flat part of the strut
      Material Aluminium
      Attribute FSTL  Seen=1  colo=2

      Shape BOX dX=FTRG_strutWth/2.0 dY=FTRG_strutThk/2.0 Dz=FTRG_strutLen/2.0

endblock
      END

