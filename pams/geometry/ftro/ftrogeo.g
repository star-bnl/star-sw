* $Id: ftrogeo.g,v 1.2 2004/07/26 22:40:18 potekhin Exp $
* $Log: ftrogeo.g,v $
* Revision 1.2  2004/07/26 22:40:18  potekhin
* Built the struts and their assembly, and two flanges
*
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
      real strutRad, strutHoleSpacing, centerStrut, inFlangeZ
      integer nHole,nStrut

      Content  FTMO,FTOF,FTOH,FSMO,FTCM,FSTL,FSTC,FSHL,FTIF
*
      Structure FTRG {Version, N,
                               ftpcZ, ftpcLen, length, angOffset,
                               Rin,   Rout,
                               inRin,  inRout,  inRthk,
                               ofRin,  ofRout,  ofRthk, ofZ, ofNholes, ofHoleRad, ofHoleR,
                               strutLen, strutHgt, strutWth, strutThk, strutHoleR, strutNholes}
*
* -----------------------------------------------------------------------------
*
   Fill FTRG                   ! FTPC readout data
      Version    =  1          ! version
      N          =  5          ! number of sectors
      ftpcZ      = 150.0       ! FTPC face from the interaction point
      ftpcLen    = 119.0       ! FTPC overall length
      length     = 80.0        ! RO barrel length
      angOffset  =  0.0        ! theta angle offset for the whle structure
      Rin        = 20.0        ! innermost radius of envelope
      Rout       = 30.0        ! outermost radius of envelope
      inRin      = 22.0        ! innermost radius of the inner flange
      inRout     = 30.0        ! outermost radius of the inner flange
      inRthk     = 0.15        ! thickness of the inner flange
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
      strutHoleR = 1.0         ! strut hole radius
      strutNholes=5            ! number of holes in the strut

   EndFill

******************************************************
      USE      FTRG
*
      raddeg     = 3.14159265/180.0
      holeAngle  = 360.0/FTRG_ofNholes
      strutAngle = 360.0/FTRG_N
      strutRad   = FTRG_ofRin+FTRG_strutHgt/2.0
      strutHoleSpacing = FTRG_strutLen/(FTRG_strutNholes+1)
      Zpos       = FTRG_ftpcZ+FTRG_ftpcLen+FTRG_length/2.0;

      centerStrut= FTRG_ofZ-FTRG_length/2.0-FTRG_strutLen/2.0
      inFlangeZ  = centerStrut-FTRG_strutLen/2.0


      Create   FTMO
      Position FTMO in CAVE z=Zpos;
* -----------------------------------------------------------------------------
Block FTMO is the mother of the single FTPC RO barrel
      Material  Air
      Attribute FTMO  Seen=1  colo=6

      Shape TUBE Rmin=FTRG_Rin Rmax=FTRG_Rout Dz=FTRG_length/2.0

      Create and Position FTCM z=centerStrut

      Create FTOF
      Position FTOF z=FTRG_ofZ - FTRG_length/2.0 + FTRG_ofRthk;

      Create FTIF
      Position FTIF z=inFlangeZ - FTRG_inRthk/2.0

endblock
* -----------------------------------------------------------------------------
Block FTCM is the mother of the core struts and PCBs
      Attribute FTCM seen=1    colo=1

      Shape TUBE Rmin=FTRG_Rin Rmax=FTRG_Rout Dz=FTRG_strutLen/2.0

      Create FSMO

      do nStrut=1,FTRG_N
         angle=(nStrut-1)*strutAngle
         anglePos = angle*raddeg
         Position FSMO x=strutRad*cos(anglePos) y=strutRad*sin(anglePos) z=0.0 AlphaZ=90+angle
      enddo

endblock
*      Shape Division Iaxis=2 Ndiv=5
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
Block FSMO is the mother of the strut
      Material Air
      Attribute FSMO  Seen=0  colo=0

      Shape BOX dX=FTRG_strutWth/2.0 dY=FTRG_strutHgt/2.0 Dz=FTRG_strutLen/2.0
      Create FSTL
      Position FSTL x=0.0 y=-FTRG_strutHgt/2.0+FTRG_strutThk/2.0 z=0.0
      Position FSTL x=0.0 y=+FTRG_strutHgt/2.0-FTRG_strutThk/2.0 z=0.0

      Create FSTC
      Position FSTC x=0.0 y=0.0 z=0.0
endblock
* -----------------------------------------------------------------------------
Block FSTL is the flat part of the strut
      Material Aluminium
      Attribute FSTL  Seen=1  colo=2

      Shape BOX dX=FTRG_strutWth/2.0 dY=FTRG_strutThk/2.0 Dz=FTRG_strutLen/2.0

endblock
* -----------------------------------------------------------------------------
Block FSTC is the central beam of the strut
      Material Aluminium
      Attribute FSTC  Seen=1  colo=2

      Shape BOX dX=FTRG_strutThk/2.0 dY=FTRG_strutHgt/2.0-FTRG_strutThk Dz=FTRG_strutLen/2.0
      Create FSHL
      do nHole=1,FTRG_strutNholes
         Position FSHL x=0.0 y=0.0 z=-FTRG_strutLen/2.0+nHole*strutHoleSpacing  AlphaY=90
      enddo

endblock
* -----------------------------------------------------------------------------
Block FSHL is a hole the beam of the strut
      Material Air
      Attribute FSHL  Seen=1  colo=2

      Shape TUBE Rmin=0.0 Rmax=FTRG_strutHoleR Dz=FTRG_strutThk/2.0

endblock
* -----------------------------------------------------------------------------
Block FTIF is the inner flange
      Material Aluminium
      Attribute FTOF  Seen=1  colo=2

      Shape TUBE Rmin=FTRG_inRin Rmax=FTRG_inRout Dz=FTRG_inRthk/2.0

endblock
* -----------------------------------------------------------------------------
      END

