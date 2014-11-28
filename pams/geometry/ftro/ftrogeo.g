* $Id: ftrogeo.g,v 1.5 2011/02/17 20:39:12 jwebb Exp $
* $Log: ftrogeo.g,v $
* Revision 1.5  2011/02/17 20:39:12  jwebb
* Renamed "raddeg" => "myraddeg" to prevent name clash with standard starsim
* constant (with exactly the opposite definition).
*
* Revision 1.4  2004/10/28 22:19:50  potekhin
* There are two cages, one on each respective
* FTPC module, so we have to position it twice
*
* Revision 1.3  2004/07/27 17:53:13  potekhin
* A large number of refinements and additions, notably the divisions
* in the central mother volume to improve efficiency, outer shell,
* aluminum rim added, strut covers (planks), and a minor positioning
* bug corrected
*
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
      real Zpos, myraddeg, holeAngle, strutAngle, anglePos, angle
      real strutRad, plankRad,  strutHoleSpacing, centerStrut, inFlangeZ
      real ftpoAnglePos,ftpiAnglePos, a, rEff

      integer nHole,nStrut

      Content  FTMO,FTOF,FTOH,FSMO,FTCM,FTCD,
               FSTL,FSTC,FSHL,FTIF,FTPO,FTPI,
               FTSH,FTRM,FTPL
*
      Structure FTRG {Version, N,
                               ftpcZ,   ftpcLen,  length,  angOffset,
                               Rin,     Rout,
                               inRin,   inRout,   inRthk,
                               ofRin,   ofRout,   ofRthk,  ofZ,
                               ofNholes,ofHoleRad,ofHoleR,
                               strutLen,strutHgt, strutWth,strutThk, strutHoleR, strutNholes,
                               ftpoWth, ftpoThk,  ftpoR,
                               ftpiWth, ftpiThk,  ftpiR,
                               shellThk,rimThk,   rimWth,
                               plankWth,plankThk }
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
      Rin        = 15.0        ! innermost radius of envelope
      Rout       = 30.0        ! outermost radius of envelope
      inRin      = 22.0        ! innermost radius of the inner flange
      inRout     = 30.0        ! outermost radius of the inner flange
      inRthk     = 0.15        ! thickness of the inner flange
      ofRin      = 20.0        ! innermost radius of the outer flange
      ofRout     = 30.0        ! outermost radius of the outer flange
      ofRthk     = 0.15        ! thickness of the outer flange
      ofZ        = 79.0        ! Z of the outer flange inner face
      ofNholes   = 15.0        ! number of holes the outer flange
      ofHoleRad  = 26.0        ! radius at which the holes the outer flange are located
      ofHoleR    = 2.5         ! radius of the holes the outer flange
      strutLen   = 50.0        ! length of the strut
      strutHgt   = 6.5         ! length of the strut
      strutWth   = 2.0         ! width of the strut
      strutThk   = 0.25        ! thickness of the strut material
      strutHoleR = 1.0         ! strut hole radius
      strutNholes= 5           ! number of holes in the strut
      ftpoWth    = 13.0        ! outer half-PCB width
      ftpoThk    = 0.3         ! outer half-PCB thickness
      ftpoR      = 22.0        ! outer half-PCB radial position (to surface)
      ftpiWth    = 12.0        ! inner half-PCB width
      ftpiThk    = 0.3         ! inner half-PCB thickness
      ftpiR      = 20.0        ! inner half-PCB radial position (to surface)
      shellThk   = 0.1         ! outer protective shell thickness
      rimThk     = 0.1         ! cylindrical tim thickness
      rimWth     = 5.0         ! cylindrical tim width
      PlankWth   = 8.0         ! the width of the plank covering the strut
      PlankThk   = 0.15        ! the thickness of the plank covering the strut
   EndFill

******************************************************
      USE      FTRG
*
*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
        Component Si  A=28.08  Z=14   W=0.6*1*28./60.
        Component O   A=16     Z=8    W=0.6*2*16./60.
        Component C   A=12     Z=6    W=0.4*8*12./174.
        Component H   A=1      Z=1    W=0.4*14*1./174.
        Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10   Dens=1.7


      myraddeg     = 3.14159265/180.0
      holeAngle  = 360.0/FTRG_ofNholes
      strutAngle = 360.0/FTRG_N
      strutRad   = FTRG_ofRin+FTRG_strutHgt/2.0
      plankRad   = strutRad+FTRG_strutHgt/2.0+FTRG_plankThk/2.0
      strutHoleSpacing = FTRG_strutLen/(FTRG_strutNholes+1)
      Zpos       = FTRG_ftpcZ+FTRG_ftpcLen+FTRG_length/2.0;

      centerStrut= FTRG_ofZ-FTRG_length/2.0-FTRG_strutLen/2.0
      inFlangeZ  = centerStrut-FTRG_strutLen/2.0

      ftpoAnglePos=atan(FTRG_ftpoWth/((FTRG_ftpoR+FTRG_ftpoThk/2.0)*2.0));
      ftpiAnglePos=atan(FTRG_ftpiWth/((FTRG_ftpiR+FTRG_ftpiThk/2.0)*2.0));

*
      Create   FTMO
      Position FTMO in CAVE z= Zpos;
      Position FTMO in CAVE z=-Zpos AlphaZ=180.;
* -----------------------------------------------------------------------------
Block FTMO is the mother of the single FTPC RO barrel
      Material  Air
      Attribute FTMO  Seen=1  colo=6

      Shape TUBE Rmin=FTRG_Rin Rmax=FTRG_Rout Dz=FTRG_length/2.0

      Create and Position FTCM z=centerStrut

      Create FTOF
      Position FTOF x=0.0 y=0.0 z=FTRG_ofZ-FTRG_length/2.0+FTRG_ofRthk;

      Create FTIF
      Position FTIF x=0.0 y=0.0 z=inFlangeZ-FTRG_inRthk/2.0

      Create FTRM
      Position FTRM x=0.0 y=0.0 z=inFlangeZ-FTRG_inRthk-FTRG_rimWth/2.0

endblock
* -----------------------------------------------------------------------------
Block FTCM is the mother of the core struts and PCBs
      Attribute FTCM  Seen=0  colo=0

      Shape TUBE Rmin=FTRG_Rin Rmax=FTRG_Rout Dz=FTRG_strutLen/2.0

      Create FTCD

*      do nStrut=1,FTRG_N
*         angle=(nStrut-1)*strutAngle
*         anglePos = angle*myraddeg
*         Position FSMO x=strutRad*cos(anglePos) y=strutRad*sin(anglePos) z=0.0 AlphaZ=90+angle
*      enddo

endblock
* -----------------------------------------------------------------------------
Block FTCD is the division of the FTCM

      Shape Division Iaxis=2 Ndiv=FTRG_N

* struts are easy to position, right in the middle
      Create   FSMO
      Position FSMO x=strutRad*cos(0.0) y=strutRad*sin(0.0) z=0.0 AlphaZ=90

* planks
      Create   FTPL
      Position FTPL x=plankRad*cos(0.0) y=plankRad*sin(0.0) z=0.0 AlphaZ=90

* PCB
      a=myraddeg*strutAngle/2.0-ftpoAnglePos
      rEff=(FTRG_ftpoR+FTRG_ftpoThk/2.0)/cos(ftpoAnglePos)

      Create FTPO
      Position FTPO x=rEff*cos(a)  y=rEff*sin(a)  z=0.0 AlphaZ=90+strutAngle/2.0
      Position FTPO x=rEff*cos(-a) y=rEff*sin(-a) z=0.0 AlphaZ=-90-strutAngle/2.0

      a=myraddeg*strutAngle/2.0-ftpiAnglePos
      rEff=(FTRG_ftpiR+FTRG_ftpiThk/2.0)/cos(ftpiAnglePos)

      Create FTPI
      Position FTPI x=rEff*cos(a)  y=rEff*sin(a)  z=0.0 AlphaZ=90+strutAngle/2.0
      Position FTPI x=rEff*cos(-a) y=rEff*sin(-a) z=0.0 AlphaZ=-90-strutAngle/2.0

      Create and Position FTSH

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
         anglePos = angle*myraddeg
         Position FTOH x=FTRG_ofHoleRad*cos(anglePos) y=FTRG_ofHoleRad*sin(anglePos)
      enddo
endblock
* -----------------------------------------------------------------------------
Block FTIF is the inner flange
      Material Aluminium
      Attribute FTIF  Seen=1  colo=2

      Shape TUBE Rmin=FTRG_inRin Rmax=FTRG_inRout Dz=FTRG_inRthk/2.0

endblock
* -----------------------------------------------------------------------------
Block FTRM is the rim connected to the inner flange
      Material Aluminium
      Attribute FTRM  Seen=1  colo=2

      Shape TUBE Rmin=FTRG_inRin Rmax=FTRG_inRin+FTRG_rimThk Dz=FTRG_rimWth/2.0

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
Block FTPL is the plank covering the strut
      Material Aluminium
      Attribute FTPL  Seen=1  colo=1

      Shape BOX dX=FTRG_plankWth/2.0 dY=FTRG_plankThk/2.0 Dz=FTRG_strutLen/2.0
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
Block FTPO is the outer PCB
      Material G10
      Attribute FTPO  Seen=1  colo=3

      Shape BOX dX=FTRG_ftpoWth/2.0 dY=FTRG_ftpoThk/2.0 dZ=FTRG_strutLen/2.0

endblock
* -----------------------------------------------------------------------------
Block FTPI is the inner PCB
      Material G10
      Attribute FTPI  Seen=1  colo=3

      Shape BOX dX=FTRG_ftpiWth/2.0 dY=FTRG_ftpiThk/2.0 dZ=FTRG_strutLen/2.0

endblock
* -----------------------------------------------------------------------------
Block FTSH is the protective shell
      Material Aluminium
      Attribute FTSH  Seen=1  colo=2

      Shape TUBS Rmin=FTRG_ofRout-FTRG_shellThk Rmax=FTRG_ofRout _
      DZ=FTRG_strutLen/2.0 _
      Phi1=-strutAngle/2.0 Phi2=strutAngle/2.0

endblock

* -----------------------------------------------------------------------------
* Obsolete, but keep as reference:
*      do nStrut=1,FTRG_N
*         angle=(nStrut-1)*strutAngle
*         anglePos = angle*myraddeg
*         Position FSMO x=strutRad*cos(anglePos) y=strutRad*sin(anglePos) z=0.0 AlphaZ=90+angle
*      enddo
* -----------------------------------------------------------------------------

      END

