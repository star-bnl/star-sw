******************************************************************************
Module PIXLGEO is the geometry of the STAR pixel detector
  Created  10/09/03
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  PXMO
*
      Structure PIXG {version, Rin, Rout, Length}
*
* -----------------------------------------------------------------------------
*
   FILL PIXG             ! Pixel detector data
      version   =  1     ! geometry version     
      Rin       =  1.0   ! Inner radius
      Rout      =  2.0   ! Outer radius
      Length    =  30.0  ! Overal length of the detector
   EndFill

*
*
   USE      PIXG  
*
      Create   PXMO
      Position PXMO in SVTT

* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute Pipe   Seen=1  colo=3
      Shape TUBE Rmin=PIXG_Rin Rmax=PIXG_Rout Dz=PIXG_Length/2.0
endblock
*
      END

