*****************************************************************************
module   CAVEGEO  builds CAVE for GSTAR
Author   Peter Jacobs, LBL
Created  March 10, 1995
*****************************************************************************
+CDE,AGECOM.
CONTENT   CAVE
Structure CAVE {version,Rmin,Rmax,Dz}
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      fill CAVE              !  STAR CAVE GEOMETRY
         version = 1             ! geometry version
         Rmin    = 0             ! inner radius
         Rmax    = 400           ! outer radius
         Dz      = 810           ! half length
      endfill

*------------------------------------------------------------------------------
      USE    CAVE   version=1
      create CAVE   "  no need to position it "
*------------------------------------------------------------------------------
block CAVE is  GSTAR cave with subsystem envelopes
      material  Air
      Medium    Standard
      Attribute CAVE  seen=0 colo=2
      SHAPE     TUBE  rmin=cave_rmin  rmax=cave_rmax  dz=cave_dz
endblock
*------------------------------------------------------------------------------
end











