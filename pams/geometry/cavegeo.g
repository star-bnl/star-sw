*****************************************************************************
module   CAVEGEO  builds CAVE for ALICE
Author   Aleksei Pavlinov, WSU
Created  October-15-2001
* Only master volume CAVE or HALL
*****************************************************************************
+CDE,AGECOM.
CONTENT   CAVE
real      dx, dy, dz
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      create CAVE   "  no need to position it "
*------------------------------------------------------------------------------
block CAVE is  ALICE building
      Medium    Standard
      Attribute CAVE seen=1 colo=2
      SHAPE     BOX  dx=600 dy=600 dz=1000
endblock
*------------------------------------------------------------------------------
end











