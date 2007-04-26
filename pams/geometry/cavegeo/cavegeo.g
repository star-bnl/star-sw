*****************************************************************************
module   CAVEGEO  builds CAVE for GSTAR
Author   Peter Jacobs, LBL
Created  March 10, 1995
*****************************************************************************
+CDE,AGECOM.
CONTENT   CAVE,HALL
Structure CAVE {version,Rmin,Rmax(2),Dz(2),Dconc}
real      D1,D2,Z1
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      fill CAVE              !  STAR CAVE GEOMETRY
         version = 1             ! geometry version
         Rmin    = 0             ! inner radius
         Rmax    = {400,100}          ! outer radius
         Dz      = {800,2000}         ! half length
         Dconc   = 20                 ! concrete thickness
      endfill 

*------------------------------------------------------------------------------
      USE    CAVE   version=1
      create HALL   "  no need to position it "
*------------------------------------------------------------------------------
block HALL is  GSTAR building
      component Si  Z=14 A=28.08  W=1
      component O2  Z=8  A=16     W=2
      mixture   Concrete  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7
      Medium    Standard
      Attribute HALL seen=1 colo=2
      D1=cave_Rmax(1)+cave_dconc
      D2=cave_Rmax(2)+cave_dconc
      Z1=cave_dz(1)+cave_dconc
      SHAPE     PCON Phi1=0  Dphi=360  Nz=6,
      zi  ={-cave_dz(2),-Z1,-Z1, Z1, Z1, cave_dz(2)},
      rmn ={cave_rmin,cave_rmin,cave_rmin,cave_rmin,cave_rmin,cave_rmin },
      rmx ={D2,D2,D1,D1,D2,D2}
      create and position CAVE
endblock
*------------------------------------------------------------------------------
block CAVE is  GSTAR cave with subsystem envelopes
      material  Air
      Medium    Standard
      Medium    something   stemax=100
      Attribute CAVE seen=1 colo=2
      SHAPE     PCON _
      zi  ={-cave_dz(2),-cave_dz(1),-cave_dz(1), 
             cave_dz(1), cave_dz(1), cave_dz(2)},
      rmx ={cave_Rmax(2),cave_Rmax(2),cave_Rmax(1),
            cave_Rmax(1),cave_Rmax(2),cave_Rmax(2)}
*     SHAPE     TUBE  rmin=cave_rmin  rmax=cave_rmax  dz=cave_dz
endblock
*------------------------------------------------------------------------------
end











