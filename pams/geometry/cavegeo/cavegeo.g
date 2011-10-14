* $Id: cavegeo.g,v 1.8 2010/02/04 20:50:50 jwebb Exp $
*
* $Log: cavegeo.g,v $
* Revision 1.8  2010/02/04 20:50:50  jwebb
* Update of the CAVE geometry:
*
* + New version 5 of the cave is implmented.
* + Shape is a 4-sided PGON.  Size of the PGON corresponds to the distance
*   from the beam pipe to the most distant wall in the cave or tunnel.
* + This version is intended to be created along with the wallgeo geometry,
*   which adds floors, ceiling, walls and shielding blocks such that the
*   dimensions of the cave and tunnel better reflect the wide angle hall.
*
* Existing geometry tags are unaffected by this code.
*
* Revision 1.7  2007/02/07 20:45:33  potekhin
* As advertised before, I'm changing the cave size as per
* Akio and Ermes' request to make it larger, once more.
*
* Revision 1.6  2007/02/02 18:22:41  potekhin
* Extended the length of the main Cave volueme, to better
* reflect the real size. This will be refined once the
* more precise data becomes available.
*
* Revision 1.5  2006/03/22 00:17:00  potekhin
* Need to expand the radius of the "CAVE" (which is an artificial
* number in the first place) to accomodate teh mupn trigger system.
* This is properly versioned for steering.
*
* Revision 1.4  2005/08/16 01:00:12  potekhin
* Shielding study: we need a bigger cave (tm).
* Created a version of the cave, steered from geometry.g,
* which is longer and can accomodate magnets and such.
*
* Added the CVS log lines, 10 years after the fact.
*
*****************************************************************************
module   CAVEGEO  builds CAVE for GSTAR
Author   Peter Jacobs, LBL
Created  March 10, 1995
*****************************************************************************
+CDE,AGECOM.
CONTENT   CAVE,HALL
Structure CVCF {version, int config}
Structure CAVE {version,Rmin,Rmax(2),Dz(2),Dconc}
real      D1,D2,Z1
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Fill CVCF              !  CAVE CONFIGURATION
         version = 1         !  version
         config  = 1         !  default config
      EndFill
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Fill CAVE              !  STAR CAVE GEOMETRY
         version = 1             ! geometry version
         Rmin    = 0             ! inner radius
         Rmax    = {400,100}          ! outer radius
         Dz      = {800,2000}         ! half length
         Dconc   = 20                 ! concrete thickness
      EndFill 

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Fill CAVE              !  STAR CAVE GEOMETRY
         version = 2             ! geometry version
         Rmin    = 0             ! inner radius
         Rmax    = {400,213}          ! outer radius
         Dz      = {800,5000}         ! half length
         Dconc   = 50                 ! concrete thickness
      EndFill 

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Fill CAVE              !  STAR CAVE GEOMETRY
         version = 3             ! geometry version
         Rmin    = 0             ! inner radius
         Rmax    = {450,100}          ! outer radius
         Dz      = {800,2000}         ! half length
         Dconc   = 50                 ! concrete thickness
      EndFill 

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Fill CAVE              !  STAR CAVE GEOMETRY
         version = 4             ! geometry version
         Rmin    = 0             ! inner radius
         Rmax    = {450,100}          ! outer radius
         Dz      = {950,2000}         ! half length
         Dconc   = 50                 ! concrete thickness
      EndFill 
*------------------------------------------------------------------------------
*                                                              == Version 5 ==
* Author Jason C. Webb (jwebb@bnl.gov)
*
* Based on drawing: "STAR TUNNEL SHIELDING" Feb 23 2009 Rev. D
*
      Fill CAVE                       ! Star CAVE Geometry
         version = 5                  ! version
         Rmin    = 0                  ! Inner radius of cave had better be zero
         Rmax    = {1575.02, 329.95 } ! furthest distance to a concrete wall
         Dz      = {807.72, 4000.0 }  ! Dz(1) is distance to east/west wall from center of STAR, Dz(2) depth into tunnels
         Dconc   = 50.0               ! concrete thickness
      EndFILL                                      

*------------------------------------------------------------------------------
      USE    CVCF
      USE    CAVE   version=CVCF_config
*
      Create HALL   "  no need to position it "
*
*     When using the latest version of the cave, communicate the
*     cave dimensions to wallgeo.g to ensure proper dimensioing
*     of walls, floors, etc...      
      IF ( CAVE_version == 5 ) THEN
         Call AgDETPnew( 'WALL' )
         Call AgDETPadd( 'cdim(1).rmax=', cave_rmax(1), 1 )
         Call AgDETPadd( 'tdim(1).rmax=', cave_rmax(2), 1 )
         Call AgDETPadd( 'cdim(1).dz=', cave_dz(1), 1 )
         Call AgDETPadd( 'tdim(1).dz=', cave_dz(2)-cave_dz(1), 1 )
      ENDIF



*------------------------------------------------------------------------------
block HALL is  GSTAR building

      component Si  Z=14 A=28.08  W=1
      component O2  Z=8  A=16     W=2
      mixture   Concrete  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7" 
      Medium    Standard
      Attribute HALL seen=1 colo=2

      D1=cave_Rmax(1)+cave_dconc
      D2=cave_Rmax(2)+cave_dconc
      Z1=cave_dz(1)+cave_dconc

  IF ( CVCF_config .ge. 5 ) THEN

      SHAPE PgON Phi1=45  Dphi=360  Nz=6 NPDIV=4,
            zi  ={-cave_dz(2), -Z1,-Z1, Z1, Z1, cave_dz(2)},
            rmn ={cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin },
            rmx ={D2,D2,D1,D1,D2,D2}

  ELSE

      SHAPE PcON Phi1=0  Dphi=360  Nz=6,
            zi  ={-cave_dz(2), -Z1,-Z1, Z1, Z1, cave_dz(2)},
            rmn ={cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin },
            rmx ={D2,D2,D1,D1,D2,D2}


  ENDIF

      Write(*,*) 'CAVE config:', CVCF_config

      Create and Position CAVE

endblock
*------------------------------------------------------------------------------
block CAVE is  GSTAR cave with subsystem envelopes
      material  Air
      Medium    Standard
      Medium    something   stemax=100
      Attribute CAVE seen=1 colo=2

  IF ( CVCF_config .ge. 5 ) THEN

      SHAPE     PGON Phi1=45 Dphi=360 NpDiv=4 DPhi=360, 
                zi ={ -cave_dz(2),
                      -cave_dz(1),
                      -cave_dz(1), 
                      +cave_dz(1), 
                      +cave_dz(1), 
                      +cave_dz(2)  },
                rmx={ +cave_Rmax(2),
                      +cave_Rmax(2),
                      +cave_Rmax(1),
                      +cave_Rmax(1),
                      +cave_Rmax(2),
                      +cave_Rmax(2) }

  ELSE

      SHAPE     PCON Phi1=0 Dphi=360,
                zi ={ -cave_dz(2),
                      -cave_dz(1),
                      -cave_dz(1), 
                      +cave_dz(1), 
                      +cave_dz(1), 
                      +cave_dz(2)  },
                rmx={ +cave_Rmax(2),
                      +cave_Rmax(2),
                      +cave_Rmax(1),
                      +cave_Rmax(1),
                      +cave_Rmax(2),
                      +cave_Rmax(2) }

  ENDIF

endblock
*------------------------------------------------------------------------------
end











