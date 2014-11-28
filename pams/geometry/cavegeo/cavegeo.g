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
*
* Drawings are marked out in inches, so we define some units.
*
* Usage:
*     oneCM = 1 unit(cm)
*     oneIN = 1 unit(in)
*
  REPLACE [unit(inch)]   with [*2.5400]
  REPLACE [unit(inches)] with [*2.5400]
  REPLACE [unit(foot)]   with [*12.0*2.5400]
  REPLACE [unit(feet)]   with [*12.0*2.5400]
  REPLACE [unit(cm)]     with [*1.0]
  REPLACE [unit(m)]      with [*100.0]
  REPLACE [unit(mm)]     with [*0.100]
*****************************************************************************
module   CAVEGEO  builds CAVE for GSTAR
Author   Peter Jacobs, LBL
Created  March 10, 1995
*****************************************************************************
+CDE,AGECOM.
CONTENT   CAVE,HALL,WALL,HOLE,SHLD,CRAT,MGWP,MGWT
Structure CVCF {version, int config}
Structure CAVE {version,Rmin,Rmax(2),Dz(2),Dconc}
*Structure CELQ {version, ExtMuDTRadius, EndOfPlatform, dz, Height1, Height2, thick }
*
* Structures hodling the dimensions of the cave and tunnel.
* =====================================================================
* NOTE: Rmax will be filled when cavegeo is created, using AgDETPadd.
*       This ensures that this module gets the correct dimensions for
*       the cavegeo.
* =====================================================================
*
*                                                              Cave Dimensions
  Structure CDIM { Version,
                   Dceiling,
                   Dfloor, 
                   Dnorth,
                   Dsouth,
	           ThickCeil,
                   Rmax,
                   Dz  }
*
*                                                            Tunnel Dimensions
  Structure TDIM { Version,
                   Dceiling,
                   Dfloor,
                   Dnorth,
                   Dsouth,
                   Rmax,
                   Dz  }
*
  Structure SHLD { Version,                
                   xwidth(4),
                   ywidth(4),
                   zwidth(4) }
*
*                   
real      D1,D2,Z1,D3
*                   
* =============================================================================
*
  Integer id_cave, i, j, k
  Integer AgExist
*
  Real wall_xx, wall_yy, wall_zz, wall_dx, wall_dy, wall_dz
*

* =============================================================================
*
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
         Dz      = {807.72, 2020.0 }  ! Dz(1) is distance to east/west wall from center of STAR, Dz(2) depth into tunnels
         Dconc   = 91.44              ! concrete thickness 36"
      EndFILL                                      

* =============================================================================
*     Fill CELQ                 !  Electronics on platform
*         version       = 1.000 ! version
*         ExtMuDTRadius =  415  ! external radius of MUTD
*         EndOfPlatform = 1000  ! end of platform
*         dz            =  300  ! half length along z
*         Height1       = -150  ! 1st floor
*         Height2       =  150  ! 2nd floor
*         thick         =    5  ! 
*     EndFill
* =============================================================================
*
  Fill CDIM                                            ! Cave Dimensions
                   version  = 1.000                    ! version
                   Dceiling = 2*170.0 unit(inch)       ! dist from beam to ceiling                           <<<<<<<<< FAKE >>>>>>>>>
                   Dfloor   = 169.75 unit(inch)        ! dist from beam to floor    
                   Dnorth   = 481.9 unit(inch)         ! dist from beam to the north (shield) wall
                   Dsouth   = 620.1 unit(inch)         ! dist from beam to south wall
                   ThickCeil = 20                      ! thickness of ceiling
  EndFILL
* Rmax, and Dz filled in cavegeo 
* Rmax = distance from beam to edge of cave
* Dz   = distance from (0,0,0) to tunnel entrance
*
* =============================================================================
*
  Fill TDIM                                            ! Tunnel Dimensions
                   version  = 1.000                    ! version
                   Dceiling = 14.0 unit(foot)-50.0 unit(inch) ! dist from beam to ceiling 
                   Dfloor   = 50.0 unit(inch)          ! dist from beam to floor    
                   Dnorth   = 329.95 unit(cm)          ! dist from beam to the north wall
                   Dsouth   = 279.65 unit(cm)          ! dist from beam to south wall
  EndFILL
* Rmax, and Dz filled in cavegeo
* Rmax = distance from beam to edge of tunnel
* Dz   = distance from start of tunnel to end of tunnel                                                           (DOUBLE CHECK)
*
* =============================================================================
*
  Fill SHLD                                            ! shield dimensions
                   version  = 1.000                    ! version
                   xwidth   = {48.0 unit(inch), 
                               54.5 unit(inch), 
                               45.5 unit(inch), 
                               45.1 unit(inch) }  ! x widths
                   ywidth   = {111. unit(inch), 
                               111. unit(inch), 
                               111. unit(inch), 
                               108. unit(inch) }  ! y widths
                   zwidth   = {48.0 unit(inch), 
                               48.0 unit(inch), 
                               48.0 unit(inch), 
                               48.0 unit(inch) }      ! z widths
  EndFILL
*------------------------------------------------------------------------------
      USE    CVCF
      USE    CAVE   version=CVCF_config
      Use    CDIM
      Use    TDIM 
*      Use    CELQ
*
      Create HALL   "  no need to position it "

*------------------------------------------------------------------------------
block HALL is  GSTAR building
      component Si  Z=14 A=28.08  W=1
      component O2  Z=8  A=16     W=2
      mixture   Concrete  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7" 
      Medium    Concrete
      Attribute HALL seen=1 colo=3


  IF ( CVCF_config .ge. 5 ) THEN
      D1=cave_Rmax(1)+cave_dconc
      D2=cave_Rmax(2)+cave_dconc
      Z1=cave_dz(1)  +cave_dconc

      SHAPE PgON Phi1=45  Dphi=360  Nz=6 NPDIV=4,
            zi  ={-cave_dz(2), -Z1,-Z1, Z1, Z1, cave_dz(2)},
            rmn ={cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin,
		  cave_rmin },
            rmx ={D2,D2,D1,D1,D2,D2}

* Ceiling hole
      wall_dx = D1
      wall_dy = cave_dconc/2;
      wall_dz = Z1
      wall_yy = D1-wall_dy
      Create and Position HOLE y=wall_yy
  ELSE
      D1=cave_Rmax(1)+cave_dconc
      D2=cave_Rmax(2)+cave_dconc
      Z1=cave_dz(1)+cave_dconc

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

      Create and Position CAVE

endblock
*------------------------------------------------------------------------------
block CAVE is  GSTAR cave with subsystem envelopes
      Material  Air
      Material  Air isvol=0
      Medium    Air 
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
      CDIM_Rmax = cave_Rmax(1)
      TDIM_Rmax = cave_Rmax(2)
      CDIM_dz   = cave_dz(1)
      TDIM_dz   = cave_dz(2)-cave_dz(1)
	
* CAVE Ceiling itself
      D1=cave_Rmax(1)
      D2=cave_Rmax(2)
      Z1=cave_dz(1)
      wall_dx = D1;
      wall_dy = CDIM_ThickCeil/2;
      wall_dz = cave_dz(1);
      D3      = CDIM_Dceiling-wall_dy;
      wall_yy = D3
      Create and Position WALL y=wall_yy
* CAVE Floor
      wall_dy = (D1-CDIM_Dfloor)/2;
      Create and Position WALL y=-cave_Rmax(1)+wall_dy
* CAVE North wall
      D3 = CDIM_Dceiling - CDIM_ThickCeil
      wall_dx = 68 unit(inch) / 2;
      wall_dy = (D3 + CDIM_Dfloor)/2
      wall_xx = -CDIM_Dnorth + wall_dx
      wall_yy = (D3 - CDIM_Dfloor)/2 
      Create and Position WALL x=wall_xx y=wall_yy
* TUNNEL
      wall_zz = (cave_dz(2) + cave_dz(1))/2
      wall_dz = (cave_dz(2) - cave_dz(1))/2
      D1 = cave_Rmax(2)
* ceiling
      wall_dx = D1
      wall_dy = (D1 - TDIM_Dceiling)/2
      wall_yy = (D1 + TDIM_Dceiling)/2
      D2 = wall_yy - wall_dy
      Create and Position WALL y=wall_yy z= wall_zz
                 Position WALL y=wall_yy z=-wall_zz
* floor
      wall_dy = (D1 - TDIM_Dfloor)/2
      wall_yy = (D1 + TDIM_Dfloor)/2
      D3 = - (wall_yy - wall_dy)
      Create and Position WALL y=-wall_yy z= wall_zz
                 Position WALL y=-wall_yy z=-wall_zz
* sourth wall (north one is defined by CAVE and HALL)
      wall_dy = (D2 - D3)/2
      wall_yy = (D2 + D3)/2
      wall_dx = (TDIM_Dnorth - TDIM_Dsouth)/2
      wall_xx = (TDIM_Dnorth + TDIM_Dsouth)/2
      Create and Position WALL x=wall_xx y=wall_yy z= wall_zz
                 Position WALL x=wall_xx y=wall_yy z=-wall_zz
*
*
* =============================================================================
*                               Tunnel Shielding
* 
* The following code implements the two blocks which are located along the N
* and S walls in the two tunnels (E-S, E-N, W-S and W-N).  This is followed
* by the large shielding blocks which are stacked around the beamlines (E-B
* and W-B).
*
* ====================================================================== E-S ==
*
  wall_dx = 0.5 * shld_xwidth(1)
  wall_dy = 0.5 * shld_ywidth(1)
  wall_dz = 0.5 * shld_zwidth(1)
*
  wall_zz = -221.0 unit(inch) - wall_dz - cdim_dz
  wall_yy = -tdim_dfloor + wall_dy
  wall_xx = +tdim_dsouth - wall_dx
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
* ====================================================================== E-N ==
  wall_dx = 0.5 * shld_xwidth(2)
  wall_dy = 0.5 * shld_ywidth(2)
  wall_dz = 0.5 * shld_zwidth(2)
*
  wall_zz = -220.0 unit(inch) - wall_dz - cdim_dz
  wall_yy = -tdim_dfloor + wall_dy
  wall_xx = -tdim_dnorth + wall_dx
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
* ====================================================================== W-S ==
  wall_dx = 0.5 * shld_xwidth(3)
  wall_dy = 0.5 * shld_ywidth(3)
  wall_dz = 0.5 * shld_zwidth(3)
*
  wall_zz = +512.3 unit(inch) + wall_dz
  wall_yy = -tdim_dfloor + wall_dy
  wall_xx = +tdim_dsouth - wall_dx
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
* ====================================================================== W-N ==
  wall_dx = 0.5 * shld_xwidth(4)
  wall_dy = 0.5 * shld_ywidth(4)
  wall_dz = 0.5 * shld_zwidth(4)
*
  wall_zz = +512.3 unit(inch) + 144.25 unit(inch) - 236.0 unit(inch) _
            + wall_dz
  wall_yy = -tdim_dfloor + wall_dy
  wall_xx = -tdim_dnorth + wall_dx
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
* =============================================================================
*
*
* =============================================================================
*                                  EAST SIDE
* =============================================================================
*
*                                                             82" concrete base
*                                                             beneath beam
                                                "set material proerties"
  wall_dx = 0.5 * ( 62.0 + 77.0 ) unit(inch)
  wall_dy = 0.5 * 25.0 unit(inch)
  wall_dz = 0.5 * 82.0 unit(inch)     
*
  wall_zz = -( 538.0 + 120.0 ) unit(inch) - wall_dz
  wall_xx = +0.5 * ( 77.0 - 62.0 )
  wall_yy = -tdim_dfloor + wall_dy
*
* East side
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
*                                                             Steel stack above
*                                                             the beam pipe
*
  wall_dz = 0.5 * 45.0 unit(inch)
  wall_dx = wall_dx                                           ! same dx
  wall_dy = 0.5 * (146.0 - 65.0) unit(inch)            
*
  wall_zz = -( 538.0 + 120.0 ) unit(inch) - wall_dz
  wall_xx = wall_xx                                           ! same xx
  wall_yy = -tdim_dfloor + 65.0 unit(inch) + wall_dy
*
  Create and Position SHLD in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
*
* =============================================================================
*                                  WEST SIDE
* =============================================================================
*
*                                                             82" concrete base
*                                                             beneath beam
                                                "set material proerties"
  wall_dx = 0.5 * ( 60.5 + 77.5 ) unit(inch)
  wall_dy = 0.5 * 25.0 unit(inch)
  wall_dz = 0.5 * 81.0 unit(inch)     
*
  wall_zz = (512.3 + 144.5) unit(inch) + wall_dz
  wall_xx = +0.5 * ( 77.5 - 60.5 )
  wall_yy = -tdim_dfloor + wall_dy
*
* East side
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
*                                                             Steel stack above
*                                                             the beam pipe
*
  wall_dz = 0.5 * 45.0 unit(inch)
  wall_dx = wall_dx                                           ! same dx
  wall_dy = 0.5 * (145.5 - 65.0) unit(inch)            
*
  wall_zz = ( 512.3 + 144.5 ) unit(inch) + wall_dz
  wall_xx = wall_xx                                           ! same xx
  wall_yy = -tdim_dfloor + 65.0 unit(inch) + wall_dy
*
  Create and Position SHLD in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
*
*****************************************************************************
*
* The following are guesstimate assuming that the scale of the drawings in XY
* are reasonable... they are to within 10%, but this is only an approximation
* Need better drawings of the shield walls.
*
*****************************************************************************

*
* =============================================================================
*                                    EAST SIDE
* =============================================================================
*                                                             Steel stack around
*                                                             the beam pipe
* cubs side of beam pipe  (north side)
  wall_dz = 0.5 * 45.0 unit(inch)                             ! exact
  wall_dy = 0.5 * (65.0 - 25.0 ) unit(inch)                   ! very good approx
  wall_dx = 0.5 * (10.0 / 28.5) * (62.+77.) unit(inch)        ! WAG (wild ass guess)
*
  wall_zz = -(538.0 + 120.0 + 82.0) unit(inch)               _
            + wall_dz                                         ! exact
        
  wall_xx = +4.0 * 4.877 unit(inch) + wall_dx                 ! WAG
  wall_yy = -tdim_dfloor + 25.0 unit(inch) + wall_dy          ! very good approx
*
  Create and Position SHLD in CAVE x=wall_xx y=wall_yy z=wall_zz
*
                                                "set material proerties"
  wall_dz = 0.5 * (82.0-45.0) unit(inch)
  wall_dx = wall_dx
  wall_dy = wall_dy
*
  wall_zz = -(538.0+120.0) unit(inch)                         _
            -wall_dz                                           ! exact
  wall_xx = wall_xx
  wall_yy = wall_yy
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
* white sox side of beam pipe (south side)
  wall_dz = 0.5 * 45.0 unit(inch)                             ! exact
  wall_dy = 0.5 * (65.0 - 25.0 ) unit(inch)                   ! very good approx
  wall_dx = 0.5 * (7.0 / 28.5) * (62.+77.) unit(inch)                     ! WAG (wild ass guess)
*
  wall_zz = -(538.0 + 120.0 + 82.0) unit(inch)               _
            + wall_dz                                         ! exact
  wall_xx = -4.0 * 4.877 unit(inch) - wall_dx                  ! WAG
  wall_yy = -tdim_dfloor + 25.0 unit(inch) + wall_dy          ! very good approx
*
  Create and Position SHLD in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
                                                "set material proerties"
  wall_dz = 0.5 * (82.0-45.0) unit(inch)
  wall_dx = wall_dx
  wall_dy = wall_dy
*
  wall_zz = -(538.0+120.0) unit(inch)                         _
            -wall_dz                                           ! exact
  wall_xx = wall_xx
  wall_yy = wall_yy
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*  



*
* =============================================================================
*                                    WEST SIDE
* =============================================================================
*                                                             Steel stack around
*                                                             the beam pipe
* cubs side of beam pipe  (north side)
  wall_dz = 0.5 * 45.0 unit(inch)                             ! exact
  wall_dy = 0.5 * (65.0 - 25.0 ) unit(inch)                   ! very good approx
  wall_dx = 0.5 * (10.0 / 28.5) * (62.+77.) unit(inch)        ! WAG (wild ass guess)
*
  wall_zz = +(512.3 + 144.25 + 81.0) unit(inch)                     _
            - wall_dz                                         ! exact
        
  wall_xx = +4.0 * 4.877 unit(inch) + wall_dx                 ! WAG
  wall_yy = -tdim_dfloor + 25.0 unit(inch) + wall_dy          ! very good approx
*
  Create and Position SHLD in CAVE x=wall_xx y=wall_yy z=wall_zz
*
                                                "set material proerties"
  wall_dz = 0.5 * (81.0-45.0) unit(inch)
  wall_dx = wall_dx
  wall_dy = wall_dy
*
  wall_zz = +(512.3+144.25+(81.0-45.0)) unit(inch)                         _
            -wall_dz                                           ! exact
  wall_xx = wall_xx
  wall_yy = wall_yy
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
*
* white sox side of beam pipe (south side)
  wall_dz = 0.5 * 45.0 unit(inch)                             ! exact
  wall_dy = 0.5 * (65.0 - 25.0 ) unit(inch)                   ! very good approx
  wall_dx = 0.5 * (7.0 / 28.5) * (62.+77.) unit(inch)                     ! WAG (wild ass guess)
*
  wall_zz = +(512.3 + 144.25 + 81.0) unit(inch)                     _
            - wall_dz                                         ! exact
  wall_xx = -4.0 * 4.877 unit(inch) - wall_dx                  ! WAG
  wall_yy = -tdim_dfloor + 25.0 unit(inch) + wall_dy          ! very good approx
*
  Create and Position SHLD in CAVE x=wall_xx y=wall_yy z=wall_zz
  wall_dz = 0.5 * (81.0-45.0) unit(inch)
  wall_dx = wall_dx
  wall_dy = wall_dy
*
  wall_zz = +(512.3+144.25+(81.0-45.0)) unit(inch)                         _
            -wall_dz                                           ! exact
  wall_xx = wall_xx
  wall_yy = wall_yy
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
*
* =============================================================================
*                                    Electronics
* =============================================================================
*   wall_dx = (CELQ_EndOfPlatform - CELQ_ExtMuDTRadius)/2
*   wall_xx = (CELQ_EndOfPlatform + CELQ_ExtMuDTRadius)/2
*   wall_dy =  CELQ_thick/2
*   wall_dz =  CELQ_dz
*   Create and Position SHLD in CAVE x=wall_xx y=CELQ_Height1
*              Position SHLD in CAVE x=wall_xx y=CELQ_Height2
    D1 = 415  ! ExtMuDTRadius
    D2 = 1000 ! CELQ_EndOfPlatform
    D3 = 5    ! CELQ_thick/2
    wall_dx = (D2 - D1)/2
    wall_xx = (D2 + D1)/2
    wall_dy =  D3/2
    wall_dz =  500  ! CELQ_dz
    Create and Position SHLD in CAVE x=wall_xx y=-150-D3/2 ! CELQ_Height1
               Position SHLD in CAVE x=wall_xx y= 150-D3/2 ! CELQ_Height2
    D1 = 415  ! ExtMuDTRadius
    D2 = 600  ! CELQ_EndOfPlatform
    wall_dx = (D2 - D1)/2
    wall_xx = (D2 + D1)/2
    Create and Position SHLD in CAVE x=-wall_xx y=-150-D3/2 ! CELQ_Height1
               Position SHLD in CAVE x=-wall_xx y= 150-D3/2 ! CELQ_Height2

    wall_dx =  40;
    wall_dy = 120;
    wall_dz =  60;
    Create CRAT
    do i = 1, 2 ! level
      wall_yy = -150 + 300*(i-1) + wall_dy
      do j = 1, 2 ! row
	wall_xx = 600 + 300*(j-1) 
        do k = 1, 8
          wall_zz = -260 + 65*(k-1)
          Position CRAT in CAVE x=wall_xx y=wall_yy z=wall_zz
        enddo
      enddo 
    end do
    
* ============   Water =============
    Create MGWP
    do i = 1, 2
      wall_xx = (2*i-3)*450
      do j = 1, 2
        wall_yy = (2*j-3)*100
	Position MGWP in CAVE x=wall_xx y=wall_yy 
      enddo
    end do
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
* =============================================================================
Block WALL is the floor (or bottom) of the cave
*
      Component Si  Z=14 A=28.08  W=1
      Component O2  Z=8  A=16     W=2
      Mixture   Concrete  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7" 
      Medium    Concrete
      Attribute WALL seen=0 colo=3
      Shape BOX dz=wall_dz dx=wall_dx dy=wall_dy
*
EndBlock ! WALL
* =============================================================================
block SHLD is the shielding

      Material  Iron
      Material  Iron isvol=0
      Medium    Iron
      Attribute SHLD seen=0 colo=2
      Shape BOX dz=wall_dz dx=wall_dx dy=wall_dy

endblock ! SHLD
* =============================================================================
Block HOLE is the floor (or bottom) of the cave
*
      Material Air
      Material Air isvol=0
      Medium   Air
      Attribute HOLE seen=0 colo=5
      Shape BOX dz=wall_dz dx=wall_dx dy=wall_dy
*
EndBlock ! HOLE  
* =============================================================================
Block CRAT is the electronics crate
*
*   G10 is given as 60% SiO2 and 40% epoxy in ftpcgeo.g, from which
*   the following is taken
      Component  Si  A=28.08  Z=14  W=0.6*1*28./60.
      Component  O   A=16     Z=8   W=0.6*2*16./60.
      Component  C   A=12     Z=6   W=0.4*8*12./174.
      Component  H   A=1      Z=1   W=0.4*14*1./174.
      Component  O   A=16     Z=8   W=0.4*4*16./174.
      Mixture Electroncs  Dens=0.13
      Medium  Electronics
      Attribute CRAT seen=0 colo=6
      Shape BOX dz=wall_dz dx=wall_dx dy=wall_dy
*
EndBlock ! HOLE  
Block MGWP is the magnet water pipe
      Material  Iron
      Medium    Iron
      Attribute WGWP seen=0 colo=6
      Shape TUBE dz=312 Rmin=0 Rmax=12
      Create and Position MGWT
*
EndBlock ! HOLE  
Block MGWT is the water in magnet water pipe
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Medium    Water
      Attribute MGWT seen=0 colo=6
      Shape TUBE dz=310 Rmin=0 Rmax=10
*
EndBlock ! HOLE  
end
