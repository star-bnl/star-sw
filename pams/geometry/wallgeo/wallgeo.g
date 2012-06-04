* =============================================================================
*
  Module  WALLGEO Defines the walls, floors and ceilings of the STAR _
                  cave and tunnels.  It also creates the shield walls_
                  present in the tunnels.
*
  Author  Jason C. Webb
  Created January 29, 2010
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
*
* REPLACE [;ASSERT(#);] with [;call assert((#1), __LINE__,'wallgeo.g');]
* REPLACE [;ASSERT(#);] with []
*
* and I really need an increment operator...
*  REPLACE [(#)++] with [#1=#1+1]
*
* ==================================================================== Notes ==
* 
* This module should be used with version=5 of the CAVE geometry.  It
* implements the STAR cave in the WAH as built.  The cave creates a
* symmetric 3D polygon shape, into which WALL adds the floor, ceiling
* and walls to best approximate the actual cave and tunnel geometry.
*
*    ------------------------  ASSUMPTION --------------------------------
*
* We assume that the east and west tunnels have the same North-South
* and Top-Bottom dimenstions
*
* Note that the CAVE geometry is responsible for initializing some of the
* variables (cdim_rmax and tdim_rmax) used in this module.
*
*    ---------------------- SIMPLICIFACTION ------------------------------
* 
* We implement the tunnel sheilding using concrete blocks.  This is not 
* correct, as the shielding is a combination of concrete and stacked iron
* plate.  This is a temporary placeholder to get the geometry in place.
*
* Helpful to keep in mind STAR coordinate system:
*
* +z is WEST / blue beam direction / towards endcap
* -z is EAST / yell beam direction / away from endcap 
* +x is SOUTH 
* -x is NORTH
* +y is UP
* -y is DOWN
*
* Cave is created with Rmax = distance to south wall.  So we only need to 
* create the floor, ceiling and north walls in the cave.
*
* The beam tunnel is opposite: Rmax = distance to north wall.  So we create
* the floor, ceiling and south walls in the cave.
*
* =============================================================================
+CDE,AGECOM.
+CDE,GCUNIT.
*
  Content WALL, TWLL
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
* =============================================================================
*
  Integer id_cave
  Integer AgExist
*
  Real wall_xx, wall_yy, wall_zz, wall_dx, wall_dy, wall_dz
*
* Enumerate material types
  Replace [kConcrete] with [1]
  Replace [kSteel]    with [2]
  Integer wall_stuff, nwall

* =============================================================================
*
  Fill CDIM                                            ! Cave Dimensions
                   version  = 1.000                    ! version
                   Dceiling = 2*170.0 unit(inch)       ! dist from beam to ceiling                           <<<<<<<<< FAKE >>>>>>>>>
                   Dfloor   = 169.75 unit(inch)        ! dist from beam to floor    
                   Dnorth   = 481.9 unit(inch)         ! dist from beam to the north (shield) wall
                   Dsouth   = 620.1 unit(inch)         ! dist from beam to south wall
  EndFILL
* Rmax, and Dz filled in cavegeo 
* Rmax = distance from beam to edge of cave
* Dz   = distance from (0,0,0) to tunnel entrance
*
* =============================================================================
*
  Fill TDIM                                            ! Cave Dimensions
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
* =============================================================================
*
  Use CDIM
  Use TDIM 
*                                                    Sanity: ensure cave exists
  id_cave = AgExist('CAVE')
*  assert( id_cave .gt. 0 )
*                                                Sanity: ensure cave dimensions
*  assert( cdim_rmax .gt. tdim_rmax )
*  assert( tdim_rmax .gt. 0. )
*
  nwall = 1
*

                                                "set material proerties"
                                                 wall_stuff = kConcrete
*
* =============================================================================
*                                  STAR CAVE
* ==================================================================== Floor ==
*
  wall_dx = cdim_rmax                                   ! half width along N-S
  wall_dy = 0.5*(cdim_rmax-cdim_dfloor)                 ! half width along T-B
  wall_dz = cdim_dz                                     ! half width along E-W
  wall_xx = 0.0                                         ! center along N-S
  wall_yy = -cdim_dfloor - wall_dy                      ! center at avg floor
  wall_zz = 0.0                                         ! center along E-W
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 

*
* ================================================================== Ceiling ==
*
  wall_dx = cdim_rmax                                 ! half width along N-S
  wall_dy = 0.5*(cdim_rmax-cdim_dceiling)             ! half width along T-B
  wall_dz = cdim_dz                                   ! half width along E-W
  wall_xx = 0.0                                       ! center along N-S
  wall_yy = +cdim_dceiling + wall_dy                  ! center at avg ceiling
  wall_zz = 0.0                                       ! center along E-W
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
* ==================================================================== North ==
*
  wall_dx = 0.5*(cdim_rmax-cdim_dnorth)               ! half width along N-S
  wall_dy = cdim_Rmax                                 ! half width along T-B
  wall_dz = cdim_dz                                   ! half width along E-W
  wall_xx = -cdim_dnorth - wall_dx                    ! center along N-S
  wall_yy = 0.0                                       ! center at avg wall
  wall_zz = 0.0                                       ! center along E-W
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
* =============================================================================
*                                WEST TUNNEL
* ==================================================================== Floor ==
*
  wall_dx = tdim_rmax                                   ! half width along N-S
  wall_dy = 0.5*(tdim_rmax-tdim_dfloor)                 ! half width along T-B
  wall_dz = tdim_dz/2                                   ! half width along E-W
  wall_xx = 0.0                                         ! center along N-S
  wall_yy = -tdim_dfloor - wall_dy                      ! center at avg floor
  wall_zz = cdim_dz+wall_dz                             ! center along E-W
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
* ================================================================== Ceiling ==
*
  wall_dx = tdim_rmax                                   ! half width along N-S
  wall_dy = 0.5*(tdim_rmax-tdim_dceiling)               ! half width along T-B
  wall_dz = tdim_dz/2                                   ! half width along E-W
  wall_xx = 0.0                                         ! center along N-S
  wall_yy = +tdim_dceiling + wall_dy                    ! center at avg floor
  wall_zz = cdim_dz+wall_dz                             ! center along E-W
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
* ==================================================================== South ==
*
  wall_dx = 0.5*(tdim_rmax-tdim_dsouth)               ! half width along N-S
  wall_dy = tdim_Rmax                                 ! half width along T-B
  wall_dz = tdim_dz/2                                 ! half width along E-W
  wall_xx = + tdim_dsouth + wall_dx                   ! center along N-S
  wall_yy = 0.0                                       ! center at avg wall
  wall_zz = cdim_dz+wall_dz                           ! center along E-W
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
* =============================================================================
*                                EAST TUNNEL
* ==================================================================== Floor ==
*
  wall_dx = tdim_rmax                                   ! half width along N-S
  wall_dy = 0.5*(tdim_rmax-tdim_dfloor)                 ! half width along T-B
  wall_dz = tdim_dz/2                                   ! half width along E-W
  wall_xx = 0.0                                         ! center along N-S
  wall_yy = -tdim_dfloor - wall_dy                      ! center at avg floor
  wall_zz = cdim_dz+wall_dz                             ! center along E-W
  wall_zz = -wall_zz
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
* ================================================================== Ceiling ==
*
  wall_dx = tdim_rmax                                   ! half width along N-S
  wall_dy = 0.5*(tdim_rmax-tdim_dceiling)               ! half width along T-B
  wall_dz = tdim_dz/2                                   ! half width along E-W
  wall_xx = 0.0                                         ! center along N-S
  wall_yy = +tdim_dceiling + wall_dy                    ! center at avg floor
  wall_zz = cdim_dz+wall_dz                             ! center along E-W
  wall_zz = -wall_zz
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
* ==================================================================== South ==
*
  wall_dx = 0.5*(tdim_rmax-tdim_dsouth)               ! half width along N-S
  wall_dy = tdim_Rmax                                 ! half width along T-B
  wall_dz = tdim_dz/2                                 ! half width along E-W
  wall_xx = + tdim_dsouth + wall_dx                   ! center along N-S
  wall_yy = 0.0                                       ! center at avg wall
  wall_zz = cdim_dz+wall_dz                           ! center along E-W
  wall_zz = -wall_zz
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
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
                                                 wall_stuff = kConcrete
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
                                                "set material proerties"
                                                 wall_stuff = kSteel
*
  wall_dz = 0.5 * 45.0 unit(inch)
  wall_dx = wall_dx                                           ! same dx
  wall_dy = 0.5 * (146.0 - 65.0) unit(inch)            
*
  wall_zz = -( 538.0 + 120.0 ) unit(inch) - wall_dz
  wall_xx = wall_xx                                           ! same xx
  wall_yy = -tdim_dfloor + 65.0 unit(inch) + wall_dy
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz 
*
*
* =============================================================================
*                                  WEST SIDE
* =============================================================================
*
*                                                             82" concrete base
*                                                             beneath beam
                                                "set material proerties"
                                                 wall_stuff = kConcrete
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
                                                "set material proerties"
                                                 wall_stuff = kSteel
*
  wall_dz = 0.5 * 45.0 unit(inch)
  wall_dx = wall_dx                                           ! same dx
  wall_dy = 0.5 * (145.5 - 65.0) unit(inch)            
*
  wall_zz = ( 512.3 + 144.5 ) unit(inch) + wall_dz
  wall_xx = wall_xx                                           ! same xx
  wall_yy = -tdim_dfloor + 65.0 unit(inch) + wall_dy
*
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
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
                                                "set material proerties"
                                                 wall_stuff = kSteel
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
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
                                                "set material proerties"
                                                 wall_stuff = kConcrete
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

                                                "set material proerties"
                                                 wall_stuff = kSteel
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
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
                                                "set material proerties"
                                                 wall_stuff = kConcrete
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
                                                "set material proerties"
                                                 wall_stuff = kSteel
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
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*
                                                "set material proerties"
                                                 wall_stuff = kConcrete
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
                                                "set material proerties"
                                                 wall_stuff = kSteel
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
  Create and Position WALL in CAVE x=wall_xx y=wall_yy z=wall_zz
*  
                                                "set material proerties"
                                                 wall_stuff = kConcrete
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








   Create TWLL
* =============================================================================
  Block WALL is the floor (or bottom) of the cave
*
  IF ( wall_stuff .eq. kConcrete ) THEN
      Component Si  Z=14 A=28.08  W=1
      Component O2  Z=8  A=16     W=2
      Mixture   Concrete  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7" 
  ENDIF
  IF ( wall_stuff .eq. kSteel ) THEN
      Material Iron
      Material Iron isvol=0
  ENDIF
      Medium    Standard
      Attribute WALL seen=0
      Shape BOX dz=wall_dz dx=wall_dx dy=wall_dy
*
  EndBlock ! WALL  
* =============================================================================

* =============================================================================
  Block TWLL is the floor (or bottom) of the cave
*
      Component Si  Z=14 A=28.08  W=1
      Component O2  Z=8  A=16     W=2
      Mixture   Concrete  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7" 
      Medium    Standard
*
      Shape BOX dz=wall_dz dx=wall_dx dy=wall_dy
*
  EndBlock ! WALL  
* =============================================================================


  END
