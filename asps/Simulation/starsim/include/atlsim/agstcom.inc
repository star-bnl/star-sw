*
* $Id: agstcom.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
*
* agstcom.inc
*
#if defined(ATLAS_INFO)
************************************************************************
gstar_event_input.inc		
gstar_evthead_ubuf.inc
gstar_vertex_ubuf.inc
gstar_track_ubuf.inc		

gstar_cfio.inc			
gstar_user_cards.inc		
gstar_geant_prim_vert.inc	
gstar_udecay.inc
gstar_decay_modes.inc
gstar_shower_tracks.inc
gstar_pid.inc			

==========================
>  gst/inc/eg_event_str_def.inc  is OBSOLETE
==========================
      STRUCTURE /eg_run_t/   !  Event generator run structure - OBSOLETE
        INTEGER*4   eg_number              ! event generator identification
        REAL*4      eg_version             ! version of event generator
        REAL*4      sqrts                  ! center of mass energy
        REAL*4      b_max                  ! maximal impact parameter
        INTEGER*4   proj_a                 ! projectile mass number
        INTEGER*4   proj_z                 ! projectile charge
        INTEGER*4   targ_a                 ! target mass number
        INTEGER*4   targ_z                 ! target charge
      END STRUCTURE
*
      STRUCTURE /eg_event_t/   !  Event generator event structure - OBSOLETE
        INTEGER*4   i_event                ! event number
        INTEGER*4   n_part_prot            ! participant protons
        INTEGER*4   n_part_neut            ! participant neutrons
        REAL*4      b_impact               ! impact parameter
        INTEGER*4   n_track                ! # tracks
        INTEGER*4   n_track                ! # tracks
        INTEGER*4   n_vertex               ! # vertices
        INTEGER*4   n_fs_track             ! # final state tracks
        INTEGER*4   n_not_fs_track         ! # non-final state tracks
        INTEGER*4   n_primary_vertex       ! # primary vertices
        INTEGER*4   n_fs_vertex            ! # non-final state vertices
        INTEGER*4   p_first_primary_vertex ! pointer to ll of primary vertices
        INTEGER*4   p_first_fs_vertex      ! pointer to ll of final state vert.
      END STRUCTURE
==========================
>  g2t/inc/eg_event_str_def.inc  <
==========================
     STRUCTURE /eg_run_t/ !  Event generator run structure
        INTEGER*4      generator              ! event generator identification
        CHARACTER*32   eg_name                ! event generator name
        REAL*4         eg_version             ! version of event generator
        INTEGER*4      eg_run                 ! generator run number
        INTEGER*4      eg_rndm(2)             ! generator random numbers
        REAL*4         sqrts                  ! center of mass energy
        INTEGER*4      is_minbias             ! minimum bias flag
        REAL*4         b_min                  ! minimum impact parameter
        REAL*4         b_max                  ! maximum impact parameter
        INTEGER*4      east_a                 ! projectile 1 mass number
        INTEGER*4      east_z                 ! projectile 1 charge
        INTEGER*4      west_a                 ! projectile 2 mass number
        INTEGER*4      west_z                 ! projectile 2 charge
        INTEGER*4      polarization_run(10)   ! to be defined
      END STRUCTURE
*
     STRUCTURE /eg_event_t/ !  Event generator event structure
        INTEGER*4   n_event                  ! eg event number
        REAL*4      b_impact                 ! actual impact parameter
        REAL*4      phi_impact               ! reaction plane
        INTEGER*4   event_type               ! trigger, minbias bkgd, cosmic, etc.
        INTEGER*4   polarization_evt(10)     ! to be defined
        INTEGER*4   n_part_prot_east         ! number of participant protons
        INTEGER*4   n_part_neut_east         ! number of participant neutrons
        INTEGER*4   n_part_prot_west         ! number of participant protons
        INTEGER*4   n_part_neut_west         ! number of participant neutrons
        INTEGER*4   n_track                  ! # tracks
        INTEGER*4   n_vertex                 ! # vertices
        INTEGER*4   n_fs_track               ! # final state tracks
        INTEGER*4   n_not_fs_track           ! # non-final state tracks
        INTEGER*4   n_primary_vertex         ! # primary vertices
        INTEGER*4   n_fs_vertex              ! # non-final state vertices
        INTEGER*4   p_first_primary_vertex   ! pointer to ll of primary vertices
        INTEGER*4   p_first_fs_vertex        ! pointer to ll of final state vert.
      END STRUCTURE
*
      STRUCTURE /eg_track_t/            !  Event generator track structure
        INTEGER*4   label                  ! event generator label
        INTEGER*4   eg_pid                 ! event generator id
        INTEGER*4   ge_pid                 ! GEANT id
        REAL*4      p(3)                   ! momentum
        INTEGER*4   p_start_vertex         ! pointer to start vertex
        INTEGER*4   p_stop_vertex          ! pointer to stop vertex
        INTEGER*4   p_parent_track         ! pointer to parent track
        INTEGER*4   p_next_fs_track        ! pointer to next final state track
        INTEGER*4   p_prev_fs_track        ! pointer to previous final state track
        INTEGER*4   p_next_not_fs_track    ! pointer to next non-fs track
        INTEGER*4   p_prev_not_fs_track    ! pointer to previous non-fs track
      END STRUCTURE
*
      STRUCTURE /eg_vertex_t/           !  Event generator vertex structure
        INTEGER*4   label                  ! event generator label
        INTEGER*4   i_eg_process           ! event generator production process
        REAL*4      x(3)                   ! space point
        REAL*4      t                      ! time coordinate
        INTEGER*4   n_fs_track             ! # final state daughter tracks
        INTEGER*4   n_not_fs_track         ! # non-final state daughter tracks
        INTEGER*4   p_parent_track         ! pointer to parent track
        INTEGER*4   p_first_fs_track       ! pointer to ll of final state tracks
        INTEGER*4   p_first_not_fs_track   ! pointer to ll of non-fs tracks
        INTEGER*4   p_next_primary_vertex  ! pointer to next primary vertex
        INTEGER*4   p_prev_primary_vertex  ! pointer to previous primary vertex
        INTEGER*4   p_next_fs_vertex       ! pointer to next final state vertex
        INTEGER*4   p_prev_fs_vertex       ! pointer to prev. final state vertex
      END STRUCTURE
*
===========================
>  gstar_event_input.inc  <
===========================
c
c*** Contains structures for event generator events
c
#include "eg_event_str_def.inc"
c
	record  /eg_run_t/ eg_run
	pointer (p_eg_run, eg_run)
	record  /eg_event_t/ eg_event
	pointer (p_eg_event, eg_event)
	record  /eg_track_t/ eg_track (1)
	pointer (p_eg_track, eg_track)
	record  /eg_vertex_t/ eg_vertex (1)
	pointer (p_eg_vertex, eg_vertex)
		
	logical start_new_event
	integer i_subevent

	common/event_input/
     &           p_eg_run
     &          ,p_eg_event
     &          ,p_eg_vertex
     &          ,p_eg_track
     &          ,start_new_event
     *          ,i_subevent
#ifdef AIX
      integer p_eg_run, p_eg_event, p_eg_vertex, p_eg_track
#endif
c-------------------------------------------------------------
c*** objects for controlling event input

	integer       input_format
     *               ,input_flag
     *               ,alt_input_flag
     *               ,p_mm_struct
	logical       input_file_ok
	logical       read_from_file
	character*256 input_filename
	character*256 alt_input_filename

	common /input_control/
     *          input_format
     *          ,input_flag
     *          ,alt_input_flag
     *          ,input_filename
     *          ,alt_input_filename
     *          ,input_file_ok
     *          ,read_from_file
     *          ,p_mm_struct
c
c*** flat phase space       structure/ phase_space_t/     ...
c*** momentum bin           structure/ momentum_bin_t/    ...
c
c*** controlling event output

        logical output_file_open
        common/output_control/  output_file_open




============================
>  gstar_evthead_ubuf.inc  <
============================
c
c*** contains defintions for run header user buffer
c
      structure /ubuf_event_t/
         character*80 author            ! run-related
         character*80 machine
         integer      date
         integer      time
         real         geant_version
         integer      ge_run
         integer      ge_rndm_run(2)
         integer      events_ordered
         integer      ge_rndm_evt(2)    ! event-related
         integer      n_event           !
         integer      cav(12)           ! geometry information
         integer      n_shtk_evt        ! event information
         integer      mx_shtk_itra      !
         integer      nw_shtk           !
         integer      p_first_eg_vertex ! pointers for event chunking
         integer      p_first_eg_track  !
         integer      equals_99999      ! test of length of structure
      end structure
      record /ubuf_event_t/   ubuf_event
      common /com_ubuf_event/ ubuf_event
      integer    nubuf_event
      parameter( nubuf_event = 68 )



> gstar_vertex_ubuf.inc
=======================
c
c*** contains vertex user buffer structure
c
      structure /vertex_ubuf/
         integer geant_mechanism
         integer geant_medium
         real    geant_volume
         real    geant_tof
         real    evgen_x(3)
         real    evgen_tof
         integer evgen_label
         integer evgen_process
         integer evgen_n_parent
         integer evgen_p_first_parent
         integer evgen_n_fs_track
         integer evgen_p_first_fs_track
         integer evgen_n_not_fs_track
         integer evgen_p_first_not_fs_track
         integer intermediate_vertex
         integer equals_99999
      end structure
      record /vertex_ubuf/ v_ubuf
      integer    n_v_ubuf
      parameter (n_v_ubuf = 18 )


> gstar_track_ubuf.inc
=======================
c
c*** contains track user buffer structure
c*** NB keep stop vertex as 4th element: used in guskip
c
      structure /track_ubuf/
         integer evgen_label
         integer evgen_pid
         integer evgen_p_start_vertex
         integer evgen_p_stop_vertex
         integer evgen_p_first_intermed_vertex
         integer evgen_p_next_parent
         integer equals_99999
      end structure
      record /track_ubuf/ t_ubuf
      integer    n_t_ubuf
      parameter (n_t_ubuf = 7 )

=============================================================================

gstar_user_cards.inc
====================
c
c*** common block containing values extracted from user-defined cards
c
c*** number of event generator events to skip before start of processing
      integer n_skip_events
     *    ,n_skip_subevents
     *    ,n_skip_subran1
     *    ,n_skip_subran2

c*** smear vertex position over thickness of target? (0=no (def), 1=yes)
      integer i_smear_target

c*** set cuts high in dense materials
      integer make_shadow

c*** location of primary vertex prim_vert
      real prim_vert(3)

c*** number of tracks per subevent
      integer n_track_per_subevt

c*** flag for treating secondary tracks
      integer i_flag_secondaries

c*** flag for treating secondary tracks
      integer i_flag_prim_vert

c*** sigmas for gaussian dithering of primary geant vertex
      real sigma_prim_vert_z
      real sigma_prim_vert_xy

c*** flag for debug output of geometry (prints out all materials, media and volumes)
      integer i_flag_geo_debug

c*** value of electron tracking cut in gases
      real cutele_gas

c---------------------------------------------------------------------
      common/c_user_cards/n_skip_events
     *    ,n_skip_subevents
     *    ,n_skip_subran1
     *    ,n_skip_subran2
     &    ,i_smear_target
     &    ,make_shadow
     &    ,prim_vert
     &    ,n_track_per_subevt
     &    ,i_flag_secondaries
     &    ,i_flag_prim_vert
     &    ,sigma_prim_vert_xy
     &    ,sigma_prim_vert_z
     &    ,i_flag_geo_debug
     *    ,cutele_gas



gstar_geant_prim_vert.inc
=========================
c*** This is the global Geant offset to the primary vertex for this
c*** event-generator event (i.e. must be same for all sub-events)

      real prim_vert_this_event(3)

      common/event_vertex/prim_vert_this_event



gstar_cfio.inc
==============
c
c*** contains i/o pointers for cfio (needed for running on shift)
c
      integer         lunptr_input, lunptr_output
      common/ c_cfio /lunptr_input, lunptr_output

c*** lun for text file read

      integer lun_tx_in
      data    lun_tx_in/71/



gstar_pid.inc	
=============
c
c*** file of parameters and other definitions for symbolic reference
c*** to particle IDs specific to STAR Geant
c
      parameter( pid_omega = 150 )
      parameter( pid_phi = 151 )
      parameter( pid_rho0 = 152 )
      parameter( pid_rho_plus = 153 )
      parameter( pid_rho_minus = 154 )
      parameter( pid_D_plus = 155 )
      parameter( pid_D0 = 156 )
      parameter( pid_dq = 157 )
      parameter( pid_dqb = 158 )
      parameter( pid_uq = 159 )
      parameter( pid_uqb = 160 )
      parameter( pid_sq = 161 )
      parameter( pid_sqb = 162 )
      parameter( pid_cq = 163 )
      parameter( pid_cqb = 164 )
      parameter( pid_bq = 165 )
      parameter( pid_bqb = 166 )
      parameter( pid_tq = 167 )
      parameter( pid_tqb = 168 )
      parameter( pid_g = 169 )
      parameter( pid_laserino = 170 )



gstar_decay_modes.inc
======================
c
c*** for storing decay modes of geant-defined particles
c
*include "g2t_decay_mode.inc" - PN: do it manually

	STRUCTURE /DECAY_MODE_ST/
	CHARACTER*80 mode           ! character string describing mode !
	INTEGER*4 daughter_p        ! Id of first daughter !
	INTEGER*4 id                ! primary key !
	INTEGER*4 mode_label        ! integer labelling mode !
	INTEGER*4 n_daughter        ! number of daughters of decay !
	INTEGER*4 next_decay_p      ! pointer to next decay mode !
	REAL*4    b_ratio           ! branching ratio for this mode !
	END STRUCTURE               ! G2T_DECAY_MODE_ST !

      record /decay_mode_st/ decay_mode

*include "g2t_decay_daughter.inc" - PN: do it manually

	STRUCTURE /DECAY_DAUGHTER_ST/
	INTEGER*4 eg_pid            ! event generator pid !
	INTEGER*4 ge_pid            ! geant pid !
	INTEGER*4 id                ! primary key !
	INTEGER*4 next_daughter_p   ! next daughter of same decay mode !
	END STRUCTURE               ! G2T_DECAY_DAUGHTER_ST !

      record /decay_daughter_st/ daughter

      integer  p_mode, p_daughter
      pointer( p_mode, decay_mode )
      pointer( p_daughter, daughter )



gstar_udecay.inc
=================
c
c*** defines structures, common block, etc. for defining uniform
c*** decay of specified particles
c
      integer udecay_count
     *        ,udecay_count_max
     *        ,udecay_nstep
     *        ,udecay_nstep_max
     *        ,i_udecay
     *        ,udecay_nstep_last

      logical udecay_track,udecay_in_region_last_step

      real udecay_pathlength_entrance

      parameter (udecay_count_max=10)
      parameter (udecay_nstep_max=5000)

      structure /udecay_t/
         integer parent
         integer new_parent
         real r_in
         real r_out
         integer daughter_1
         integer daughter_2
         integer daughter_3
         real mass_p
         real mass_1
         real mass_2
         real mass_3
         integer nbody
      end structure

      record /udecay_t/ udecay(udecay_count_max)

      structure /udecay_step_t/
         real x
         real y
         real z
         real px
         real py
         real pz
         real pathlength
      end structure

      record /udecay_step_t/ udecay_step(udecay_nstep_max)

      common/c_udecay/
     *        udecay_count
     *        ,udecay
     *        ,udecay_nstep
     *        ,udecay_step
     *        ,i_udecay
     *        ,udecay_track
     *        ,udecay_in_region_last_step
     *        ,udecay_nstep_last
     *        ,udecay_pathlength_entrance




gstar_shower_tracks.inc
=======================
c
c*** Contains counter to ennumerate shower tracks in each subevent that
c*** produce hits in the sensitive detectors
c
      integer n_shower_track_evt
     &        ,max_shower_track_itra
     &        ,n_shower_track_itra

      common/com_shower_tracks/n_shower_track_evt
     &        ,max_shower_track_itra
     &        ,n_shower_track_itra
c
c
c*** define user buffer here

      structure /shower_t/
         integer n_shower_track
         integer pid
         real    px
         real    py
         real    pz
         integer equals_88888
      end structure

      record /shower_t/        ubuf_shower_track
      common /com_ubuf_shower/ ubuf_shower_track
      integer    nw_shower_track
      parameter( nw_shower_track = 6 )

*** contains defintions for run header user buffer
*
      COMMON /AgCHEAD/ author,machine,date,time,
     >       geant_version,ge_run,ge_rndm_run(2)
*
      character*80 author,machine
      integer      date,time
      real         geant_version
      integer      ge_run,ge_rndm_run(2)
*




#endif
