*
*
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
*
      STRUCTURE /eg_track_t/ !  Event generator track structure 
        INTEGER*4   label                 ! event generator label
        INTEGER*4   eg_pid                ! event generator id
        INTEGER*4   ge_pid                ! GEANT id
        REAL*4      p(3)                  ! momentum
        INTEGER*4   p_start_vertex        ! pointer to start vertex
        INTEGER*4   p_stop_vertex         ! pointer to stop vertex
        INTEGER*4   p_parent_track        ! pointer to parent track
        INTEGER*4   p_next_fs_track       ! pointer to next final state track
        INTEGER*4   p_prev_fs_track       ! pointer to previous final state track
        INTEGER*4   p_next_not_fs_track   ! pointer to next non-fs track
        INTEGER*4   p_prev_not_fs_track   ! pointer to previous non-fs track
      END STRUCTURE
*
*
      STRUCTURE /eg_vertex_t/ !  Event generator vertex structure 
        INTEGER*4   label                   ! event generator label
        INTEGER*4   i_eg_process            ! event generator production process
        REAL*4      x(3)                    ! space point
        REAL*4      t                       ! time coordinate
        INTEGER*4   n_fs_track              ! # final state daughter tracks
        INTEGER*4   n_not_fs_track          ! # non-final state daughter tracks
        INTEGER*4   p_parent_track          ! pointer to parent track
        INTEGER*4   p_first_fs_track        ! pointer to ll of final state tracks
        INTEGER*4   p_first_not_fs_track    ! pointer to ll of non-fs tracks
        INTEGER*4   p_next_primary_vertex   ! pointer to next primary vertex
        INTEGER*4   p_prev_primary_vertex   ! pointer to previous primary vertex
        INTEGER*4   p_next_fs_vertex        ! pointer to next final state vertex
        INTEGER*4   p_prev_fs_vertex        ! pointer to prev. final state vertex
      END STRUCTURE
