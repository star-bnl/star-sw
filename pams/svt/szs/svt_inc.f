c
c     svt_inc.f
c
c     include file for svt 
c
c     author: C. Pruneau
c

      integer svt_adc_n
      integer svt_time_n
      integer svt_anode_n
      integer svt_wafer_n
      integer svt_event_n
      integer svt_gsmear_para_n

      integer svt_adc_nmax
      integer svt_time_nmax
      integer svt_anode_nmax
      integer svt_wafer_nmax
      integer svt_event_nmax
      integer svt_gsmear_para_nmax

      parameter ( svt_adc_nmax   = 200000)
      parameter ( svt_time_nmax  = 2000)
      parameter ( svt_anode_nmax = 1000)
      parameter ( svt_wafer_nmax = 5)
      parameter ( svt_event_nmax = 210000)
      parameter ( svt_gsmear_para_nmax = 5)

      structure /svt_adc_st/
         integer adc
      end structure

      structure /svt_time_st/
         integer time
         integer nadc
         integer ptr
      end structure

      structure /svt_anode_st/
         integer anode
         integer nclus
         integer ptr
      end structure

      structure /svt_wafer_st/
         integer wafer
         integer nanode
         integer ptr
      end structure

      structure /svt_event_st/
         integer idata
         real    data
      end structure

      structure /svt_gsmear_para_st/
         integer nx
         real    mx
         real    sx
         real    apitch
         real    xoff
         integer nxmax

         integer ny
         real    my
         real    sy
         real    tpitch
         real    yoff
         integer nymax

         real    speed
      end structure

      structure /svt_asics_para_st/
         integer   wafer
         integer   n_anode
         integer   n_sample
         integer   high
         integer   nhigh
         integer   low
         integer   nlow
         integer   n_before
         integer   n_after
      end structure

      structure /svt_asics_data_st/
         integer   wafer
         integer*2 q(256,224)
         integer*2 select(256,224)
         integer   mask(1792)
      end structure

      structure /svt_asics_calib_st/
         integer   wafer
         integer*2 ped(256,224)
         integer*2 gain(256,224)
      end structure

      integer   svt_asics_para_n
      integer   svt_asics_data_n
      integer   svt_asics_calib_n

      integer   svt_asics_para_nmax
      integer   svt_asics_data_nmax
      integer   svt_asics_calib_nmax

      parameter ( svt_asics_para_nmax = 5)
      parameter ( svt_asics_data_nmax = 5)
      parameter ( svt_asics_calib_nmax = 5)

c     description: this file contains the definition of some temporary structures
c     to provide convenient interfacing with GEANT
c
c     the structure are the following:
c
c     vertex_st       :  store primary and secondary vertices locations
c     part_st         :  particle table definition
c     track_st        :  kinematic info for real tracks
c     svt_hit_st      :  hit structure in the SVT detector
c
      structure /vertex_st/
          integer   id        ! vertex id, value = 0,1 for primary vertex
          real      x         ! coordinates of vertex in global coordinates
          real      y
          real      z
          integer   tid       ! parent track id, value = 0 if primary track
      end structure

      structure /part_st/
          integer   id        ! geant particle id
          character name*20   ! particle's name
          real      mass      ! mass in GeV
          real      z         ! electric charge
          real      t         ! half life
      end structure

      structure /track_st/
          integer   id        ! track id
          real      px        ! momentum
          real      py
          real      pz
          real      e         ! energy
          integer   v         ! vertex
      end structure

      structure /svt_hit_st/
          integer   tid       ! track id
          real      x,y,z     ! hit, space point
          real      th, ph    ! polar, azimuthal angle of track at space point
          integer   wid       ! wafer id
          real      lx        ! (local) x-coordinate wafer frame, transverse to drift direction
          real      ly        ! (local) y-coordinate wafer frame, drift direction
          real      dedx      ! energy loss returned by GEANT
          real      t         ! TOF
          real      thn, thd  ! angles of track relative to normal, and drift direction
      end structure

      structure /svt_geom_st/
          integer   id                         ! wafer id
          real      x,y,z                      ! coordinate of wafer center of gravity in global coordinates
          real      wxp, wxm, wxs, wyp, wym    ! wafer half width in direction transverse and parallel to drift direction
          real      wnx, wny, wnz              ! wafer normal direction (pointing outward)
          real      wdx, wdy, wdz              ! wafer drift direction for ly>0
          real      wtx, wty, wtz              ! wafer transverse direction
          real      ddp, ddm
          real      dv
      end structure


      integer vertex_n
      integer part_n
      integer track_n
      integer svt_hit_n
      integer svt_geom_n

      integer vertex_nmax
      integer part_nmax
      integer track_nmax
      integer svt_hit_nmax
      integer svt_geom_nmax


      parameter ( vertex_nmax   =  1000)
      parameter ( part_nmax     =  200)
      parameter ( track_nmax    =  5000)
      parameter ( svt_hit_nmax  =  5000)
      parameter ( svt_geom_nmax =  5 )












