      INTEGER*4 FUNCTION SVG_NML(
     1            config_h,            config ,
     2             shape_h,             shape ,
     3              geom_h,              geom )

#include "svg_nml.inc"

      call svt_matmixt_define
      call svt_nml_define(
     1            config_h,            config ,
     2             shape_h,             shape ,
     3              geom_h,              geom )

      
      stop
      end
      
C>    svt_geom_define.f - a module to define the svt geometry
      
C>    Copyright 1993
      
C>    
C     modification history
C     --------------------
C     25-jul-1993,cap written
c     30-jul-1996,wkw modified for STAF
C>    
      
C>    
C     DESCRIPTION:
C     this module defines the svt geometry for an arbitrary number of
C     layers, ladder/layers, wafer/ladder. The thickness of the detector
C     and frame/misc material is also ajustable.
C     
C<    
      
C>-----------------------------------------------------------------------------
C     
C     svt_geom_define - a module to define the svt geometry
C     
C     DESCRIPTION:
C     this module defines the svt geometry for an arbitrary number of
C     layers, ladder/layers, wafer/ladder. The thickness of the detector
C     and frame/misc material is also ajustable.
C     
C     ARGUMENTS:
C     svt_geom_h header to table svt_geom
C     svt_geom   rows of table   svt_geom
C     svt_shape_h header to table svt_shape
C     svt_shape   rows of table   svt_shape
C     svt_config_h header to table svt_config
C     svt_config   rows of table   svt_config
C     
C     GEANT NAMES:
C     The following hierachy is used:
C     'RHIC' : star mother volume
C     'SVT ' : SVT top volume, shape=tube
C     'SLYn' : SVT LaYer n (n=1,9); shape=tube
C     'SL n' : SVT Ladder m (m=A,Z) for layer n (n=1,9)
C     'SF n' : Frame 'a' for layer n (n=1,9)
C     'SVTD ': Wafer 
C     'SCAn' : SVT End Cap n (n=1,2); shape=disk
C     
C     DETECTOR CONFIGURATION:
C     The configuration of the detector is defined though the table
C     SVT_CONFIG which has the folowing entries:
C     
C     x(3)              : position of the svt in the STAR global ref 
C     size(3)           : TUBE inner, outer radius, half lenght
C     n_layer           : number of SVT layers (a 3 super layers counts as 6)
C                         range: 1-9
C     n_ladder(10)      : number of ladders per layer
C                         range: A-Z (1-26)
C     n_wafer(10)       : number of wafers per layer
C                         range: no appriory limit
C     layer_radius(10)  : radius of layer
C     layer_phioff(10)  : phi (azimuthal) offset for first ladder
C                         positioned on layer
C     layer_shape(10)   : shape id for layer
C     layer_lateral(10) : lateral offset for ladder
C     layer_longi(10)   : longitudinal offset for ladder
C     layer_radial(10)  : radial offset (wafer stagering)
C     layer_drift(10)   : drift direction: 3 indicates z-axis
C  
C  A frame is included on both sides of the ladder, i.e. the carrier
C  The dimensions of the frame are also given in the svt_config table
C     
C     frame_trackmed    : tracking medium for frames
C     frame_width(10)   : width of the frames
C     frame_thick(10)   : thickness of the frames
C     frame_length(10)  : lenght of the frames
C
C  The size of the end caps support structure of the svt is also
C  given in the svt_config table:
C
C     cap_trackmed      : tracking medium for end caps
C     cap_shape_id      : shape of end caps (most likely='TUBE')
C     cap_nshape        : number of shape parameters
C     cap_shape(20)     : shape parameters
C     cap_position      : distance for origin (0,0,0)    
C
C     The wafer shape is defined through the shape table SVT_SHAPE
C     which has the following entries:
C
C     id                : shape id, 1='BOX '
C     n_shape           : number of shape parameters
C     shape(20)         : dimensions
C
C     It is understood that the given dimension (x,y,z) correspond to
C     x : the drift direction
C     y : transverse direction
C     z : the wafer thickness
C     
C     The geometry of the wafer is then written to the table SVT_GEOM
C     with the following entries (one row per wafer):
C     
C     id        : wafer sequential number
C     x(3)      : position of wafer center in STAR
C     n(3)      : normal to the wafer in STAR
C     d(3)      : drift direction for current wafer
C     t(3)      : t = n x d
C     shape     : shape id for current wafer
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     4-nov-1993,cap integrated in STAR/GEANT
C
C     Copyright 1993  Wayne State University
C     
C<-----------------------------------------------------------------------------
      subroutine svt_nml_define(
     1            config_h,            config ,
     2             shape_h,             shape ,
     3              geom_h,              geom )

      
      IMPLICIT NONE
      
C     Argument Declarations
C     =====================
#include "svg_nml.inc"
      
C     Local Declarations: 
C     ===================
      character*1  layer_label(9)
      character*1  ladder_label(26)
      CHARACTER*4  ATT_OPT
      integer      ishape, ilayer, iladder, iwafer, ipar, i, istat
      integer      ivolu, irot, ladder_rot, ichip, nwafer, full
      real         zstep, zfirst, zoff, phix,r1,r2,phi1,phi2
      real         dx,dy,phi3
      
      character*4    v_m
      character*4    v_name
      character*4    set_id
      character*4    v_shape
      character*4    namesv(4)
      integer        v_npar
      integer        v_tmed
      real           v_x(3)
      real           v_siz(20)
      integer        v_copy
      integer        v_rot
      character*4    v_only
      integer        v_seen

      real           phi_off, phi_step, radius, lateral, radian
      parameter      (radian=0.017453)
      real           philadder
      real           xlad, ylad, zlad
      real           xlad2, ylad2, zlad2
      real           Th1lad, Ph1lad
      real           Th2lad, Ph2lad
      real           Th3lad, Ph3lad
      real           x(3), normal(3), drift(3), trans(3)
      real           wafer_thick, wafer_width, wafer_lenght
      real           tube_radius, tube_inner, tube_outer, tube_lenght
      real           ladder_thick, ladder_width, ladder_lenght
      integer        Nrotm,nmate,nmixt,ntmed,nvols,npos,ndets,nhits,natts
c     The following are used in GSDETH    
c     Hit parameters will be:
C        1 : position
C        2 : energy deposition
C        3 : time of flight
C        4 : hit position - x
C        5 : hit position - y
C        6 : hit position - z
C        7 : local px
C        8 : local py
C        9 : local pz
C       10 : path length
C       11 : total energy
C
C     try to get 1 kev energy deposition resolution and momentum resolution

      integer     nv, nwpa, nwsa, nh, iset, idet, idtype
      real        orig(11), fact(11)
      integer     nbitsh(11), nbitsv(11)
      character*4 namesh(11)

C     Externals:
C     ==========
      integer gusvolu 
      integer numb_vol /0/
      integer ngsvol /0/
      integer guspos 
      integer nmpos /0/
      integer ngspos /0/
      integer gusatt 
      integer nmatt /0/
      integer nmatta /0/
      integer gusrotm 
      integer numb_rot /0/
      integer ngsrot /0/
      integer gusdet 
      integer numb_det /0/
      integer ngsdet /0/
      integer gusdeth 
      integer numb_deth /0/
      integer ngsdeth /0/

C     Name lists:
C     ===========
      integer  n_layer_max
      parameter (n_layer_max=9)
      integer  n_layer
      integer  n_ladder(n_layer_max)
      integer  n_wafer(n_layer_max)
      integer  layer_drift(n_layer_max)
      integer  layer_shape(n_layer_max)
      real     layer_radius(n_layer_max)
      real     layer_phioff(n_layer_max)
      real     layer_lateral(n_layer_max)
      real     layer_longi(n_layer_max)
      real     layer_radial(n_layer_max)
      real     frame_si_width(n_layer_max)
      real     frame_si_thick(n_layer_max)
      real     frame_si_length(n_layer_max)
      real     frame_al_width(n_layer_max)
      real     frame_al_thick(n_layer_max)
      real     frame_al_length(n_layer_max)
      real     frame_cs_width(n_layer_max)
      real     frame_cs_thick(n_layer_max)
      real     frame_cs_length(n_layer_max)
      real     be_support_width(n_layer_max)
      real     be_support_thick(n_layer_max)
      real     water_channel_width(n_layer_max)
      real     water_channel_thick(n_layer_max)
      real     layer_back(n_layer_max)
      real     svt_x(3), svt_size(3)
      integer  frame_trackmed    ! tracking medium for frames
      real     frame_width(10)   ! width of the frames
      real     frame_thick(10)   ! thickness of the frames
      real     frame_length(10)  ! lenght of the frames
      integer  cap_trackmed      ! tracking medium for end caps
      integer  cap_shape_id      ! shape of end caps (most likely='TUBE')
      integer  cap_nshape        ! number of shape parameters
      real     cap_shape(20)     ! shape parameters
      real     cap_position      ! distance from origin (0,0,0)    
      real     eta,rho2

      integer  id_shape
      integer  n_shape
      real     r_shape(20)

      namelist /svtconfig/n_layer,n_ladder,n_wafer,
     &     layer_drift,layer_shape,layer_radius,layer_phioff,
     &     layer_lateral,layer_longi,layer_radial,svt_x,svt_size,
     &     frame_trackmed,frame_width,frame_thick,frame_length,
     &     layer_back,frame_si_width,frame_si_thick,frame_si_length,
     &     frame_al_width,frame_al_thick,frame_al_length,
     &     frame_cs_width,frame_cs_thick,frame_cs_length,
     &     be_support_width,be_support_thick,water_channel_width,
     &     water_channel_thick,
     &     cap_trackmed,cap_shape_id,cap_nshape,cap_shape,cap_position

      namelist /svtshape/id_shape,n_shape,r_shape

C     Returned Values:
C     ================
C     Format:
C     =======
C     Executable Code:
C     ================
c     set tas return code to success

      layer_label(1) = '1'
      layer_label(2) = '2'
      layer_label(3) = '3'
      layer_label(4) = '4'
      layer_label(5) = '5'
      layer_label(6) = '6'
      layer_label(7) = '7'
      layer_label(8) = '8'
      layer_label(9) = '9'
C
      ladder_label(1) = 'A'
      ladder_label(2) = 'B'
      ladder_label(3) = 'C'
      ladder_label(4) = 'D'
      ladder_label(5) = 'E'
      ladder_label(6) = 'F'
      ladder_label(7) = 'G'
      ladder_label(8) = 'H'
      ladder_label(9) = 'I'
      ladder_label(10) = 'J'
      ladder_label(11) = 'K'
      ladder_label(12) = 'L'
      ladder_label(13) = 'M'
      ladder_label(14) = 'N'
      ladder_label(15) = 'O'
      ladder_label(16) = 'P'
      ladder_label(17) = 'Q'
      ladder_label(18) = 'R'
      ladder_label(19) = 'S'
      ladder_label(20) = 'T'
      ladder_label(21) = 'U'
      ladder_label(22) = 'V'
      ladder_label(23) = 'W'
      ladder_label(24) = 'X'
      ladder_label(25) = 'Y'
      ladder_label(26) = 'Z'
C
      open (file='for008.dat',unit=8 ,status='unknown',form='formatted')
      open (file='for009.dat',unit=9 ,status='unknown',form='formatted')
      open (file='for010.dat',unit=10,status='unknown',form='formatted')
      open (file='for011.dat',unit=11,status='unknown',form='formatted')
      open (file='for012.dat',unit=12,status='unknown',form='formatted')
      open (file='for013.dat',unit=13,status='unknown',form='formatted')

C     Read Data from definition from name list file
      
      open (file='svt_geom.dat',unit=99,form='formatted',status='old')
      
      read(99,nml=svtconfig)
      read(99,nml=svtshape)

      close (99)

C     Use information loaded from name list to define the TAS tables

C     Configuration Definition

      config(1).n_layer        = n_layer
      config(1).x(1)           = svt_x(1)
      config(1).x(2)           = svt_x(2)
      config(1).x(3)           = svt_x(3)
      config(1).size(1)        = svt_size(1)
      config(1).size(2)        = svt_size(2)
      config(1).size(3)        = svt_size(3)
      config(1).frame_trackmed = frame_trackmed
      config(1).cap_trackmed   = cap_trackmed
      config(1).cap_shape_id   = cap_shape_id

      config(1).cap_nshape     = cap_nshape
      config(1).cap_position   = cap_position

c     loop on every layer

      do ilayer = 1, n_layer
         config(1).n_ladder(ilayer)      = n_ladder(ilayer)
         config(1).n_wafer(ilayer)       = n_wafer(ilayer)
         config(1).layer_radius(ilayer)  = layer_radius(ilayer)
         config(1).layer_phioff(ilayer)  = layer_phioff(ilayer)
         config(1).layer_shape(ilayer)   = layer_shape(ilayer)
         config(1).layer_lateral(ilayer) = layer_lateral(ilayer)
         config(1).layer_longi(ilayer)   = layer_longi(ilayer)
         config(1).layer_radial(ilayer)  = layer_radial(ilayer)
         config(1).layer_drift(ilayer)   = layer_drift(ilayer)
         config(1).layer_back(ilayer)    = layer_back(ilayer)
         config(1).frame_width(ilayer)   = frame_width(ilayer)
         config(1).frame_thick(ilayer)   = frame_thick(ilayer)
         config(1).frame_length(ilayer)  = frame_length(ilayer)
         config(1).frame_cs_width(ilayer)   = frame_cs_width(ilayer)
         config(1).frame_cs_thick(ilayer)   = frame_cs_thick(ilayer)
         config(1).frame_cs_length(ilayer)  
     &                                         = frame_cs_length(ilayer)
         config(1).frame_al_width(ilayer)   
     &                                          = frame_al_width(ilayer)
         config(1).frame_al_thick(ilayer)   = frame_al_thick(ilayer)
         config(1).frame_al_length(ilayer)  
     &                                         = frame_al_length(ilayer)
         config(1).frame_si_width(ilayer)   = frame_si_width(ilayer)
         config(1).frame_si_thick(ilayer)   = frame_si_thick(ilayer)
         config(1).frame_si_length(ilayer)  
     &                                         = frame_si_length(ilayer)
         config(1).be_support_width(ilayer) 
     &                                        = be_support_width(ilayer)
         config(1).be_support_thick(ilayer) 
     &                                        = be_support_thick(ilayer)
	 config(1).water_channel_width(ilayer) 
     &                                     = water_channel_width(ilayer)
	 config(1).water_channel_thick(ilayer) 
     &                                     = water_channel_thick(ilayer)
      end do

C     Shape Definition

c     end cap shape/size

      do ishape = 1, cap_nshape
         config(1).cap_shape(ishape) = cap_shape(ishape)
      end do

c     wafer shape/size
c     define only one wafer shape for the time being

      shape(1).id           = id_shape
      shape(1).n_shape      = n_shape

      do ishape = 1, shape(1).n_shape
         shape(1).shape(ishape)   = r_shape(ishape)
      end do

c     ===========================================================
c     GEANT/NAME LIST and SVT_GEOM table DEFINITION
c     ===========================================================
c     position the svt within star
      
      v_name    = 'SVT '
      v_m       = 'HELI'
      v_shape   = 'TUBE'
      v_npar    = 3
      v_tmed    = 2      ! air -> locally defined
      v_siz(1)  = config(1).size(1)
      v_siz(2)  = config(1).size(2)
      v_siz(3)  = config(1).size(3)
      v_seen    = 0
      istat     = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
      att_opt   = 'SEEN'
      istat     = gusatt(v_name,ATT_OPT,v_seen,nmatta)
      v_x(1)    = config(1).x(1)
      v_x(2)    = config(1).x(2)
      v_x(3)    = config(1).x(3)
      v_copy    = 1
      v_only    = 'ONLY'
      istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3),-9999
     &,                                                   v_only,ngspos)

c     SVT end cap  in SVT

      v_name    = 'SCAP'
      v_m       = 'SVT '
      v_shape   = 'TUBE'
      v_npar    = 3
      v_tmed    = CONFIG(1).CAP_TRACKMED
      v_siz(1)  = CONFIG(1).CAP_SHAPE(1)
      v_siz(2)  = CONFIG(1).CAP_SHAPE(2)
      v_siz(3)  = CONFIG(1).CAP_SHAPE(3)
      v_seen    = 0
      istat     = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
      att_opt   = 'SEEN'
      istat     = gusatt(v_name,ATT_OPT,v_seen,nmatta)
      v_only    = 'ONLY'
      v_x(1)    = 0.
      v_x(2)    = 0.
      v_x(3)    = CONFIG(1).CAP_POSITION
      v_copy    = 1
      istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3),-9999
     &,                                                   v_only,ngspos)
      v_x(3)    = -v_x(3)
      v_copy    = 2
      istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3),-9999
     &,                                                   v_only,ngspos)

c     SVTD - active wafer volume definition
         
      v_name     = 'SVTD'
      v_shape    = 'BOX ' 
c->rm
      ilayer = 1

      id_shape   = config(1).layer_shape(ilayer)
      v_npar     = 3
      v_tmed     = 1            ! silicon -> locally defined
      do ipar = 1, v_npar
         v_siz(ipar) = shape(id_shape).shape(ipar)
      end do
      istat     = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
      att_opt   = 'SEEN'
      istat     = gusatt(v_name,ATT_OPT,1,nmatta)
      att_opt   = 'COLO'
      istat     = gusatt(v_name,ATT_OPT,9,nmatta)
      att_opt   = 'FILL'
      istat     = gusatt(v_name,ATT_OPT,1,nmatta)

C     loop on layers
      
      v_rot    = 0
      ichip      = 0

      do ilayer = 1, config(1).n_layer

c     layer volume definition and positioning
         
         tube_radius  = config(1).layer_radius(ilayer)
         ishape       = config(1).layer_shape(ilayer)
         wafer_thick  = shape(ishape).shape(3)
	 layer_back(ilayer)   = config(1).layer_back(ilayer)
         if (config(1).layer_drift(ilayer).eq.3) then
            wafer_width = shape(ishape).shape(2)    ! transverse size
	    wafer_lenght = shape(ishape).shape(1)
         else
            wafer_width  = shape(ishape).shape(1)    ! drift direction size
	    wafer_lenght = shape(ishape).shape(2)
         end if
         tube_inner    = tube_radius - wafer_thick
         tube_outer    = sqrt((tube_radius+wafer_thick)**2
     &                                                  +wafer_width**2)
         tube_lenght   = config(1).size(3)
         ladder_thick  = layer_back(ilayer)+wafer_thick
         ladder_width  = wafer_width
         ladder_lenght = tube_lenght

         v_name     = 'SLY'//layer_label(ilayer)
         v_m        = 'SVT '
         v_shape    = 'TUBE'
         v_npar     = 3
         v_tmed     = 2   ! air -> locally defined
         v_siz(1)   = tube_inner
         v_siz(2)   = tube_outer
         v_siz(3)   = tube_lenght
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         v_seen     = 0
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)
         v_x(1)     = 0.
         v_x(2)     = 0.
         v_x(3)     = 0.
         v_copy     = 1
         v_only     = 'ONLY'
         istat      = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             -9999,v_only,ngspos)
         
c     prepare loop on ladders
c     loop on ladder of current layer

         phi_off    = config(1).layer_phioff(ilayer)
         phi_step   = 360./config(1).n_ladder(ilayer) 
         radius     = config(1).layer_radius(ilayer)
         lateral    = config(1).layer_lateral(ilayer)

c     SLD - ladder volume

         v_name     = 'SLD'//layer_label(ilayer)
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 2   ! air -> locally defined
         v_siz(1)   = ladder_width
         v_siz(2)   = ladder_lenght
         v_siz(3)   = ladder_thick
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  

c     SU - structure

         v_name     = 'SU '//layer_label(ilayer)
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 3   ! beryllium -> locally defined 
         v_siz(1)   = ladder_width
         v_siz(2)   = ladder_lenght
         v_siz(3)   = config(1).be_support_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         v_seen     = 1
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)

c     SA

         v_name     = 'SA '//layer_label(ilayer)
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 2     ! air -> locally defined
         v_siz(1)   = ladder_width
     &                           -config(1).be_support_width(ilayer)
         v_siz(2)   = ladder_lenght
         v_siz(3)   = config(1).be_support_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         v_seen     = 1
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)

c     FR
         v_name     = 'FR '//layer_label(ilayer)
         v_seen     = 0
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 1         ! Si -> locally defined
         v_siz(1)   = config(1).frame_width(ilayer)
         v_siz(2)   = config(1).frame_length(ilayer)
         v_siz(3)   = config(1).frame_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)

c     SI
         v_name     = 'SI '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 1         ! Si -> locally defined 
         v_siz(1)   = config(1).frame_SI_width(ilayer)
         v_siz(2)   = config(1).frame_SI_length(ilayer)
         v_siz(3)   = config(1).frame_SI_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)
         att_opt    = 'COLO'
         istat      = gusatt(v_name,ATT_OPT,17,nmatta)
         att_opt    = 'FILL'
         istat      = gusatt(v_name,ATT_OPT,1,nmatta)

c     AL
         v_name     = 'AL '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 4         ! Al Ox -> locally defined 
         v_siz(1)   = config(1).frame_AL_width(ilayer)
         v_siz(2)   = config(1).frame_AL_length(ilayer)
         v_siz(3)   = config(1).frame_AL_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)
         att_opt    = 'COLO'
         istat      = gusatt(v_name,ATT_OPT,12,nmatta)
         att_opt    = 'FILL'
         istat      = gusatt(v_name,ATT_OPT,1,nmatta)
         
c     CS
         v_name     = 'CS '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 3         ! beryllium -> locally defined
         v_siz(1)   = config(1).frame_CS_width(ilayer)
         v_siz(2)   = config(1).frame_CS_length(ilayer)
         v_siz(3)   = config(1).frame_CS_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)
         att_opt    = 'COLO'
         istat      = gusatt(v_name,ATT_OPT,13,nmatta)
         att_opt    = 'FILL'
         istat      = gusatt(v_name,ATT_OPT,1,nmatta)

c     WA
         v_name     = 'WA '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 5         ! water (in pipes) -> locally defined
         v_siz(1)   = config(1).WATER_CHANNEL_width(ilayer)
         v_siz(2)   = config(1).frame_CS_length(ilayer)
         v_siz(3)   = config(1).WATER_CHANNEL_thick(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,v_seen,nmatta)
         att_opt    = 'COLO'
         istat      = gusatt(v_name,ATT_OPT,2,nmatta)
         att_opt    = 'FILL'
         istat      = gusatt(v_name,ATT_OPT,1,nmatta)

c     FB
         v_name     = 'FB '//layer_label(ilayer)
         v_m        = 'SLD'//layer_label(ilayer)
         v_only     = 'ONLY'
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 1         ! Silicon -> locally defined
         v_siz(1)   = wafer_width
         v_siz(2)   = tube_lenght
         v_siz(3)   = config(1).layer_back(ilayer)
         istat      = gusvolu(v_name,v_shape,v_tmed,v_siz,v_npar,ivolu
     &,                                                          ngsvol)  
         att_opt    = 'SEEN'
         istat      = gusatt(v_name,ATT_OPT,1,nmatta)
         att_opt    = 'COLO'
         istat      = gusatt(v_name,ATT_OPT,2,nmatta)
         att_opt    = 'FILL'
         istat      = gusatt(v_name,ATT_OPT,1,nmatta)
 
c     Positioning
c     ===========
c     SA in SU

         v_name    = 'SA '//layer_label(ilayer)
         v_m       = 'SU '//layer_label(ilayer)
         v_copy    = 1
         v_only    = 'ONLY'
         istat     = guspos(v_name,v_copy,v_m,0.,0.,0.,-9999,v_only
     &,                                                          ngspos)

c     SI in FR

         v_name    = 'SI '//layer_label(ilayer)
         v_m       = 'FR '//layer_label(ilayer)
         v_only    = 'ONLY'
         v_x(1)    = ((config(1).frame_width(ilayer)
     &                      -config(1).frame_si_width(ilayer)*2)/2.
     &                      +config(1).frame_si_width(ilayer))
         v_x(2)    = 0.
         v_x(3)    = (config(1).frame_thick(ilayer)
     &                            -config(1).frame_si_thick(ilayer)) 
         v_copy    = 1
         istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             -9999,v_only,ngspos)
         v_x(1)    = -v_x(1)
         v_copy    = 2
         istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             -9999,v_only,ngspos)
         
c     AL in FR
         
         v_name    = 'AL '//layer_label(ilayer)
         v_m       = 'FR '//layer_label(ilayer)
         v_only    = 'ONLY'
         v_copy    = 1
         v_x(1)    = 0.
         v_x(2)    = 0.
         v_x(3)    = (config(1).frame_thick(ilayer)
     &                           -2*config(1).frame_si_thick(ilayer)
     &                            -config(1).frame_al_thick(ilayer)) 
         istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             -9999,v_only,ngspos)

c     CS in FR

         v_name    = 'CS '//layer_label(ilayer)
         v_m       = 'FR '//layer_label(ilayer)
         v_only    = 'ONLY'
         v_copy    = 1
         v_x(1)    = 0.
         v_x(2)    = 0.
         v_x(3)    = -(config(1).frame_thick(ilayer)
     &                            -config(1).frame_CS_thick(ilayer)) 
         istat     = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             -9999,v_only,ngspos)
         
c     WA in CS 
         
         v_name   = 'WA '//layer_label(ilayer)
         v_m      = 'CS '//layer_label(ilayer)
         v_copy   = 1
         v_only   = 'ONLY'
         v_x(1)   = 0.
         v_x(2)   = 0.
         v_x(3)   = config(1).frame_cs_thick(ilayer)
     &                        -config(1).water_channel_thick(ilayer)
         istat    = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3),-9999
     &,                                                   v_only,ngspos)

c     FB in SL

         v_name   = 'FB '//layer_label(ilayer)
         v_m      = 'SLD'//layer_label(ilayer)
         v_only   = 'ONLY'
         v_x(1)   = 0.
         v_x(2)   = 0.
         v_x(3)   = -ladder_thick+config(1).layer_back(ilayer)
         istat    = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3),-9999
     &,                                                   v_only,ngspos)

c     loop on ladders of current layer

         do iladder = 1, config(1).n_ladder(ilayer)
            
c     position current ladder into layer
c     calculate the x,y,z position of the center of the ladder
c     include a lateral shift of the ladder position
            
            v_rot     = v_rot + 1
            philadder     = (phi_off + phi_step*float(iladder-1))
            xlad       = radius*cos(radian*philadder) 
     &                                   + lateral*sin(radian*philadder)
            ylad       = radius*sin(radian*philadder) 
     &                                   - lateral*cos(radian*philadder)
            zlad       = 0.
            
            Th1lad     = 90.0
            Ph1lad     = 90.0+philadder
            Th2lad     = 0.0
            Ph2lad     = 0.0
            Th3lad     = 90.0
            Ph3lad     = philadder
 
c     SLD in SLY
            v_name     = 'SLD'//layer_label(ilayer)
            v_m        = 'SLY'//layer_label(ilayer)
            v_copy     = iladder
            v_only     = 'ONLY'
            istat   = gusrotm(V_rot,Th1lad,Ph1lad,Th2lad,Ph2lad,Th3lad
     &,                                                   Ph3lad,ngsrot)
            istat   = guspos(v_name,v_copy,v_m,Xlad,Ylad,Zlad,v_rot
     &,                                                   v_only,ngspos)

c     SU in SLY
            v_name  = 'SU '//layer_label(ilayer)
            v_m     = 'SLY'//layer_label(ilayer)
            v_copy  = iladder
            v_only  = 'ONLY'
            xlad2   = (radius-ladder_thick)* cos(radian*philadder)
     &                                    +lateral*sin(radian*philadder)
            ylad2   = (radius-ladder_thick)* sin(radian*philadder)
     &                                    -lateral*cos(radian*philadder)
            zlad2   = 0.
            istat   = guspos(v_name,v_copy,v_m,Xlad2,Ylad2,Zlad2,v_rot
     &,                                                   v_only,ngspos)

c     FR in SLY - 2 copies per layer
c     frame/electronic structure volume definition
c     position frame on both sides of ladder = carrier

            v_name   = 'FR '//layer_label(ilayer)
            v_m      = 'SLY'//layer_label(ilayer)
            v_only   = 'ONLY'

            V_rot    = V_rot + 1
            Th1lad   = 90.0
            Ph1lad   = 90.0+philadder+phi_step/2.
            Th2lad   = 0.0
            Ph2lad   = 0.0
            Th3lad   = 90.0
            Ph3lad   = philadder+phi_step/2.
            istat    = gusrotm(V_rot,Th1lad,Ph1lad,Th2lad,Ph2lad,Th3lad
     &,                                                   Ph3lad,ngsrot)
 

	    phi1  = atan2(ladder_width,radius)
	    phi3  = radian*180./(2.*config(1).n_ladder(ilayer))
            rho2  = 3.14159/2.-phi3
	    r1    = radius/cos(phi1)
            dy    = config(1).frame_thick(ilayer)
     &                       -config(1).layer_back(ilayer)*cos(phi3)
            dx    = ladder_thick+config(1).layer_back(ilayer)

	    r2    = sqrt(config(1).frame_width(ilayer)**2+r1**2-
     &           2*r1*config(1).frame_width(ilayer)*cos(rho2))+dy
            phi2  = asin((config(1).frame_width(ilayer)
     &                                      +dx*cos(rho2))*sin(rho2)/r2)
            phix  = (phi_off +phi_step*float(iladder-1))
     &                                               +(phi1+phi2)/radian
            v_x(1)  = r2*cos(radian*phix) 
            v_x(2)  = r2*sin(radian*phix) 
            v_x(3)  = 0.
            v_copy  = 2*iladder-1
            istat   = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             v_rot,v_only,ngspos)

c     FR in SLY

            V_rot    = V_rot + 1
            Th1lad   = 90.0
            Ph1lad   = 90.0+philadder-phi_step/2.
            Th2lad   = 0.0
            Ph2lad   = 0.0
            Th3lad   = 90.0
            Ph3lad   = philadder-phi_step/2.
            istat    = gusrotm(V_rot,Th1lad,Ph1lad,Th2lad,Ph2lad,Th3lad
     &,                                                   Ph3lad,ngsrot)

	    phi1     = atan2(ladder_width,radius)
	    phi3     = radian * 180./(2.*config(1).n_ladder(ilayer))
            rho2     = 3.14159/2.-phi3
	    r1       = radius/cos(phi1)
            dy       = config(1).frame_thick(ilayer)
     &                       -config(1).layer_back(ilayer)*cos(phi3)
            dx       = ladder_thick+config(1).layer_back(ilayer)
	    r2       = sqrt(config(1).frame_width(ilayer)**2+r1**2-
     &         2*r1*config(1).frame_width(ilayer)*cos(rho2))+dy
            phi2     = asin((config(1).frame_width(ilayer)
     &                                      +dx*cos(rho2))*sin(rho2)/r2)
            phix     = (phi_off + phi_step*float(iladder-1))
     &                                               -(phi1+phi2)/radian
            v_x(1)   = r2*cos(radian*phix) 
            v_x(2)   = r2*sin(radian*phix) 
            v_x(3)   = 0.
            v_copy   = 2*iladder
            istat    = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             v_rot,v_only,ngspos)

c     prepare loop on wafers

            nwafer = config(1).n_wafer(ilayer)
            zstep  = 2.*wafer_lenght+0.2
            
            if (mod(iladder,2).eq.0) then
               zoff = config(1).layer_longi(ilayer)
            else
               zoff = -config(1).layer_longi(ilayer)
            end if
            zfirst  = zoff + float(nwafer-1)*wafer_lenght

            v_name    = 'SVTD'
            v_m       = 'SLD'//layer_label(ilayer)
            v_only    = 'ONLY'

c     loop on wafers

            do iwafer = 1, nwafer
               
               v_copy    = iwafer
               v_x(1) = 0.
               v_x(2) = zfirst - float(iwafer-1)*zstep
               v_x(3) = config(1).layer_back(ilayer)

c->rm          ONLY if iladder = 1 do we need the independent guspos...
               if(iladder .eq. 1)then
                  istat = guspos(v_name,v_copy,v_m,v_x(1),v_x(2),v_x(3)
     &,                                             -9999,v_only,ngspos)
               endif

c     TAS table definition

c     calculate normal vector to the wafer

               normal(1) = cos(ph3lad)
               normal(2) = sin(ph3lad)
               normal(3) = 0.

c     calculate drift direction

               if (config(1).layer_drift(ilayer).eq.3) then
                  drift(1) = 0.
                  drift(2) = 0.
                  drift(3) = 1.
               else
                  drift(1) = -sin(ph3lad)
                  drift(2) = cos(ph3lad)
                  drift(3) = 0.
               end if

c     calculate transverse direction

               trans(1) =  normal(2)*drift(3) - normal(3)*drift(2)
               trans(2) = -normal(1)*drift(3) + normal(3)*drift(1)
               trans(3) =  normal(1)*drift(2) - normal(2)*drift(1)

c     calculate position of wafer position in STAR

               x(1)  = svt_x(1) + xlad 
               x(2)  = svt_x(2) + ylad 
               x(3)  = svt_x(3) + zlad + zfirst - float(iwafer-1)*zstep

               ichip                     = ichip + 1
               geom(iwafer).id       = ichip
               geom(iwafer).id_shape 
     &                              = config(1).layer_shape(ilayer)
               geom(iwafer).ladder   = iladder
               geom(iwafer).layer    = ilayer
               geom(iwafer).num_chip = iwafer
               geom(iwafer).x(1)     = x(1)
               geom(iwafer).x(2)     = x(2)
               geom(iwafer).x(3)     = x(3)
               geom(iwafer).n(1)     = normal(1)
               geom(iwafer).n(2)     = normal(2)
               geom(iwafer).n(3)     = normal(3)
               geom(iwafer).d(1)     = drift(1)
               geom(iwafer).d(2)     = drift(2)
               geom(iwafer).d(3)     = drift(3)
               geom(iwafer).t(1)     = trans(1)
               geom(iwafer).t(2)     = trans(2)
               geom(iwafer).t(3)     = trans(3)
               
            end do              ! wafer on ladder loop
         end do                 ! ladder in layer loop
      end do                    ! layer loop
      
c     make the volume active detectors
c     and define its conversion parameters
      
      set_id = 'SVT '
      NV=1
      
      orig(1)  = 1000.
      orig(2)  = 1000.
      orig(3)  = 1000.
      orig(4)  = 50.
      orig(5)  = 50.
      orig(6)  = 50.
      orig(7)  = 50.
      orig(8)  = 0.
      orig(9)  = 0.
      orig(10) = 0.
      orig(11) = 0.
c
c     The following gains give
c
c                               - 10 micron position resolution
c                               - 100KeV/c momentum resolution
c                              
c                       
      fact(1)  = 1000.
      fact(2)  = 1000.
      fact(3)  = 1000.
      fact(4)  = 10000.
      fact(5)  = 10000.
      fact(6)  = 10000.
      fact(7)  = 10000.
      fact(8)  = 10.
      fact(9)  = 1.e10
      fact(10) = 1.e6
      fact(11) = 1.

      namesh(1)  = 'X   '
      namesh(2)  = 'Y   '
      namesh(3)  = 'Z   '
      namesh(4)  = 'Px  '
      namesh(5)  = 'Py  '
      namesh(6)  = 'Pz  '
      namesh(7)  = 'Ptot'
      namesh(8)  = 'S   '
      namesh(9)  = 'Tof '
      namesh(10) = 'delE'
      namesh(11) = '    '

      do i = 1, 11
         nbitsh(i) = 32
      end do
         
      v_name    = 'SVTD'
      namesv(1) = 'SVTD'
      nbitsv(1) = 24
      nwpa      = 10000
      nwsa      = 1000
      istat = gusdet (set_id,v_name,nv,namesv,nbitsv,idtype,nwpa,nwsa
     &,                                                iset,idet,ngsdet)
      istat = gusdeth(set_id,v_name,nh,namesh,nbitsh,orig,fact,ngsdeth)

      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      call namelist(n_layer,n_ladder,n_wafer,ngsvol,ngspos,ngsrot,
     &                                            ngsdet,ngsdeth,nmatta)

      return
      end

c=============================================================
      Subroutine Namelist(n_layer,n_ladder,n_wafer,nvols,npos,
     &                                          nrotm,ndets,nhits,natts)

      IMPLICIT NONE

      integer n_layer,n_ladder,n_wafer,nvols,npos,ndiv,
     & 		nrotm,ndets,nhits,natts,nmate,nmixt,ntmed
      integer   major, minor
      character dttm*60
      character author*60

c     format
c     ======
 1031 format(' $GeoVrsn'/
     &     '       Ivrs_Maj    =',i5,','/
     &     '       Ivrs_Min    =',i5,','/
     &     '       Date_Time   =''',a,'''',','/
     &     '       Author      =''',a,'''',','/
     &     '       $END')
 1011 format(' $NMate'/
     &     '       Number_Mate =',i5,/
     &     '       $END')
 1012 format(' $NMixt'/
     &     '       Number_Mixt =',i5,/
     &     '       $END')
 1013 format(' $NTMed'/
     &     '       Number_TMed =',i5,/
     &     '       $END')
 1010 format(' $NROTM'/
     &     '       Number_ROTM =',i5,/
     &     '       $END')
 1006 format(' $NVols'/
     &       '       Number_Vols =',i5,/
     &       '       $END')
 1030 format(' $NDiv'/
     &     '       Number_Div  =',i5,/
     &     '       $END')
 1002 format(' $NPos'/
     &       '       Number_Pos =',i5,/
     &       '       $END')
 1008 format(' $NAtts'/
     &       '       Number_Atts =',i5,/
     &       '       $END')
 1015 format(' $NDets'/
     &     '       Number_Dets =',i5,/
     &     '       $END')
 1017 format(' $NHits'/
     &     '       Number_HitTypes =',i5,/
     &     '       $END')
  
c     executable code:
c     ================
      open(unit=8,file='for008.dat',status='old')
      open(unit=9,file='for009.dat',status='old')
      open(unit=10,file='for010.dat',status='old')
      open(unit=11,file='for011.dat',status='old')
      open(unit=12,file='for012.dat',status='old')
      open(unit=13,file='for013.dat',status='old')
      open(unit=20,file='for020.dat',status='unknown')
      open(unit=21,file='for021.dat',status='unknown')
      open(unit=22,file='for022.dat',status='unknown')
      open(unit=60,file='for060.dat',status='unknown')

      major = 0
      minor = 0
      Dttm = '06-AUG-1993 11:11:00.00'
      Author = 'Claude Pruneau & Gabe Roger, WSU'
      Write(60,1031)major,minor,dttm,author

      nmate = 1
      write(60,1011)nmate ! lun=22
      call filecopy(22,60)
      nmixt = 2
      write(60,1012)nmixt  !lun=20
      call filecopy(20,60)
      ntmed = 5
      write(60,1013)ntmed  ! lun=21
      call filecopy(21,60)
      write(60,1010)nrotm  ! lun=11
      call filecopy(11,60)
      write(60,1006)nvols  ! lun=9
      call filecopy(9,60)
      ndiv = 0
      write(60,1030)ndiv
      write(60,1002)npos  ! lun =8
      call filecopy(8,60)
      write(60,1008)natts       ! lun=10
      call filecopy(10,60)
      write(60,1015)ndets       ! lun=12
      call filecopy(12,60)
      write(60,1017)nhits       ! lun=13
      call filecopy(13,60)

      RETURN
      END
C>-----------------------------------------------------------------------------
C
C     filecopy - a function to copy one file to another
C     
C     DESCRIPTION:
C     this function copies file all lines in file on unit lun1 to file open 
C     on unit = lun2
C     
C     ARGUMENTS:
C     lun  : logical of file to copy from
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     copyright 1993 Lawrence Berkeley Laboratory
C     
C<-----------------------------------------------------------------------------
      subroutine filecopy(lun1,lun2)
      implicit none

      integer   lun1, lun2, i
      character string*80

c     format
c     ======
 1001 format(a)

c     executable
c     ==========
      do i = 1, 100000

         read  (lun1,1001,end=100) string
         write (lun2,1001) string

      end do

 100  continue

      return
      end
C>-----------------------------------------------------------------------------
C
C     gusvolu - a function to define the GEANT volumes and print namelist parameters
C     
C     DESCRIPTION:
C     this function defines the GEANT geometry of a volume and prints out to a
C     namelist file.
C     
C     ARGUMENTS:
C     integer  name    : volume name
C     integer  shape   : volume shape
C     integer  nmed    : tracking medium
C     integer  par(*)  : shape parameters
C     integer  npar    : number of shape parameters passed
C     integer  ivolu   : volume index returned by GEANT
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     copyright 1993 Lawrence Berkeley Laboratory
C     
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION gusvolu(name,shape,nmed,par,npar,ivolu,nvols)

      IMPLICIT NONE

C     Argument Declarations
C     =====================
      character*4  name
      character*4  shape
      integer      nmed
      real         par(*)
      integer      npar
      integer      ivolu,nvols

       
C     Local Declarations:
C     ===================
      integer  i

C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Volume'/
     &       '  VOL_NAME=''',a,''', SHAPE=''',a,''', NMED=',i5,',')
 1001 format('  PAR(',i1,')=',f14.6,',')
c1001 format('  PAR(',i2,')    =',f14.6,',')
 1002 format(' $END')
 1003 format(' NPAR=',i5,',')

C     Executable Code:
C     ================
c     call gsvolu(name,shape,nmed,par,npar,ivolu)  
      
      write(9,1000) name,shape,nmed
      write(9,1001) (i,par(i),i=1,npar)
      write(9,1003) NPAR
      write(9,1002) 
      
      nvols=nvols+1
      
      gusvolu = 0
      return
      end
c     
      INTEGER FUNCTION gusdet(iuset,iudet,nv,namesv,nbitsv,idtype,nwpa
     &,                                            nwsa,iset,idet,numb)

      IMPLICIT NONE

C     Argument Declarations
C     =====================
      character*4  iuset,namesv(11)
      character*4  iudet
      integer      nmed,nbitsv(11),idtype,nwpa,nwsa
      real         par(11)
      integer      npar,iset,idet,nv,ndet
      integer      ivolu,numb
       
C     Local Declarations:
C     ===================
      integer  i

C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Detector'/
     &     '       IUSET  =''',a,''', IUDET  =''',a,''', IDTYPE =',i5,'
     &     ,'/'       NWHI   =',i5,', NWDI   =',i5,', Alias  = .FALSE.'
     &     ,','/'  $END')

C     Executable Code:
C     ================
c      call gsdet(iuset,iudet,nv,namesv,nbitsv,
c     & 		idtype,nwpa,nwsa,iset,idet)  

c->rm 
      idtype = 1

      write(12,1000) IUSET,IUDET,IDTYPE,NWPA,NWSA

      numb = numb + 1

      gusdet = 0
      return
      end
c
      INTEGER FUNCTION gusdeth(set_id,v_name,nh,namesh,nbitsh,orig,fact
     &,                                                          ndeth)

      IMPLICIT NONE

C     Argument Declarations
C     =====================
      character*4  set_id,v_name,namesh(11)
      integer      nbitsh(11),nh,ndeth
      real         orig(11),fact(11)

       
C     Local Declarations:
C     ===================
      integer  i

C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Hits'/
     &     '       HIT_IUSET  =''',a4,'''',','/
     &     '       HIT_IUDET  =''',a4,'''',','/
     &     '       NH         =',i5,',')
 1001 format('       NAMESH(',I1,')=''',a4,'''',',')
 1002 format('       NBITSH(',I1,')=',i5,',')
 1003 format('       ORIG(',I1,')=',f14.2,',')
 1004 format('       FACT(',I1,')=',f14.2,',')
 1006 format('       NAMESH(',I2,')=''',a4,'''',',')
 1007 format('       NBITSH(',I2,')=',i5,',')
 1008 format('       ORIG(',I2,')=',f14.2,',')
 1009 format('       FACT(',I2,')=',f14.2,',')
 1005 format('       $END')

C     Executable Code:
C     ================
c      call gsdeth(set_id,v_name,nh,namesh,nbitsh,
c     & 		orig,fact)  

c->rm 
      nh = 10

      write(13,1000) set_id,v_name,nh
      write(13,1001) (i,namesh(i),i=1,9)
      write(13,1006) (i,namesh(i),i=10,10)
      write(13,1002) (i,nbitsh(i),i=1,9)
      write(13,1007) (i,nbitsh(i),i=10,10)
      write(13,1003) (i,orig(i),i=1,9)
      write(13,1008) (i,orig(i),i=10,10)
      write(13,1004) (i,fact(i),i=1,9)
      write(13,1009) (i,fact(i),i=10,10)
      write(13,1005)
     
      ndeth=ndeth+1

      gusdeth = 0
      return
      end
      
C>-----------------------------------------------------------------------------
C
C     guspos - a function to position the GEANT volumes and print namelist parameters
C     
C     DESCRIPTION:
C     this module call the GEANT routine GSPOS that position the volume. It also
C     prints out the corresponding name list file.
C     
C     ARGUMENTS:
C     integer  name    : volume name
C     integer  nr      : volume copy number
C     integer  mname   : mother volume name
C     real     x,y,z   : coordinate of the center of the volume within mother volume
C     integer  irot    : rotation matrix index
C     integer  konly   : ONLY 
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     Copyright 1993 Lawrence Berkeley Laboratory
C     
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION guspos(name,nr,mname,x,y,z,irot,konly,npos)

      IMPLICIT NONE
            
C     Argument Declarations
C     =====================
      character*4 name, mname, konly
      integer      nr, irot,npos
      real         x,y,z
       
C     Local Declarations: none
C     ===================
C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Position'/
     &     '  POS_NAME=''',a,''', NR=',i3,', POS_MOTHER =''',a,''',',/
     &     '  X=',f12.4,', Y=',f12.4,',Z=',f12.4,', POS_IROT=',i5,'
     &     , KONLY=''',a,''''/
     &     ' $END')

C     Executable Code:
C     ================
c     call gspos(name,nr,mname,x,y,z,irot,konly)

      write(8,1000) name,nr,mname,x,y,z,irot,konly
      
      npos=npos+1
      guspos = 0
      
      return
      end

C>-----------------------------------------------------------------------------
C
C     gusatt - a function to define the attribute of a volume
C     
C     DESCRIPTION:
C     this function calls the GEANT routine GSATT to define the attributes
C     of a volume and prints out the corresponding name list.
C     
C     ARGUMENTS:
C     integer   att_name : volume name
C     integer   att_opt  : attribute name
C     integer   att_val  : attribute value
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     Copyright 1993 Lawrence Berkeley Laboratory
C     
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION gusatt(att_name,att_opt,att_val,natt)
      
      IMPLICIT NONE
      
C     Argument Declarations
C     =====================
      character*4   att_name
      character*4   att_opt
      integer   att_val,natt
      
C     Local Declarations: none
C     ===================
C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1001 format(' $Attributes'/
     &     '  Att_Name =''',a,''', ATT_Opt  =''',a,''', ATT_VAL  = ',i8,/
     &     ' $END')
      
C     Executable Code:
C     ================      
c     call gsatt(att_name,att_opt,att_val)
      write(10,1001) att_name,att_opt,att_val
      
      natt=natt+1
      gusatt = natt
      
      return
      end
      
C>-----------------------------------------------------------------------------
C     
C     gusrotm - a function to define a rotation matrix
C     
C     DESCRIPTION:
C     this function calls the GEANT routine GSROTM to define a rotation
C     matrix and print the corresponding name list.
C     
C     ARGUMENTS:
C     integer irot
C     real    th1   : polar angle axis 1
C     real    th2   : polar angle axis 2
C     real    th3   : polar angle axis 3
C     real    ph1   : azimuthal angle axis 1
C     real    ph2   : azimuthal angle axis 2
C     real    ph3   : azimuthal angle axis 3
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     Copyright 1993 Lawrence Berkeley Laboratory
C     
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION gusrotm(irot,th1,ph1,th2,ph2,th3,ph3,nrot)
      
      IMPLICIT NONE
      
C     Argument Declarations
C     =====================
      integer irot,nrot
      real    th1,th2,th3,ph1,ph2,ph3
      
C     Local Declarations: none
C     ===================
C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Rotation'/
     &     '       IROT   = ',i4,','/
     &     '       THETA1 =',f12.4,', PHI1   =',f12.4,','/
     &     '       THETA2 =',f12.4,', PHI2   =',f12.4,','/
     &     '       THETA3 =',f12.4,', PHI3   =',f12.4,/
     &     '  $END')
      
      
C     Executable Code:
C     ================            
      if (th1.ge.360.) then
         th1=th1-360.
      endif
      if (th2.ge.360.) then
         th2=th2-360.
      endif
      if (th3.ge.360.) then
         th3=th3-360.
      endif
      if (ph1.ge.360.) then
         ph1=ph1-360.
      endif
      if (ph2.ge.360.) then
         ph2=ph2-360.
      endif
      if (ph3.ge.360.) then
         ph3=ph3-360.
      endif
      nrot=nrot+1
c     call gsrotm(irot,th1,ph1,th2,ph2,th3,ph3)
      
      write(11,1000) irot,th1,ph1,th2,ph2,th3,ph3
      
      gusrotm = nrot
      
      return
      end
