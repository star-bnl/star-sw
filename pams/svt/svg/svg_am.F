c:>------------------------------------------------------------------
C:FILE:         svg_am.F
C:DESCRIPTION:  a module to define the svt geometry
c:AUTHOR:       Claude Pruneau  pruneau@rhic.physics.wayne.edu
C:BUGS:         -- STILL IN DEVELOPMENT --
C:HISTORY:      25jul93-v001a Creation.
C:              18nov93-v001b Updated by Stathes paganis, Spiros Margetis
C:              26jun96-v001c Translated into STAF by Ken Wilson
c:>------------------------------------------------------------------

c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 svg_am
C:DESCRIPTION:  this module defines the svt geometry for an arbitrary 
C:              number of layers, ladder/layers, wafer/ladder. The 
C:              thickness of the detector and frame/misc material is 
C:              also ajustable.
C:    GEANT NAMES:
C:    The following hierachy is used:
C:    'RHIC' : star mother volume
C:    'SVT ' : SVT top volume, shape=tube
C:    'SLYn' : SVT LaYer n (n=1,9); shape=tube
C:    'SL n' : SVT Ladder m (m=A,Z) for layer n (n=1,9)
C:    'SF n' : Frame 'a' for layer n (n=1,9)
C:    'SVTD ': Wafer 
C:    'SCAn' : SVT End Cap n (n=1,2); shape=disk
C:    
C:    DETECTOR CONFIGURATION:
C:    The configuration of the detector is defined though the table
C:    CONFIG which has the folowing entries:
C:    
C:    x(3)              : position of the svt in the STAR global ref 
C:    size(3)           : TUBE inner, outer radius, half lenght
C:    n_layer           : number of SVT layers (a 3 super layers counts as 6)
C:                        range: 1-9
C:    n_ladder(10)      : number of ladders per layer
C:                        range: A-Z (1-26)
C:    n_wafer(10)       : number of wafers per layer
C:                        range: no appriory limit
C:    layer_radius(10)  : radius of layer
C:    layer_phioff(10)  : phi (azimuthal) offset for first ladder
C:                        positioned on layer
C:    layer_shape(10)   : shape id for layer
C:    layer_lateral(10) : lateral offset for ladder
C:    layer_longi(10)   : longitudinal offset for ladder
C:    layer_radial(10)  : radial offset (wafer stagering)
C:    layer_drift(10)   : drift direction: 3 indicates z-axis
C: 
C: A frame is included on both sides of the ladder, i.e. the carrier
C: The dimensions of the frame are also given in the config table
C:    
C:    frame_trackmed    : tracking medium for frames
C:    frame_width(10)   : width of the frames
C:    frame_thick(10)   : thickness of the frames
C:    frame_length(10)  : lenght of the frames
C:
C: The size of the end caps support structure of the svt is also
C: given in the config table:
C:
C:    cap_trackmed      : tracking medium for end caps
C:    cap_shape_id      : shape of end caps (most likely='TUBE')
C:    cap_nshape        : number of shape parameters
C:    cap_shape(20)     : shape parameters
C:    cap_position      : distance for origin (0,0,0)    
C:
C:    The wafer shape is defined through the shape table SHAPE
C:    which has the following entries:
C:
C:    id                : shape id, 1='BOX '
C:    n_shape           : number of shape parameters
C:    shape(20)         : dimensions
C:
C:    It is understood that the given dimension (x,y,z) correspond to
C:    x : the drift direction
C:    y : transverse direction
C:    z : the wafer thickness
C:    
C:    The geometry of the wafer is then written to the table GEOM
C:    with the following entries (one row per wafer):
C:    
C:    id        : wafer sequential number
C:    x(3)      : position of wafer center in STAR
C:    n(3)      : normal to the wafer in STAR
C:    d(3)      : drift direction for current wafer
C:    t(3)      : t = n x d
C:    shape     : shape id for current wafer
C:    
C:ARGUMENTS:    svg_config, svg_shape, svg_geom
C:RETURN VALUE: STAF Condition Value
c:>------------------------------------------------------------------
C:     Copyright 1993  Wayne State University
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION SVG_AM(
     1            config_h,            config ,
     2             shape_h,             shape ,
     3              geom_h,              geom ) 

      IMPLICIT NONE

C     Argument Declarations
C     =====================
#include "svg_am.inc"

C     Local Declarations: 
C     ===================
      integer      config_count/0/     ! to check if the configuration 
      integer      shape_count/0/      ! or the wafer shape has been update

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
      real     pi
      integer  id_shape
      integer  n_shape
      real     r_shape(20)

c      namelist /svtconfig/n_layer,n_ladder,n_wafer,
c     &     layer_drift,layer_shape,layer_radius,layer_phioff,
c     &     layer_lateral,layer_longi,layer_radial,svt_x,svt_size,
c     &     frame_trackmed,frame_width,frame_thick,frame_length,
c     &     layer_back,frame_si_width,frame_si_thick,frame_si_length,
c     &     frame_al_width,frame_al_thick,frame_al_length,
c     &     frame_cs_width,frame_cs_thick,frame_cs_length,
c     &     be_support_width,be_support_thick,water_channel_width,
c     &     water_channel_thick,
c     &     cap_trackmed,cap_shape_id,cap_nshape,cap_shape,cap_position
c
c      namelist /svtshape/id_shape,n_shape,shape

C     Returned Values:
C     ================
C     Format:
C     =======
C     Executable Code:
C     ================
c     set tas return code to success

      pi=acos(-1.)
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
c      open (file='for008.dat',unit=8 ,status='unknown',form='formatted')
c      open (file='for009.dat',unit=9 ,status='unknown',form='formatted')
c      open (file='for010.dat',unit=10,status='unknown',form='formatted')
c      open (file='for011.dat',unit=11,status='unknown',form='formatted')
c      open (file='for012.dat',unit=12,status='unknown',form='formatted')
c      open (file='for013.dat',unit=13,status='unknown',form='formatted')

C     Read Data from definition from name list file
      
c      open (file='geom.dat',unit=99,form='formatted',status='old')
      
c      read(99,nml=svtconfig)
c      read(99,nml=svtshape)

c      close (99)

C     Use information loaded from name list to define the TAS tables

c     ===========================================================
c     GEANT/NAME LIST and GEOM table DEFINITION
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
      att_opt   = 'SEEN'
      v_x(1)    = config(1).x(1)
      v_x(2)    = config(1).x(2)
      v_x(3)    = config(1).x(3)
      v_copy    = 1
      v_only    = 'ONLY'

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
      att_opt   = 'SEEN'
      v_only    = 'ONLY'
      v_x(1)    = 0.
      v_x(2)    = 0.
      v_x(3)    = CONFIG(1).CAP_POSITION
      v_copy    = 1
      v_x(3)    = -v_x(3)
      v_copy    = 2

c     SVTD - active wafer volume definition
         
      v_name     = 'SVTD'
      v_shape    = 'BOX ' 
      ilayer     = 1
      id_shape   = config(1).layer_shape(ilayer)
      v_npar     = 3
      v_tmed     = 1            ! silicon -> locally defined
      do ipar = 1, v_npar
         v_siz(ipar) = shape(id_shape).shape(ipar)
      end do
      att_opt   = 'SEEN'
      att_opt   = 'COLO'
      att_opt   = 'FILL'

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
            wafer_width = shape(ishape).shape(2)    
	    wafer_lenght = shape(ishape).shape(1)
         else
            wafer_width  = shape(ishape).shape(1)   
	    wafer_lenght = shape(ishape).shape(2)
         end if
         tube_inner    = tube_radius - wafer_thick
         tube_outer    = sqrt((tube_radius+wafer_thick)**2
     +                                        +wafer_width**2)
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
         v_seen     = 0
         att_opt    = 'SEEN'
         v_x(1)     = 0.
         v_x(2)     = 0.
         v_x(3)     = 0.
         v_copy     = 1
         v_only     = 'ONLY'
         
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

c     SU - structure

         v_name     = 'SU '//layer_label(ilayer)
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 3   ! beryllium -> locally defined 
         v_siz(1)   = ladder_width
         v_siz(2)   = ladder_lenght
         v_siz(3)   = config(1).be_support_thick(ilayer)
         v_seen     = 1
         att_opt    = 'SEEN'

c     SA

         v_name     = 'SA '//layer_label(ilayer)
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 2     ! air -> locally defined
         v_siz(1)   = ladder_width
     +                -config(1).be_support_width(ilayer)
         v_siz(2)   = ladder_lenght
         v_siz(3)   = config(1).be_support_thick(ilayer)
         v_seen     = 1
         att_opt    = 'SEEN'

c     FR
         v_name     = 'FR '//layer_label(ilayer)
         v_seen     = 0
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 1         ! Si -> locally defined
         v_siz(1)   = config(1).frame_width(ilayer)
         v_siz(2)   = config(1).frame_length(ilayer)
         v_siz(3)   = config(1).frame_thick(ilayer)
         att_opt    = 'SEEN'

c     SI
         v_name     = 'SI '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 1         ! Si -> locally defined 
         v_siz(1)   = config(1).frame_SI_width(ilayer)
         v_siz(2)   = config(1).frame_SI_length(ilayer)
         v_siz(3)   = config(1).frame_SI_thick(ilayer)
         att_opt    = 'SEEN'
         att_opt    = 'COLO'
         att_opt    = 'FILL'

c     AL
         v_name     = 'AL '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 4         ! Al Ox -> locally defined 
         v_siz(1)   = config(1).frame_AL_width(ilayer)
         v_siz(2)   = config(1).frame_AL_length(ilayer)
         v_siz(3)   = config(1).frame_AL_thick(ilayer)
         att_opt    = 'SEEN'
         att_opt    = 'COLO'
         att_opt    = 'FILL'
         
c     CS
         v_name     = 'CS '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 3         ! beryllium -> locally defined
         v_siz(1)   = config(1).frame_CS_width(ilayer)
         v_siz(2)   = config(1).frame_CS_length(ilayer)
         v_siz(3)   = config(1).frame_CS_thick(ilayer)
         att_opt    = 'SEEN'
         att_opt    = 'COLO'
         att_opt    = 'FILL'

c     WA
         v_name     = 'WA '//layer_label(ilayer)
         v_seen     = 1
         v_shape    = 'BOX '
         v_npar     = 3
         v_tmed     = 5         ! water (in pipes) -> locally defined
         v_siz(1)   = config(1).WATER_CHANNEL_width(ilayer)
         v_siz(2)   = config(1).frame_CS_length(ilayer)
         v_siz(3)   = config(1).WATER_CHANNEL_thick(ilayer)
         att_opt    = 'SEEN'
         att_opt    = 'COLO'
         att_opt    = 'FILL'

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
         att_opt    = 'SEEN'
         att_opt    = 'COLO'
         att_opt    = 'FILL'
 
c     Positioning
c     ===========
c     SA in SU

         v_name    = 'SA '//layer_label(ilayer)
         v_m       = 'SU '//layer_label(ilayer)
         v_copy    = 1
         v_only    = 'ONLY'

c     SI in FR

         v_name    = 'SI '//layer_label(ilayer)
         v_m       = 'FR '//layer_label(ilayer)
         v_only    = 'ONLY'
         v_x(1)    = ((config(1).frame_width(ilayer)
     &                 -config(1).frame_si_width(ilayer)*2)/2.+
     &                 config(1).frame_si_width(ilayer))
         v_x(2)    = 0.
         v_x(3)    = (config(1).frame_thick(ilayer)
     &                  -config(1).frame_si_thick(ilayer)) 
         v_copy    = 1
         v_x(1)    = -v_x(1)
         v_copy    = 2
         
c     AL in FR
         
         v_name    = 'AL '//layer_label(ilayer)
         v_m       = 'FR '//layer_label(ilayer)
         v_only    = 'ONLY'
         v_copy    = 1
         v_x(1)    = 0.
         v_x(2)    = 0.
         v_x(3)    = (config(1).frame_thick(ilayer)
     &                  -2*config(1).frame_si_thick(ilayer)-
     &                    config(1).frame_al_thick(ilayer)) 

c     CS in FR

         v_name    = 'CS '//layer_label(ilayer)
         v_m       = 'FR '//layer_label(ilayer)
         v_only    = 'ONLY'
         v_copy    = 1
         v_x(1)    = 0.
         v_x(2)    = 0.
         v_x(3)    = -(config(1).frame_thick(ilayer)
     &                   -config(1).frame_CS_thick(ilayer)) 
         
c     WA in CS 
         
         v_name   = 'WA '//layer_label(ilayer)
         v_m      = 'CS '//layer_label(ilayer)
         v_copy   = 1
         v_only   = 'ONLY'
         v_x(1)   = 0.
         v_x(2)   = 0.
         v_x(3)   = config(1).frame_cs_thick(ilayer)
     &                  -config(1).water_channel_thick(ilayer)

c     FB in SL

         v_name   = 'FB '//layer_label(ilayer)
         v_m      = 'SLD'//layer_label(ilayer)
         v_only   = 'ONLY'
         v_x(1)   = 0.
         v_x(2)   = 0.
         v_x(3)   = -ladder_thick+config(1).layer_back(ilayer)

c     loop on ladders of current layer

         do iladder = 1, config(1).n_ladder(ilayer)
            
c     position current ladder into layer
c     calculate the x,y,z position of the center of the ladder
c     include a lateral shift of the ladder position
            
            v_rot     = v_rot + 1
            philadder     = (phi_off + phi_step*float(iladder-1))
            xlad       = radius*cosd(philadder) 
     &                            + lateral*sind(philadder)
            ylad       = radius*sind(philadder) 
     &                            - lateral*cosd(philadder)
            zlad       = 0.
            
            Th1lad     = 90.
            Ph1lad     = 90.+philadder
            Th2lad     = 0.0
            Ph2lad     = 0.0
            Th3lad     = 90.
            Ph3lad     = philadder


c     prepare loop on wafers

            nwafer = config(1).n_wafer(ilayer)
c            zstep  = 2.*wafer_lenght+0.2 ! wkw 10-9-96
            zstep  = 2.*wafer_lenght
            
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
                                      
c     TAS table definition

c     calculate normal vector to the wafer

               normal(1) = cosd(ph3lad)
               normal(2) = sind(ph3lad)
               normal(3) = 0.

c     calculate drift direction

               if (config(1).layer_drift(ilayer).eq.3) then
                  drift(1) = 0.
                  drift(2) = 0.
                  drift(3) = 1.
               else
                  drift(1) = -sind(ph3lad)
                  drift(2) = cosd(ph3lad)
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

               if (ichip .le. geom_h.maxlen) then

                  geom(ichip).id       = 1000*ilayer+100*iwafer
     &                                                  +iladder
                  geom(ichip).id_shape = 
     &                             config(1).layer_shape(ilayer)
                  geom(ichip).ladder   = iladder
                  geom(ichip).layer    = ilayer
                  geom(ichip).num_chip = iwafer
                  geom(ichip).x(1)     = x(1)
                  geom(ichip).x(2)     = x(2)
                  geom(ichip).x(3)     = x(3)
                  geom(ichip).n(1)     = normal(1)
                  geom(ichip).n(2)     = normal(2)
                  geom(ichip).n(3)     = normal(3)
                  geom(ichip).d(1)     = drift(1)
                  geom(ichip).d(2)     = drift(2)
                  geom(ichip).d(3)     = drift(3)
                  geom(ichip).t(1)     = trans(1)
                  geom(ichip).t(2)     = trans(2)
                  geom(ichip).t(3)     = trans(3)
               end if
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

      fact(1)  = 10.
      fact(2)  = 10.
      fact(3)  = 10.
      fact(4)  = 10000.
      fact(5)  = 10000.
      fact(6)  = 10000.
      fact(7)  = 10000.
      fact(8)  = 10.
      fact(9)  = 1.e9
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
      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      
c     set nok row in geom table and increment mod_count
      
      if (ichip .lt. geom_h.maxlen) then
         geom_h.nok = ichip
      else
         geom_h.nok = geom_h.maxlen
      end if

      SVG_AM=STAFCV_OK
      RETURN
      END
