CC:>--------------------------------------------------------------------
CC: FILE:       svg_am_ssd.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.11 1998/06/23 15:52:43 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION SVG_AM_SSD(
     1              geom_h,              geom ) 
      IMPLICIT NONE
#include "svg_am_ssd.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    SVG_AM_SSD
CC: DESCRIPTION: extends the SVT geometry table with the SSD geometry
CC:              it is assumed that the table is already filled with 
CC:              the SVT information. The module reads the SSD info from
CC:              the file geom_svt4.ascii attached to the logical unit
CC:              number 44. This file has been produced after reading
CC:              an output file of geant (in euclid format).
CC:
CC: AUTHOR:    Lilian MARTIN
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:             geom     - description here
CC:           geom_h     - Header Structure for geom
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

      INTEGER*4 I,J,wafer,layer,ladder,num_chip
      REAL x,y,z,dx,dy,dz,nx,ny,nz,tx,ty,tz

C: Sanity check of header for table "geom"...
      IF( ( geom_h.maxlen .LE. 0 ) .OR.
     +    ( geom_h.nok .GT. geom_h.maxlen ) ) THEN
         SVG_AM_SSD = STAFCV_BAD
         RETURN
      ENDIF
      IF (geom_h.maxlen.ne.536.or.geom_h.nok.ne.536) THEN
         WRITE(6,*)' WARNING WARNING WARNING'
         WRITE(6,*)' The SSD geometry can not be loaded, the geom table has the wrong size'
         RETURN
      ENDIF
      open(44,file='params/svt/geom_svt4.ascii',status='old')
      layer=7
      do i=1,320
         j=i+216
         read(44,*)wafer,x,y,z,dx,dy,dz,nx,ny,nz,tx,ty,tz
         num_chip=(wafer-layer*1000)/100
         ladder=wafer-1000*layer-100*num_chip
         geom(j).id=wafer
         geom(j).id_shape=2
         geom(j).ladder=ladder
         geom(j).layer=layer
         geom(j).num_chip=num_chip
         geom(j).x(1)=x
         geom(j).x(2)=y
         geom(j).x(3)=z
         geom(j).n(1)=nx
         geom(j).n(2)=ny
         geom(j).n(3)=nz
         geom(j).d(1)=dx
         geom(j).d(2)=dy
         geom(j).d(3)=dz
         geom(j).t(1)=tx
         geom(j).t(2)=ty
         geom(j).t(3)=tz
      enddo
      close(44)
      SVG_AM_SSD=STAFCV_OK
      RETURN
      END
