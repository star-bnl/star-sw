      INTEGER*4 FUNCTION STK_AM_VERTEX_DIRECT(
     1         g2t_event_h,         g2t_event ,
     2        g2t_vertex_h,        g2t_vertex ,
     3               vtx_h,               vtx ,
     4        vtx_direct_h,        vtx_direct ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     fill vtx with the smeared mc/geant primary vertex
C     
C     DESCRIPTION:
c     This module fills the table vtx with the postion of the primary
c     vertex used by geant/monte-carlo. The position is smeared along the 3 
c     axis with a gaussian using parameters from table vtx_direct
c     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
c     
C     DATE:
c     15-nov-1993 C.A.P. written
c     19-nov-1993 C.A.P  added the error on the vertex position
c     22-mar-1995 WKW put random seed initialization via 
c                                       vtx_direct.init_ran
c     
C     ARGUMENTS:
c     vtx        smeared vertex
c     vtx_direct smearing parameters
c     
     
C     argument declarations
C     ---------------------
#include "stk_am_vertex_direct.inc"
      
      
C     Local Declarations:
C     ===================
      real     dummy
      external rg32
      real     rg32
      integer ivert

C     RETURN:
C     =======
C     FORMAT:
C     =======
 1001 format(1x,'<e>-stk_am_vertex_direct : no rows in',
     &     'g2t_vertex table ')
 1002 format(1x,'<w>-stk_am_vertex_direct : no rows in',
     &     'vtx_direct, will use defaults')

C     EXECUTABLE CODE:
C     ================

      WRITE(*,*) 'STK_AM_VERTEX_DIRECT: Starting ######################'

      stk_am_vertex_direct = stafcv_ok

      vtx_h.nok=0            

c     check if the input tables are empty

c     initialize gaussain random number generator 
      if (vtx_direct(1).init_ran.ne.0) then
          call rg32in(vtx_direct(1).init_ran)
      endif

      if (g2t_vertex_h.nok.eq.0) then
         stk_am_vertex_direct = stafcv_bad ! no vertex
         write(6,1001)
         return
      else if (vtx_direct_h.nok.eq.0) then
         write(6,1002)
         vtx_direct(1).sx(1) = 0.005  ! 50 microns
         vtx_direct(1).sx(2) = 0.005  ! 50 microns
         vtx_direct(1).sx(3) = 0.005  ! 50 microns
         vtx_direct_h.nok    = 1
      end if

      vtx(1).flag   = 1    ! means OK
      vtx(1).id     = g2t_event(1).prim_vertex_p
      do ivert=1,g2t_vertex_h.nok
	 if (g2t_vertex(ivert).id.eq.g2t_event(1).prim_vertex_p) then
	      vtx(1).t      = 0 
	      vtx(1).x(1)   = g2t_vertex(ivert).ge_x(1) +
     &		     vtx_direct(1).sx(1)*rg32(dummy)
	      vtx(1).x(2)   = g2t_vertex(ivert).ge_x(2) +
     & 		     vtx_direct(1).sx(2)*rg32(dummy)
	      vtx(1).x(3)   = g2t_vertex(ivert).ge_x(3) +
     &		     vtx_direct(1).sx(3)*rg32(dummy)
	      vtx(1).cov(1) = vtx_direct(1).sx(1)
	      vtx(1).cov(2) = 0.
	      vtx(1).cov(3) = 0.
	      vtx(1).cov(4) = vtx_direct(1).sx(2)
	      vtx(1).cov(5) = 0.
	      vtx(1).cov(6) = vtx_direct(1).sx(3)

	      vtx_h.nok = vtx_h.nok+1
	  endif
      enddo

      if (vtx_h.nok.ne.1) then
	  stk_am_vertex_direct=stafcv_bad
          return
      endif	

      WRITE(*,*) 'STK_AM_VERTEX_DIRECT: Stopping #####################'
      STK_AM_VERTEX_DIRECT=STAFCV_OK
      RETURN
      END
