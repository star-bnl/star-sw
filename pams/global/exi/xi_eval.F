C-------------------------------------------------------------------------
      INTEGER FUNCTION XI_EVAL(nok,idd,trak,
     +                         exipar_h,exipar,
     +                         exiout_h,exiout,
     +                         g2t_track_h,  g2t_track,
     +                         g2t_vertex_h,  g2t_vertex)
C>---------------------------------------------------------------------
C   Functional Description : Search for secondary vertices. 
C			     K_0_short and Lambda particles
C			     and anti-particles are 
C			     considered. Only charged decay modes.
C   Created  MARCH-1994	  S. Margetis 
C   Error conditions	: None-Yet
C   Status		: Tested with svt_hits
C<---------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "exi_exipar.inc"
#include "exi_exiout.inc"
#include "g2t_vertex.inc"
#include "g2t_track.inc"
C----------------------------------------------------------------------
      RECORD/ TABLE_HEAD_ST/ exipar_h
      RECORD/ EXI_EXIPAR_ST/ exipar(*)
      RECORD/ TABLE_HEAD_ST/ exiout_h
      RECORD/ EXI_EXIOUT_ST/ exiout(*)
      RECORD/ TABLE_HEAD_ST/ g2t_vertex_h
      RECORD/ G2T_VERTEX_ST/ g2t_vertex(*)
      RECORD/ TABLE_HEAD_ST/ g2t_track_h
      RECORD/ G2T_TRACK_ST/ g2t_track(*)

C---for testing purposes only
      STRUCTURE/local_track/
        INTEGER id
        REAL*4 r
        REAL*4 rf
        REAL*4 z
        REAL*4 f
        REAL*4 tanl
        REAL*4 qR
      END STRUCTURE
 
      RECORD /local_track/ trak(10000)
C---------------
 
      INTEGER nok,id,idd(10000),id1,id2
      INTEGER ierr,ii,kk,ll,iflag,iflag1
      INTEGER i,j,k,l,iv1,iv2
 
      REAL degrad,raddeg,trki(6),pi,ok
      REAL masst(50),mass1(3),mass2(3),qqt(50)
*
      DATA masst/7*0.0005,.139567,.139567
     +,            3*0.493667,2*.93828,.93828
     +,            35*0./
      DATA mass1/0.139567,.93828,.139567/
      DATA mass2/0.139567,.139567,.93828/
      DATA qqt/0.,1.,-1.,0.,1.,-1.,0. ,1.,-1.,0.,1.,-1.,0.
     +,        1.,-1.,35*0./
*
*
*-----executable statements--------------------------------------------
*
      pi=acos(-1.)
      degrad = pi/180.
      raddeg = 180./pi      
*
* loop over all found Xi
*
      DO 10 i=1,exiout_h.nok
*
* check if daughter particles came from a secondary vertex
*
        iv1=g2t_track(exiout(i).idba).start_vertex_p
        iv2=g2t_track(exiout(i).idv0).start_vertex_p
        IF( (iv1.eq.iv2).and.(iv1.gt.1) ) THEN
         exiout(i).itrue=1
         exiout(i).x_true(1)=g2t_vertex(iv1).ge_x(1)
         exiout(i).x_true(2)=g2t_vertex(iv1).ge_x(2)
         exiout(i).x_true(3)=g2t_vertex(iv1).ge_x(3)
         exiout(i).p_true(1)=g2t_track(g2t_vertex(iv1).parent_p).p(1)
         exiout(i).p_true(2)=g2t_track(g2t_vertex(iv1).parent_p).p(2)
         exiout(i).p_true(3)=g2t_track(g2t_vertex(iv1).parent_p).p(3)
        ELSE
         exiout(i).itrue=0
        ENDIF

 10   CONTINUE

999   CONTINUE
      XI_EVAL = 0
      RETURN

      END


