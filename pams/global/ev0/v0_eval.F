C
      INTEGER FUNCTION V0_EVAL(nok,idd,trak,
     +                         ev0par_h,ev0par,
     +                         ev0out_h,ev0out,
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
C>----------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "ev0_ev0par.inc"
#include "ev0_ev0out.inc"
#include "g2t_vertex.inc"
#include "g2t_track.inc"
C----------------------------------------------------------------------
      RECORD/ table_head_st/ ev0par_h
      RECORD/ ev0_ev0par_st/ ev0par(*)
      RECORD/ table_head_st/ ev0out_h
      RECORD/ ev0_ev0out_st/ ev0out(*)
      RECORD/ table_head_st/ g2t_vertex_h
      RECORD/ g2t_vertex_st/ g2t_vertex(*)
      RECORD/ table_head_st/ g2t_track_h
      RECORD/ g2t_track_st/ g2t_track(*)

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
 
      INTEGER nok,idd(10000)
      INTEGER i,iv1,iv2
 
      REAL degrad,raddeg,pi
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
* loop over all found V0
*
      DO 10 i=1,ev0out_h.nok
*
* check if daughter particles came from a secondary vertex
*
        iv1=g2t_track(ev0out(i).idneg).start_vertex_p
        iv2=g2t_track(ev0out(i).idpos).start_vertex_p
        IF( (iv1.eq.iv2).and.(iv1.ne.1).and.(iv2.ne.1) ) THEN
         ev0out(i).itrue=1
         ev0out(i).x_true(1)=g2t_vertex(iv1).ge_x(1)
         ev0out(i).x_true(2)=g2t_vertex(iv1).ge_x(2)
         ev0out(i).x_true(3)=g2t_vertex(iv1).ge_x(3)
         ev0out(i).p_true(1)=g2t_track(g2t_vertex(iv1).parent_p).p(1)
         ev0out(i).p_true(2)=g2t_track(g2t_vertex(iv1).parent_p).p(2)
         ev0out(i).p_true(3)=g2t_track(g2t_vertex(iv1).parent_p).p(3)
        ELSE
         ev0out(i).itrue=0
        ENDIF

 10   CONTINUE

999   RETURN
      END
