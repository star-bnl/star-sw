C
      INTEGER FUNCTION XI_EVAL(nok,idd,trak,
     +                         exipar_h,exipar,
     +                         exiout_h,exiout,
     +                         mkine_h,  mkine,
     +                         mvert_h,  mvert)

      IMPLICIT NONE
C   Functional Description : Search for secondary vertices. 
C			     K_0_short and Lambda particles
C			     and anti-particles are 
C			     considered. Only charged decay modes.
C   Created  MARCH-1994	  S. Margetis 
C   Error conditions	: None-Yet
C   Status		: Tested with svt_hits
C----------------------------------------------------------------------
#include "/star/starlib/star/dev/sys/inc/tas_structures.inc"
#include "/star/starlib/star/dev/sys/inc/tas_user_codes.inc"
#include "exi_exipar_pars.inc"
#include "exi_exipar_st.inc"
#include "exi_exiout_pars.inc"
#include "exi_exiout_st.inc"
#include "mct_mvert_pars.inc"
#include "mct_mvert_st.inc"
#include "mct_mkine_pars.inc"
#include "mct_mkine_st.inc"
C----------------------------------------------------------------------
      RECORD/ table_head_st/ exipar_h
      RECORD/ exipar_row_st/ exipar(*)
      RECORD/ table_head_st/ exiout_h
      RECORD/ exiout_row_st/ exiout(*)
      RECORD/ table_head_st/ mvert_h
      RECORD/ mvert_row_st/ mvert(*)
      RECORD/ table_head_st/ mkine_h
      RECORD/ mkine_row_st/ mkine(*)

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
      INTEGER i,j,k,l,q,iv1,iv2
 
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
        iv1=mkine(exiout(i).idba).ivor
        iv2=mkine(exiout(i).idv0).ivor
        IF( (iv1.eq.iv2).and.(iv1.ne.1).and.(iv2.ne.1) ) THEN
         exiout(i).itrue=1
         exiout(i).x_true(1)=mvert(iv1).x(1)
         exiout(i).x_true(2)=mvert(iv1).x(2)
         exiout(i).x_true(3)=mvert(iv1).x(3)
         exiout(i).p_true(1)=mkine(mvert(iv1).orgtrk).p(1)
         exiout(i).p_true(2)=mkine(mvert(iv1).orgtrk).p(2)
         exiout(i).p_true(3)=mkine(mvert(iv1).orgtrk).p(3)
        ELSE
         exiout(i).itrue=0
        ENDIF

 10   CONTINUE

999   RETURN
      END


