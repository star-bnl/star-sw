      INTEGER FUNCTION V0_CLEAN( ev0out_h,ev0out   )

C>------------------------------------------------------------------ 
C   Functional Description : Set masses to zero is v0 doesn't
C                            sit in correct armenteros plot position
C   Created  MARCH-1994	  S. Margetis 
C   Error conditions	: None-Yet
C   Status		: Tested with svt_hits
C>--------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "dst_v0_vertex.inc"
C----------------------------------------------------------------------
      RECORD/ table_head_st/ ev0out_h
      RECORD/ dst_v0_vertex_st/ ev0out(*)

C---------------
 
      INTEGER i
 
*
*
*-----executable statements--------------------------------------------
*    
*
*
*
* loop over all found V0
*
C      DO 10 i=1,ev0out_h.nok
*
* K0 is everything with Armenteros pt>0.12
*
C        IF(ev0out(i).pt.gt.0.12) THEN
C         ev0out(i).inv_mass_la=0.       ! set lambda/lambda_bar mass to 0
C         ev0out(i).inv_mass_lb=0.
C        ENDIF
*
* Lambda is everything with Armenteros pt<0.12 and pt>0.05 and alpha>0.3
*
C        IF( (ev0out(i).pt.lt.0.12).and.(ev0out(i).pt.gt.0.05)
C     +       .and.(ev0out(i).alpha.gt.0.3) ) THEN
C         ev0out(i).inv_mass_k0=0.       ! set K0/lambda_bar mass to 0
C         ev0out(i).inv_mass_lb=0.
C        ENDIF
*
*Anti- Lambda is everything with Armenteros pt<0.12 and pt>0.05 and alpha<-0.3
*
C        IF( (ev0out(i).pt.lt.0.12).and.(ev0out(i).pt.gt.0.05)
C     +       .and.(ev0out(i).alpha.lt.-0.3) ) THEN
C         ev0out(i).inv_mass_k0=0.       ! set K0/lambda mass to 0
C         ev0out(i).inv_mass_la=0.
C        ENDIF

C 10   CONTINUE

999   RETURN
      END


