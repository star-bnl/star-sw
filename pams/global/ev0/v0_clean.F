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
#include "ev0_ev0out.inc"
C----------------------------------------------------------------------
      RECORD/ table_head_st/ ev0out_h
      RECORD/ ev0_ev0out_st/ ev0out(*)

C---------------
 
      INTEGER i
 
      REAL degrad,raddeg,pi
*
*
*-----executable statements--------------------------------------------
*
      pi=acos(-1.)
      degrad = pi/180.
      raddeg = 180./pi      
*
*
*
* loop over all found V0
*
      DO 10 i=1,ev0out_h.nok
*
* K0 is everything with Armenteros pt>0.12
*
        IF(ev0out(i).pt.gt.0.12) THEN
         ev0out(i).inv_mass(2)=0.       ! set lambda/lambda_bar mass to 0
         ev0out(i).inv_mass(3)=0.
        ENDIF
*
* Lambda is everything with Armenteros pt<0.12 and pt>0.05 and alpha>0.3
*
        IF( (ev0out(i).pt.lt.0.12).and.(ev0out(i).pt.gt.0.05)
     +       .and.(ev0out(i).alpha.gt.0.3) ) THEN
         ev0out(i).inv_mass(1)=0.       ! set K0/lambda_bar mass to 0
         ev0out(i).inv_mass(3)=0.
        ENDIF
*
*Anti- Lambda is everything with Armenteros pt<0.12 and pt>0.05 and alpha<-0.3
*
        IF( (ev0out(i).pt.lt.0.12).and.(ev0out(i).pt.gt.0.05)
     +       .and.(ev0out(i).alpha.lt.-0.3) ) THEN
         ev0out(i).inv_mass(1)=0.       ! set K0/lambda mass to 0
         ev0out(i).inv_mass(2)=0.
        ENDIF

 10   CONTINUE

999   RETURN
      END


