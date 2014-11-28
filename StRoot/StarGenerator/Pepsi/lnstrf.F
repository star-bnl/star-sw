
C ********************************************************************

      SUBROUTINE LNSTRF(X,Q2,XPQ)

      IMPLICIT NONE

C...Structure function per nucleon for a proton/neutron mixture
C...according to defined nucleus.
CGI-001122 Updated to save and mix total d- and u-quark distributions
C...instead of only the valence parts.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/


      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      REAL X,Q2,XPQ,XDV,XUV,XD,XU,XUB,XDB
      DIMENSION XPQ(-6:6)

      CALL LYSTFU(2212,X,Q2,XPQ)

      IF(PARI(11).LE.1.E-06) RETURN
C...Total d- and u-quark densities in proton, save for use in LEPTOX
      XD=XPQ(1)
      XU=XPQ(2)
      PARI(12)=XD
      PARI(13)=XU
CMM-010306 Saving the anti d- and anti u-quark densities
Cstep1
C...anti-d- and anti-u-quark densities in proton, save for use in LEPTOX
      XDB=XPQ(-1)
      XUB=XPQ(-2)
      PARI(42)=XDB
      PARI(43)=XUB
      IF(LST(39).EQ.0) THEN
C...For nuclear target, mix d- and u-quark distributions.
        XPQ(1)=(1.-PARI(11))*XD+PARI(11)*XU
        XPQ(2)=(1.-PARI(11))*XU+PARI(11)*XD
C...mix anti-d- and anti-u-quark distributions.
        XPQ(-1)=(1.-PARI(11))*XDB+PARI(11)*XUB    
        XPQ(-2)=(1.-PARI(11))*XUB+PARI(11)*XDB
      ENDIF
      RETURN
      END
