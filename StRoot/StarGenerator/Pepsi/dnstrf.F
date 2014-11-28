
*72*********************************************************************

      SUBROUTINE DNSTRF(X,Q2,XDPQ)

      IMPLICIT NONE

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      
      REAL X,Q2,XDPQ,XDU,XDD,XDUB,XDDB
      DIMENSION XDPQ(-6:6)

      IF(LST(15).LE.100) THEN
         WRITE(6,5000) LST(15) 
      ELSEIF(LST(15).gt.100.and.LST(15).lt.1000) THEN !|bs> add LST(15)<1000
         CALL DSTFU(2212,X,Q2,XDPQ)
         IF(PARI(11).LE.1.E-06) RETURN
*...  Save polarized d and u valence in proton. 
*...  USE TWO EMPTY CELLS OF ARRAY PARI: PARI(38) AND PARI(39)
         XDD=XDPQ(1)
         XDU=XDPQ(2)
         PARI(38)=XDD
         PARI(39)=XDU
C... ALEX: Seems to me that we need these in polleptox.F as well
C...  USE TWO EMPTY CELLS OF ARRAY PARI: PARI(44) AND PARI(45)
         XDDB=XDPQ(-1)
         XDUB=XDPQ(-2)
         PARI(44)=XDDB
         PARI(45)=XDUB
C...  For nuclear target, mix u- and d-valence distributions.
         IF(LST(39).EQ.0) THEN
               XDPQ(1) =(1.-PARI(11))*XDD+PARI(11)*XDU
               XDPQ(2) =(1.-PARI(11))*XDU+PARI(11)*XDD
               XDPQ(-1)=(1.-PARI(11))*XDDB+PARI(11)*XDUB
               XDPQ(-2)=(1.-PARI(11))*XDUB+PARI(11)*XDDB
         ENDIF
      ENDIF

 5000 FORMAT(' Error: unknown polarized structure function;', 
     & ' library, set =',3I5)

      RETURN
      END
