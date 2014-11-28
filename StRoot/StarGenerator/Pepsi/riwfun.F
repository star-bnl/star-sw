
C **********************************************************************

      DOUBLE PRECISION FUNCTION RIWFUN(V)
      
      IMPLICIT NONE
      
      DOUBLE PRECISION V(2)
      
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/


      REAL V2MIN,V2MAX,V1MIN,V1MAX,V1,V2,DCROSS
      DATA V2MIN,V2MAX/2*0./

      RIWFUN=0.D0
      V1MIN=XMIN
      V1MAX=XMAX
      IF(LST(31).EQ.1) THEN
        V2MIN=Q2MIN
        V2MAX=Q2MAX
      ELSEIF(LST(31).EQ.2) THEN
        V2MIN=YMIN
        V2MAX=YMAX
      ELSEIF(LST(31).EQ.3) THEN
        V2MIN=W2MIN
        V2MAX=W2MAX
      ENDIF
      V1=V1MIN+V(1)*(V1MAX-V1MIN)
      V2=V2MIN+V(2)*(V2MAX-V2MIN)
      RIWFUN=DCROSS(V1,V2)*(V1MAX-V1MIN)*(V2MAX-V2MIN)

      RETURN
      END
