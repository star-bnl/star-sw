
*72*********************************************************************
      FUNCTION POLDQCDI(IPART,IP,XP,ZPMIN,ZPMAX)

       IMPLICIT NONE
C...First order QCD matrix elements as in function DQCD but analytically
C...integrated over ZP from ZPMIN to ZPMAX, also a factor 1/(1-XP) is
C...factored out (since XP chosen randomly according to 1/(1-XP) distr.)

*...Added common block LEPTOU to access to polarization state.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER IPART,IP
      REAL XP,ZPMIN,ZPMAX,C1,C2,ZLOG,POLDQCDI
      DATA C1,C2/0.2122066,0.0795775/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        POLDQCDI=0.0
        RETURN   
      ENDIF      

      IF(IPART.EQ.1) THEN
       IF(IP.EQ.1) THEN
         POLDQCDI = 0.
       ELSEIF(IP.EQ.2) THEN
         POLDQCDI = 0.
       ELSEIF(IP.EQ.3) THEN
        ZLOG=ALOG(ZPMAX/ZPMIN)  
        POLDQCDI=C1*(XP**2*ZLOG+ZPMIN-ZPMAX+(ZPMIN**2-ZPMAX**2)/2.+ZLOG+
     &  2.*XP*(1.-XP)*(ZPMAX-ZPMIN)+(1.-XP)*(ZPMAX**2-ZPMIN**2))
       ELSE
         WRITE(6,1000) IPART,IP
       ENDIF

      ELSEIF(IPART.EQ.2) THEN
       IF(IP.EQ.1) THEN
         POLDQCDI = 0.
       ELSEIF(IP.EQ.2) THEN
         POLDQCDI = 0.
       ELSEIF(IP.EQ.3)  THEN
         ZLOG=ALOG(ZPMAX/ZPMIN)  
         POLDQCDI=C2*(2.*XP-1.)*(1.-XP)*(2.*ZLOG+2.*ZPMIN-2.*ZPMAX)
       ELSE
         WRITE(6,1000) IPART,IP
       ENDIF

      ELSE
       WRITE(6,1000) IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine POLDQCDI     ',
     &' IPART, IP = ',2I10)
      END
