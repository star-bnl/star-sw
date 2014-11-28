*72*********************************************************************
      FUNCTION POLDQCD(ICOSFI,IPART,IP,XP,ZP,YY)

      IMPLICIT NONE

*...Added common block LEPTOU to access to polarization state,

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER ICOSFI,IP,IPART
      REAL POLDQCD,XP,ZP,YY,C1,C2,C3,C4

C...First order QCD matrix elements from R.D. Peccei and R. Ruckl:
C...Nucl. Phys. B162 (1980) 125
*...Polarized part: notes of L. Mankiewicz

C...Constants C1 to C5 are resp. 2/3/pi, 1/4/pi, 4/3/pi, 1/2/pi, 1/pi
      DATA C1,C2,C3,C4/0.2122066,0.0795775,0.4244132,0.1591549/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        POLDQCD=0.0
        RETURN  
      ENDIF     

      IF(ICOSFI.EQ.0) THEN
       IF(IPART.EQ.1) THEN
         IF(IP.EQ.1) THEN
           POLDQCD=0
         ELSEIF(IP.EQ.2) THEN
           POLDQCD=0
         ELSEIF(IP.EQ.3) THEN
              POLDQCD=C1*((1.-XP)/(1.-ZP) + (1.-ZP)/(1.-XP) + 
     1             2.*XP*ZP/((1.-XP)*(1.-ZP)) + 2.*(ZP+XP-1.))
         ELSE
           WRITE(6,1000) ICOSFI,IPART,IP
         ENDIF
       ELSEIF(IPART.EQ.2) THEN
         IF(IP.EQ.1) THEN
           POLDQCD= 0.
         ELSEIF(IP.EQ.2) THEN
           POLDQCD=0.
         ELSEIF(IP.EQ.3) THEN
            POLDQCD=C2*(2.*XP-1)*(ZP/(1.-ZP) + (1.-ZP)/ZP)
         ELSE
           WRITE(6,1000) ICOSFI,IPART,IP
         ENDIF
       ELSE
         WRITE(6,1000) ICOSFI,IPART,IP
       ENDIF

      ELSEIF(ICOSFI.EQ.1) THEN
       IF(IPART.EQ.1) THEN
         IF(IP.EQ.1) THEN
          POLDQCD= 0.
         ELSEIF(IP.EQ.3) THEN
              POLDQCD = C3*YY*
     &             SQRT((1.-YY)*XP*ZP/(1.-XP)/(1.-ZP))* 
     &             (1.-XP-ZP)
          ELSE
           WRITE(6,1000) ICOSFI,IPART,IP
         ENDIF
       ELSEIF(IPART.EQ.2) THEN
         IF(IP.EQ.1) THEN
           POLDQCD= 0.
         ELSEIF(IP.EQ.3) THEN
             POLDQCD=C4*YY*SQRT((1.-YY)*XP*(1.-XP)/ZP/(1.-ZP))*
     &        (1.-2.*ZP)
         ELSE
           WRITE(6,1000) ICOSFI,IPART,IP
         ENDIF
       ENDIF

      ELSEIF(ICOSFI.EQ.2) THEN
       IF(IPART.EQ.1) THEN
         POLDQCD= 0.
       ELSEIF(IPART.EQ.2) THEN
         POLDQCD= 0.
       ELSE
         WRITE(6,1000) ICOSFI,IPART,IP
       ENDIF

      ELSE
       WRITE(6,1000) ICOSFI,IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine POLDQCD     ',
     &' ICOSFI, IPART, IP = ',3I10)
      END
