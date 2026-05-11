
      DOUBLE PRECISION FUNCTION DT_SANO(Ecm)
 
C***********************************************************************
C This version dated 31.07.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION afra1 , afra2 , Ecm , ecmano , fraano , ONE , 
     &                 rate , sighrd , TINY10 , TINY14 , TWO , ZERO
      INTEGER ie , j1 , j2 , NE
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY14=1.0D-14,ZERO=0.0D0,ONE=1.0D0,
     &           TWO=2.0D0)
      PARAMETER (NE=8)
 
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
 
      DIMENSION ecmano(NE) , fraano(NE) , sighrd(NE)
      DATA ecmano/0.200D+02 , 0.500D+02 , 0.100D+03 , 0.200D+03 , 
     &     0.500D+03 , 0.100D+04 , 0.200D+04 , 0.500D+04/
C fixed cut (3 GeV/c)
      DATA fraano/0.085D+00 , 0.114D+00 , 0.105D+00 , 0.091D+00 , 
     &     0.073D+00 , 0.062D+00 , 0.054D+00 , 0.042D+00/
      DATA sighrd/4.0099D-04 , 3.3104D-03 , 1.1905D-02 , 3.6435D-02 , 
     &     1.3493D-01 , 3.3086D-01 , 7.6255D-01 , 2.1319D+00/
C running cut (based on obsolete Phojet-caluclations, bugs..)
C     DATA FRAANO /
C    &             0.251E+00,0.313E+00,0.279E+00,0.239E+00,0.186E+00,
C    &             0.167E+00,0.150E+00,0.131E+00
C    &            /
C     DATA SIGHRD /
C    &           6.6569E-04,4.4949E-03,1.4837E-02,4.1466E-02,1.5071E-01,
C    &           2.5736E-01,4.5593E-01,8.2550E-01
C    &            /
 
      DT_SANO = ZERO
      IF ( (ISHad(2).NE.1) .OR. (IJProj.NE.7) ) RETURN
      j1 = 0
      j2 = 0
      rate = ONE
      IF ( Ecm.GE.ecmano(NE) ) THEN
         j1 = NE
         j2 = NE
      ELSE IF ( Ecm.GT.ecmano(1) ) THEN
         DO ie = 2 , NE
            IF ( Ecm.LT.ecmano(ie) ) THEN
               j1 = ie - 1
               j2 = ie
               rate = LOG10(Ecm/ecmano(j1))/LOG10(ecmano(j2)/ecmano(j1))
               GOTO 100
            END IF
         END DO
      END IF
 100  IF ( (j1.GT.0) .AND. (j2.GT.0) ) THEN
         afra1 = LOG10(MAX(fraano(j1)*sighrd(j1),TINY14))
         afra2 = LOG10(MAX(fraano(j2)*sighrd(j2),TINY14))
         DT_SANO = 10.0D0**(afra1+rate*(afra2-afra1))
      END IF
 
      END FUNCTION
