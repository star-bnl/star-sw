
      SUBROUTINE PHO_CKMTPA(Ipa,Xmi,Xma,Ala,Q2mi,Q2ma,Pdfna)
C**********************************************************************
C
C     PDF based on Regge theory, evolved with .... by ....
C
C     input: IPAR     2212   proton (not installed)
C                      990   Pomeron
C
C     output: parameters of parametrization
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Ala , Q2ma , Q2mi , Xma , Xmi
      INTEGER Ipa
      SAVE 
 
      CHARACTER*8 Pdfna
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      REAL prop(40) , pomp(40)
      DATA prop/.230000E+00 , .200000E+01 , .150200E+00 , .120000E+01 , 
     &     .263100E+00 , .645200E+00 , .354890E+01 , .111700E+01 , 
     &     .415000E+00 , .768400E-01 , .100000E+00 , .330000E-01 , 
     &     .352102E-01 , .200000E+01 , .200000E+01 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .100000E+00 , .200000E+01 , .100000E+09/
      DATA pomp/.230000E+00 , .500000E+01 , .150200E+00 , .120000E+01 , 
     &     .263100E+00 , .645200E+00 , .354890E+01 , .111700E+01 , 
     &     .415000E+00 , .768400E-01 , .700000E-01 , .700000E-01 , 
     &     .137161E+00 , .300000E+01 , .200000E+01 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .000000E+00 , .000000E+00 , .000000E+00 , 
     &     .000000E+00 , .100000E+00 , .500000E+01 , .100000E+09/
 
      IF ( Ipa.EQ.2212 ) THEN
         Ala = prop(1)
         Q2mi = prop(39)
         Q2ma = prop(40)
         Pdfna = 'CKMT-PRO'
      ELSE IF ( Ipa.EQ.990 ) THEN
         Ala = pomp(1)
         Q2mi = pomp(39)
         Q2ma = pomp(40)
         Pdfna = 'CKMT-POM'
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I7)')
     &         'PHO_CKMTPA:ERROR: invalid particle code' , Ipa
         STOP
      END IF
      Xmi = 1.D-4
      Xma = 1.D0
      END SUBROUTINE
