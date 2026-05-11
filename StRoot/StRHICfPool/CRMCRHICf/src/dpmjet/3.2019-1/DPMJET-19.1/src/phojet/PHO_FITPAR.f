
      SUBROUTINE PHO_FITPAR(Ioutp)
C**********************************************************************
C
C     read input parameters according to PDFs
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alam2 , DEFA , DEFB , dummy , PHO_ALPHAS , 
     &                 q2max , q2min , THOUS , xmax , xmin
      INTEGER i , i1 , i2 , idpa1 , idpa2 , ierr , ifound , ifpas , 
     &        init , inum , Ioutp , k , MAX_TAB, kpflen
      SAVE 
 
      PARAMETER (DEFA=-99999.D0,DEFB=-100000.D0,THOUS=1.D3)
 
#ifdef FOR_FLUKA
      INCLUDE '(IOUNIT)'
#endif
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  current beam selection
      INCLUDE 'inc/pobeam'
 
      DIMENSION inum(3) , ifpas(2)
      CHARACTER*8 cname8 , pdfna1 , pdfna2
      CHARACTER*10 cnam10
 
      PARAMETER (MAX_TAB=23)
      DIMENSION xdptab(27,MAX_TAB) , idptab(8,MAX_TAB)
      REAL xdptab
      INTEGER idptab
 
C  parameter set for   2212 (GRV94 LO)     2212 (GRV94 LO)
      DATA (idptab(k,1),k=1,8)/2212 , 5 , 6 , 0 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,1),k=1,27)/1.1000E+00 , 2.5000E-01 , 6.3870E+00 , 
     &      6.3870E+00 , 1.1610E+00 , 1.1610E+00 , 4.5000E-01 , 
     &      9.0000E-01 , 1.0263E+01 , 1.0263E+01 , 1.1710E+00 , 
     &      1.1710E+00 , 1.5600E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 1.0000E+00 , 0.0000E+00 , 1.0000E+00 , 
     &      0.0000E+00 , 3.5000E+00 , 2.0000E+00 , 6.0000E-01 , 
     &      6.0000E-01 , 1.1000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for   2212 (GRV94 LO)    -2212 (GRV94 LO)
      DATA (idptab(k,2),k=1,8)/2212 , 5 , 6 , 0 , -2212 , 5 , 6 , 0/
      DATA (xdptab(k,2),k=1,27)/1.1000E+00 , 2.5000E-01 , 6.3870E+00 , 
     &      6.3870E+00 , 1.1610E+00 , 1.1610E+00 , 4.5000E-01 , 
     &      9.0000E-01 , 1.5174E+01 , 1.5174E+01 , 1.5400E+00 , 
     &      1.5400E+00 , 1.5600E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 1.0000E+00 , 0.0000E+00 , 1.0000E+00 , 
     &      0.0000E+00 , 3.5000E+00 , 2.0000E+00 , 6.0000E-01 , 
     &      6.0000E-01 , 1.1000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (GRV-G LO)     2212 (GRV94 LO)
      DATA (idptab(k,3),k=1,8)/22 , 5 , 3 , 0 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,3),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (GRV-G LO)       22 (GRV-G LO)
      DATA (idptab(k,4),k=1,8)/22 , 5 , 3 , 0 , 22 , 5 , 3 , 0/
      DATA (xdptab(k,4),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      2.7450E+00 , 1.2250E+00 , 1.2250E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 4.7210E+00 , 4.6200E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 4.3100E-03 , 
     &      8.0000E-05 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (GRS-G LO)     2212 (GRV94 LO)
      DATA (idptab(k,5),k=1,8)/22 , 5 , 4 , 4 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,5),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (GRS-G LO)       22 (GRS-G LO)
      DATA (idptab(k,6),k=1,8)/22 , 5 , 4 , 4 , 22 , 5 , 4 , 4/
      DATA (xdptab(k,6),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      2.7450E+00 , 1.2250E+00 , 1.2250E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 4.7210E+00 , 4.6200E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 4.3100E-03 , 
     &      8.0000E-05 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (SaS-1D  )       22 (SaS-1D  )
      DATA (idptab(k,7),k=1,8)/22 , 1 , 1 , 4 , 22 , 1 , 1 , 4/
      DATA (xdptab(k,7),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.1170E+00 , 
     &      3.1170E+00 , 1.3450E+00 , 1.3450E+00 , 3.0200E-01 , 
     &      1.0000E+00 , 6.6050E+00 , 6.6050E+00 , 1.7500E-01 , 
     &      1.7500E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.0900E-03 , 9.0000E-05 , 4.0900E-03 , 
     &      9.0000E-05 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (SaS-1M  )       22 (SaS-1M  )
      DATA (idptab(k,8),k=1,8)/22 , 1 , 2 , 4 , 22 , 1 , 2 , 4/
      DATA (xdptab(k,8),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.5540E+00 , 
     &      2.5540E+00 , 1.0910E+00 , 1.0910E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.2580E+00 , 4.2580E+00 , 4.9000E-01 , 
     &      4.9000E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.5700E-03 , 1.0000E-04 , 4.5700E-03 , 
     &      1.0000E-04 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (SaS-2D  )       22 (SaS-2D  )
      DATA (idptab(k,9),k=1,8)/22 , 1 , 3 , 4 , 22 , 1 , 3 , 4/
      DATA (xdptab(k,9),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.5330E+00 , 
     &      2.5330E+00 , 1.1340E+00 , 1.1340E+00 , 5.0100E-01 , 
     &      1.0000E+00 , 4.2300E+00 , 4.2300E+00 , 4.9300E-01 , 
     &      4.9300E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.5900E-03 , 1.0000E-04 , 4.5900E-03 , 
     &      1.0000E-04 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (SaS-2M  )       22 (SaS-2M  )
      DATA (idptab(k,10),k=1,8)/22 , 1 , 4 , 4 , 22 , 1 , 4 , 4/
      DATA (xdptab(k,10),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.8220E+00 , 
     &      2.8220E+00 , 1.0910E+00 , 1.0910E+00 , 4.9100E-01 , 
     &      1.0000E+00 , 4.6870E+00 , 4.6870E+00 , 4.5800E-01 , 
     &      4.5800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.6600E-03 , 3.0000E-05 , 4.6600E-03 , 
     &      3.0000E-05 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (LAC     )     2212 (GRV94 LO)
      DATA (idptab(k,11),k=1,8)/22 , 3 , 1 , 3 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,11),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.3050E+00 , 
     &      6.8270E+00 , 9.4500E-01 , 1.1360E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.7120E+00 , 1.1740E+01 , 2.5800E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.3400E-03 , 2.4000E-04 , 1.0000E+00 , 
     &      0.0000E+00 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (PDFLIB2 )     2212 (GRV94 LO)
      DATA (idptab(k,12),k=1,8)/22 , 3 , 1 , 2 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,12),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.3050E+00 , 
     &      6.8270E+00 , 9.4500E-01 , 1.1360E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.7120E+00 , 1.1740E+01 , 2.5800E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.3400E-03 , 2.4000E-04 , 1.0000E+00 , 
     &      0.0000E+00 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (LAC     )       22 (LAC     )
      DATA (idptab(k,13),k=1,8)/22 , 3 , 1 , 3 , 22 , 3 , 1 , 3/
      DATA (xdptab(k,13),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.3050E+00 , 
     &      3.3050E+00 , 9.4500E-01 , 9.4500E-01 , 4.5000E-01 , 
     &      1.0000E+00 , 6.7120E+00 , 6.7120E+00 , 2.5800E-01 , 
     &      2.5800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.3400E-03 , 2.4000E-04 , 3.3400E-03 , 
     &      2.4000E-04 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (PDFLIB2 )       22 (PDFLIB2 )
      DATA (idptab(k,14),k=1,8)/22 , 3 , 1 , 2 , 22 , 3 , 1 , 2/
      DATA (xdptab(k,14),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.3050E+00 , 
     &      3.3050E+00 , 9.4500E-01 , 9.4500E-01 , 4.5000E-01 , 
     &      1.0000E+00 , 6.7120E+00 , 6.7120E+00 , 2.5800E-01 , 
     &      2.5800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.3400E-03 , 2.4000E-04 , 3.3400E-03 , 
     &      2.4000E-04 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (LAC     )     2212 (GRV94 LO)
      DATA (idptab(k,15),k=1,8)/22 , 3 , 2 , 3 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,15),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.1450E+00 , 
     &      6.8270E+00 , 1.0490E+00 , 1.1360E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.3680E+00 , 1.1740E+01 , 1.4700E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.8700E-03 , 1.1000E-04 , 1.0000E+00 , 
     &      0.0000E+00 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (PDFLIB2 )     2212 (GRV94 LO)
      DATA (idptab(k,16),k=1,8)/22 , 3 , 2 , 2 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,16),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.1450E+00 , 
     &      6.8270E+00 , 1.0490E+00 , 1.1360E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.3680E+00 , 1.1740E+01 , 1.4700E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.8700E-03 , 1.1000E-04 , 1.0000E+00 , 
     &      0.0000E+00 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (LAC     )       22 (LAC     )
      DATA (idptab(k,17),k=1,8)/22 , 3 , 2 , 3 , 22 , 3 , 2 , 3/
      DATA (xdptab(k,17),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.1450E+00 , 
     &      3.1450E+00 , 1.0490E+00 , 1.0490E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.3680E+00 , 6.3680E+00 , 1.4700E-01 , 
     &      1.4700E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.8700E-03 , 1.1000E-04 , 3.8700E-03 , 
     &      1.0000E-04 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (PDFLIB2 )       22 (PDFLIB2 )
      DATA (idptab(k,18),k=1,8)/22 , 3 , 2 , 2 , 22 , 3 , 2 , 2/
      DATA (xdptab(k,18),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.1450E+00 , 
     &      3.1450E+00 , 1.0490E+00 , 1.0490E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.3680E+00 , 6.3680E+00 , 1.4700E-01 , 
     &      1.4700E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 3.8700E-03 , 1.1000E-04 , 3.8700E-03 , 
     &      1.0000E-04 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (LAC     )     2212 (GRV94 LO)
      DATA (idptab(k,19),k=1,8)/22 , 3 , 3 , 3 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,19),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.0510E+00 , 
     &      6.8270E+00 , 1.0500E+00 , 1.1360E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.0060E+00 , 1.1740E+01 , 2.0500E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.0200E-03 , 1.0000E-04 , 1.0000E+00 , 
     &      0.0000E+00 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (PDFLIB2 )     2212 (GRV94 LO)
      DATA (idptab(k,20),k=1,8)/22 , 3 , 3 , 2 , 2212 , 5 , 6 , 0/
      DATA (xdptab(k,20),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.0510E+00 , 
     &      6.8270E+00 , 1.0500E+00 , 1.1360E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.0060E+00 , 1.1740E+01 , 2.0500E-01 , 
     &      4.6200E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.0200E-03 , 1.0000E-04 , 1.0000E+00 , 
     &      0.0000E+00 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
C  parameter set for     22 (LAC     )       22 (LAC     )
      DATA (idptab(k,21),k=1,8)/22 , 3 , 3 , 3 , 22 , 3 , 3 , 3/
      DATA (xdptab(k,21),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.0510E+00 , 
     &      3.0510E+00 , 1.0500E+00 , 1.0500E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.0060E+00 , 6.0060E+00 , 2.0500E-01 , 
     &      2.0500E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.0200E-03 , 1.0000E-04 , 4.0200E-03 , 
     &      1.0000E-04 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C  parameter set for     22 (PDFLIB2 )       22 (PDFLIB2 )
      DATA (idptab(k,22),k=1,8)/22 , 3 , 3 , 2 , 22 , 3 , 3 , 2/
      DATA (xdptab(k,22),k=1,27)/1.0970E+00 , 2.5000E-01 , 3.0510E+00 , 
     &      3.0510E+00 , 1.0500E+00 , 1.0500E+00 , 4.5000E-01 , 
     &      1.0000E+00 , 6.0060E+00 , 6.0060E+00 , 2.0500E-01 , 
     &      2.0500E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.0200E-03 , 1.0000E-04 , 4.0200E-03 , 
     &      1.0000E-04 , 2.0000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      7.0000E-01 , 1.0000E+00 , 1.0000E+00 , 3.000E+00/
 
C*anfe 12.01.2016 fix for gamma nucleus
C  parameter set for     22 (GRV-G LO)     2112 (GRV94 LO)
      DATA (idptab(k,23),k=1,8)/22 , 5 , 3 , 0 , 2112 , 5 , 6 , 0/
      DATA (xdptab(k,23),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/

C*anfe 09.07.2019 another fix for gamma nucleus (parameteres not
C not fitted. Fix might produce bad results.)
C  parameter set for     22 (GRV-G LO)     211 (GRV-PiLO)
      DATA (idptab(k,23),k=1,8)/22 , 5 , 3 , 0 , 211 , 5 , 2 , 0/
      DATA (xdptab(k,23),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
      DATA (idptab(k,23),k=1,8)/22 , 5 , 3 , 0 , -211 , 5 , 2 , 0/
      DATA (xdptab(k,23),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
      DATA (idptab(k,23),k=1,8)/22 , 5 , 3 , 0 , 321 , 5 , 2 , 0/
      DATA (xdptab(k,23),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
      DATA (idptab(k,23),k=1,8)/22 , 5 , 3 , 0 , -321 , 5 , 2 , 0/
      DATA (xdptab(k,23),k=1,27)/1.0970E+00 , 2.5000E-01 , 2.7450E+00 , 
     &      6.8270E+00 , 1.2250E+00 , 1.1360E+00 , 5.0000E-01 , 
     &      1.0000E+00 , 4.7210E+00 , 1.1740E+01 , 4.6200E-01 , 
     &      4.2800E-01 , 1.7000E-01 , 5.0000E-01 , 6.1200E-01 , 
     &      3.0000E-01 , 4.3100E-03 , 8.0000E-05 , 1.0000E+00 , 
     &      0.0000E+00 , 3.2000E+00 , 1.0000E+00 , 7.0000E-01 , 
     &      6.0000E-01 , 1.0000E+00 , 1.1000E+00 , 3.000E+00/
 
      DATA cname8/'        '/
      DATA cnam10/'          '/
      DATA init/0/
      DATA ifpas/0 , 0/
 
      IF ( (init.NE.1) .OR. (IFPap(1).NE.ifpas(1)) .OR. 
     &     (IFPap(2).NE.ifpas(2)) ) THEN
 
         init = 1
 
C  parton distribution functions
         CALL PHO_ACTPDF(IFPap(1),1)
         CALL PHO_GETPDF(1,pdfna1,alam2,q2min,q2max,xmin,xmax)
         CALL PHO_ACTPDF(IFPap(2),2)
         CALL PHO_GETPDF(2,pdfna2,alam2,q2min,q2max,xmin,xmax)
C  initialize alpha_s calculation
         dummy = PHO_ALPHAS(0.D0,-4)
 
         IF ( IDEb(54).GE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I7,2X,A,3I7)')
     &            'PHO_FITPAR: looking for PDF' , IFPap(1) , pdfna1 , 
     &           IGRp(1) , ISEt(1) , IEXt(1)
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I7,2X,A,3I7)')
     &            'PHO_FITPAR: looking for PDF' , IFPap(2) , pdfna2 , 
     &           IGRp(2) , ISEt(2) , IEXt(2)
         END IF
 
         ifound = 0
 
C  load parameter set from internal tables
         i1 = 1
         i2 = 2
 
 50      DO i = 1 , MAX_TAB
            IF ( (IFPap(i1).EQ.idptab(1,i)) .AND. 
     &           (IGRp(i1).EQ.idptab(2,i)) .AND. 
     &           (ISEt(i1).EQ.idptab(3,i)) .AND. 
     &           (IEXt(i1).EQ.idptab(4,i)) ) THEN
               IF ( (IFPap(i2).EQ.idptab(5,i)) .AND. 
     &              (IGRp(i2).EQ.idptab(6,i)) .AND. 
     &              (ISEt(i2).EQ.idptab(7,i)) .AND. 
     &              (IEXt(i2).EQ.idptab(8,i)) ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 
     &               'PHO_FITPAR: parameter set found in internal table'
                  ALPom = xdptab(1,i)
                  ALPomp = xdptab(2,i)
                  GP(i1) = xdptab(3,i)
                  GP(i2) = xdptab(4,i)
                  B0Pom(i1) = xdptab(5,i)
                  B0Pom(i2) = xdptab(6,i)
                  ALReg = xdptab(7,i)
                  ALRegp = xdptab(8,i)
                  GR(i1) = xdptab(9,i)
                  GR(i2) = xdptab(10,i)
                  B0Reg(i1) = xdptab(11,i)
                  B0Reg(i2) = xdptab(12,i)
                  GPPp = xdptab(13,i)
                  B0Ppp = xdptab(14,i)
                  GPPr = xdptab(15,i)
                  B0Ppr = xdptab(16,i)
                  VDMfac(2*i1-1) = xdptab(17,i)
                  VDMfac(2*i1) = xdptab(18,i)
                  VDMfac(2*i2-1) = xdptab(19,i)
                  VDMfac(2*i2) = xdptab(20,i)
                  B0Har = xdptab(21,i)
                  AKFac = xdptab(22,i)
                  PHIsup(i1) = xdptab(23,i)
                  PHIsup(i2) = xdptab(24,i)
                  RMAss(i1) = xdptab(25,i)
                  RMAss(i2) = xdptab(26,i)
                  VAR = xdptab(27,i)
                  ifound = 1
                  GOTO 100
               END IF
            END IF
         END DO
 
         IF ( i1.EQ.1 ) THEN
            i1 = 2
            i2 = 1
            GOTO 50
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 
     &           'PHO_FITPAR: parameter set not found in internal table'
         END IF
 
 
C  get parameters of soft cross sections from dpmjpar.dat
 100     IF ( IPAmdl(99).GT.ifound ) THEN
 
#ifndef FOR_FLUKA
            kpflen = INDEX(PARfn,'.dat') + 3
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &            'PHO_FITPAR: loading parameter set from file '//
     &           PARfn(1:kpflen)
            OPEN (12,FILE=PARfn(1:kpflen),ERR=160,STATUS='OLD')
 
 120        READ (12,'(A8)',ERR=140,END=160) cname8
            IF ( cname8.EQ.'STOP' ) GOTO 160
            IF ( cname8.EQ.'NEXTDATA' ) THEN
               READ (12,'(I8,2X,A8,3I6)',ERR=140,END=160) idpa1 , 
     &               cname8 , inum
               IF ( (idpa1.EQ.IFPap(1)) .AND. (cname8.EQ.pdfna1) .AND. 
     &              (inum(1).EQ.IGRp(1)) .AND. (inum(2).EQ.ISEt(1)) )
     &              THEN
                  READ (12,'(I8,2X,A8,3I6)',ERR=140,END=160) idpa2 , 
     &                  cname8 , inum
                  IF ( (idpa2.EQ.IFPap(2)) .AND. (cname8.EQ.pdfna2)
     &                 .AND. (inum(1).EQ.IGRp(2)) .AND. 
     &                 (inum(2).EQ.ISEt(2)) ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &                     'PHO_FITPAR: parameter set found'
                     READ (12,*) ALPom , ALPomp , GP , B0Pom
                     READ (12,*) ALReg , ALRegp , GR , B0Reg
                     READ (12,*) GPPp , B0Ppp , GPPr , B0Ppr
                     READ (12,*) VDMfac(1) , VDMfac(2) , VDMfac(3) , 
     &                     VDMfac(4)
                     READ (12,*) B0Har
                     READ (12,*) AKFac
                     READ (12,*) PHIsup
                     READ (12,*) RMAss , VAR
                     ifound = 1
                     GOTO 180
                  END IF
               END IF
            END IF
            GOTO 120
 
 140        IF ( LPRi.GT.4 ) WRITE (LO,'(/A)')
     &            ' PHO_FITPAR: cannot read file dpmjpar.dat'
            IF ( LPRi.GT.4 ) WRITE (LO,'(A,A10,A8)')
     &           ' last data card: ' , cnam10 , cname8
 160        IF ( LPRi.GT.4 ) WRITE (LO,'(/A)') 
     &      ' PHO_FITPAR: cannot find parameter set in file dpmjpar.dat'
 
 180        CLOSE (12)

#else

            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 
     &         'PHO_FITPAR: loading parameter set from file dpmjpar.dat'
            CALL OAUXFI('dpmjpar.dat',LUNRDB,'OLD',ierr)
 
 200        READ (LUNRDB,'(A8)',ERR=140,END=160) cname8
            IF ( cname8.EQ.'STOP' ) GOTO 160
            IF ( cname8.EQ.'NEXTDATA' ) THEN
               READ (LUNRDB,'(I8,2X,A8,3I6)',ERR=140,END=160) idpa1 , 
     &               cname8 , inum
               IF ( (idpa1.EQ.IFPap(1)) .AND. (cname8.EQ.pdfna1) .AND. 
     &              (inum(1).EQ.IGRp(1)) .AND. (inum(2).EQ.ISEt(1)) )
     &              THEN
                  READ (LUNRDB,'(I8,2X,A8,3I6)',ERR=140,END=160) idpa2 , 
     &                  cname8 , inum
                  IF ( (idpa2.EQ.IFPap(2)) .AND. (cname8.EQ.pdfna2)
     &                 .AND. (inum(1).EQ.IGRp(2)) .AND. 
     &                 (inum(2).EQ.ISEt(2)) ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &                     'PHO_FITPAR: parameter set found'
                     READ (LUNRDB,*) ALPom , ALPomp , GP , B0Pom
                     READ (LUNRDB,*) ALReg , ALRegp , GR , B0Reg
                     READ (LUNRDB,*) GPPp , B0Ppp , GPPr , B0Ppr
                     READ (LUNRDB,*) VDMfac(1) , VDMfac(2) , VDMfac(3) , 
     &                     VDMfac(4)
                     READ (LUNRDB,*) B0Har
                     READ (LUNRDB,*) AKFac
                     READ (LUNRDB,*) PHIsup
                     READ (LUNRDB,*) RMAss , VAR
                     ifound = 1
                     GOTO 180
                  END IF
               END IF
            END IF
            GOTO 120
 
 220        IF ( LPRi.GT.4 ) WRITE (LO,'(/A)')
     &            ' PHO_FITPAR: cannot read file dpmjpar.dat'
            IF ( LPRi.GT.4 ) WRITE (LO,'(A,A10,A8)')
     &           ' last data card: ' , cnam10 , cname8
 240        IF ( LPRi.GT.4 ) WRITE (LO,'(/A)') 
     &      ' PHO_FITPAR: cannot find parameter set in file dpmjpar.dat'
 
 260        CLOSE (LUNRDB)
#endif
         END IF
 
C  nothing found
         IF ( ifound.EQ.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/A)')
     &            ' PHO_FITPAR: could not find parameter set'
            IF ( LPRi.GT.4 ) WRITE (LO,'(3(10X,A,/))')
     &            '(copy dpmjpar.dat into the working directory and/or'
     &           , ' request the missing parameter set via e-mail from'
     &           , ' eng@lepton.bartol.udel.edu)'
            STOP
         END IF
      END IF
 
 
C  overwrite parameters with user settings
      IF ( PARmdl(301).GT.DEFA ) THEN
         ALPom = PARmdl(301)
         PARmdl(301) = DEFB
      END IF
      IF ( PARmdl(302).GT.DEFA ) THEN
         ALPomp = PARmdl(302)
         PARmdl(302) = DEFB
      END IF
      IF ( PARmdl(303).GT.DEFA ) THEN
         GP(1) = PARmdl(303)
         PARmdl(303) = DEFB
      END IF
      IF ( PARmdl(304).GT.DEFA ) THEN
         GP(2) = PARmdl(304)
         PARmdl(304) = DEFB
      END IF
      IF ( PARmdl(305).GT.DEFA ) THEN
         B0Pom(1) = PARmdl(305)
         PARmdl(305) = DEFB
      END IF
      IF ( PARmdl(306).GT.DEFA ) THEN
         B0Pom(2) = PARmdl(306)
         PARmdl(306) = DEFB
      END IF
      IF ( PARmdl(307).GT.DEFA ) THEN
         ALReg = PARmdl(307)
         PARmdl(307) = DEFB
      END IF
      IF ( PARmdl(308).GT.DEFA ) THEN
         ALRegp = PARmdl(308)
         PARmdl(308) = DEFB
      END IF
      IF ( PARmdl(309).GT.DEFA ) THEN
         GR(1) = PARmdl(309)
         PARmdl(309) = DEFB
      END IF
      IF ( PARmdl(310).GT.DEFA ) THEN
         GR(2) = PARmdl(310)
         PARmdl(310) = DEFB
      END IF
      IF ( PARmdl(311).GT.DEFA ) THEN
         B0Reg(1) = PARmdl(311)
         PARmdl(311) = DEFB
      END IF
      IF ( PARmdl(312).GT.DEFA ) THEN
         B0Reg(2) = PARmdl(312)
         PARmdl(312) = DEFB
      END IF
      IF ( PARmdl(313).GT.DEFA ) THEN
         GPPp = PARmdl(313)
         PARmdl(313) = DEFB
      END IF
      IF ( PARmdl(314).GT.DEFA ) THEN
         B0Ppp = PARmdl(314)
         PARmdl(314) = DEFB
      END IF
      IF ( PARmdl(315).GT.DEFA ) THEN
         VDMfac(1) = PARmdl(315)
         PARmdl(315) = DEFB
      END IF
      IF ( PARmdl(316).GT.DEFA ) THEN
         VDMfac(2) = PARmdl(316)
         PARmdl(316) = DEFB
      END IF
      IF ( PARmdl(317).GT.DEFA ) THEN
         VDMfac(3) = PARmdl(317)
         PARmdl(317) = DEFB
      END IF
      IF ( PARmdl(318).GT.DEFA ) THEN
         VDMfac(4) = PARmdl(318)
         PARmdl(318) = DEFB
      END IF
      IF ( PARmdl(319).GT.DEFA ) THEN
         B0Har = PARmdl(319)
         PARmdl(319) = DEFB
      END IF
      IF ( PARmdl(320).GT.DEFA ) THEN
         AKFac = PARmdl(320)
         PARmdl(320) = DEFB
      END IF
      IF ( PARmdl(321).GT.DEFA ) THEN
         PHIsup(1) = PARmdl(321)
         PARmdl(321) = DEFB
      END IF
      IF ( PARmdl(322).GT.DEFA ) THEN
         PHIsup(2) = PARmdl(322)
         PARmdl(322) = DEFB
      END IF
      IF ( PARmdl(323).GT.DEFA ) THEN
         RMAss(1) = PARmdl(323)
         PARmdl(323) = DEFB
      END IF
      IF ( PARmdl(324).GT.DEFA ) THEN
         RMAss(2) = PARmdl(324)
         PARmdl(324) = DEFB
      END IF
      IF ( PARmdl(325).GT.DEFA ) THEN
         VAR = PARmdl(325)
         PARmdl(325) = DEFB
      END IF
      IF ( PARmdl(327).GT.DEFA ) THEN
         GPPr = PARmdl(327)
         PARmdl(327) = DEFB
      END IF
      IF ( PARmdl(328).GT.DEFA ) THEN
         B0Ppr = PARmdl(328)
         PARmdl(328) = DEFB
      END IF
 
      VDMq2f(1) = VDMfac(1)
      VDMq2f(2) = VDMfac(2)
      VDMq2f(3) = VDMfac(3)
      VDMq2f(4) = VDMfac(4)
 
C  output of parameter set
      IF ( (IDEb(54).GE.5) .OR. (Ioutp.GT.0) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,A,/,A)')
     &         ' PHO_FITPAR: parameter set' , 
     &        ' -------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(A,F7.3),2(A,2F9.3))')
     &        '  ALPOM:' , ALPom , ' ALPOMP:' , ALPomp , ' GP:' , GP , 
     &        ' B0POM:' , B0Pom
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(A,F7.3),2(A,2F9.3))')
     &        '  ALREG:' , ALReg , ' ALREGP:' , ALRegp , ' GR:' , GR , 
     &        ' B0REG:' , B0Reg
         IF ( LPRi.GT.4 ) WRITE (LO,'(4(A,F7.3))') '  GPPP :' , GPPp , 
     &        ' B0PPP:' , B0Ppp , ' GPPR :' , GPPr , ' B0PPR:' , B0Ppr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4F10.5)') ' VDMFAC:' , VDMfac
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4F10.5)') ' VDMQ2F:' , VDMq2f
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,F8.3)') '  B0HAR:' , B0Har
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,F8.3)') '  AKFAC:' , AKFac
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2F8.3)') ' PHISUP:' , PHIsup
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,3F8.3)') '  RMASS:' , RMAss , 
     &        VAR
      END IF
 
      CALL PHO_HARINI(1,IFPap(1),IFPap(2),PVIrt(1),PVIrt(2),6,Ioutp-1)
 
      END SUBROUTINE
