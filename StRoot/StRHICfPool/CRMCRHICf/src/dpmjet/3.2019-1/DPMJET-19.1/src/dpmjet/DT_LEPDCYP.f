
      SUBROUTINE DT_LEPDCYP(Ama,Aml,Pol,Etl,Pxl,Pyl,Pzl,Etb,Pxb,Pyb,Pzb,
     &                      Etn,Pxn,Pyn,Pzn)
C
C-----------------------------------------------------------------
C
C   Author   :- G. Battistoni         10-NOV-1995
C
C=================================================================
C
C   Purpose   : performs decay of polarized lepton in
C               its rest frame: a => b + l + anti-nu
C               (Example: mu- => nu-mu + e- + anti-nu-e)
C               Polarization is assumed along Z-axis
C               WARNING:
C               1) b AND anti-nu ARE ASSUMED TO BE NEUTRINOS
C                  OF NEGLIGIBLE MASS
C               2) RADIATIVE CORRECTIONS ARE NOT CONSIDERED
C                  IN THIS VERSION
C
C   Method    : modifies phase space distribution obtained
C               by routine EXPLOD using a rejection against the
C               matrix element for unpolarized lepton decay
C
C   Inputs    : Mass of a :  AMA
C               Mass of l :  AML
C               Polar. of a: POL
C               (Example: fully polar. mu- decay: AMA=AMMUON, AML=AMELCT,
C                                                 POL = -1)
C
C   Outputs   : kinematic variables in the rest frame of decaying lepton
C               ETL,PXL,PYL,PZL 4-moment of l
C               ETB,PXB,PYB,PZB 4-moment of b
C               ETN,PXN,PYN,PZN 4-moment of anti-nu
C
C============================================================
C +
C Declarations.
C -
      IMPLICIT NONE
      DOUBLE PRECISION AINFNT , ALGVMV , ALPFSC , Ama , AMELCT , 
     &                 AMELGR , amexpl , Aml , AMMUMU , AMMUON , 
     &                 AMUGEV , AMUGRM , ANDRFL , ANGLGB , ANGLSQ , 
     &                 ANINEN , AVOGAD , AVRFLW , AXCSSV , AZRZRZ
      DOUBLE PRECISION CLIGHT , CSNNRM , cth , DEGRAD , DMXTRN , 
     &                 DT_RNDM , EIGEIG , EINFNT , ELCCGS , ELCMKS , 
     &                 elemat , elemax , elerat , EMVGEV , ENEPER , 
     &                 Etb , etexpl , Etl , Etn , etotex
      DOUBLE PRECISION EZRZRZ , FIVFIV , FOUFOU , FSCTO2 , FSCTO3 , 
     &                 FSCTO4 , GEVMEV , HLFHLF , ONEMNS , ONEONE , 
     &                 ONEPLS , ONETHI , PIPIPI , PLABRC , PLCKBR , 
     &                 Pol , prod1 , prod2 , Pxb , pxexpl
      DOUBLE PRECISION Pxl , Pxn , Pyb , pyexpl , Pyl , Pyn , Pzb , 
     &                 pzexpl , Pzl , Pzn , RADDEG , RCLSEL , SEVSEV , 
     &                 SIXSIX , SQRENT , TENTEN , test , THRTHR , 
     &                 TWOTHI , TWOTWO
      DOUBLE PRECISION ZERZER
      INTEGER KALGNM , KPMX , npexpl , ntry
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (KALGNM=2)
      PARAMETER (ANGLGB=5.0D-16)
      PARAMETER (ANGLSQ=2.5D-31)
      PARAMETER (AXCSSV=0.2D+16)
      PARAMETER (ANDRFL=1.0D-38)
      PARAMETER (AVRFLW=1.0D+38)
      PARAMETER (AINFNT=1.0D+30)
      PARAMETER (AZRZRZ=1.0D-30)
      PARAMETER (EINFNT=+69.07755278982137D+00)
      PARAMETER (EZRZRZ=-69.07755278982137D+00)
      PARAMETER (ONEMNS=0.999999999999999D+00)
      PARAMETER (ONEPLS=1.000000000000001D+00)
      PARAMETER (CSNNRM=2.0D-15)
      PARAMETER (DMXTRN=1.0D+08)
      PARAMETER (ZERZER=0.D+00)
      PARAMETER (ONEONE=1.D+00)
      PARAMETER (TWOTWO=2.D+00)
      PARAMETER (THRTHR=3.D+00)
      PARAMETER (FOUFOU=4.D+00)
      PARAMETER (FIVFIV=5.D+00)
      PARAMETER (SIXSIX=6.D+00)
      PARAMETER (SEVSEV=7.D+00)
      PARAMETER (EIGEIG=8.D+00)
      PARAMETER (ANINEN=9.D+00)
      PARAMETER (TENTEN=10.D+00)
      PARAMETER (HLFHLF=0.5D+00)
      PARAMETER (ONETHI=ONEONE/THRTHR)
      PARAMETER (TWOTHI=TWOTWO/THRTHR)
      PARAMETER (PIPIPI=3.1415926535897932270D+00)
      PARAMETER (ENEPER=2.7182818284590452354D+00)
      PARAMETER (SQRENT=1.6487212707001281468D+00)
      PARAMETER (CLIGHT=2.99792458D+10)
      PARAMETER (AVOGAD=6.0221367D+23)
      PARAMETER (AMELGR=9.1093897D-28)
      PARAMETER (PLCKBR=1.05457266D-27)
      PARAMETER (ELCCGS=4.8032068D-10)
      PARAMETER (ELCMKS=1.60217733D-19)
      PARAMETER (AMUGRM=1.6605402D-24)
      PARAMETER (AMMUMU=0.113428913D+00)
      PARAMETER (ALPFSC=7.2973530791728595D-03)
      PARAMETER (FSCTO2=5.3251361962113614D-05)
      PARAMETER (FSCTO3=3.8859399018437826D-07)
      PARAMETER (FSCTO4=2.8357075508200407D-09)
      PARAMETER (PLABRC=0.197327053D+00)
      PARAMETER (AMELCT=0.51099906D-03)
      PARAMETER (AMUGEV=0.93149432D+00)
      PARAMETER (AMMUON=0.105658389D+00)
      PARAMETER (RCLSEL=2.8179409183694872D-13)
      PARAMETER (GEVMEV=1.0D+03)
      PARAMETER (EMVGEV=1.0D-03)
      PARAMETER (ALGVMV=6.90775527898214D+00)
      PARAMETER (RADDEG=180.D+00/PIPIPI)
      PARAMETER (DEGRAD=PIPIPI/180.D+00)
C +
C    variables for EXPLOD
C -
      PARAMETER (KPMX=10)
      DIMENSION amexpl(KPMX) , pxexpl(KPMX) , pyexpl(KPMX) , 
     &          pzexpl(KPMX) , etexpl(KPMX)
C +
C      test variables
C -
C*sr - removed (not needed)
C     COMMON /GBATNU/ ELERAT,NTRY
C*
C +
C     Initializes test variables
C -
      ntry = 0
      elerat = 0.D+00
C +
C     Maximum value for matrix element
C -
      elemax = (Ama**2+Aml**2)
     &         **2/Ama**2*(Ama**2-Aml**2+SQRT(Ama**4+Aml**4-
     &         3.D+00*Ama**2*Aml**2))
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C     Inputs for EXPLOD
C part. no. 1 is l       (e- in mu- decay)
C part. no. 2 is b       (nu-mu in mu- decay)
C part. no. 3 is anti-nu (anti-nu-e in mu- decay)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      npexpl = 3
      etotex = Ama
      amexpl(1) = Aml
      amexpl(2) = 0.D+00
      amexpl(3) = 0.D+00
C +
C     phase space distribution
C -
 100  ntry = ntry + 1
 
      CALL EXPLOD(npexpl,amexpl,etotex,etexpl,pxexpl,pyexpl,pzexpl)
 
C +
C  Calculates matrix element:
C  64*GF**2{[P(a)-ama*S(a)]*P(anti-nu)}{P(l)*P(b)}
C  Here CTH is the cosine of the angle between anti-nu and Z axis
C -
      cth = pzexpl(3)/SQRT(pxexpl(3)**2+pyexpl(3)**2+pzexpl(3)**2)
      prod1 = etexpl(3)*Ama*(1.D+00-Pol*cth)
      prod2 = etexpl(1)*etexpl(2) - pxexpl(1)*pxexpl(2) - pyexpl(1)
     &        *pyexpl(2) - pzexpl(1)*pzexpl(2)
      elemat = 16.D+00*prod1*prod2
      IF ( elemat.GT.elemax ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'Problems in LEPDCY' , elemax , 
     &        elemat
         STOP
      END IF
C +
C     Here performs the rejection
C -
      test = DT_RNDM(etotex)*elemax
C +
C     final assignment of variables
C -
      IF ( test.GT.elemat ) GOTO 100
      elerat = elemat/elemax
      Etl = etexpl(1)
      Pxl = pxexpl(1)
      Pyl = pyexpl(1)
      Pzl = pzexpl(1)
      Etb = etexpl(2)
      Pxb = pxexpl(2)
      Pyb = pyexpl(2)
      Pzb = pzexpl(2)
      Etn = etexpl(3)
      Pxn = pxexpl(3)
      Pyn = pyexpl(3)
      Pzn = pzexpl(3)
C 999 CONTINUE
      END SUBROUTINE
