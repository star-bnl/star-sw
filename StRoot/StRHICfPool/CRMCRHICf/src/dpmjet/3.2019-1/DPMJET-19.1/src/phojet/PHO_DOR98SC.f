
      SUBROUTINE PHO_DOR98SC(Xinp,Q2inp,Uv,Dv,Us,Ds,Ss,Gl)
C***********************************************************************
C
C   GRV98 parton densities, leading order set
C
C                  For a detailed explanation see
C                   M. Glueck, E. Reya, A. Vogt :
C        hep-ph/9806404  =  DO-TH 98/07  =  WUE-ITP-98-019
C                  (To appear in Eur. Phys. J. C)
C
C   interpolation routine based on the original GRV98PA routine,
C   adapted to define interpolation table as DATA statements
C
C                                                   (R.Engel, 09/98)
C
C   CAUTION: this is a version with gluon shadowing corrections
C                                                   (R.Engel, 09/99)
C
C
C   INPUT:   X  =  Bjorken-x        (between  1.E-9 and 1.)
C            Q2 =  scale in GeV**2  (between  0.8 and 1.E6)
C
C   OUTPUT:  UV = u - u(bar),  DV = d - d(bar),  US = u(bar),
C            DS = d(bar),  SS = s = s(bar),  GL = gluon.
C            Always x times the distribution is returned.
C
C******************************************************i****************
      IMPLICIT NONE
      DOUBLE PRECISION arrf , de , Ds , Dv , Gl , PHO_DBFINT , q2 , 
     &                 Q2inp , Ss , ud , Us , Uv , x , x1 , xdef , 
     &                 xdef_l , xdvf , xdvf_l , xgf , xgf_l
      DOUBLE PRECISION Xinp , xs , xsf , xsf_l , xt , xudf , xudf_l , 
     &                 xuvf , xuvf_l , xv
      INTEGER na , NARG , NPART , NQ , NX
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      PARAMETER (NPART=6,NX=68,NQ=27,NARG=2)
      DIMENSION xuvf(NX,NQ) , xdvf(NX,NQ) , xdef(NX,NQ) , xudf(NX,NQ) , 
     &          xsf(NX,NQ) , xgf(NX,NQ) , xt(NARG) , na(NARG) , 
     &          arrf(NX+NQ)
 
      DIMENSION xuvf_l(NX*NQ) , xdvf_l(NX*NQ) , xdef_l(NX*NQ) , 
     &          xudf_l(NX*NQ) , xsf_l(NX*NQ) , xgf_l(NX*NQ)
 
      EQUIVALENCE (xuvf(1,1),xuvf_l(1))
      EQUIVALENCE (xdvf(1,1),xdvf_l(1))
      EQUIVALENCE (xdef(1,1),xdef_l(1))
      EQUIVALENCE (xudf(1,1),xudf_l(1))
      EQUIVALENCE (xsf(1,1),xsf_l(1))
      EQUIVALENCE (xgf(1,1),xgf_l(1))
 
C#################### data statements for shadowed LO PDF ##############
C  ... deleted ...
C#######################################################################
 
      x = Xinp
C...CHECK OF X AND Q2 VALUES :
C        WRITE(LO,91) X
C 91     FORMAT (2X,'GRV98_SC: x out of range',1p,E12.4)
C        STOP
      IF ( (x.LT.0.99D-09) .OR. (x.GT.1.D+00) ) x = 0.99D-09
 
      q2 = Q2inp
C        WRITE(LO,92) Q2
C 92     FORMAT (2X,'GRV98_SC: Q2 out of range',1p,E12.4)
C        STOP
      IF ( (q2.LT.0.799D+00) .OR. (q2.GT.1.D+06) ) q2 = 0.99D+06
 
C
C...INTERPOLATION :
      na(1) = NX
      na(2) = NQ
      xt(1) = LOG(x)
      xt(2) = LOG(q2)
      x1 = 1.D+00 - x
      xv = x**0.5D+00
      xs = x**(-0.2D+00)
      Uv = PHO_DBFINT(NARG,xt,na,arrf,xuvf)*x1**3*xv
      Dv = PHO_DBFINT(NARG,xt,na,arrf,xdvf)*x1**4*xv
      de = PHO_DBFINT(NARG,xt,na,arrf,xdef)*x1**7*xv
      ud = PHO_DBFINT(NARG,xt,na,arrf,xudf)*x1**7*xs
      Us = 0.5*(ud-de)
      Ds = 0.5*(ud+de)
      Ss = PHO_DBFINT(NARG,xt,na,arrf,xsf)*x1**7*xs
      Gl = PHO_DBFINT(NARG,xt,na,arrf,xgf)*x1**5*xs
 
      END SUBROUTINE
