
      SUBROUTINE DT_DALTRA(Ga,Bgx,Bgy,Bgz,Pcx,Pcy,Pcz,Ec,P,Px,Py,Pz,E)
 
C***********************************************************************
C Arbitrary Lorentz-transformation.                                    *
C Adopted from the original by S. Roesler. This version dated 15.01.95 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Bgx , Bgy , Bgz , E , Ec , ep , Ga , ONE , P , 
     &                 Pcx , Pcy , Pcz , pe , Px , Py , Pz
      SAVE 
      PARAMETER (ONE=1.0D0)
 
      ep = Pcx*Bgx + Pcy*Bgy + Pcz*Bgz
      pe = ep/(Ga+ONE) + Ec
      Px = Pcx + Bgx*pe
      Py = Pcy + Bgy*pe
      Pz = Pcz + Bgz*pe
      P = SQRT(Px*Px+Py*Py+Pz*Pz)
      E = Ga*Ec + ep
 
      END SUBROUTINE
