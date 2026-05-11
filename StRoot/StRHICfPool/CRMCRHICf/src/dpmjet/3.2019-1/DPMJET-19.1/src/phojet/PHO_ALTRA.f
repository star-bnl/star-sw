
      SUBROUTINE PHO_ALTRA(Ga,Bgx,Bgy,Bgz,Pcx,Pcy,Pcz,Ec,P,Px,Py,Pz,E)
C*********************************************************************
C
C    arbitrary Lorentz transformation
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Bgx , Bgy , Bgz , E , Ec , ep , Ga , P , Pcx , 
     &                 Pcy , Pcz , pe , Px , Py , Pz
      SAVE 
 
      ep = Pcx*Bgx + Pcy*Bgy + Pcz*Bgz
      pe = ep/(Ga+1.D0) + Ec
      Px = Pcx + Bgx*pe
      Py = Pcy + Bgy*pe
      Pz = Pcz + Bgz*pe
      P = SQRT(Px*Px+Py*Py+Pz*Pz)
      E = Ga*Ec + ep
 
      END SUBROUTINE
