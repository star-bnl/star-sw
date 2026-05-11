
      SUBROUTINE DT_DTRAFO(Gam,Bgam,Cx,Cy,Cz,Cod,Cof,Sif,P,Ecm,Pl,Cxl,
     &                     Cyl,Czl,El)
 
C     LORENTZ TRANSFORMATION INTO THE LAB - SYSTEM
 
      IMPLICIT NONE
      DOUBLE PRECISION Bgam , Cod , Cof , coz , Cx , Cxl , Cy , Cyl , 
     &                 Cz , Czl , Ecm , El , Gam , P , pcmz , Pl , plx , 
     &                 ply , plz , sid
      DOUBLE PRECISION Sif , siz
      SAVE 
 
      IF ( ABS(Cod).GT.1.0D0 ) Cod = SIGN(1.0D0,Cod)
      sid = SQRT(1.D0-Cod*Cod)
      plx = P*sid*Cof
      ply = P*sid*Sif
      pcmz = P*Cod
      plz = Gam*pcmz + Bgam*Ecm
      Pl = SQRT(plx*plx+ply*ply+plz*plz)
      El = Gam*Ecm + Bgam*pcmz
C     ROTATION INTO THE ORIGINAL DIRECTION
      coz = plz/Pl
      siz = SQRT(1.D0-coz**2)
      CALL DT_STTRAN(Cx,Cy,Cz,coz,siz,Sif,Cof,Cxl,Cyl,Czl)
 
      END SUBROUTINE
