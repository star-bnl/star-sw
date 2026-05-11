
      SUBROUTINE PHO_LTRANS(Gam,Bgam,Cx,Cy,Cz,Cod,Cof,Sif,P,Ecm,Pl,Cxl,
     &                      Cyl,Czl,El)
C**********************************************************************
C
C     Lorentz transformation into lab - system
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION a , amax , amin , ar , ax , ay , Bgam , Cod , 
     &                 Cof , coz , Cx , Cxl , Cy , Cyl , Cz , Czl , 
     &                 Ecm , El , Gam , P
      DOUBLE PRECISION pcmz , Pl , plx , ply , plz , sid , Sif , siz , 
     &                 TINY , TINY2 , xi , yi , zi
      SAVE 
 
      PARAMETER (TINY=1.D-08,TINY2=1.D-30)
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      sid = SQRT(1.D0-Cod*Cod)
      plx = P*sid*Cof
      ply = P*sid*Sif
      pcmz = P*Cod
      plz = Gam*pcmz + Bgam*Ecm
      Pl = SQRT(plx*plx+ply*ply+plz*plz)
      El = Gam*Ecm + Bgam*pcmz
 
C  rotation into the original direction
      coz = plz/Pl
      siz = SQRT(MAX((1.D0-coz)*(1.D0+coz),0.D0))
 
C      CALL PHO_DTRANS(CX,CY,CZ,COZ,SIZ,COF,SIF,CXL,CYL,CZL)
 
      ax = ABS(Cx)
      ay = ABS(Cy)
      IF ( ax.LT.ay ) THEN
         amax = ay
         amin = ax
      ELSE
         amax = ax
         amin = ay
      END IF
      IF ( ABS(Cx).GT.TINY ) GOTO 200
 
      IF ( ABS(Cy).GT.TINY ) GOTO 200
C     WRITE(LO,*)' PHO_DTRANS CX CY CZ =',CX,CY,CZ
 100  Cxl = siz*Cof
      Cyl = siz*Sif
      Czl = coz*Cz
C     WRITE(LO,*)' PHO_DTRANS CXL=SIZ*COF CYL=SIZ*SIF CZL=COZ'
C     WRITE(LO,*) CXL,CYL,CZL
      RETURN
 
C       WRITE(LO,*)' PHO_DTRANS AMAX LE TINY2 '
 200  IF ( amax.LE.TINY2 ) GOTO 100
      ar = amin/amax
      ar = ar*ar
      a = amax*SQRT(1.D0+ar)
      xi = siz*Cof
      yi = siz*Sif
      zi = coz
      Cxl = -Cy*xi/a - Cz*Cx*yi/a + Cx*zi
      Cyl = Cx*xi/a - Cz*Cy*yi/a + Cy*zi
      Czl = a*yi + Cz*zi
 
      END SUBROUTINE
