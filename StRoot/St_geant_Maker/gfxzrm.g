+DECK,gfxzrm.
*CMZ :          10/11/98      by  Ruben Badalian
*-- Author :
      SUBROUTINE GFXZRM(NLEV_0, X,Y,Z, TET1,PHI1, TET2,PHI2, TET3,PHI3, TYPE)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Return POSITIONS AND ROTATION MATRICE parameters         *
C.    *                                                                *
C.    ******************************************************************
C.
+CDE,GCVOLU
C.
      DIMENSION XM(3), XD(3)
C      DIMENSION ROTMAT(3,3), ROWMAT(3), THETA(3), PHI(3)
      DIMENSION ROWMAT(3)
      LOGICAL ROTATE
C
      PI = 4.0*ATAN(1.0)
      RADDEG = 180.0/PI
C
      NLEV_0 = NLEVEL
      NLEV = NLEVEL-1                ! level of mother
      IF (NLEV .LE. 0)  GO TO 777
C
C      NUMBER(NLEV + 1) = NCOPY
      NCOPY = NUMBER(NLEV + 1)
C
      DO N  = 1, 3
      XM(N) = GTRAN(N, NLEV + 1)
C      XM(N) = GTRAN(N, NLEV)
C
C      ROTMAT(1, N) = GRMAT(0+N, NLEV+1)
C      ROTMAT(2, N) = GRMAT(3+N, NLEV+1)
C      ROTMAT(3, N) = GRMAT(6+N, NLEV+1)
C
      ENDDO

      NLEVEL = NLEV
      CALL GMTOD(XM, XD, 1)
C
      X    = XD(1)
      Y    = XD(2)
      Z    = XD(3)
C
C      DO 10 I  = 1, 3
C         ROWMAT(1)  = ROTMAT(I, 1)
C         ROWMAT(2)  = ROTMAT(I, 2)
C         ROWMAT(3)  = ROTMAT(I, 3)
C         CALL GFANG(ROWMAT, COSTH,SINTH, COSPH,SINPH, ROTATE)
C         THETA(I)   = ATAN2(SINTH, COSTH)
C         PHI(I)     = ATAN2(SINPH, COSPH)
C 10      CONTINUE
C
      DO JB= 1, 3
      DO J = 1, 3
      XM(J) = GRMAT(J + 3*(JB-1), NLEV + 1)
C      XM(J) = GRMAT(J + 3*(JB-1), NLEV)
      ENDDO
C
      NLEVEL = NLEV
      CALL GMTOD(XM, XD, 2)
C
         ROWMAT(1)  = XD(1)
         ROWMAT(2)  = XD(2)
         ROWMAT(3)  = XD(3)
         CALL GFANG(ROWMAT, COSTH,SINTH, COSPH,SINPH, ROTATE)
      IF(JB .EQ. 1) THEN
      TET1     = ATAN2(SINTH, COSTH)*RADDEG
      PHI1     = ATAN2(SINPH, COSPH)*RADDEG
      ENDIF
           IF(JB .EQ. 2) THEN
           TET2     = ATAN2(SINTH, COSTH)*RADDEG
           PHI2     = ATAN2(SINPH, COSPH)*RADDEG
           ENDIF
                IF(JB .EQ. 3) THEN
                TET3     = ATAN2(SINTH, COSTH)*RADDEG
                PHI3     = ATAN2(SINPH, COSPH)*RADDEG
                ENDIF
      ENDDO
C
C      X    = GTRAN(1, NLEV+1)
C      Y    = GTRAN(2, NLEV+1)
C      Z    = GTRAN(3, NLEV+1)
C
C      TET1 = GRMAT(1, NLEV+1)
C      PHI1 = GRMAT(2, NLEV+1)
C      TET2 = GRMAT(3, NLEV+1)
C      PHI2 = GRMAT(4, NLEV+1)
C      TET3 = GRMAT(5, NLEV+1)
C      PHI3 = GRMAT(6, NLEV+1)
C
      TYPE = GRMAT(10, NLEV)
C
 777  CONTINUE
      NLEVEL = NLEV_0
C      print *,'xyz= ', Nlevel, Ncopy, (Gtran(i,Nlevel),i=1,3), 
C     *(Grmat(i,Nlevel),i=1,9)
      RETURN
      END
