        SUBROUTINE VXBTRD(R1,P1,R2,P2,DE,IRET)
*-----------------------------------------------------------------------
*--
*--     Tracking in the Barrel Region with Derivatives
*--     inward tracking
*--
*--     Input:
*--       R1      ... Radius of initial reference surface
*--       P1(5)   ... Parameter vector at ref. sur. (R*PHI,z,theta,phi,1/p)
*--       R2      ... Radius of final refernce surface (R2<R1)
*--
*--     Output:
*--       P2(5)   ... Parameter vector at final refence surface
*--       DE(5,5) ... deriavative matrix dP2/dP1
*--       IRET    ... return code (0 o.k.)
*--
*--     Dietrich Liko
*-- Modified to 5x5 by A.C. Saulys
*-----------------------------------------------------------------------
*--
        PARAMETER  (RMIN = 1.E-10)
        DOUBLE PRECISION PI
        PARAMETER  (PI     = 3.1415926536D0)
*--
        REAL R1,P1(5),R2,P2(5),DE(5,5)
        REAL XYZ(3),TKIN(1),UBUF(1)
        CHARACTER*4 MECAN
        CHARACTER*20 NATMED
*--
        DOUBLE PRECISION DR1,DP1(5),DR2,DP2(5),SAVE,DP(5),D(5),RES
CCC        DATA D/0.1,0.01,0.01,0.01,0.001/
        DATA D/0.1,0.1,0.01,0.01,0.01/
*--
        IF ((R1.LT.RMIN) .OR. (R2.LT.RMIN)) THEN
           IRET = 1
           RETURN
        ENDIF
CCC        IF (( P1(3) .LT. 0. ) .OR. ( P1(3) .GT. 2 * PI )) THEN
CCC           IRET = 2
CCC           RETURN
CCC        ENDIF
*--
        IF (ABS(R1-R2).LT.0.0001) THEN
           CALL VZERO(DE(1,1),25)
           DE(1,1) = 1.
           DE(2,2) = 1.
           DE(3,3) = 1.
           DE(4,4) = 1.
           DE(5,5) = 1.
           CALL UCOPY(P1(1),P2(1),5)
           IRET = 0
           RETURN
        ENDIF
*--
        DR1    = R1
        DO 100 I = 1, 5
           DP1(I) = P1(I)
 100    CONTINUE
        DR2    = R2
*--
        CALL VXBTR1(DR1,DP1(1),DR2,DP2(1),IERR)
        IF (IERR .NE. 0) THEN
           IRET = 10 + IERR
           RETURN
        ENDIF
*--
        DO 200 I = 1, 5
           P2(I) = DP2(I)
CCC           D(I) = P1(I)*0.01 ! take 1% steps
 200    CONTINUE
*--
CCC        D(3) = SIGN(D(3),PI/2 - DP1(3))
        D(5) = SIGN(D(5),DP1(5))
*--
        DO 300 I = 1, 5
           SAVE = DP1(I)
           DP1(I) = DP1(I) + D(I)
           CALL VXBTR1(DR1,DP1,DR2,DP,IERR)
           IF (IERR .NE. 0) THEN
              IRET = 10 + IERR
              RETURN
           ENDIF
           RES = MOD( DP(1) - DP2(1) , 2 * PI * DR2 )
           IF ( ABS( RES ) .GT. PI * DR2 )
     &        RES = RES - SIGN( 2 * PI * DR2 , RES)
           DE(1,I) = RES / D(I)
           DE(2,I) = ( DP(2) - DP2(2) ) / D(I)
           DE(3,I) = ( DP(3) - DP2(3) ) / D(I)
           RES = MOD( DP(4) - DP2(4), 2 * PI )
           IF ( ABS( RES ) .GT. PI ) RES = RES - SIGN(2 * PI , RES)
           DE(4,I) = RES / D(I)
           DE(5,I) = (DP(5) - DP2(5))/D(I)
           DP1(I) = SAVE
 300    CONTINUE

        IRET = 0
        RETURN
*--
        END

        SUBROUTINE VXBTRK(R1,P1,R2,P2,IRET)
*-----------------------------------------------------------------------
*--
*--     Tracking in the Barrel Region
*--     inward tracking
*--
*--     Input:
*--       R1      ... Radius of initial reference surface
*--       P1(5)   ... Parameter vector at ref. sur. (R*PHI,z,theta,phi,1/p)
*--       R2      ... Radius of final refernce surface (R2<R1)
*--
*--     Output:
*--       P2(5)   ... Parameter vector at final refence surface
*--       IRET    ... return code (0 o.k.)
*--
*--     Dietrich Liko
*-----------------------------------------------------------------------
*--
        PARAMETER  (PI     = 3.1415926536)
        PARAMETER (RMIN = 1.E-10)
*--
        REAL R1,P1(5),R2,P2(5)
*--
        DOUBLE PRECISION DR1,DP1(5),DR2,DP2(5)
*--
        IF ((R1.LT.RMIN) .OR. (R2.LT.RMIN)) THEN
           IRET = 1
           RETURN
        ENDIF
CCC        IF (( P1(3) .LT. 0. ) .OR. ( P1(3) .GT. 2 * PI )) THEN
CCC           IRET = 2
CCC           RETURN
CCC        ENDIF
*--
        IF (ABS(R1-R2).LT.0.0001) THEN
           CALL UCOPY(P1(1),P2(1),5)
           IRET = 0
           RETURN
        ENDIF
*--
        DR1    = R1
        DO 100 I = 1, 5
           DP1(I) = P1(I)
 100    CONTINUE
        DR2    = R2
*--
        CALL VXBTR1(DR1,DP1(1),DR2,DP2(1),IERR)
        IF (IERR .NE. 0) THEN
           IRET = 10 + IERR
           RETURN
        ENDIF
*--
        DO 200 I = 1, 5
           P2(I) = DP2(I)
 200    CONTINUE
*--
        IRET = 0
        RETURN
*--
        END
        SUBROUTINE VXBTR1(RIN,PIN,ROU,POU,IRET)
*-----------------------------------------------------------------------
*--
*--     Tracking in the Barrel Region (internal routine)
*--
*--     Dietrich Liko
*--     Modified by A. C. Saulys to account for energy loss
*-----------------------------------------------------------------------
*--
        IMPLICIT NONE

        DOUBLE PRECISION PI, PINMIN, RR1MIN
        DOUBLE PRECISION PH0, X0, Y0, X1, Y1, HRH, RRH, RR1
        DOUBLE PRECISION R1, PH1, DPH, X2, Y2, PH2

***        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*--
        PARAMETER  (PI     = 3.1415926536D0)
        PARAMETER  (PINMIN = 1.D-10)
        PARAMETER  (RR1MIN = 1.D-10)
*--

        INTEGER IGPART
        COMMON/GPARID/IGPART

        REAL PMASS
        COMMON/HMASS/PMASS

        DOUBLE PRECISION CONSB
        COMMON /VXMAGF/CONSB
*--
        DOUBLE PRECISION RIN,PIN(5),ROU,POU(5)

        REAL XYZ(3),XYZOU(3),TKIN(1),UBUF(1)
        REAL FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN
        REAL DX,DP,DEDX,MOMENTUM,TMOMENTUM,ENERGY,RE1,RE2

        INTEGER IPART,KDIN,NMED,NMAT,ISVOL,IFIELD,NWBUF,IRET

        CHARACTER*4 MECAN
        CHARACTER*20 NATMED
*--
        PH0 = PIN(1) / RIN
        X0  = RIN * COS( PH0 )
        Y0  = RIN * SIN( PH0 )
*--
CCC        HRH = - SIN( PIN(3) ) / ( CONSB * PIN(5) )
        HRH = - 1.0 / ( CONSB * PIN(5) )
        RRH = HRH ** 2
*--
        X1  = X0 - HRH * SIN( PIN(4) )
        Y1  = Y0 + HRH * COS( PIN(4) )
        RR1 = X1 ** 2 + Y1 ** 2
        IF (RR1 .LT. RR1MIN) THEN
           IRET = 1
           RETURN
        ENDIF
        R1  = SQRT( RR1 )
        PH1 = ATAN2( Y1, X1 )
*--
        DPH = ( ROU ** 2 - RRH - RR1 ) / ( 2 * R1 * HRH )
        IF ( ABS( DPH ) .GT. 1.) THEN
           IRET = 2
           RETURN
        ENDIF
        DPH = ASIN( DPH ) + PH1 - PIN(4)
        DPH = MOD( DPH, 2 * PI )
*--
*--     track in reverse direction !
*--
        IF (HRH .GT. 0) THEN
           IF (DPH .GT. 0) DPH = DPH - 2 * PI
        ELSE
           IF (DPH .LT. 0) DPH = DPH + 2 * PI
        ENDIF
*--
        X2  = X1 + HRH * SIN( PIN(4) + DPH )
        Y2  = Y1 - HRH * COS( PIN(4) + DPH )
        PH2 = ATAN2( Y2, X2 )
        IF (PH2 .LT. 0.) PH2 = PH2 + 2 * PI
*--
        POU(1) = ROU * PH2
        POU(2) = PIN(2) + DPH * HRH * PIN(3)
CCC        POU(2) = PIN(2) + DPH * HRH / TAN( PIN(3) )
        POU(3) = PIN(3)
        POU(4) = MOD( PIN(4) + DPH , 2 * PI)
        IF (POU(4) .LT. 0.) POU(4) = POU(4) + 2 * PI
*--     put in the energy loss
*-- set up the E loss calculation
        RE1 = RIN
        RE2 = ROU
        XYZ(1) = RE1*COS(PIN(1)/RE1)
        XYZ(2) = RE1*SIN(PIN(1)/RE1)
        XYZ(3) = PIN(2)
        MOMENTUM = SQRT(1.+PIN(3)**2)/PIN(5)
        ENERGY = SQRT(MOMENTUM**2+PMASS**2)

        TKIN(1) = ENERGY - PMASS ! KE 
        IPART = IGPART ! GEANT particle ID
CCC        IPART = 14 ! hard wire proton for now
        MECAN = 'LOSS'
        KDIN = 1
        CALL GMEDIA(XYZ,NMED)
        CALL GFTMED(NMED,NATMED,NMAT,ISVOL,IFIELD,FIELDM,
     +           TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)
        CALL GDEDXCALC(NMAT,IPART,MECAN,KDIN,TKIN,DEDX)
        XYZOU(1) = RE2*COS(POU(1)/RE2)
        XYZOU(2) = RE2*SIN(POU(1)/RE2)
        XYZOU(3) = POU(2)

        DX = SQRT((XYZOU(1)-XYZ(1))**2
     +           +(XYZOU(2)-XYZ(2))**2
     +           +(XYZOU(3)-XYZ(3))**2)
        IF(ROU.GT.RIN)DX = -DX ! Energy loss or gain
        
        DP = ENERGY*DEDX*DX/MOMENTUM ! approximation
        MOMENTUM = MOMENTUM+DP
        TMOMENTUM = MOMENTUM/SQRT(1.+PIN(3)**2)
        POU(5) = SIGN(1./TMOMENTUM,PIN(5))

CCC        POU(5) = PIN(5)
*--
        IRET = 0
        RETURN
*--
        END
        SUBROUTINE VXXTRK(X,Q,R,P,IRET)
*-----------------------------------------------------------------------
*--
*--     Tracking in the Vertex Region
*--
*--     Input:
*--        X(3) ... Vertex position (x,y,z)
*--        Q(3) ... Momentum and direction at vertex (theta,phi,1/p)
*--        R    ... Radius at final reference surface
*--
*--     Output:
*--        P(5) ... Parameter vector at final reference surface
*--        IRET ... Return code (0 o.k.)
*--
*--     Dietrich Liko
*-----------------------------------------------------------------------
*--
        PARAMETER  (PI     = 3.1415926536)
        PARAMETER  (RMIN = 1.E-10)
*--
        REAL X(3), Q(3), P(5)
        DOUBLE PRECISION DX(3), DQ(3), DR, DP(5)
*--
        IF (R.LT.RMIN) THEN
           IRET = 1
           RETURN
        ENDIF
        IF (( Q(1) .LT. 0. ) .OR. ( Q(1) .GT. 2 * PI )) THEN
           IRET = 2
           RETURN
        ENDIF
*--
        DO 100 I = 1, 3
           DX(I) = X(I)
           DQ(I) = Q(I)
 100    CONTINUE
        DR = R
*--
        CALL VXXTR1(DX(1),DQ(1),DR,DP(1),IERR)
        IF (IERR .NE. 0) THEN
           IRET = 10 + IERR
           RETURN
        ENDIF
*--
        DO 200 I = 1, 5
           P(I) = DP(I)
 200    CONTINUE
*--
        IRET = 0
        RETURN
*--
        END
        SUBROUTINE VXXTRD(X,Q,R,P,A,B,IRET)
*-----------------------------------------------------------------------
*--
*--     Tracking in the Vertex Region with Derivatives
*--
*--     Input:
*--        X(3)   ... Vertex position (x,y,z)
*--        Q(3)   ... Momentum and direction at vertex (theta,phi,1/p)
*--        R      ... Radius at final reference surface
*--
*--     Output:
*--        P(5)   ... Parameter vector at final reference surface
*--        A(5,3) ... Derivative matrix dP/dX
*--        B(5,3) ... Derivative matrix dP/dQ
*--        IRET   ... Return code (0 o.k.)
*--
*--     Dietrich Liko
*-----------------------------------------------------------------------
*--
        PARAMETER  (RMIN = 1.E-10)
        DOUBLE PRECISION PI
        PARAMETER  (PI     = 3.1415926536D0)
*--
        REAL X(3), Q(3), P(5), A(5,3), B(5,3)
        DOUBLE PRECISION DX(3), DQ(3), DR, DP(5), DP1(5)
        DOUBLE PRECISION DDX(3),DDQ(3),SAVE,RES
        DATA DDX/3*0.01/DDQ/0.001,0.001,0.0055/
*--
        IF (R.LT.RMIN) THEN
           IRET = 1
           RETURN
        ENDIF
        IF (( Q(1) .LT. 0. ) .OR. ( Q(1) .GT. 2 * PI )) THEN
           IRET = 2
           RETURN
        ENDIF
*--
        DO 100 I = 1, 3
           DX(I) = X(I)
           DQ(I) = Q(I)
 100    CONTINUE
        DR = R
*--
        CALL VXXTR1(DX(1),DQ(1),DR,DP(1),IERR)
        IF (IERR .NE. 0) THEN
           IRET = 10 + IERR
           RETURN
        ENDIF
*--
        DO 200 I = 1, 5
           P(I) = DP(I)
 200    CONTINUE
*--
        DO 300 I = 1, 3
           SAVE = DX(I)
           DX(I) = DX(I) + DDX(I)
           CALL VXXTR1(DX,DQ,DR,DP1,IERR)
           IF (IERR .NE. 0) THEN
              IRET = 10 + IERR
              RETURN
           ENDIF
           RES = MOD( DP1(1) - DP(1) , 2 * PI * DR )
           IF ( ABS( RES ) .GT. PI * DR )
     &        RES = RES - SIGN( 2 * PI * DR , RES)
           A(1,I) = RES / DDX(I)
           A(2,I) = ( DP1(2) - DP(2) ) / DDX(I)
           A(3,I) = ( DP1(3) - DP(3) ) / DDX(I)
           RES = MOD( DP1(4) - DP(4), 2 * PI )
           IF ( ABS( RES ) .GT. PI ) RES = RES - SIGN(2 * PI , RES)
           A(4,I) = RES / DDX(I)
           A(5,I) = ( DP1(5) - DP(5) ) / DDX(I)
           DX(I) = SAVE
 300    CONTINUE
*--
        DDQ(1) = SIGN(DDQ(1),PI/2 - DQ(1))
        DDQ(3) = SIGN(DDQ(3),DQ(3))
*--
        DO 400 I = 1, 3
           SAVE = DQ(I)
           DQ(I) = DQ(I) + DDQ(I)
           CALL VXXTR1(DX,DQ,DR,DP1,IERR)
           IF (IERR .NE. 0) THEN
              IRET = 10 + IERR
              RETURN
           ENDIF
           RES = MOD( DP1(1) - DP(1) , 2 * PI * DR )
           IF ( ABS( RES ) .GT. PI * DR )
     &        RES = RES - SIGN( 2 * PI * DR , RES)
           B(1,I) = RES / DDQ(I)
           B(2,I) = ( DP1(2) - DP(2) ) / DDQ(I)
           B(3,I) = ( DP1(3) - DP(3) ) / DDQ(I)
           RES = MOD( DP1(4) - DP(4), 2 * PI )
           IF ( ABS( RES ) .GT. PI ) RES = RES - SIGN(2 * PI , RES)
           B(4,I) = RES / DDQ(I)
           B(5,I) = ( DP1(5) - DP(5) ) / DDQ(I)
           DQ(I) = SAVE
 400    CONTINUE
*--
        IRET = 0
        RETURN
*--
        END
        SUBROUTINE VXXTR1(XIN,QIN,ROU,POU,IRET)
*-----------------------------------------------------------------------
*--
*--     Tracking in the Vertex Region (internal routine)
*--
*--     Dietrich Liko
*-----------------------------------------------------------------------
*--
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*--
        PARAMETER  (PI     = 3.1415926536D0)
        PARAMETER  (RR1MIN = 1.D-10)
*--
        DOUBLE PRECISION CONSB
        COMMON /VXMAGF/CONSB
*--
        DOUBLE PRECISION XIN(3),QIN(3),ROU,POU(5)
*--
        HRH = -SIN( QIN(1) ) / ( CONSB * QIN(3) )
        RRH = HRH ** 2
*--
        X1  = XIN(1) - HRH * SIN( QIN(2) )
        Y1  = XIN(2) + HRH * COS( QIN(2) )
        RR1 = X1 ** 2 + Y1 ** 2
        IF (RR1 .LT. RR1MIN) THEN
           IRET = 1
           RETURN
        ENDIF
        R1  = SQRT( RR1 )
        PH1 = ATAN2( Y1, X1 )
*--
        DPH = ( ROU ** 2 - RRH - RR1 ) / ( 2 * R1 * HRH )
        IF ( ABS( DPH ) .GT. 1.) THEN
           IRET = 2
           RETURN
        ENDIF
        DPH = ASIN( DPH ) + PH1 - QIN(2)
        DPH = MOD( DPH, 2 * PI )
        IF (HRH .GT. 0) THEN
           IF (DPH .LT. 0) DPH = DPH + 2 * PI
        ELSE
           IF (DPH .GT. 0) DPH = DPH - 2 * PI
        ENDIF
*--
        X2  = X1 + HRH * SIN( QIN(2) + DPH )
        Y2  = Y1 - HRH * COS( QIN(2) + DPH )
        PH2 = ATAN2( Y2, X2 )
        IF (PH2 .LT. 0.) PH2 = PH2 + 2 * PI
*--
        POU(1) = ROU * PH2
        POU(2) = XIN(3) + DPH * HRH / TAN( QIN(1) )
        POU(3) = QIN(1)
        POU(4) = MOD( QIN(2) + DPH , 2 * PI)
        IF (POU(4) .LT. 0.) POU(4) = POU(4) + 2 * PI
        POU(5) = QIN(3)
*--
        IRET = 0
        RETURN
*--
        END
        SUBROUTINE VXNORM(R,P)
*-----------------------------------------------------------------------
*--
*--     correct a parameter vector for allowed values
*--
*--     Dietrich Liko
*-----------------------------------------------------------------------
*--
        PARAMETER (PI = 3.1415926536)
*--
        REAL P(5)
*--
        P(1) = MOD( P(1) , 2 * PI * R )
        IF ( P(1) .LT. 0.) P(1) = P(1) + 2 * PI * P(1)
        P(3) = MOD( P(3) , 2 * PI )
        IF ( P(3) .LT. 0.) P(3) = P(3) + 2 * PI
        IF ( P(3) .GT. PI) THEN
           P(3) = 2 * PI - P(3)
           P(4) = P(4) + PI
        ENDIF
        P(4) = MOD( P(4) , 2 * PI )
        IF ( P(4) .LT. 0.) P(4) = P(4) + 2 * PI
*--
        RETURN
*--
        END
        SUBROUTINE VXCART(R,P1,XYZ,PXYZ)
*-----------------------------------------------------------------------
*--
*--     go to cartesian cordinates
*--
*--     Dietrich Liko
*-----------------------------------------------------------------------
*--

        REAL P1(5),XYZ(3),PXYZ(3)

        DOUBLE PRECISION PHI

        IF (R.LT.1.E-10) THEN
           XYZ(1) = 0.
           XYZ(2) = 0.
           XYZ(3) = 0.
        ELSE
           PHI     = DBLE(P1(1))/DBLE(R)
           XYZ(1)  = R * DCOS(PHI)
           XYZ(2)  = R * DSIN(PHI)
           XYZ(3)  = P1(2)
        ENDIF

        IF (ABS(P1(5)).LT.1.E-10) THEN
           P = 1.E10
        ELSE
           P = 1./P1(5)
        ENDIF

        PXYZ(1) = P * SIN( P1(3) ) * COS( P1(4) )
        PXYZ(2) = P * SIN( P1(3) ) * SIN( P1(4) )
        PXYZ(3) = P * COS( P1(3) )

        END
