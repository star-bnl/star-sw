      SUBROUTINE SETUPANALYTICFIELD(IFAIL)
*-----------------------------------------------------------------------
*     SETUP  - Routine calling the appropriate setup routine.
*     (Last changed on 19/ 9/07.)
*-----------------------------------------------------------------------
      implicit none
      INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -     MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,IFAIL1

       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! SETUP  WARNING : Unable to allocate'//
     -           ' storage for the capacitance matrix; no charges.'
            RETURN
      ELSE
      ENDIF
*** Set a separate set of plane variables to avoid repeated loops.
       IF(YNPLAN(1))THEN
            COPLAX=COPLAN(1)
            YNPLAX=.TRUE.
       ELSEIF(YNPLAN(2))THEN
            COPLAX=COPLAN(2)
            YNPLAX=.TRUE.
       ELSE
            YNPLAX=.FALSE.
       ENDIF
       IF(YNPLAN(3))THEN
            COPLAY=COPLAN(3)
            YNPLAY=.TRUE.
       ELSEIF(YNPLAN(4))THEN
            COPLAY=COPLAN(4)
            YNPLAY=.TRUE.
       ELSE
            YNPLAY=.FALSE.
       ENDIF
*** Set the correction parameters for the planes.
       IF(TUBE)THEN
            CORVTA=0.0
            CORVTB=0.0
            CORVTC=VTTUBE
       ELSEIF((YNPLAN(1).AND.YNPLAN(2)).AND.
     -      .NOT.(YNPLAN(3).OR.YNPLAN(4)))THEN
            CORVTA=(VTPLAN(1)-VTPLAN(2))/(COPLAN(1)-COPLAN(2))
            CORVTB=0.0
            CORVTC=(VTPLAN(2)*COPLAN(1)-VTPLAN(1)*COPLAN(2))/
     -             (COPLAN(1)-COPLAN(2))
       ELSEIF((YNPLAN(3).AND.YNPLAN(4)).AND.
     -      .NOT.(YNPLAN(1).OR.YNPLAN(2)))THEN
            CORVTA=0.0
            CORVTB=(VTPLAN(3)-VTPLAN(4))/(COPLAN(3)-COPLAN(4))
            CORVTC=(VTPLAN(4)*COPLAN(3)-VTPLAN(3)*COPLAN(4))/
     -             (COPLAN(3)-COPLAN(4))
       ELSE
            CORVTA=0
            CORVTB=0
            CORVTC=0
            IF(YNPLAN(1))CORVTC=VTPLAN(1)
            IF(YNPLAN(2))CORVTC=VTPLAN(2)
            IF(YNPLAN(3))CORVTC=VTPLAN(3)
            IF(YNPLAN(4))CORVTC=VTPLAN(4)
       ENDIF
*** Skip wire calculations if there aren't any.
       IF(NWIRE.LE.0)GOTO 10
*** Call the set routine appropriate for the present cell type.
      IF(TYPE.EQ.'A  '.AND.NXMATT.EQ.0.AND.NYMATT.EQ.0)THEN
         CALL SETA00(IFAIL)
      ELSEIF(TYPE.EQ.'A  ')THEN
            CALL EFQA00(IFAIL)
       ENDIF
       IF(TYPE.EQ.'B1X')CALL SETB1X(IFAIL)
       IF(TYPE.EQ.'B1Y')CALL SETB1Y(IFAIL)
       IF(TYPE.EQ.'B2X')CALL SETB2X(IFAIL)
       IF(TYPE.EQ.'B2Y')CALL SETB2Y(IFAIL)
       IF(TYPE.EQ.'C1 ')CALL SETC10(IFAIL)
       IF(TYPE.EQ.'C2X')CALL SETC2X(IFAIL)
       IF(TYPE.EQ.'C2Y')CALL SETC2Y(IFAIL)
       IF(TYPE.EQ.'C3 ')CALL SETC30(IFAIL)
       IF(TYPE.EQ.'D1 ')CALL SETD10(IFAIL)
       IF(TYPE.EQ.'D2 ')CALL SETD20(IFAIL)
       IF(TYPE.EQ.'D3 ')CALL SETD30(IFAIL)
C      IF(TYPE.EQ.'D4 ')CALL SETD40(IFAIL)
*** Add dipole terms if required
       IF(LDIPOL)THEN
            CALL SETDIP(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SETUP WARNING : Computing the'//
     -                ' dipole moments failed; DIPOLE option ignored.'
                 IFAIL=1
            ENDIF
       ENDIF
*** Check the error condition.
       IF(IFAIL.EQ.1)PRINT *,' ###### SETUP  ERROR   : Preparing the'//
     -      ' the cell for field calculations did not succeed.'
10     CONTINUE
       END
CDECK  ID>, SETNEW.
       SUBROUTINE SETNEW(VNEW,VPLNEW,IFAIL)
*-----------------------------------------------------------------------
*   SETNEW - Calculates charges when the potentials have changed.
*   (Last changed on 10/ 9/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       REAL VNEW(MXWIRE),VPLNEW(5)
       INTEGER IFAIL,I,J
       CHARACTER*10 USER
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE SETNEW ///'
*** Assume the routine will be successful.
       IFAIL=0
       IF(USER.NE.'CELL      ')THEN
            IF(LDEBUG)PRINT *,' ++++++ SETNEW DEBUG   : Recalculating'//
     -           ' the capacitance matrix.'
            CALL SETUP(IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' ###### SETNEW ERROR   : Error computing'//
     -               ' the charges; further cell computations useless.'
                 NWIRE=0
                 RETURN
            ENDIF
       ELSEIF(LDEBUG)THEN
            PRINT *,' ++++++ SETNEW DEBUG   : Capacitance'//
     -           ' matrix still available.'
       ENDIF
*** Set the correction parameters for the planes.
       IF(TUBE)THEN
            CORVTA=0.0
            CORVTB=0.0
            CORVTC=VPLNEW(5)
       ELSEIF((YNPLAN(1).AND.YNPLAN(2)).AND.
     -      .NOT.(YNPLAN(3).OR.YNPLAN(4)))THEN
            CORVTA=(VPLNEW(1)-VPLNEW(2))/(COPLAN(1)-COPLAN(2))
            CORVTB=0.0
            CORVTC=(VPLNEW(2)*COPLAN(1)-VPLNEW(1)*COPLAN(2))/
     -             (COPLAN(1)-COPLAN(2))
       ELSEIF((YNPLAN(3).AND.YNPLAN(4)).AND.
     -      .NOT.(YNPLAN(1).OR.YNPLAN(2)))THEN
            CORVTA=0.0
            CORVTB=(VPLNEW(3)-VPLNEW(4))/(COPLAN(3)-COPLAN(4))
            CORVTC=(VPLNEW(4)*COPLAN(3)-VPLNEW(3)*COPLAN(4))/
     -             (COPLAN(3)-COPLAN(4))
       ELSE
            CORVTA=0
            CORVTB=0
            CORVTC=0
            IF(YNPLAN(1))CORVTC=VPLNEW(1)
            IF(YNPLAN(2))CORVTC=VPLNEW(2)
            IF(YNPLAN(3))CORVTC=VPLNEW(3)
            IF(YNPLAN(4))CORVTC=VPLNEW(4)
       ENDIF
*** Handle the case when the sum of the charges is zero autmatically.
       IF(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4))THEN
            V0=0.0
*** Force sum charges =0 in case of absence of equipotential planes.
       ELSE
            V0=0
            DO 40 I=1,NWIRE
            V0=V0+A(NWIRE+1,I)*VNEW(I)
40          CONTINUE
       ENDIF
*** Next reconstruct the charges.
       DO 30 I=1,NWIRE
       E(I)=0
       DO 20 J=1,NWIRE
       E(I)=E(I)+A(I,J)*(VNEW(J)-V0-(CORVTA*X(J)+CORVTB*Y(J)+CORVTC))
20     CONTINUE
30     CONTINUE
*** Replace the potentials.
       DO 60 I=1,NWIRE
       V(I)=VNEW(I)
60     CONTINUE
       DO 70 I=1,4
       VTPLAN(I)=VPLNEW(I)
70     CONTINUE
       VTTUBE=VPLNEW(5)
*** Add dipole terms if required
       IF(LDIPOL)THEN
            CALL SETDIP(IFAIL)
       ENDIF
       END
CDECK  ID>, SETA00.
       SUBROUTINE SETA00(IFAIL)
*-----------------------------------------------------------------------
*   SETA00 - Subroutine preparing the field calculations by calculating
*            the charges on the wires, for the cell with one charge and
*            not more than one plane in either x or y.
*            The potential used is log(r).
*   Variables : No local variables.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
*** Loop over all wire combinations.
       DO 10 I=1,NWIRE
       A(I,I)=0.25*D(I)**2
*** Take care of the equipotential planes.
       IF(YNPLAX)A(I,I)=A(I,I)/(2.0*(X(I)-COPLAX))**2
       IF(YNPLAY)A(I,I)=A(I,I)/(2.0*(Y(I)-COPLAY))**2
*** Take care of combinations of equipotential planes.
       IF(YNPLAX.AND.YNPLAY)A(I,I)=4.0*A(I,I)*((X(I)-COPLAX)**2
     -                                        +(Y(I)-COPLAY)**2)
*** Define the final version of A(I,I).
       A(I,I)=-0.5*LOG(A(I,I))
*** Loop over all other wires for the off-diagonal elements.
       DO 20 J=I+1,NWIRE
       A(I,J)=(X(I)-X(J))**2+(Y(I)-Y(J))**2
*** Take care of equipotential planes.
       IF(YNPLAX)A(I,J)=A(I,J)/((X(I)+X(J)-2.*COPLAX)**2+(Y(I)-Y(J))**2)
       IF(YNPLAY)A(I,J)=A(I,J)/((X(I)-X(J))**2+(Y(I)+Y(J)-2.*COPLAY)**2)
*** Take care of pairs of equipotential planes in different directions.
       IF(YNPLAX.AND.YNPLAY)A(I,J)=
     -      A(I,J)*((X(I)+X(J)-2.*COPLAX)**2+(Y(I)+Y(J)-2.*COPLAY)**2)
*** Define a final version of A(I,J).
       A(I,J)=-0.5*LOG(A(I,J))
*** Copy this to A(J,I) since the capacitance matrix is symmetric.
       A(J,I)=A(I,J)
20     CONTINUE
10     CONTINUE
*** Call CHARGE to calculate the charges really.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETB1X.
       SUBROUTINE SETB1X(IFAIL)
*-----------------------------------------------------------------------
*   SETB1X - Routine preparing the field calculations by filling the
*            c-matrix, the potential used is re(log(sin pi/s (z-z0))).
*   VARIABLES : XX         : Difference in x of two wires * factor.
*               YY         : Difference in y of two wires * factor.
*               YYMIRR     : Difference in y of one wire and the mirror
*                            image of another * factor.
*               R2PLAN     : Periodic length of (XX,YYMIRR)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
*** Loop over all wires and calculate the diagonal elements first.
       DO 10 I=1,NWIRE
       A(I,I)=-LOG(0.5*D(I)*PI/SX)
*** Take care of a plane at constant y if it exist.
       IF(YNPLAY)THEN
            YY=(PI/SX)*2.0*(Y(I)-COPLAY)
            IF(ABS(YY).GT.20.0)A(I,I)=A(I,I)+ABS(YY)-CLOG2
            IF(ABS(YY).LE.20.0)A(I,I)=A(I,I)+LOG(ABS(SINH(YY)))
       ENDIF
*** Loop over all other wires to obtain off-diagonal elements.
       DO 20 J=I+1,NWIRE
       XX=(PI/SX)*(X(I)-X(J))
       YY=(PI/SX)*(Y(I)-Y(J))
       IF(ABS(YY).GT.20.0)A(I,J)=-ABS(YY)+CLOG2
       IF(ABS(YY).LE.20.0)A(I,J)=-0.5*LOG(SINH(YY)**2+SIN(XX)**2)
*** Take equipotential planes into account if they exist.
       IF(YNPLAY)THEN
            YYMIRR=(PI/SX)*(Y(I)+Y(J)-2.0*COPLAY)
            IF(ABS(YYMIRR).GT.20.0)R2PLAN=ABS(YYMIRR)-CLOG2
            IF(ABS(YYMIRR).LE.20.0)
     -           R2PLAN=0.5*LOG(SINH(YYMIRR)**2+SIN(XX)**2)
            A(I,J)=A(I,J)+R2PLAN
       ENDIF
*** Copy A(I,J) to A(J,I), the capactance matrix is symmetric.
       A(J,I)=A(I,J)
*** Finish the wire loops.
20     CONTINUE
10     CONTINUE
*** Call routine CHARGE calculating all kinds of useful things.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETB1Y.
       SUBROUTINE SETB1Y(IFAIL)
*-----------------------------------------------------------------------
*   SETB1Y - Routine preparing the field calculations by setting the
*            charges. The potential used is Re log(sinh pi/sy(z-z0)).
*   VARIABLES : YY         : Difference in y of two wires * factor.
*               XXMIRR     : Difference in x of one wire and the mirror
*                            image of another * factor.
*               R2PLAN     : Periodic length of (XXMIRR,YY).
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
*** Loop over all wires and calculate the diagonal elements first.
       DO 10 I=1,NWIRE
       A(I,I)=-LOG(0.5*D(I)*PI/SY)
*** Take care of planes 1 and 2 if present.
       IF(YNPLAX)THEN
            XX=(PI/SY)*2.0*(X(I)-COPLAX)
            IF(ABS(XX).GT.20.0)A(I,I)=A(I,I)+ABS(XX)-CLOG2
            IF(ABS(XX).LE.20.0)A(I,I)=A(I,I)+LOG(ABS(SINH(XX)))
       ENDIF
*** Loop over all other wires to obtain off-diagonal elements.
       DO 20 J=I+1,NWIRE
       XX=(PI/SY)*(X(I)-X(J))
       YY=(PI/SY)*(Y(I)-Y(J))
       IF(ABS(XX).GT.20.0)A(I,J)=-ABS(XX)+CLOG2
       IF(ABS(XX).LE.20.0)A(I,J)=-0.5*LOG(SINH(XX)**2+SIN(YY)**2)
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=(PI/SY)*(X(I)+X(J)-2.0*COPLAX)
            IF(ABS(XXMIRR).GT.20.0)R2PLAN=ABS(XXMIRR)-CLOG2
            IF(ABS(XXMIRR).LE.20.0)
     -           R2PLAN=0.5*LOG(SINH(XXMIRR)**2+SIN(YY)**2)
            A(I,J)=A(I,J)+R2PLAN
       ENDIF
*** Copy A(I,J) to A(J,I), the capacitance matrix is symmetric.
       A(J,I)=A(I,J)
*** Finish the wire loops.
20     CONTINUE
10     CONTINUE
*** Call routine CHARGE calculating all kinds of useful things.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETB2X.
       SUBROUTINE SETB2X(IFAIL)
*-----------------------------------------------------------------------
*   SETB2X - Routine preparing the field calculations by setting the
*            charges.
*   VARIABLES : XX         : Difference in x of two wires * factor.
*               YY         : Difference in y of two wires * factor.
*               XXNEG      : Difference in x of one wire and the mirror
*                            image in period direction of another * fac.
*               YYMIRR     : Difference in y of one wire and the mirror
*                            image of another * factor.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
*** Loop over all wires and calculate the diagonal elements first.
       DO 10 I=1,NWIRE
       XX=(PI/SX)*(X(I)-COPLAX)
       A(I,I)=(0.25*D(I)*PI/SX)/SIN(XX)
*** Take care of a plane at constant y if it exists.
       IF(YNPLAY)THEN
            YYMIRR=(PI/SX)*(Y(I)-COPLAY)
            IF(ABS(YYMIRR).LE.20.0) A(I,I)=A(I,I)*
     -           SQRT(SINH(YYMIRR)**2+SIN(XX)**2)/SINH(YYMIRR)
       ENDIF
*** Store the true value of A(I,I).
       A(I,I)=-LOG(ABS(A(I,I)))
*** Loop over all other wires to obtain off-diagonal elements.
       DO 20 J=I+1,NWIRE
       XX=0.5*PI*(X(I)-X(J))/SX
       YY=0.5*PI*(Y(I)-Y(J))/SX
       XXNEG=0.5*PI*(X(I)+X(J)-2.0*COPLAX)/SX
       IF(ABS(YY).LE.20.0)
     -      A(I,J)=(SINH(YY)**2+SIN(XX)**2)/(SINH(YY)**2+SIN(XXNEG)**2)
       IF(ABS(YY).GT.20.0)A(I,J)=1.0
*** Take an equipotential plane at constant y into account.
       IF(YNPLAY)THEN
            YYMIRR=0.5*PI*(Y(I)+Y(J)-2.0*COPLAY)/SX
            IF(ABS(YYMIRR).LE.20.0) A(I,J)=A(I,J)*
     -      (SINH(YYMIRR)**2+SIN(XXNEG)**2)/(SINH(YYMIRR)**2+SIN(XX)**2)
       ENDIF
*** Store the true value of A(I,J) in both A(I,J) and A(J,I).
       A(I,J)=-0.5*LOG(A(I,J))
       A(J,I)=A(I,J)
*** Finish the wire loops.
20     CONTINUE
*** Set the B2SIN vector.
       B2SIN(I)=SIN(PI*(COPLAX-X(I))/SX)
10     CONTINUE
*** Call routine CHARGE calculating all kinds of useful things.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETB2Y.
       SUBROUTINE SETB2Y(IFAIL)
*-----------------------------------------------------------------------
*   SETB2Y - Routine preparing the field calculations by setting the
*            charges.
*   VARIABLES : XX         : Difference in x of two wires * factor.
*               YY         : Difference in y of two wires * factor.
*               XXMIRR     : Difference in x of one wire and the mirror
*                            image of another * factor.
*               YYNEG      : Difference in y of one wire and the mirror
*                            image in period direction of another * fac.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
*** Loop over all wires and calculate the diagonal elements first.
       DO 10 I=1,NWIRE
       YY=(PI/SY)*(Y(I)-COPLAY)
       A(I,I)=(0.25*D(I)*PI/SY)/SIN(YY)
*** Take care of a plane at constant x if present.
       IF(YNPLAX)THEN
            XXMIRR=(PI/SY)*(X(I)-COPLAX)
            IF(ABS(XXMIRR).LE.20.0)A(I,I)=A(I,I)*
     -           SQRT(SINH(XXMIRR)**2+SIN(YY)**2)/SINH(XXMIRR)
       ENDIF
*** Store the true value of A(I,I).
       A(I,I)=-LOG(ABS(A(I,I)))
*** Loop over all other wires to obtain off-diagonal elements.
       DO 20 J=I+1,NWIRE
       XX=0.5*PI*(X(I)-X(J))/SY
       YY=0.5*PI*(Y(I)-Y(J))/SY
       YYNEG=0.5*PI*(Y(I)+Y(J)-2.0*COPLAY)/SY
       IF(ABS(XX).LE.20.0)
     -      A(I,J)=(SINH(XX)**2+SIN(YY)**2)/(SINH(XX)**2+SIN(YYNEG)**2)
       IF(ABS(XX).GT.20.0)A(I,J)=1.0
*** Take an equipotential plane at constant x into account.
       IF(YNPLAX)THEN
            XXMIRR=0.5*PI*(X(I)+X(J)-2.0*COPLAX)/SY
            IF(ABS(XXMIRR).LE.20.0)A(I,J)=A(I,J)*
     -      (SINH(XXMIRR)**2+SIN(YYNEG)**2)/(SINH(XXMIRR)**2+SIN(YY)**2)
       ENDIF
*** Store the true value of A(I,J) in both A(I,J) and A(J,I).
       A(I,J)=-0.5*LOG(A(I,J))
       A(J,I)=A(I,J)
*** Finish the wire loops.
20     CONTINUE
*** Set the B2SIN vector.
       B2SIN(I)=SIN(PI*(COPLAY-Y(I))/SY)
10     CONTINUE
*** Call routine CHARGE calculating all kinds of useful things.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETC10.
       SUBROUTINE SETC10(IFAIL)
*-----------------------------------------------------------------------
*   SETC10 - This initialising routine computes the wire charges E and
*            sets certain constants in common. The wire are located at
*            (X(J),Y(J))+(LX*SX,LY*SY), J=1(1)NWIRE,
*            LX=-infinity(1)infinity, LY=-infinity(1)infinity.
*            Use is made of the routine PH2.
*
*  (Written by G.A.Erskine/DD, 14.8.1984 modified to some extent)
*-----------------------------------------------------------------------
       IMPLICIT COMPLEX (W,Z)
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
*** Statement function returning XX if mode is 0, YY else.
       UTYPE(XX,YY)=(1-MODE)*XX+MODE*YY
*** Set some of the constants used by PH2 and E2SUM.
       CONST=2.*PI/(SX*SY)
       IF(SX.LE.SY)THEN
            MODE=1
            IF(SY/SX.LT.8.0)THEN
                 P=EXP(-PI*SY/SX)
            ELSE
                 P=0.0
            ENDIF
            ZMULT=CMPLX(PI/SX,0.0)
       ELSE
            MODE=0
            IF(SX/SY.LT.8.0)THEN
                 P=EXP(-PI*SX/SY)
            ELSE
                 P=0.0
            ENDIF
            ZMULT=CMPLX(0.0,PI/SY)
       ENDIF
       P1=P**2
       P2=P**6
*** Store the capacitance matrix.
       DO 20 I=1,NWIRE
       DO 10 J=1,NWIRE
       TEMP=CONST*UTYPE(X(I),Y(I))*UTYPE(X(J),Y(J))
       IF(I.EQ.J)THEN
            A(I,I)=PH2LIM(0.5*D(I))-TEMP
       ELSE
            A(I,J)=PH2(X(I)-X(J),Y(I)-Y(J))-TEMP
       ENDIF
10     CONTINUE
20     CONTINUE
*** Call CHARGE to find the charges.
       CALL CHARGE(IFAIL)
       IF(IFAIL.EQ.1)RETURN
*** Calculate the non-logarithmic term in the potential.
       S=0.0
       DO 30 J=1,NWIRE
       S=S+E(J)*UTYPE(X(J),Y(J))
30     CONTINUE
       C1=-CONST*S
       END
CDECK  ID>, SETC2X.
       SUBROUTINE SETC2X(IFAIL)
*-----------------------------------------------------------------------
*   SETC2X - This initializing subroutine stores the capacitance matrix
*            for the configuration:
*            wires at zw(j)+cmplx(lx*2*sx,ly*sy),
*            j=1(1)n, lx=-infinity(1)infinity, ly=-infinity(1)infinity.
*            but the signs of the charges alternate in the x-direction
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
*** Initialise the constants.
       P=0.0
       P1=0.0
       P2=0.0
       IF(2.0*SX.LE.SY)THEN
            MODE=1
            IF(SY/SX.LT.25.0)P=EXP(-0.5*PI*SY/SX)
            ZMULT=CMPLX(0.5*PI/SX,0.0)
       ELSE
            MODE=0
            IF(SX/SY.LT.6.0)P=EXP(-2.0*PI*SX/SY)
            ZMULT=CMPLX(0.0,PI/SY)
       ENDIF
       P1=P**2
       IF(P1.GT.1.0E-10)P2=P**6
*** Produce some debugging output.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ SETC2X DEBUG   : P, P1, P2=',P,P1,P2
            PRINT *,'                         ZMULT=',ZMULT
            PRINT *,'                         MODE=',MODE
       ENDIF
*** Fill the capacitance matrix.
       DO 10 I=1,NWIRE
       CX=COPLAX-SX*ANINT((COPLAX-X(I))/SX)
       DO 20 J=1,NWIRE
       IF(MODE.EQ.0)THEN
            TEMP=(X(I)-CX)*(X(J)-CX)*2.0*PI/(SX*SY)
       ELSE
            TEMP=0.0
       ENDIF
       IF(I.EQ.J)THEN
            A(I,I)=PH2LIM(0.5*D(I))-
     -            PH2(2.0*(X(I)-CX),0.0)-TEMP
       ELSE
            A(I,J)=PH2(X(I)-X(J),Y(I)-Y(J))-
     -            PH2(X(I)+X(J)-2.0*CX,Y(I)-Y(J))-TEMP
       ENDIF
20     CONTINUE
10     CONTINUE
*** Call CHARGE to find the wire charges.
       CALL CHARGE(IFAIL)
       IF(IFAIL.EQ.1)RETURN
*** Determine the non-logaritmic part of the potential (0 if MODE=1).
       IF(MODE.EQ.0)THEN
            S=0.0
            DO 30 I=1,NWIRE
            CX=COPLAX-SX*ANINT((COPLAX-X(I))/SX)
            S=S+E(I)*(X(I)-CX)
30          CONTINUE
            C1=-S*2.0*PI/(SX*SY)
       ELSE
            C1=0.0
       ENDIF
       RETURN
       END
CDECK  ID>, SETC2Y.
       SUBROUTINE SETC2Y(IFAIL)
*-----------------------------------------------------------------------
*   SETC2Y - This initializing subroutine stores the capacitance matrix
*            for the configuration:
*            wires at zw(j)+cmplx(lx*sx,ly*2*sy),
*            j=1(1)n, lx=-infinity(1)infinity, ly=-infinity(1)infinity.
*            but the signs of the charges alternate in the y-direction
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
*** Initialise the constants.
       P=0
       P1=0
       P2=0
       IF(SX.LE.2.0*SY)THEN
            MODE=1
            IF(SY/SX.LE.6.0)P=EXP(-2.0*PI*SY/SX)
            ZMULT=CMPLX(PI/SX,0.0)
       ELSE
            MODE=0
            IF(SX/SY.LE.25.0)P=EXP(-0.5*PI*SX/SY)
            ZMULT=CMPLX(0.0,0.5*PI/SY)
       ENDIF
       P1=P**2
       IF(P1.GT.1.0E-10)P2=P**6
*** Produce some debugging output.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ SETC2Y DEBUG   : P, P1, P2=',P,P1,P2
            PRINT *,'                         ZMULT=',ZMULT
            PRINT *,'                         MODE=',MODE
       ENDIF
*** Fill the capacitance matrix.
       DO 10 I=1,NWIRE
       CY=COPLAY-SY*ANINT((COPLAY-Y(I))/SY)
       DO 20 J=1,NWIRE
       IF(MODE.EQ.0)THEN
            TEMP=0.0
       ELSE
            TEMP=(Y(I)-CY)*(Y(J)-CY)*2.0*PI/(SX*SY)
       ENDIF
       IF(I.EQ.J)THEN
            A(I,I)=PH2LIM(0.5*D(I))-
     -            PH2(0.0,2.0*(Y(J)-CY))-TEMP
       ELSE
            A(I,J)=PH2(X(I)-X(J),Y(I)-Y(J))-
     -            PH2(X(I)-X(J),Y(I)+Y(J)-2.0*CY)-TEMP
       ENDIF
20     CONTINUE
10     CONTINUE
*** Call CHARGE to find the wire charges.
       CALL CHARGE(IFAIL)
       IF(IFAIL.EQ.1)RETURN
*** The non-logarithmic part of the potential is zero if MODE=0.
       IF(MODE.EQ.1)THEN
            S=0.0
            DO 30 I=1,NWIRE
            CY=COPLAY-SY*ANINT((COPLAY-Y(I))/SY)
            S=S+E(I)*(Y(I)-CY)
30          CONTINUE
            C1=-S*2.0*PI/(SX*SY)
       ELSE
            C1=0.0
       ENDIF
       END
CDECK  ID>, SETC30.
       SUBROUTINE SETC30(IFAIL)
*-----------------------------------------------------------------------
*   SETC30 - This initializing subroutine stores the capacitance matrix
*            for a configuration with
*            wires at zw(j)+cmplx(lx*2*sx,ly*2*sy),
*            j=1(1)n, lx=-infinity(1)infinity, ly=-infinity(1)infinity.
*            but the signs of the charges alternate in both directions.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
*** Initialise the constants.
       P=0.0
       P1=0.0
       P2=0.0
       IF(SX.LE.SY)THEN
            MODE=1
            IF(SY/SX.LE.13.0)P=EXP(-PI*SY/SX)
            ZMULT=CMPLX(0.5*PI/SX,0.0)
       ELSE
            MODE=0
            IF(SX/SY.LE.13.0)P=EXP(-PI*SX/SY)
            ZMULT=CMPLX(0.0,0.5*PI/SY)
       ENDIF
       P1=P**2
       IF(P1.GT.1.0E-10)P2=P**6
*** Produce some debugging output.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ SETC30 DEBUG   : P, P1, P2=',P,P1,P2
            PRINT *,'                         ZMULT=',ZMULT
            PRINT *,'                         MODE=',MODE
       ENDIF
*** Fill the capacitance matrix.
       DO 10 I=1,NWIRE
       CX=COPLAX-SX*ANINT((COPLAX-X(I))/SX)
       CY=COPLAY-SY*ANINT((COPLAY-Y(I))/SY)
       DO 20 J=1,NWIRE
       IF(I.EQ.J)THEN
            A(I,I)=PH2LIM(0.5*D(I))-
     -           PH2(0.0,2.0*(Y(I)-CY))-
     -           PH2(2.0*(X(I)-CX),0.0)+
     -           PH2(2.0*(X(I)-CX),2.0*(Y(I)-CY))
       ELSE
            A(I,J)=PH2(X(I)-X(J),Y(I)-Y(J))-
     -           PH2(X(I)-X(J),Y(I)+Y(J)-2.0*CY)-
     -           PH2(X(I)+X(J)-2.0*CX,Y(I)-Y(J))+
     -           PH2(X(I)+X(J)-2.0*CX,Y(I)+Y(J)-2.0*CY)
       ENDIF
20     CONTINUE
10     CONTINUE
*** Call CHARGE to find the wire charges.
       CALL CHARGE(IFAIL)
       IF(IFAIL.EQ.1)RETURN
*** The non-logarithmic part of the potential is zero in this case.
       C1=0.0
       END
CDECK  ID>, SETD10.
       SUBROUTINE SETD10(IFAIL)
*-----------------------------------------------------------------------
*   SETD10 - Subroutine preparing the field calculations by calculating
*            the charges on the wires, for cells with a tube.
*   VARIABLES :
*   (Last changed on  4/ 9/95.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMPLEX ZI,ZJ
*** Loop over all wires.
       DO 10 I=1,NWIRE
*   Set the diagonal terms.
       A(I,I)=-LOG(0.5*D(I)*COTUBE/(COTUBE**2-(X(I)**2+Y(I)**2)))
*   Set a complex wire-coordinate to make things a little easier.
       ZI=CMPLX(X(I),Y(I))
*** Loop over all other wires for the off-diagonal elements.
       DO 20 J=I+1,NWIRE
*   Set a complex wire-coordinate to make things a little easier.
       ZJ=CMPLX(X(J),Y(J))
       A(I,J)=-LOG(ABS(COTUBE*(ZI-ZJ)/(COTUBE**2-CONJG(ZI)*ZJ)))
*** Copy this to A(J,I) since the capacitance matrix is symmetric.
       A(J,I)=A(I,J)
20     CONTINUE
10     CONTINUE
*** Call CHARGE to calculate the charges really.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETD20.
       SUBROUTINE SETD20(IFAIL)
*-----------------------------------------------------------------------
*   SETD20 - Subroutine preparing the field calculations by calculating
*            the charges on the wires, for cells with a tube and a
*            phi periodicity. Assymetric capacitance matrix !
*   VARIABLES :
*   (Last changed on 18/ 2/93.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMPLEX ZI,ZJ
*** Loop over all wires.
       DO 10 I=1,NWIRE
*   Set a complex wire-coordinate to make things a little easier.
       ZI=CMPLX(X(I),Y(I))
*** Case of a wire near the centre.
       IF(ABS(ZI).LT.D(I)/2)THEN
*   Inner loop over the wires.
            DO 20 J=1,NWIRE
*   Set the diagonal terms.
            IF(I.EQ.J)THEN
                 A(I,I)=-LOG(0.5*D(I)/
     -                (COTUBE-(X(I)**2+Y(I)**2)/COTUBE))
*   Off-diagonal terms.
            ELSE
                 ZJ=CMPLX(X(J),Y(J))
                 A(J,I)=-LOG(ABS((1/COTUBE)*(ZI-ZJ)/
     -                (1-CONJG(ZI)*ZJ/COTUBE**2)))
            ENDIF
20          CONTINUE
*** Normal case.
       ELSE
*   Inner wire loop.
            DO 30 J=1,NWIRE
*   Diagonal elements.
            IF(I.EQ.J)THEN
                 A(I,I)=-LOG(ABS(0.5*D(I)*MTUBE*ZI**(MTUBE-1)/
     -                ((COTUBE**MTUBE)*(1-(ABS(ZI)/COTUBE)**
     -                (2*MTUBE)))))
*   Off-diagonal terms.
            ELSE
                 ZJ=CMPLX(X(J),Y(J))
                 A(J,I)=-LOG(ABS((1/COTUBE**MTUBE)*
     -                (ZJ**MTUBE-ZI**MTUBE)/
     -                (1-(ZJ*CONJG(ZI)/COTUBE**2)**MTUBE)))
            ENDIF
30          CONTINUE
       ENDIF
*** Next wire.
10     CONTINUE
*** Call CHARGE to calculate the charges really.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, SETD30.
       SUBROUTINE SETD30(IFAIL)
*-----------------------------------------------------------------------
*   SETD30 - Subroutine preparing the field calculations by calculating
*            the charges on the wires, for cells with wires inside a
*            polygon.
*   Variables : No local variables.
*   (Last changed on 21/ 2/94.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMPLEX WD
*** Evaluate kappa, a constant needed by EFCMAP.
       KAPPA=GAMMA(REAL(NTUBE+1)/REAL(NTUBE))*
     -      GAMMA(REAL(NTUBE-2)/REAL(NTUBE))/
     -      GAMMA(REAL(NTUBE-1)/REAL(NTUBE))
*** Loop over all wire combinations.
       DO 10 I=1,NWIRE
*** Compute wire mappings only once.
       CALL EFCMAP(CMPLX(X(I),Y(I))/COTUBE,WMAP(I),WD)
*   Diagonal elements.
       A(I,I)=-LOG(ABS((0.5*D(I)/COTUBE)*WD/(1-ABS(WMAP(I))**2)))
*** Loop over all other wires for the off-diagonal elements.
       DO 20 J=1,I-1
       A(I,J)=-LOG(ABS((WMAP(I)-WMAP(J))/(1-CONJG(WMAP(I))*WMAP(J))))
*** Copy this to A(J,I) since the capacitance matrix is symmetric.
       A(J,I)=A(I,J)
20     CONTINUE
10     CONTINUE
*** Call CHARGE to calculate the charges really.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, CHARGE.
       SUBROUTINE CHARGE(IFAIL)
*-----------------------------------------------------------------------
*   CHARGE - Routine actually inverting the capacitance matrix filled in
*            the SET... routines thereby providing the charges.
*   (Last changed on 30/ 1/93.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION T
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE CHARGE ///'
*** Dump the capacitance matrix before inversion, if DEBUG is requested.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : Dump of the'',
     -           '' capacitance matrix before inversion follows:''/)')
            DO 160 I=0,NWIRE-1,10
            DO 170 J=0,NWIRE-1,10
            WRITE(LUNOUT,'(''1 Block '',I2,''.'',I2/)') I/10,J/10
            DO 180 II=1,10
            IF(I+II.GT.NWIRE)GOTO 180
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (A(I+II,J+JJ),JJ=1,MIN(NWIRE-J,10))
180         CONTINUE
170         CONTINUE
160         CONTINUE
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : End of the'',
     -           '' uninverted capacitance matrix dump.''/)')
       ENDIF
*** Transfer the voltages to A, correcting for the equipotential planes.
       DO 10 I=1,NWIRE
       A(I,MXWIRE+3)=V(I)-(CORVTA*X(I)+CORVTB*Y(I)+CORVTC)
10     CONTINUE
*** Force sum charges =0 in case of absence of equipotential planes.
       IF(.NOT.(YNPLAN(1).OR.YNPLAN(2).OR.
     -      YNPLAN(3).OR.YNPLAN(4).OR.TUBE))THEN
*   Add extra elements to A, acting as constraints.
            A(NWIRE+1,MXWIRE+3)=0.0
            DO 20 I=1,NWIRE
            A(I,NWIRE+1)=1.0
            A(NWIRE+1,I)=1.0
20          CONTINUE
            A(NWIRE+1,NWIRE+1)=0.0
*   Solve equations to yield charges, using KERNLIB (scalar).
            CALL DEQINV(NWIRE+1,A,MXWIRE+1,A(1,NWIRE+2),IFAIL,1,
     -           A(1,MXWIRE+3))
*   Modify A to give true inverse of capacitance matrix.
            IF(A(NWIRE+1,NWIRE+1).NE.0.0)THEN
                 T=1.0/A(NWIRE+1,NWIRE+1)
                 DO 40 I=1,NWIRE
                 DO 30 J=1,NWIRE
                 A(I,J)=A(I,J)-T*A(I,NWIRE+1)*A(NWIRE+1,J)
30               CONTINUE
40               CONTINUE
            ELSE
                 PRINT *,' !!!!!! CHARGE WARNING : True inverse of'//
     -                ' the capacitance matrix could not be calculated.'
                 PRINT *,'                         Use of the FACTOR'//
     -                ' instruction should be avoided.'
            ENDIF
*   Store reference potential.
            V0=A(NWIRE+1,MXWIRE+3)
       ELSE
*** Handle the case when the sum of the charges is zero automatically.
            CALL DEQINV(NWIRE,A,MXWIRE+1,A(1,NWIRE+2),IFAIL,1,
     -           A(1,MXWIRE+3))
*   Reference potential chosen to be zero.
            V0=0.0
       ENDIF
*** Check the error condition flag.
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### CHARGE ERROR   : Failure to solve the'//
     -           ' capacitance equations; no charges are available.'
            IFAIL=1
            RETURN
       ENDIF
*** Copy the charges to E.
       DO 50 I=1,NWIRE
       E(I)=A(I,MXWIRE+3)
50     CONTINUE
*** If LDEBUG is on, print the capacitance matrix.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : Dump of the'',
     -           '' capacitance matrix follows:''/)')
            DO 60 I=0,NWIRE-1,10
            DO 70 J=0,NWIRE-1,10
            WRITE(LUNOUT,'(''1 Block '',I2,''.'',I2/)') I/10,J/10
            DO 80 II=1,10
            IF(I+II.GT.NWIRE)GOTO 80
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (A(I+II,J+JJ),JJ=1,MIN(NWIRE-J,10))
80          CONTINUE
70          CONTINUE
60          CONTINUE
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : End of the'',
     -           '' capacitance matrix.''/)')
       ENDIF
*   And also check the quality of the matrix inversion.
       IF(LCHGCH)THEN
            WRITE(LUNOUT,'(/''  QUALITY CHECK'',
     -           '' OF THE CHARGE CALCULATION.''//
     -           ''                          wire       E as obtained'',
     -           ''     E reconstructed''/)')
            DO 100 I=1,NWIRE
            A(I,MXWIRE+2)=0
            DO 110 J=1,NWIRE
            A(I,MXWIRE+2)=A(I,MXWIRE+2)+
     -           A(I,J)*(V(J)-V0-(CORVTA*X(J)+CORVTB*Y(J)+CORVTC))
110         CONTINUE
            WRITE(LUNOUT,'(26X,I4,5X,E15.8,5X,E15.8)')
     -           I,E(I),A(I,MXWIRE+2)
100         CONTINUE
            WRITE(LUNOUT,'('' '')')
       ENDIF
       END
CDECK  ID>, SETDIP.
       SUBROUTINE SETDIP(IFAIL)
*-----------------------------------------------------------------------
*   SETDIP - Subroutine computing coefficients for the dipole terms in
*            cells without periodicities.
*   (Last changed on 19/ 9/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       INTEGER IFAIL,IW,I,N,ILOC,ITER,NITMAX
       PARAMETER(N=20)
       REAL ANGLE(N),VOLT(N),DRES,RMULT,EX,EY,EZ,ETOT,
     -      AMPDIP,PHIDIP,PHI2(MXWIRE),PHIT2(MXWIRE),AMPT2(MXWIRE),
     -      EPSA,EPSP
       LOGICAL REITER
*** Return code.
       IFAIL=1
*** Parameters.
       RMULT=1
       NITMAX=10
       EPSP=1.0E-3
       EPSA=1.0E-3
*** Initial dipole moments
       DO 20 IW=1,NWIRE
       PHI2(IW)=0.0
       COSPH2(IW)=COS(PHI2(IW))
       SINPH2(IW)=SIN(PHI2(IW))
       AMP2(IW)=0.0
20     CONTINUE
*** Iterate until the dipole terms have converged
       DO 30 ITER=1,NITMAX
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SETDIP DEBUG   :'',
     -      '' Iteration '',I3,''/'',I3)') ITER,NITMAX
*** Loop over the wires
       DO 10 IW=1,NWIRE
*   Set the radius of the wire to 0.
       DRES=D(IW)
       D(IW)=0.0
       DO 50 I=1,N
*   Loop around the wire.
       ANGLE(I)=2*PI*REAL(I)/REAL(N)
       CALL EFIELD(
     -      REAL(X(IW)+0.5*RMULT*DRES*COS(ANGLE(I))),
     -      REAL(Y(IW)+0.5*RMULT*DRES*SIN(ANGLE(I))),
     -      0.0,
     -      EX,EY,EZ,ETOT,VOLT(I),1,ILOC)
       IF(ILOC.NE.0)THEN
            PRINT *,' !!!!!! SETDIP WARNING : Unexpected'//
     -           ' location code received from EFIELD ;'//
     -           ' computation stopped.'
            D(IW)=DRES
            GOTO 10
       ENDIF
       VOLT(I)=VOLT(I)-V(IW)
50     CONTINUE
*   Restore wire diameter
       D(IW)=DRES
*   Determine the dipole term
       CALL DIPFIT(ANGLE,VOLT,N,AMPDIP,PHIDIP,IFAIL)
*   Store the parameters, removing the radial dependence.
       PHIT2(IW)=PHIDIP
       AMPT2(IW)=AMPDIP*(0.5*RMULT*DRES)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SETDIP DEBUG   :'',
     -      '' Wire '',I3,'': correction angle = '',F10.3,
     -      '' degrees, amplitude = '',E12.5)')
     -      IW,180*PHIT2(IW)/PI,AMPT2(IW)
10     CONTINUE
*** Transfer to the arrays where the dipole moments have impact
       REITER=.FALSE.
       DO 40 IW=1,NWIRE
*   See whether we need further refinements
       IF(ABS(PHI2(IW)).GT.EPSP*(1+ABS(PHI2(IW))).OR.
     -      ABS(AMP2(IW)).GT.EPSA*(1+ABS(AMP2(IW))))THEN
            REITER=.TRUE.
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SETDIP DEBUG   :'',
     -           '' Convergence on wire '',I3,'' achieved.'')') IW
       ENDIF
*   Add the new term to the existing one.
       PHI2(IW)=ATAN2(
     -      SINPH2(IW)*AMP2(IW)+SIN(PHIT2(IW))*AMPT2(IW),
     -      COSPH2(IW)*AMP2(IW)+COS(PHIT2(IW))*AMPT2(IW))
       COSPH2(IW)=COS(PHI2(IW))
       SINPH2(IW)=SIN(PHI2(IW))
       AMP2(IW)=SQRT(
     -      (SINPH2(IW)*AMP2(IW)+SIN(PHIT2(IW))*AMPT2(IW))**2+
     -      (COSPH2(IW)*AMP2(IW)+COS(PHIT2(IW))*AMPT2(IW))**2)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SETDIP DEBUG   :'',
     -      '' Wire '',I3,'': new angle = '',F10.3,
     -      '' degrees, amplitude = '',E12.5)')
     -      IW,180*PHI2(IW)/PI,AMP2(IW)
40     CONTINUE
*** Next iteration ?
       IF(.NOT.REITER)THEN
            IFAIL=0
            RETURN
       ENDIF
30     CONTINUE
*** Maximum number of iterations exceeded.
       PRINT *,' !!!!!! SETDIP WARNING : Maximum number of dipole'//
     -      ' iterations exceeded without convergence; abandoned.'
       IFAIL=1
       END
CDECK  ID>, DIPFIT.
       SUBROUTINE DIPFIT(ANGLE,VOLT,N,AMPDIP,PHIDIP,IFAIL)
*-----------------------------------------------------------------------
*   DIPFIT - Determines the dipole moment of a wire.
*   (Last changed on 26/10/07.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,N,NTRY,ITRY,I,J,NITMAX
       PARAMETER(NTRY=100)
       REAL ANGLE(N),VOLT(N),AMPDIP,PHIDIP,X1,X2,X3,F1,F2,F3,
     -      XPARA,FPARA,PHITRY(NTRY),SUMTRY(NTRY),PHIMAX,SUMMAX,
     -      EPS,EPSF,EPSX,DET
*** Initial values.
       PHIDIP=0.0
       AMPDIP=0.0
       IFAIL=1
*** Initial search for a maximum
       SUMMAX=0
       DO 10 ITRY=1,NTRY
*   Make the internal product with a shifted cosine
       PHITRY(ITRY)=REAL(ITRY-1)*2.0*PI/REAL(NTRY)
       SUMTRY(ITRY)=0
       DO 20 J=1,N
       SUMTRY(ITRY)=SUMTRY(ITRY)+VOLT(J)*COS(PHITRY(ITRY)-ANGLE(J))
20     CONTINUE
       SUMTRY(ITRY)=SUMTRY(ITRY)*2.0/REAL(N)
*   See whether this one beats earlier
       IF(SUMTRY(ITRY).GT.SUMMAX)THEN
            PHIMAX=PHITRY(ITRY)
            SUMMAX=SUMTRY(ITRY)
       ENDIF
10     CONTINUE
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DIPFIT DEBUG   :'',
     -           '' Maximum of scan at phi = '',E12.5,
     -           '', product = '',E12.5)') PHIMAX,SUMMAX
       ENDIF
       PHIDIP=PHIMAX
       AMPDIP=SUMMAX
*** Scan in the neighbourbood
       EPS=0.1
       X1=PHIMAX-EPS
       X2=PHIMAX
       X3=PHIMAX+EPS
       F1=0
       F2=SUMMAX
       F3=0
       DO 30 J=1,N
       F1=F1+VOLT(J)*COS(X1-ANGLE(J))
       F3=F3+VOLT(J)*COS(X3-ANGLE(J))
30     CONTINUE
       F1=F1*2.0/REAL(N)
       F3=F3*2.0/REAL(N)
*** Refine the estimate by parabolic extremum search.
       NITMAX=10
       EPSF=1E-3*SUMMAX
       EPSX=1E-3*2*PI
       DO 40 I=1,NITMAX
*   Estimate parabolic extremum.
       DET=(F1-F2)*X3 + (F3-F1)*X2 + (F2-F3)*X1
       IF(ABS(DET).LE.0)THEN
            PRINT *,' ++++++ DIPFIT WARNING : Determinant = 0;'//
     -           ' parabolic search stopped.'
            PHIDIP=X2
            AMPDIP=F2
            IFAIL=1
            RETURN
       ENDIF
       XPARA=((F1-F2)*X3**2+(F3-F1)*X2**2+(F2-F3)*X1**2)/(2*DET)
       FPARA=0
       DO 50 J=1,N
       FPARA=FPARA+VOLT(J)*COS(XPARA-ANGLE(J))
50     CONTINUE
       FPARA=FPARA*2.0/REAL(N)
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DIPFIT DEBUG   :'',
     -      '' Start of iteration '',I3//
     -      26X,''Point 1:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 2:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 3:  x='',E15.8,'' f='',E15.8//
     -      26X,''Parabola: x='',E15.8,'' f='',E15.8)')
     -      I,X1,F1,X2,F2,X3,F3,XPARA,FPARA
*   Check that the new estimate doesn't coincide with an old point.
       IF(ABS(XPARA-X1).LT.EPSX*(EPSX+ABS(XPARA)).OR.
     -      ABS(XPARA-X2).LT.EPSX*(EPSX+ABS(XPARA)).OR.
     -      ABS(XPARA-X3).LT.EPSX*(EPSX+ABS(XPARA)))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Location convergence'',
     -           '' criterion satisfied.''/)')
            PHIDIP=XPARA
            AMPDIP=FPARA
            IFAIL=0
            RETURN
       ENDIF
*   Check convergence.
       IF(ABS(FPARA-F1).LT.EPSF*(ABS(FPARA)+ABS(F1)+EPSF))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Function value convergence'',
     -           '' criterion satisfied.''/)')
            PHIDIP=XPARA
            AMPDIP=FPARA
            IFAIL=0
            RETURN
       ENDIF
*   Store the value in the table.
       IF(FPARA.GT.F1)THEN
            F3=F2
            X3=X2
            F2=F1
            X2=X1
            F1=FPARA
            X1=XPARA
       ELSEIF(FPARA.GT.F2)THEN
            F3=F2
            X3=X2
            F2=FPARA
            X2=XPARA
       ELSEIF(FPARA.GT.F3)THEN
            F3=FPARA
            X3=XPARA
       ELSE
            PRINT *,' !!!!!! DIPFIT WARNING : Parabolic extremum'//
     -           ' is worse than current optimum; search stopped.'
            WRITE(*,'(
     -      26X,''Point 1:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 2:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 3:  x='',E15.8,'' f='',E15.8//
     -      26X,''Parabola: x='',E15.8,'' f='',E15.8)')
     -      X1,F1,X2,F2,X3,F3,XPARA,FPARA
            PHIDIP=X2
            AMPDIP=F2
            IFAIL=1
            RETURN
       ENDIF
40     CONTINUE
*** No convergence.
       PRINT *,' !!!!!! DIPFIT WARNING : No convergence after maximum'//
     -      ' number of steps.'
       PRINT *,'                         Current extremum f=',F2
       PRINT *,'                         Found for        x=',X2
       PHIDIP=X2
       AMPDIP=F2
       IFAIL=1
       END
CDECK  ID>, EFIELD.
       SUBROUTINE EFIELD(XIN,YIN,ZIN,EX,EY,EZ,ETOT,VOLT,IOPT,ILOC)
*-----------------------------------------------------------------------
*   EFIELD - Subroutine calculating the electric field and the potential
*            at a given place. It makes use of the routines POT...,
*            depending on the type of the cell.
*   VARIABLES : XPOS       : x-coordinate of the place where the field
*                            is to be calculated.
*               YPOS, ZPOS : y- and z-coordinates
*               EX, EY, EZ : x-, y-, z-component of the electric field.
*               VOLT       : potential at (XPOS,YPOS).
*               IOPT       : 1 if both E and V are required, 0 if only E
*                            is to be computed.
*               ILOC       : Tells where the point is located (0: normal
*                            I > 0: in wire I, -1: outside a plane,
*                            -5: in a material, -6: outside the mesh, 
*                            -10: unknown potential).
*   (Last changed on 28/ 9/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL XIN,YIN,ZIN,EX,EY,EZ,ETOT,VOLT,XPOS,YPOS,ZPOS,DXWIR,DYWIR,
     -      AROT,EX3D,EY3D,EZ3D,V3D,XAUX,YAUX,
     -      EXD,EYD,VOLTD
       INTEGER IOUT,ILOC,IOPT,I
*** Initialise the field for returns without actual calculations.
       EX=0.0
       EY=0.0
       EZ=0.0
       ETOT=0.0
       VOLT=0.0
       ILOC=0
*** In case of periodicity, move the point into the basic cell.
       IF(ICTYPE.NE.0)THEN
            IF(PERX)THEN
                 XPOS=XIN-SX*ANINT(XIN/SX)
            ELSE
                 XPOS=XIN
            ENDIF
            IF(PERY.AND.TUBE)THEN
                 CALL CFMCTP(XIN,YIN,XPOS,YPOS,1)
                 AROT=180*SY*ANINT((PI*YPOS)/(SY*180.0))/PI
                 YPOS=YPOS-AROT
                 CALL CFMPTC(XPOS,YPOS,XPOS,YPOS,1)
            ELSEIF(PERY)THEN
                 YPOS=YIN-SY*ANINT(YIN/SY)
            ELSE
                 YPOS=YIN
            ENDIF
*** Move the point to the correct side of the plane.
            IF(PERX.AND.YNPLAN(1).AND.XPOS.LE.COPLAN(1))XPOS=XPOS+SX
            IF(PERX.AND.YNPLAN(2).AND.XPOS.GE.COPLAN(2))XPOS=XPOS-SX
            IF(PERY.AND.YNPLAN(3).AND.YPOS.LE.COPLAN(3))YPOS=YPOS+SY
            IF(PERY.AND.YNPLAN(4).AND.YPOS.GE.COPLAN(4))YPOS=YPOS-SY
*** In case (XPOS,YPOS) is located behind a plane there is no field.
            IOUT=0
            IF(TUBE)THEN
                 CALL INTUBE(XPOS,YPOS,COTUBE,NTUBE,IOUT)
c                 write(*,*) 'in a tube ', iout
                 IF(IOUT.NE.0)VOLT=VTTUBE
            ELSE
                 IF(YNPLAN(1).AND.XPOS.LT.COPLAN(1))IOUT=1
                 IF(YNPLAN(2).AND.XPOS.GT.COPLAN(2))IOUT=2
                 IF(YNPLAN(3).AND.YPOS.LT.COPLAN(3))IOUT=3
                 IF(YNPLAN(4).AND.YPOS.GT.COPLAN(4))IOUT=4
                 IF(IOUT.EQ.1)VOLT=VTPLAN(1)
                 IF(IOUT.EQ.2)VOLT=VTPLAN(2)
                 IF(IOUT.EQ.3)VOLT=VTPLAN(3)
                 IF(IOUT.EQ.4)VOLT=VTPLAN(4)
            ENDIF
            IF(IOUT.NE.0)THEN
                 ILOC=-4
                 RETURN
            ENDIF
*** If (XPOS,YPOS) is within a wire, there is no field either.
            DO 10 I=1,NWIRE
*   Correct for x-periodicity.
            IF(PERX)THEN
                 DXWIR=(XPOS-X(I))-SX*ANINT((XPOS-X(I))/SX)
            ELSE
                 DXWIR=XPOS-X(I)
            ENDIF
*   Correct for y-periodicity.
            IF(PERY.AND..NOT.TUBE)THEN
                 DYWIR=(YPOS-Y(I))-SY*ANINT((YPOS-Y(I))/SY)
            ELSE
                 DYWIR=YPOS-Y(I)
            ENDIF
*   Check the actual position.
            IF(DXWIR**2+DYWIR**2.LT.0.25*D(I)**2)THEN
                 VOLT=V(I)
                 ILOC=I
                 RETURN
            ENDIF
*   Next wire.
10          CONTINUE
       ELSE
            XPOS=XIN
            YPOS=YIN
            ZPOS=ZIN
       ENDIF
*** Call the appropriate potential calculation function.
       IF(ICTYPE.EQ.0)THEN
          write(*,*) 'Unknown potential type.'
          IF(ILOC.NE.0.AND.ILOC.NE.-5)RETURN
       ELSEIF(ICTYPE.EQ.1.AND.NXMATT.EQ.0.AND.NYMATT.EQ.0)THEN
            CALL EFCA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
         ELSEIF(ICTYPE.EQ.1)THEN
            CALL EFDA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.2)THEN
            CALL EFCB1X(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.3)THEN
            CALL EFCB1Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.4)THEN
            CALL EFCB2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.5)THEN
            CALL EFCB2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.6)THEN
            CALL EFCC10(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.7)THEN
            CALL EFCC2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.8)THEN
            CALL EFCC2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.9)THEN
            CALL EFCC30(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.10)THEN
            CALL EFCD10(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.11)THEN
            CALL EFCD20(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.12)THEN
            CALL EFCD30(XPOS,YPOS,EX,EY,VOLT,IOPT)
C       ELSEIF(ICTYPE.EQ.13)THEN
C           CALL EFCD40(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSE
            ILOC=-10
            RETURN
       ENDIF
*** Add dipole terms if requested
       IF(LDIPOL)THEN
            IF(ICTYPE.EQ.1)THEN
                 CALL EMCA00(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.2)THEN
                 CALL EMCB1X(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.3)THEN
                 CALL EMCB1Y(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.4)THEN
                 CALL EMCB2X(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.5)THEN
                 CALL EMCB2Y(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSE
                 EXD=0
                 EYD=0
                 VOLTD=0
            ENDIF
            EX=EX+EXD
            EY=EY+EYD
            VOLT=VOLT+VOLTD
       ENDIF
*** Rotate the field in some special cases.
       IF(ICTYPE.NE.0)THEN
            IF(PERY.AND.TUBE)THEN
                 CALL CFMCTP(EX,EY,XAUX,YAUX,1)
                 YAUX=YAUX+AROT
                 CALL CFMPTC(XAUX,YAUX,EX,EY,1)
            ENDIF
*** Correct for the equipotential planes.
            EX=EX-CORVTA
            EY=EY-CORVTB
            VOLT=VOLT+CORVTA*XPOS+CORVTB*YPOS+CORVTC
       ENDIF
*** Add three dimensional point charges.
       IF(N3D.GT.0)THEN
            IF(ICTYPE.EQ.1.OR.ICTYPE.EQ.2.OR.ICTYPE.EQ.3)THEN
                 CALL E3DA00(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSEIF(ICTYPE.EQ.4)THEN
                 CALL E3DB2X(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSEIF(ICTYPE.EQ.5)THEN
                 CALL E3DB2Y(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSEIF(ICTYPE.EQ.10)THEN
                 CALL E3DD10(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSE
                 EX3D=0.0
                 EY3D=0.0
                 EZ3D=0.0
                 V3D=0.0
                 CALL E3DA00(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ENDIF
            EX=EX+EX3D
            EY=EY+EY3D
            EZ=EZ+EZ3D
            VOLT=VOLT+V3D
       ENDIF
*** Add a background field if present.
*       IF(IENBGF.GT.0)THEN
*          CALL EFCBGF(XIN,YIN,ZIN,EXBGF,EYBGF,EZBGF,VBGF)
*          EX=EX+EXBGF
*          EY=EY+EYBGF
*          EZ=EZ+EZBGF
*          VOLT=VOLT+VBGF
*       ENDIF
*** Finally calculate the value of ETOT (magnitude of the E-field).
       ETOT=SQRT(EX**2+EY**2+EZ**2)
       END
CDECK  ID>, EFCA00SC.
       SUBROUTINE EFCA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCA00 - Subroutine performing the actual field calculations in case
*            only one charge and not more than 1 mirror-charge in either
*            x or y is present.
*            The potential used is 1/2*pi*eps0  log(r).
*   VARIABLES : R2         : Potential before taking -log(sqrt(...))
*               EX, EY     : x,y-component of the electric field.
*               ETOT       : Magnitude of electric field.
*               VOLT       : Potential.
*               EXHELP etc : One term in the series to be summed.
*               (XPOS,YPOS): The position where the field is calculated.
*   (Last changed on 25/ 1/96.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
*** Calculate the field in case there are no planes.
       R2=(XPOS-X(I))**2+(YPOS-Y(I))**2
       EXHELP=(XPOS-X(I))/R2
       EYHELP=(YPOS-Y(I))/R2
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=X(I)+(XPOS-2.0*COPLAX)
            R2PLAN=XXMIRR**2+(YPOS-Y(I))**2
            EXHELP=EXHELP-XXMIRR/R2PLAN
            EYHELP=EYHELP-(YPOS-Y(I))/R2PLAN
            R2=R2/R2PLAN
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            YYMIRR=Y(I)+(YPOS-2.0*COPLAY)
            R2PLAN=(XPOS-X(I))**2+YYMIRR**2
            EXHELP=EXHELP-(XPOS-X(I))/R2PLAN
            EYHELP=EYHELP-YYMIRR/R2PLAN
            R2=R2/R2PLAN
       ENDIF
*** Take care of pairs of planes.
       IF(YNPLAX.AND.YNPLAY)THEN
            R2PLAN=XXMIRR**2+YYMIRR**2
            EXHELP=EXHELP+XXMIRR/R2PLAN
            EYHELP=EYHELP+YYMIRR/R2PLAN
            R2=R2*R2PLAN
       ENDIF
*** Calculate the electric field and the potential.
       IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*LOG(R2)
       EX=EX+E(I)*EXHELP
       EY=EY+E(I)*EYHELP
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, E3DA00.
       SUBROUTINE E3DA00(XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT)
*-----------------------------------------------------------------------
*   E3DA00 - Subroutine adding 3-dimensional charges for A cells.
*            The potential used is 1/2*pi*eps0  1/r
*   VARIABLES : EX, EY     : x,y-component of the electric field.
*               ETOT       : Magnitude of electric field.
*               VOLT       : Potential.
*               EXHELP etc : One term in the series to be summed.
*               (XPOS,YPOS): The position where the field is calculated.
*   (Last changed on  5/12/94.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       EZ=0.0
       VOLT=0.0
*** Loop over all charges.
       DO 10 I=1,N3D
*** Calculate the field in case there are no planes.
       R=SQRT((XPOS-X3D(I))**2+(YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)
       IF(R.EQ.0)GOTO 10
       EXHELP=-(XPOS-X3D(I))/R**3
       EYHELP=-(YPOS-Y3D(I))/R**3
       EZHELP=-(ZPOS-Z3D(I))/R**3
       VHELP =1/R
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=X3D(I)+(XPOS-2*COPLAX)
            RPLAN=SQRT(XXMIRR**2+(YPOS-Y3D(I))**2)
            IF(RPLAN.EQ.0)GOTO 10
            EXHELP=EXHELP+XXMIRR/RPLAN**3
            EYHELP=EYHELP+(YPOS-Y3D(I))/RPLAN**3
            EZHELP=EZHELP+(ZPOS-Z3D(I))/RPLAN**3
            VHELP =VHELP-1/RPLAN
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            YYMIRR=Y3D(I)+(YPOS-2*COPLAY)
            RPLAN=SQRT((XPOS-X3D(I))**2+YYMIRR**2)
            IF(RPLAN.EQ.0)GOTO 10
            EXHELP=EXHELP+(XPOS-X3D(I))/RPLAN**3
            EYHELP=EYHELP+YYMIRR/RPLAN**3
            EZHELP=EZHELP+(ZPOS-Z3D(I))/RPLAN**3
            VHELP =VHELP-1/RPLAN
       ENDIF
*** Take care of pairs of planes.
       IF(YNPLAX.AND.YNPLAY)THEN
            RPLAN=SQRT(XXMIRR**2+YYMIRR**2)
            IF(RPLAN.EQ.0)GOTO 10
            EXHELP=EXHELP-XXMIRR/RPLAN**3
            EYHELP=EYHELP-YYMIRR/RPLAN**3
            EZHELP=EZHELP-(ZPOS-Z3D(I))/RPLAN**3
            VHELP =VHELP+1/RPLAN
       ENDIF
*** Add the terms to the electric field and the potential.
       EX=EX-E3D(I)*EXHELP
       EY=EY-E3D(I)*EYHELP
       EZ=EZ-E3D(I)*EZHELP
       VOLT=VOLT+E3D(I)*VHELP
*** Finish the loop over the charges.
10     CONTINUE
       END
CDECK  ID>, EMCA00.
       SUBROUTINE EMCA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EMCA00 - Subroutine computing dipole terms in a field generated by
*            wires without periodicities.
*   (Last changed on 10/ 9/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       REAL XPOS,YPOS,EX,EY,VOLT,DX,DY,EXHELP,EYHELP,VHELP
       INTEGER IOPT,I
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=0.0
*** Loop over all wires.
       VHELP=0
       DO 10 I=1,NWIRE
*** Calculate the field in case there are no planes.
       DX=XPOS-X(I)
       DY=YPOS-Y(I)
       EXHELP=((DX**2-DY**2)*COSPH2(I)+2*DX*DY*SINPH2(I))/
     -      (DX**2+DY**2)**2
       EYHELP=(2*DX*DY*COSPH2(I)-(DX**2-DY**2)*SINPH2(I))/
     -      (DX**2+DY**2)**2
       IF(IOPT.NE.0)VHELP=(DX*COSPH2(I)+DY*SINPH2(I))/(DX**2+DY**2)
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            DX=X(I)+(XPOS-2.0*COPLAX)
            DY=YPOS-Y(I)
            EXHELP=EXHELP-((DX**2-DY**2)*COSPH2(I)+2*DX*DY*SINPH2(I))/
     -           (DX**2+DY**2)**2
            EYHELP=EYHELP-(2*DX*DY*COSPH2(I)-(DX**2-DY**2)*SINPH2(I))/
     -           (DX**2+DY**2)**2
            IF(IOPT.NE.0)VHELP=VHELP-(DX*COSPH2(I)+DY*SINPH2(I))/
     -           (DX**2+DY**2)
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            DX=XPOS-X(I)
            DY=Y(I)+(YPOS-2.0*COPLAY)
            EXHELP=EXHELP-((DX**2-DY**2)*COSPH2(I)+2*DX*DY*SINPH2(I))/
     -           (DX**2+DY**2)**2
            EYHELP=EYHELP-(2*DX*DY*COSPH2(I)-(DX**2-DY**2)*SINPH2(I))/
     -           (DX**2+DY**2)**2
            IF(IOPT.NE.0)VHELP=VHELP-(DX*COSPH2(I)+DY*SINPH2(I))/
     -           (DX**2+DY**2)
       ENDIF
*** Take care of pairs of planes.
       IF(YNPLAX.AND.YNPLAY)THEN
            DX=X(I)+(XPOS-2.0*COPLAX)
            DY=Y(I)+(YPOS-2.0*COPLAY)
            EXHELP=EXHELP+((DX**2-DY**2)*COSPH2(I)+2*DX*DY*SINPH2(I))/
     -           (DX**2+DY**2)**2
            EYHELP=EYHELP+(2*DX*DY*COSPH2(I)-(DX**2-DY**2)*SINPH2(I))/
     -           (DX**2+DY**2)**2
            IF(IOPT.NE.0)VHELP=VHELP+(DX*COSPH2(I)+DY*SINPH2(I))/
     -           (DX**2+DY**2)
       ENDIF
*** Normalise
       VOLT=VOLT-AMP2(I)*VHELP
       EX=EX-AMP2(I)*EXHELP
       EY=EY-AMP2(I)*EYHELP
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, EFCB1XSC.
       SUBROUTINE EFCB1X(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCB1X - Routine calculating the potential for a row of positive
*            charges. The potential used is Re(Log(sin pi/s (z-z0))).
*   VARIABLES : See routine EFCA00 for most of the variables.
*               Z,ZZMIRR   : X + I*Y , XXMIRR + I*YYMIRR ; I**2=-1
*               ECOMPL     : EX + I*EY                   ; I**2=-1
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX ZZ,ECOMPL,ZZMIRR
*** Initialise EX, EY and VOLT.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
       XX=(PI/SX)*(XPOS-X(I))
       YY=(PI/SX)*(YPOS-Y(I))
       ZZ=CMPLX(XX,YY)
*** Calculate the field in case there are no equipotential planes.
       IF(    YY.GT.+20.0)ECOMPL=-ICONS
       IF(ABS(YY).LE.20.0)ECOMPL=
     -      ICONS*(EXP(2.0*ICONS*ZZ)+1.0)/(EXP(2.0*ICONS*ZZ)-1.0)
       IF(    YY.LT.-20.0)ECOMPL=+ICONS
       IF(IOPT.NE.0)THEN
            IF(ABS(YY).GT.20.0)R2=-ABS(YY)+CLOG2
            IF(ABS(YY).LE.20.0)R2=-0.5*LOG(SINH(YY)**2+SIN(XX)**2)
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            YYMIRR=(PI/SX)*(YPOS+Y(I)-2.0*COPLAY)
            ZZMIRR=CMPLX(XX,YYMIRR)
            IF(    YYMIRR.GT.+20.0)ECOMPL=ECOMPL+ICONS
            IF(ABS(YYMIRR).LE.20.0)ECOMPL=ECOMPL-ICONS*
     -           (EXP(2.0*ICONS*ZZMIRR)+1.0)/(EXP(2.0*ICONS*ZZMIRR)-1.0)
            IF(    YYMIRR.LT.-20.0)ECOMPL=ECOMPL-ICONS
            IF(IOPT.NE.0.AND.ABS(YYMIRR).GT.20.0)
     -           R2=R2+ABS(YYMIRR)-CLOG2
            IF(IOPT.NE.0.AND.ABS(YYMIRR).LE.20.0)
     -           R2=R2+0.5*LOG(SINH(YYMIRR)**2+SIN(XX)**2)
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX+E(I)*(PI/SX)*REAL(ECOMPL)
       EY=EY-E(I)*(PI/SX)*AIMAG(ECOMPL)
       IF(IOPT.NE.0)VOLT=VOLT+E(I)*R2
*** Finish loop over all wires.
10     CONTINUE
       END
CDECK  ID>, EMCB1X.
       SUBROUTINE EMCB1X(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EMCB1X - Subroutine computing dipole terms in a field generated by
*            a row of wires along the x-axis.
*   (Last changed on 24/ 9/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL XPOS,YPOS,EX,EY,VOLT,DX,DY,EXHELP,EYHELP,VHELP
       INTEGER IOPT,I
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=0.0
*** Loop over all wires.
       VHELP=0
       DO 10 I=1,NWIRE
*** Calculate the field in case there are no planes.
       DX=(PI/SX)*(XPOS-X(I))
       DY=(PI/SX)*(YPOS-Y(I))
       EXHELP=(COSPH2(I)*(1-COS(2*DX)*COSH(2*DY))+
     -      SINPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -      (SIN(DX)**2+SINH(DY)**2)**2
       EYHELP=(-SINPH2(I)*(1-COS(2*DX)*COSH(2*DY))+
     -      COSPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -      (SIN(DX)**2+SINH(DY)**2)**2
       IF(IOPT.NE.0)VHELP=(COSPH2(I)*SIN(2*DX)+SINPH2(I)*SINH(2*DY))/
     -      (SIN(DX)**2+SINH(DY)**2)
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            DY=(PI/SX)*(YPOS+Y(I)-2.0*COPLAY)
            EXHELP=EXHELP-(COSPH2(I)*(1-COS(2*DX)*COSH(2*DY))-
     -           SINPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -           (SIN(DX)**2+SINH(DY)**2)**2
            EYHELP=EYHELP-(SINPH2(I)*(1-COS(2*DX)*COSH(2*DY))+
     -           COSPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -           (SIN(DX)**2+SINH(DY)**2)**2
            IF(IOPT.NE.0)VHELP=VHELP-
     -           (COSPH2(I)*SIN(2*DX)-SINPH2(I)*SINH(2*DY))/
     -           (SIN(DX)**2+SINH(DY)**2)
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX-AMP2(I)*0.5*(PI/SX)**2*EXHELP
       EY=EY-AMP2(I)*0.5*(PI/SX)**2*EYHELP
       IF(IOPT.NE.0)VOLT=VOLT-0.5*(PI/SX)*AMP2(I)*VHELP
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, EFCB1YSC.
       SUBROUTINE EFCB1Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCB1Y - Routine calculating the potential for a row of positive
*            charges. The potential used is Re(Log(sinh pi/sy(z-z0)).
*   VARIABLES : See routine EFCA00 for most of the variables.
*               Z,ZZMIRR   : X + I*Y , XXMIRR + I*YYMIRR ; I**2=-1
*               ECOMPL     : EX + I*EY                   ; I**2=-1
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX ZZ,ECOMPL,ZZMIRR
*** Initialise EX, EY and VOLT.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
       XX=(PI/SY)*(XPOS-X(I))
       YY=(PI/SY)*(YPOS-Y(I))
       ZZ=CMPLX(XX,YY)
*** Calculate the field in case there are no equipotential planes.
       IF(    XX.GT.+20.0)ECOMPL=+1.0
       IF(ABS(XX).LE.20.0)ECOMPL=(EXP(2.0*ZZ)+1.0)/(EXP(2.0*ZZ)-1.0)
       IF(    XX.LT.-20.0)ECOMPL=-1.0
       IF(IOPT.NE.0)THEN
            IF(ABS(XX).GT.20.0)R2=-ABS(XX)+CLOG2
            IF(ABS(XX).LE.20.0)R2=-0.5*LOG(SINH(XX)**2+SIN(YY)**2)
       ENDIF
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=(PI/SY)*(XPOS+X(I)-2.0*COPLAX)
            ZZMIRR=CMPLX(XXMIRR,YY)
            IF(XXMIRR.GT.+20.0)ECOMPL=ECOMPL-1.0
            IF(XXMIRR.LT.-20.0)ECOMPL=ECOMPL+1.0
            IF(ABS(XXMIRR).LE.20.0)ECOMPL=ECOMPL-
     -           (EXP(2.0*ZZMIRR)+1.0)/(EXP(2.0*ZZMIRR)-1.0)
            IF(IOPT.NE.0.AND.ABS(XXMIRR).GT.20.0)
     -           R2=R2+ABS(XXMIRR)-CLOG2
            IF(IOPT.NE.0.AND.ABS(XXMIRR).LE.20.0)
     -           R2=R2+0.5*LOG(SINH(XXMIRR)**2+SIN(YY)**2)
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX+E(I)*(PI/SY)*REAL(ECOMPL)
       EY=EY-E(I)*(PI/SY)*AIMAG(ECOMPL)
       IF(IOPT.NE.0)VOLT=VOLT+E(I)*R2
*** Finish loop over the wires.
10     CONTINUE
       END
CDECK  ID>, EMCB1Y.
       SUBROUTINE EMCB1Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EMCB1Y - Subroutine computing dipole terms in a field generated by
*            a row of wires along the y-axis.
*   (Last changed on 24/ 9/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL XPOS,YPOS,EX,EY,VOLT,DX,DY,EXHELP,EYHELP,VHELP
       INTEGER IOPT,I
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=0.0
*** Loop over all wires.
       VHELP=0
       DO 10 I=1,NWIRE
*** Calculate the field in case there are no planes.
       DX=(PI/SY)*(XPOS-X(I))
       DY=(PI/SY)*(YPOS-Y(I))
       EXHELP=(-COSPH2(I)*(1-COSH(2*DX)*COS(2*DY))+
     -      SINPH2(I)*SINH(2*DX)*SIN(2*DY))/
     -      (SINH(DX)**2+SIN(DY)**2)**2
       EYHELP=( SINPH2(I)*(1-COSH(2*DX)*COS(2*DY))+
     -      COSPH2(I)*SINH(2*DX)*SIN(2*DY))/
     -      (SINH(DX)**2+SIN(DY)**2)**2
       IF(IOPT.NE.0)VHELP=(COSPH2(I)*SINH(2*DX)+SINPH2(I)*SIN(2*DY))/
     -      (SINH(DX)**2+SIN(DY)**2)
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            DX=(PI/SY)*(XPOS+X(I)-2.0*COPLAX)
            EXHELP=EXHELP-(COSPH2(I)*(1-COSH(2*DX)*COS(2*DY))+
     -           SINPH2(I)*SINH(2*DX)*SIN(2*DY))/
     -           (SINH(DX)**2+SIN(DY)**2)**2
            EYHELP=EYHELP-(SINPH2(I)*(1-COSH(2*DX)*COS(2*DY))-
     -           COSPH2(I)*SINH(2*DX)*SIN(2*DY))/
     -           (SINH(DX)**2+SIN(DY)**2)**2
            IF(IOPT.NE.0)VHELP=VHELP-
     -           (-COSPH2(I)*SINH(2*DX)+SINPH2(I)*SIN(2*DY))/
     -           (SINH(DX)**2+SIN(DY)**2)
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX-AMP2(I)*0.5*(PI/SY)**2*EXHELP
       EY=EY-AMP2(I)*0.5*(PI/SY)**2*EYHELP
       IF(IOPT.NE.0)VOLT=VOLT-0.5*(PI/SY)*AMP2(I)*VHELP
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, EFCB2X.
       SUBROUTINE EFCB2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCB2X - Routine calculating the potential for a row of alternating
*            + - charges. The potential used is re log(sin pi/sx (z-z0))
*   VARIABLES : See routine EFCA00 for most of the variables.
*               Z, ZZMRR   : X + i*Y , XXMIRR + i*YYMIRR ; i**2=-1
*               ECOMPL     : EX + i*EY                   ; i**2=-1
*   (Cray vectorisable)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX ZZ,ECOMPL,ZZMIRR,ZZNEG,ZZNMIR
*** Initialise EX, EY and VOLT.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
       XX=(0.5*PI/SX)*(XPOS-X(I))
       YY=(0.5*PI/SX)*(YPOS-Y(I))
       XXNEG=(0.5*PI/SX)*(XPOS+X(I)-2.0*COPLAX)
       ZZ=CMPLX(XX,YY)
       ZZNEG=CMPLX(XXNEG,YY)
*** Calculate the field in case there are no equipotential planes.
       ECOMPL=0.0
       R2=1.0
       IF(ABS(YY).LE.20)ECOMPL=-B2SIN(I)/(SIN(ZZ)*SIN(ZZNEG))
       IF(IOPT.NE.0.AND.ABS(YY).LE.20.0)
     -      R2=(SINH(YY)**2+SIN(XX)**2)/(SINH(YY)**2+SIN(XXNEG)**2)
*** Take care of a planes at constant y.
       IF(YNPLAY)THEN
            YYMIRR=(0.5*PI/SX)*(YPOS+Y(I)-2.0*COPLAY)
            ZZMIRR=CMPLX(XX,YYMIRR)
            ZZNMIR=CMPLX(XXNEG,YYMIRR)
            IF(ABS(YYMIRR).LE.20.0)
     -           ECOMPL=ECOMPL+B2SIN(I)/(SIN(ZZMIRR)*SIN(ZZNMIR))
            IF(IOPT.NE.0.AND.ABS(YYMIRR).LE.20.0)THEN
                 R2PLAN=(SINH(YYMIRR)**2+SIN(XX)**2)/
     -                  (SINH(YYMIRR)**2+SIN(XXNEG)**2)
                 R2=R2/R2PLAN
            ENDIF
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX+E(I)*(0.5*PI/SX)*REAL(ECOMPL)
       EY=EY-E(I)*(0.5*PI/SX)*AIMAG(ECOMPL)
       IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*LOG(R2)
*** Finish the wire loop.
10     CONTINUE
       END
CDECK  ID>, E3DB2X.
       SUBROUTINE E3DB2X(XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT)
*-----------------------------------------------------------------------
*   E3DB2X - Routine calculating the potential for a 3 dimensional point
*            charge between two plates at constant x.
*            The series expansions for the modified Bessel functions
*            have been taken from Abramowitz and Stegun.
*   VARIABLES : See routine E3DA00 for most of the variables.
*   (Last changed on  5/12/94.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION EXSUM,EYSUM,EZSUM,VSUM,
     -      I0S,I1S,K0S,K0L,K1S,K1L,K0R,K1R,K0RM,K1RM,
     -      XX,RR,RRM,ZZP,ZZN,RR1,RR2,RM1,RM2,ERR,EZZ
       REAL XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT,RCUT
       PARAMETER(RCUT=1.0)
*** Statement functions for the modified Bessel functions:
       I0S(XX)=1
     -      +3.5156229*(XX/3.75)**2
     -      +3.0899424*(XX/3.75)**4
     -      +1.2067492*(XX/3.75)**6
     -      +0.2659732*(XX/3.75)**8
     -      +0.0360768*(XX/3.75)**10
     -      +0.0045813*(XX/3.75)**12
       I1S(XX)=XX*(
     -      +0.5
     -      +0.87890594*(XX/3.75)**2
     -      +0.51498869*(XX/3.75)**4
     -      +0.15084934*(XX/3.75)**6
     -      +0.02658733*(XX/3.75)**8
     -      +0.00301532*(XX/3.75)**10
     -      +0.00032411*(XX/3.75)**12)
       K0S(XX)=-LOG(XX/2)*I0S(XX)
     -      -0.57721566
     -      +0.42278420*(XX/2)**2
     -      +0.23069756*(XX/2)**4
     -      +0.03488590*(XX/2)**6
     -      +0.00262698*(XX/2)**8
     -      +0.00010750*(XX/2)**10
     -      +0.00000740*(XX/2)**12
       K0L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      -0.07832358*(2/XX)
     -      +0.02189568*(2/XX)**2
     -      -0.01062446*(2/XX)**3
     -      +0.00587872*(2/XX)**4
     -      -0.00251540*(2/XX)**5
     -      +0.00053208*(2/XX)**6)
       K1S(XX)=LOG(XX/2)*I1S(XX)+(1/XX)*(
     -      +1
     -      +0.15443144*(XX/2)**2
     -      -0.67278579*(XX/2)**4
     -      -0.18156897*(XX/2)**6
     -      -0.01919402*(XX/2)**8
     -      -0.00110404*(XX/2)**10
     -      -0.00004686*(XX/2)**12)
       K1L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      +0.23498619*(2/XX)
     -      -0.03655620*(2/XX)**2
     -      +0.01504268*(2/XX)**3
     -      -0.00780353*(2/XX)**4
     -      +0.00325614*(2/XX)**5
     -      -0.00068245*(2/XX)**6)
*** Initialise the sums for the field components.
       EX=0.0
       EY=0.0
       EZ=0.0
       VOLT=0.0
*** Loop over all wires.
       DO 10 I=1,N3D
*   Skip wires that are on the charge.
       IF(XPOS.EQ.X3D(I).AND.YPOS.EQ.Y3D(I).AND.ZPOS.EQ.Z3D(I))GOTO 10
*** In the far away zone, sum the modified Bessel function series.
       IF((YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2.GT.(RCUT*2*SX)**2)THEN
*   Initialise the per-wire sum.
            EXSUM=0.0
            EYSUM=0.0
            EZSUM=0.0
            VSUM=0.0
*   Loop over the terms in the series.
            DO 20 J=1,NTERMB
*   Obtain reduced coordinates.
            RR=PI*J*SQRT((YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)/SX
            ZZP=PI*J*(XPOS-X3D(I))/SX
            ZZN=PI*J*(XPOS+X3D(I)-2*COPLAX)/SX
*   Evaluate the Bessel functions for this R.
            IF(RR.LT.2)THEN
                 K0R=K0S(RR)
                 K1R=K1S(RR)
            ELSE
                 K0R=K0L(RR)
                 K1R=K1L(RR)
            ENDIF
*   Get the field components.
            VSUM=VSUM+(1/SX)*K0R*(COS(ZZP)-COS(ZZN))
            ERR=(2*J*PI/SX**2)*K1R*(COS(ZZP)-COS(ZZN))
            EZZ=(2*J*PI/SX**2)*K0R*(SIN(ZZP)-SIN(ZZN))
            EXSUM=EXSUM+EZZ
            EYSUM=EYSUM+ERR*(YPOS-Y3D(I))/
     -           SQRT((YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)
            EZSUM=EZSUM+ERR*(ZPOS-Z3D(I))/
     -           SQRT((YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)
20          CONTINUE
*** Direct polynomial summing, obtain reduced coordinates.
       ELSE
*   Loop over the terms.
            DO 30 J=0,NTERMP
*   Simplify the references to the distances.
            RR1=SQRT((XPOS-X3D(I)+J*2*SX)**2+(YPOS-Y3D(I))**2+
     -           (ZPOS-Z3D(I))**2)
            RR2=SQRT((XPOS-X3D(I)-J*2*SX)**2+(YPOS-Y3D(I))**2+
     -           (ZPOS-Z3D(I))**2)
            RM1=SQRT((XPOS+X3D(I)-J*2*SX-2*COPLAX)**2+
     -           (YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)
            RM2=SQRT((XPOS+X3D(I)+J*2*SX-2*COPLAX)**2+
     -           (YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)
*   Initialisation of the sum: only a charge and a mirror charge.
            IF(J.EQ.0)THEN
                 VSUM=1/RR1-1/RM1
                 EXSUM=(XPOS-X3D(I))/RR1**3-
     -                (XPOS+X3D(I)-2*COPLAX)/RM1**3
                 EYSUM=(YPOS-Y3D(I))*(1/RR1**3-1/RM1**3)
                 EZSUM=(ZPOS-Z3D(I))*(1/RR1**3-1/RM1**3)
*   Further terms in the series: 2 charges and 2 mirror charges.
            ELSE
                 VSUM=VSUM+1/RR1+1/RR2-1/RM1-1/RM2
                 EXSUM=EXSUM+
     -                (XPOS-X3D(I)+J*2*SX)/RR1**3+
     -                (XPOS-X3D(I)-J*2*SX)/RR2**3-
     -                (XPOS+X3D(I)-J*2*SX-2*COPLAX)/RM1**3-
     -                (XPOS+X3D(I)+J*2*SX-2*COPLAX)/RM2**3
                 EYSUM=EYSUM+(YPOS-Y3D(I))*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                 EZSUM=EZSUM+(ZPOS-Z3D(I))*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
            ENDIF
30          CONTINUE
       ENDIF
*** Take care of a planes at constant y.
       IF(YNPLAY)THEN
*** Bessel function series.
            IF((YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2.GT.
     -           (RCUT*2*SX)**2)THEN
*   Loop over the terms in the series.
                 DO 40 J=1,NTERMB
*   Obtain reduced coordinates.
                 RRM=PI*J*SQRT((YPOS+Y3D(I)-2*COPLAY)**2+
     -                (ZPOS-Z3D(I))**2)/SX
                 ZZP=PI*J*(XPOS-X3D(I))/SX
                 ZZN=PI*J*(XPOS+X3D(I)-2*COPLAX)/SX
*   Evaluate the Bessel functions for this R.
                 IF(RRM.LT.2)THEN
                      K0RM=K0S(RRM)
                      K1RM=K1S(RRM)
                 ELSE
                      K0RM=K0L(RRM)
                      K1RM=K1L(RRM)
                 ENDIF
*   Get the field components.
                 VSUM=VSUM+(1/SX)*K0RM*(COS(ZZP)-COS(ZZN))
                 ERR=(2*PI/SX**2)*K1RM*(COS(ZZP)-COS(ZZN))
                 EZZ=(2*PI/SX**2)*K0RM*(SIN(ZZP)-SIN(ZZN))
                 EXSUM=EXSUM+EZZ
                 EYSUM=EYSUM+ERR*(YPOS+Y3D(I)-2*COPLAY)/
     -                SQRT((YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
                 EZSUM=EZSUM+ERR*(ZPOS-Z3D(I))/
     -                SQRT((YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
40               CONTINUE
*** Polynomial sum.
            ELSE
*   Loop over the terms.
                 DO 50 J=0,NTERMP
*   Simplify the references to the distances.
                 RR1=SQRT((XPOS-X3D(I)+J*2*SX)**2+
     -                (YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
                 RR2=SQRT((XPOS-X3D(I)-J*2*SX)**2+
     -                (YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
                 RM1=SQRT((XPOS+X3D(I)-J*2*SX-2*COPLAX)**2+
     -                (YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
                 RM2=SQRT((XPOS+X3D(I)+J*2*SX-2*COPLAX)**2+
     -                (YPOS+Y3D(I)-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
*   Initialisation of the sum: only a charge and a mirror charge.
                 IF(J.EQ.0)THEN
                      VSUM=VSUM-1/RR1+1/RM1
                      EXSUM=EXSUM-(XPOS-X3D(I))/RR1**3+
     -                     (XPOS+X3D(I)-2*COPLAX)/RM1**3
                      EYSUM=EYSUM-(YPOS+Y3D(I)-2*COPLAY)*
     -                     (1/RR1**3-1/RM1**3)
                      EZSUM=EZSUM-(ZPOS-Z3D(I))*(1/RR1**3-1/RM1**3)
*   Further terms in the series: 2 charges and 2 mirror charges.
                 ELSE
                      VSUM=VSUM-1/RR1-1/RR2+1/RM1+1/RM2
                      EXSUM=EXSUM-
     -                     (XPOS-X3D(I)+J*2*SX)/RR1**3-
     -                     (XPOS-X3D(I)-J*2*SX)/RR2**3+
     -                     (XPOS+X3D(I)-J*2*SX-2*COPLAX)/RM1**3+
     -                     (XPOS+X3D(I)+J*2*SX-2*COPLAX)/RM2**3
                      EYSUM=EYSUM-(YPOS+Y3D(I)-2*COPLAY)*
     -                     (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                      EZSUM=EZSUM-(ZPOS-Z3D(I))*
     -                     (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                 ENDIF
50               CONTINUE
            ENDIF
       ENDIF
*** Convert the double precision sum to single precision.
       EX=EX+E3D(I)*REAL(EXSUM)
       EY=EY+E3D(I)*REAL(EYSUM)
       EZ=EZ+E3D(I)*REAL(EZSUM)
       VOLT=VOLT+E3D(I)*REAL(VSUM)
*** Finish the loop over the charges.
10     CONTINUE
       END
CDECK  ID>, EMCB2X.
       SUBROUTINE EMCB2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EMCB2X - Routine calculating the dipole terms for a charge between
*            parallel conducting planes.
*   (Last changed on 17/10/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL XPOS,YPOS,EX,EY,VOLT,DX,DY,DXNEG,EXHELP,EYHELP,VHELP
       INTEGER IOPT,I
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=0.0
*** Loop over all wires.
       VHELP=0
       DO 10 I=1,NWIRE
       DX=(0.5*PI/SX)*(XPOS-X(I))
       DY=(0.5*PI/SX)*(YPOS-Y(I))
       DXNEG=(0.5*PI/SX)*(XPOS+X(I)-2.0*COPLAX)
*** Calculate the field in case there are no equipotential planes.
       EXHELP =
     -    (COSPH2(I)*(1-COS(2*DX)*COSH(2*DY)) + 
     -     SINPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -         (SIN(DX)**2+SINH(DY)**2)**2 + 
     -    (2*COSPH2(I)*(COSH(DY)**2*SIN(DXNEG)**2 -
     -                  COS(DXNEG)**2*SINH(DY)**2) - 
     -    SINPH2(I)*SIN(2*DXNEG)*SINH(2*DY))/
     -         (SIN(DXNEG)**2+SINH(DY)**2)**2
       EYHELP =
     -    (SINPH2(I)*(COS(2*DX)*COSH(2*DY)-1) + 
     -     COSPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -         (SIN(DX)**2+SINH(DY)**2)**2 + 
     -    (2*SINPH2(I)*(COSH(DY)**2*SIN(DXNEG)**2 -
     -                 COS(DXNEG)**2*SINH(DY)**2) + 
     -    COSPH2(I)*SIN(2*DXNEG)*SINH(2*DY))/
     -         (SIN(DXNEG)**2+SINH(DY)**2)**2
       IF(IOPT.NE.0)VHELP = -SIN(DX + DXNEG)*(
     -      -2*COSPH2(I)*(COSH(DY)**2*SIN(DX)*SIN(DXNEG) +
     -                    COS(DX)*COS(DXNEG)*SINH(DY)**2) + 
     -      SINPH2(I)*B2SIN(I)*SINH(2*DY))/
     -      ((SIN(DX)**2+SINH(DY)**2)*(SIN(DXNEG)**2+SINH(DY)**2))
*** Take care of a planes at constant y.
       IF(YNPLAY)THEN
            DY=(0.5*PI/SX)*(YPOS+Y(I)-2.0*COPLAY)
            EXHELP=EXHELP-
     -           (COSPH2(I)*(1-COS(2*DX)*COSH(2*DY)) - 
     -            SINPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -                (SIN(DX)**2+SINH(DY)**2)**2 -
     -           (2*COSPH2(I)*(COSH(DY)**2*SIN(DXNEG)**2 -
     -                         COS(DXNEG)**2*SINH(DY)**2) +
     -           SINPH2(I)*SIN(2*DXNEG)*SINH(2*DY))/
     -                (SIN(DXNEG)**2+SINH(DY)**2)**2
            EYHELP=EYHELP-
     -           (-SINPH2(I)*(COS(2*DX)*COSH(2*DY)-1) + 
     -             COSPH2(I)*SIN(2*DX)*SINH(2*DY))/
     -                (SIN(DX)**2+SINH(DY)**2)**2 -
     -           (-2*SINPH2(I)*(COSH(DY)**2*SIN(DXNEG)**2 -
     -                         COS(DXNEG)**2*SINH(DY)**2) + 
     -           COSPH2(I)*SIN(2*DXNEG)*SINH(2*DY))/
     -                (SIN(DXNEG)**2+SINH(DY)**2)**2
            IF(IOPT.NE.0)VHELP=VHELP+SIN(DX + DXNEG)*(
     -           -2*COSPH2(I)*(COSH(DY)**2*SIN(DX)*SIN(DXNEG) +
     -                         COS(DX)*COS(DXNEG)*SINH(DY)**2) -
     -           SINPH2(I)*B2SIN(I)*SINH(2*DY))/
     -           ((SIN(DX)**2+SINH(DY)**2)*(SIN(DXNEG)**2+SINH(DY)**2))
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX-AMP2(I)*0.5*(0.5*PI/SX)**2*EXHELP
       EY=EY-AMP2(I)*0.5*(0.5*PI/SX)**2*EYHELP
       IF(IOPT.NE.0)VOLT=VOLT-0.5*(0.5*PI/SX)*AMP2(I)*VHELP
*** Finish the wire loop.
10     CONTINUE
       END
CDECK  ID>, EFCB2Y.
       SUBROUTINE EFCB2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCB2Y - Routine calculating the potential for a row of alternating
*            + - charges. The potential used is re log(sin pi/sx (z-z0))
*   VARIABLES : See routine EFCA00 for most of the variables.
*               Z, ZMIRR   : X + i*Y , XXMIRR + i*YYMIRR ; i**2=-1
*               ECOMPL     : EX + i*EY                   ; i**2=-1
*   (Cray vectorisable)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX ZZ,ECOMPL,ZZMIRR,ZZNEG,ZZNMIR
*** Initialise EX, EY and VOLT.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
       XX=(0.5*PI/SY)*(XPOS-X(I))
       YY=(0.5*PI/SY)*(YPOS-Y(I))
       YYNEG=(0.5*PI/SY)*(YPOS+Y(I)-2.0*COPLAY)
       ZZ=CMPLX(XX,YY)
       ZZNEG=CMPLX(XX,YYNEG)
*** Calculate the field in case there are no equipotential planes.
       ECOMPL=0.0
       R2=1.0
       IF(ABS(XX).LE.20.0)
     -      ECOMPL=ICONS*B2SIN(I)/(SIN(ICONS*ZZ)*SIN(ICONS*ZZNEG))
       IF(IOPT.NE.0.AND.ABS(XX).LE.20.0)
     -      R2=(SINH(XX)**2+SIN(YY)**2)/(SINH(XX)**2+SIN(YYNEG)**2)
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=(0.5*PI/SY)*(XPOS+X(I)-2.0*COPLAX)
            ZZMIRR=CMPLX(XXMIRR,YY)
            ZZNMIR=CMPLX(XXMIRR,YYNEG)
            IF(ABS(XXMIRR).LE.20.0)ECOMPL=ECOMPL-
     -           ICONS*B2SIN(I)/(SIN(ICONS*ZZMIRR)*SIN(ICONS*ZZNMIR))
            IF(IOPT.NE.0.AND.ABS(XXMIRR).LE.20.0)THEN
                 R2PLAN=(SINH(XXMIRR)**2+SIN(YY)**2)/
     -                  (SINH(XXMIRR)**2+SIN(YYNEG)**2)
                 R2=R2/R2PLAN
            ENDIF
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX+E(I)*(0.5*PI/SY)*REAL(ECOMPL)
       EY=EY-E(I)*(0.5*PI/SY)*AIMAG(ECOMPL)
       IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*LOG(R2)
*** Finish the wire loop.
10     CONTINUE
       END
CDECK  ID>, E3DB2Y.
       SUBROUTINE E3DB2Y(XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT)
*-----------------------------------------------------------------------
*   E3DB2Y - Routine calculating the potential for a 3 dimensional point
*            charge between two plates at constant y.
*            The series expansions for the modified Bessel functions
*            have been taken from Abramowitz and Stegun.
*   VARIABLES : See routine E3DA00 for most of the variables.
*   (Last changed on  5/12/94.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION EXSUM,EYSUM,EZSUM,VSUM,
     -      I0S,I1S,K0S,K0L,K1S,K1L,K0R,K1R,K0RM,K1RM,
     -      XX,RR,RRM,ZZP,ZZN,RR1,RR2,RM1,RM2,ERR,EZZ
       REAL XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT,RCUT
       PARAMETER(RCUT=1.0)
*** Statement functions for the modified Bessel functions:
       I0S(XX)=1
     -      +3.5156229*(XX/3.75)**2
     -      +3.0899424*(XX/3.75)**4
     -      +1.2067492*(XX/3.75)**6
     -      +0.2659732*(XX/3.75)**8
     -      +0.0360768*(XX/3.75)**10
     -      +0.0045813*(XX/3.75)**12
       I1S(XX)=XX*(
     -      +0.5
     -      +0.87890594*(XX/3.75)**2
     -      +0.51498869*(XX/3.75)**4
     -      +0.15084934*(XX/3.75)**6
     -      +0.02658733*(XX/3.75)**8
     -      +0.00301532*(XX/3.75)**10
     -      +0.00032411*(XX/3.75)**12)
       K0S(XX)=-LOG(XX/2)*I0S(XX)
     -      -0.57721566
     -      +0.42278420*(XX/2)**2
     -      +0.23069756*(XX/2)**4
     -      +0.03488590*(XX/2)**6
     -      +0.00262698*(XX/2)**8
     -      +0.00010750*(XX/2)**10
     -      +0.00000740*(XX/2)**12
       K0L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      -0.07832358*(2/XX)
     -      +0.02189568*(2/XX)**2
     -      -0.01062446*(2/XX)**3
     -      +0.00587872*(2/XX)**4
     -      -0.00251540*(2/XX)**5
     -      +0.00053208*(2/XX)**6)
       K1S(XX)=LOG(XX/2)*I1S(XX)+(1/XX)*(
     -      +1
     -      +0.15443144*(XX/2)**2
     -      -0.67278579*(XX/2)**4
     -      -0.18156897*(XX/2)**6
     -      -0.01919402*(XX/2)**8
     -      -0.00110404*(XX/2)**10
     -      -0.00004686*(XX/2)**12)
       K1L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      +0.23498619*(2/XX)
     -      -0.03655620*(2/XX)**2
     -      +0.01504268*(2/XX)**3
     -      -0.00780353*(2/XX)**4
     -      +0.00325614*(2/XX)**5
     -      -0.00068245*(2/XX)**6)
*** Initialise the sums for the field components.
       EX=0.0
       EY=0.0
       EZ=0.0
       VOLT=0.0
*** Loop over all wires.
       DO 10 I=1,N3D
*   Skip wires that are on the charge.
       IF(XPOS.EQ.X3D(I).AND.YPOS.EQ.Y3D(I).AND.ZPOS.EQ.Z3D(I))GOTO 10
*** In the far away zone, sum the modified Bessel function series.
       IF((XPOS-X3D(I))**2+(ZPOS-Z3D(I))**2.GT.(RCUT*2*SY)**2)THEN
*   Initialise the per-wire sum.
            EXSUM=0.0
            EYSUM=0.0
            EZSUM=0.0
            VSUM=0.0
*   Loop over the terms in the series.
            DO 20 J=1,NTERMB
*   Obtain reduced coordinates.
            RR=PI*J*SQRT((XPOS-X3D(I))**2+(ZPOS-Z3D(I))**2)/SY
            ZZP=PI*J*(YPOS-Y3D(I))/SY
            ZZN=PI*J*(YPOS+Y3D(I)-2*COPLAY)/SY
*   Evaluate the Bessel functions for this R.
            IF(RR.LT.2)THEN
                 K0R=K0S(RR)
                 K1R=K1S(RR)
            ELSE
                 K0R=K0L(RR)
                 K1R=K1L(RR)
            ENDIF
*   Get the field components.
            VSUM=VSUM+(1/SY)*K0R*(COS(ZZP)-COS(ZZN))
            ERR=(2*J*PI/SY**2)*K1R*(COS(ZZP)-COS(ZZN))
            EZZ=(2*J*PI/SY**2)*K0R*(SIN(ZZP)-SIN(ZZN))
            EXSUM=EXSUM+ERR*(XPOS-X3D(I))/
     -           SQRT((XPOS-X3D(I))**2+(ZPOS-Z3D(I))**2)
            EYSUM=EYSUM+EZZ
            EZSUM=EZSUM+ERR*(ZPOS-Z3D(I))/
     -           SQRT((XPOS-X3D(I))**2+(ZPOS-Z3D(I))**2)
20          CONTINUE
*** Direct polynomial summing, obtain reduced coordinates.
       ELSE
*   Loop over the terms.
            DO 30 J=0,NTERMP
*   Simplify the references to the distances.
            RR1=SQRT((XPOS-X3D(I))**2+(YPOS-Y3D(I)+J*2*SY)**2+
     -           (ZPOS-Z3D(I))**2)
            RR2=SQRT((XPOS-X3D(I))**2+(YPOS-Y3D(I)-J*2*SY)**2+
     -           (ZPOS-Z3D(I))**2)
            RM1=SQRT((XPOS-X3D(I))**2+
     -           (YPOS+Y3D(I)-J*2*SY-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
            RM2=SQRT((XPOS-X3D(I))**2+
     -           (YPOS+Y3D(I)+J*2*SY-2*COPLAY)**2+(ZPOS-Z3D(I))**2)
*   Initialisation of the sum: only a charge and a mirror charge.
            IF(J.EQ.0)THEN
                 VSUM=1/RR1-1/RM1
                 EXSUM=(XPOS-X3D(I))*(1/RR1**3-1/RM1**3)
                 EYSUM=(YPOS-Y3D(I))/RR1**3-
     -                (YPOS+Y3D(I)-2*COPLAY)/RM1**3
                 EZSUM=(ZPOS-Z3D(I))*(1/RR1**3-1/RM1**3)
*   Further terms in the series: 2 charges and 2 mirror charges.
            ELSE
                 VSUM=VSUM+1/RR1+1/RR2-1/RM1-1/RM2
                 EXSUM=EXSUM+(XPOS-X3D(I))*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                 EYSUM=EYSUM+
     -                (YPOS-Y3D(I)+J*2*SY)/RR1**3+
     -                (YPOS-Y3D(I)-J*2*SY)/RR2**3-
     -                (YPOS+Y3D(I)-J*2*SY-2*COPLAY)/RM1**3-
     -                (YPOS+Y3D(I)+J*2*SY-2*COPLAY)/RM2**3
                 EZSUM=EZSUM+(ZPOS-Z3D(I))*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
            ENDIF
30          CONTINUE
       ENDIF
*** Take care of a planes at constant x.
       IF(YNPLAX)THEN
*** Bessel function series.
            IF((XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2.GT.
     -           (RCUT*2*SY)**2)THEN
*   Loop over the terms in the series.
                 DO 40 J=1,NTERMB
*   Obtain reduced coordinates.
                 RRM=PI*J*SQRT((XPOS+X3D(I)-2*COPLAX)**2+
     -                (ZPOS-Z3D(I))**2)/SY
                 ZZP=PI*J*(YPOS-Y3D(I))/SY
                 ZZN=PI*J*(YPOS+Y3D(I)-2*COPLAY)/SY
*   Evaluate the Bessel functions for this R.
                 IF(RRM.LT.2)THEN
                      K0RM=K0S(RRM)
                      K1RM=K1S(RRM)
                 ELSE
                      K0RM=K0L(RRM)
                      K1RM=K1L(RRM)
                 ENDIF
*   Get the field components.
                 VSUM=VSUM+(1/SY)*K0RM*(COS(ZZP)-COS(ZZN))
                 ERR=(2*PI/SY**2)*K1RM*(COS(ZZP)-COS(ZZN))
                 EZZ=(2*PI/SY**2)*K0RM*(SIN(ZZP)-SIN(ZZN))
                 EXSUM=EXSUM+ERR*(XPOS+X3D(I)-2*COPLAX)/
     -                SQRT((XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2)
                 EYSUM=EYSUM+EZZ
                 EZSUM=EZSUM+ERR*(ZPOS-Z3D(I))/
     -                SQRT((XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2)
40               CONTINUE
*** Polynomial sum.
            ELSE
*   Loop over the terms.
                 DO 50 J=0,NTERMP
*   Simplify the references to the distances.
                 RR1=SQRT((YPOS-Y3D(I)+J*2*SY)**2+
     -                (XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2)
                 RR2=SQRT((YPOS-Y3D(I)-J*2*SY)**2+
     -                (XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2)
                 RM1=SQRT((YPOS+Y3D(I)-J*2*SY-2*COPLAY)**2+
     -                (XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2)
                 RM2=SQRT((YPOS+Y3D(I)+J*2*SY-2*COPLAY)**2+
     -                (XPOS+X3D(I)-2*COPLAX)**2+(ZPOS-Z3D(I))**2)
*   Initialisation of the sum: only a charge and a mirror charge.
                 IF(J.EQ.0)THEN
                      VSUM=VSUM-1/RR1+1/RM1
                      EXSUM=EXSUM-(XPOS+X3D(I)-2*COPLAX)*
     -                     (1/RR1**3-1/RM1**3)
                      EYSUM=EYSUM-(YPOS-Y3D(I))/RR1**3+
     -                     (YPOS+Y3D(I)-2*COPLAY)/RM1**3
                      EZSUM=EZSUM-(ZPOS-Z3D(I))*(1/RR1**3-1/RM1**3)
*   Further terms in the series: 2 charges and 2 mirror charges.
                 ELSE
                      VSUM=VSUM-1/RR1-1/RR2+1/RM1+1/RM2
                      EXSUM=EXSUM-(XPOS+X3D(I)-2*COPLAX)*
     -                     (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                      EYSUM=EYSUM-
     -                     (YPOS-Y3D(I)+J*2*SY)/RR1**3-
     -                     (YPOS-Y3D(I)-J*2*SY)/RR2**3+
     -                     (YPOS+Y3D(I)-J*2*SY-2*COPLAY)/RM1**3+
     -                     (YPOS+Y3D(I)+J*2*SY-2*COPLAY)/RM2**3
                      EZSUM=EZSUM-(ZPOS-Z3D(I))*
     -                     (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                 ENDIF
50               CONTINUE
            ENDIF
       ENDIF
*** Convert the double precision sum to single precision.
       EX=EX+E3D(I)*REAL(EXSUM)
       EY=EY+E3D(I)*REAL(EYSUM)
       EZ=EZ+E3D(I)*REAL(EZSUM)
       VOLT=VOLT+E3D(I)*REAL(VSUM)
*** Finish the loop over the charges.
10     CONTINUE
       END
CDECK  ID>, EMCB2Y.
       SUBROUTINE EMCB2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EMCB2Y - Routine calculating the dipole terms for a charge between
*            parallel conducting planes.
*   (Last changed on 20/10/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL XPOS,YPOS,EX,EY,VOLT,DX,DY,DYNEG,EXHELP,EYHELP,VHELP
       INTEGER IOPT,I
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=0.0
*** Loop over all wires.
       VHELP=0
       DO 10 I=1,NWIRE
       DX=(0.5*PI/SY)*(XPOS-X(I))
       DY=(0.5*PI/SY)*(YPOS-Y(I))
       DYNEG=(0.5*PI/SY)*(YPOS+Y(I)-2.0*COPLAY)
*** Calculate the field in case there are no equipotential planes.
       EXHELP = 2*(
     -    COSPH2(I)*COSH(DX)**2*(
     -      -SIN(DY   )**2/(SINH(DX)**2+SIN(DY   )**2)**2 + 
     -       SIN(DYNEG)**2/(SINH(DX)**2+SIN(DYNEG)**2)**2) +
     -    2*SINPH2(I)*COSH(DX)*SINH(DX)*(
     -       COS(DY   )*SIN(DY   )/(SINH(DX)**2+SIN(DY   )**2)**2 + 
     -       COS(DYNEG)*SIN(DYNEG)/(SINH(DX)**2+SIN(DYNEG)**2)**2) + 
     -    COSPH2(I)*SINH(DX)**2*(
     -      COS(DY   )**2/(SINH(DX)**2+SIN(DY   )**2)**2 -
     -      COS(DYNEG)**2/(SINH(DX)**2+SIN(DYNEG)**2)**2))
       EYHELP = (SINPH2(I)/
     -      (SINH(DX)**2+SIN(DY   )**2)**2 - 
     -    (SINPH2(I)*COS(2*DY)*COSH(2*DX))/
     -      (SINH(DX)**2+SIN(DY   )**2)**2 +
     -    (2*SINPH2(I)*COSH(DX)**2*SIN(DYNEG)**2)/
     -      (SINH(DX)**2+SIN(DYNEG)**2)**2 - 
     -    (2*SINPH2(I)*COS(DYNEG)**2*SINH(DX)**2)/
     -      (SINH(DX)**2+SIN(DYNEG)**2)**2 + 
     -    (COSPH2(I)*SIN(2*DY)*SINH(2*DX))/
     -      (SINH(DX)**2+SIN(DY   )**2)**2 - 
     -    (COSPH2(I)*SIN(2*DYNEG)*SINH(2*DX))/
     -      (SINH(DX)**2+SIN(DYNEG)**2)**2)
       VHELP = (-4*SIN(DY + DYNEG)*(-(SINPH2(I)*COS(DY)*COS(DYNEG)) +
     -       SINPH2(I)*COS(DY - DYNEG)*COSH(DX)**2 + 
     -      SINPH2(I)*SIN(DY)*SIN(DYNEG) -
     -       2*COSPH2(I)*COS(DYNEG)*COSH(DX)*SIN(DY)*SINH(DX) + 
     -      SINPH2(I)*COS(DY - DYNEG)*SINH(DX)**2 +
     -       COSPH2(I)*COS(DY)*SIN(DYNEG)*SINH(2*DX)))/
     -  ((COS(2*DY) - COSH(2*DX))*(-COS(2*DYNEG) + COSH(2*DX)))
*** Take care of a planes at constant y.
       IF(YNPLAX)THEN
            DX=(0.5*PI/SY)*(XPOS+X(I)-2.0*COPLAX)
            EXHELP=EXHELP-2*(
     -           -COSPH2(I)*COSH(DX)**2*(
     -             -SIN(DY   )**2/(SINH(DX)**2+SIN(DY   )**2)**2 + 
     -              SIN(DYNEG)**2/(SINH(DX)**2+SIN(DYNEG)**2)**2) +
     -         2*SINPH2(I)*COSH(DX)*SINH(DX)*(
     -           COS(DY   )*SIN(DY   )/(SINH(DX)**2+SIN(DY   )**2)**2 + 
     -           COS(DYNEG)*SIN(DYNEG)/(SINH(DX)**2+SIN(DYNEG)**2)**2) -
     -           COSPH2(I)*SINH(DX)**2*(
     -              COS(DY   )**2/(SINH(DX)**2+SIN(DY   )**2)**2 -
     -              COS(DYNEG)**2/(SINH(DX)**2+SIN(DYNEG)**2)**2))
            EYHELP=EYHELP-(SINPH2(I)/
     -           (SINH(DX)**2+SIN(DY   )**2)**2 - 
     -         (SINPH2(I)*COS(2*DY)*COSH(2*DX))/
     -           (SINH(DX)**2+SIN(DY   )**2)**2 +
     -         (2*SINPH2(I)*COSH(DX)**2*SIN(DYNEG)**2)/
     -           (SINH(DX)**2+SIN(DYNEG)**2)**2 - 
     -         (2*SINPH2(I)*COS(DYNEG)**2*SINH(DX)**2)/
     -           (SINH(DX)**2+SIN(DYNEG)**2)**2 -
     -         (COSPH2(I)*SIN(2*DY)*SINH(2*DX))/
     -           (SINH(DX)**2+SIN(DY   )**2)**2 +
     -         (COSPH2(I)*SIN(2*DYNEG)*SINH(2*DX))/
     -           (SINH(DX)**2+SIN(DYNEG)**2)**2)
            IF(IOPT.NE.0)VHELP=VHELP-
     -           (-4*SIN(DY + DYNEG)*(-(SINPH2(I)*COS(DY)*COS(DYNEG)) +
     -            SINPH2(I)*COS(DY - DYNEG)*COSH(DX)**2 + 
     -           SINPH2(I)*SIN(DY)*SIN(DYNEG) +
     -            2*COSPH2(I)*COS(DYNEG)*COSH(DX)*SIN(DY)*SINH(DX) + 
     -           SINPH2(I)*COS(DY - DYNEG)*SINH(DX)**2 -
     -            COSPH2(I)*COS(DY)*SIN(DYNEG)*SINH(2*DX)))/
     -           ((COS(2*DY) - COSH(2*DX))*(-COS(2*DYNEG) + COSH(2*DX)))
       ENDIF
*** Calculate the electric field and the potential.
       EX=EX-AMP2(I)*0.5*(0.5*PI/SY)**2*EXHELP
       EY=EY-AMP2(I)*0.5*(0.5*PI/SY)**2*EYHELP
       IF(IOPT.NE.0)VOLT=VOLT-0.5*(0.5*PI/SY)*AMP2(I)*VHELP
*** Finish the wire loop.
10     CONTINUE
       END
CDECK  ID>, EFCC10.
       SUBROUTINE EFCC10(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCC10 - Routine returning the potential and electric field. It
*            calls the routines PH2 and E2SUM written by G.A.Erskine.
*   VARIABLES : No local variables.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
*** Calculate voltage first, if needed.
       IF(IOPT.NE.0)THEN
            IF(MODE.EQ.0)VOLT=V0+C1*XPOS
            IF(MODE.EQ.1)VOLT=V0+C1*YPOS
            DO 10 I=1,NWIRE
            VOLT=VOLT+E(I)*PH2(XPOS-X(I),YPOS-Y(I))
10          CONTINUE
       ENDIF
*** And finally the electric field.
       CALL E2SUM(XPOS,YPOS,EX,EY)
       IF(MODE.EQ.0)EX=EX-C1
       IF(MODE.EQ.1)EY=EY-C1
       END
CDECK  ID>, EFCC2X.
       SUBROUTINE EFCC2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCC2X - Routine returning the potential and electric field in a
*            configuration with 2 x planes and y periodicity.
*   VARIABLES : see the writeup
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX WSUM1,WSUM2,ZSIN,ZCOF,ZU,ZUNEW,ZTERM1,ZTERM2,ZETA
*** Initial values.
       WSUM1=0
       WSUM2=0
       VOLT=0.0
*** Wire loop.
       DO 10 I=1,NWIRE
*   Compute the direct contribution.
       ZETA=ZMULT*CMPLX(XPOS-X(I),YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM1=WSUM1-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM1=WSUM1+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM1=WSUM1+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Find the plane nearest to the wire.
       CX=COPLAX-SX*ANINT((COPLAX-X(I))/SX)
*   Mirror contribution.
       ZETA=ZMULT*CMPLX(2*CX-XPOS-X(I),YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM2=WSUM2-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM2=WSUM2+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM2=WSUM2+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Correct the voltage, if needed (MODE).
       IF(IOPT.NE.0.AND.MODE.EQ.0)VOLT=VOLT-
     -      2*E(I)*PI*(XPOS-CX)*(X(I)-CX)/(SX*SY)
10     CONTINUE
*** Convert the two contributions to a real field.
       EX=REAL(ZMULT*(WSUM1+WSUM2))
       EY=-AIMAG(ZMULT*(WSUM1-WSUM2))
*** Constant correction terms.
       IF(MODE.EQ.0)EX=EX-C1
       END
CDECK  ID>, EFCC2Y.
       SUBROUTINE EFCC2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCC2Y - Routine returning the potential and electric field in a
*            configuration with 2 y planes and x periodicity.
*   VARIABLES : see the writeup
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX WSUM1,WSUM2,ZSIN,ZCOF,ZU,ZUNEW,ZTERM1,ZTERM2,ZETA
*** Initial values.
       WSUM1=0
       WSUM2=0
       VOLT=0.0
*** Wire loop.
       DO 10 I=1,NWIRE
*   Compute the direct contribution.
       ZETA=ZMULT*CMPLX(XPOS-X(I),YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM1=WSUM1-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM1=WSUM1+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM1=WSUM1+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Find the plane nearest to the wire.
       CY=COPLAY-SY*ANINT((COPLAY-Y(I))/SY)
*   Mirror contribution from the y plane.
       ZETA=ZMULT*CMPLX(XPOS-X(I),2.0*CY-YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM2=WSUM2-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM2=WSUM2+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM2=WSUM2+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Correct the voltage, if needed (MODE).
       IF(IOPT.NE.0.AND.MODE.EQ.1)VOLT=VOLT-
     -      2*E(I)*PI*(YPOS-CY)*(Y(I)-CY)/(SX*SY)
10     CONTINUE
*** Convert the two contributions to a real field.
       EX=REAL(ZMULT*(WSUM1-WSUM2))
       EY=-AIMAG(ZMULT*(WSUM1+WSUM2))
*** Constant correction terms.
       IF(MODE.EQ.1)EY=EY-C1
       END
CDECK  ID>, EFCC30.
       SUBROUTINE EFCC30(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCC30 - Routine returning the potential and electric field in a
*            configuration with 2 y and 2 x planes.
*   VARIABLES : see the writeup
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX WSUM1,WSUM2,WSUM3,WSUM4,ZSIN,ZCOF,ZU,ZUNEW,
     -      ZTERM1,ZTERM2,ZETA
*** Initial values.
       WSUM1=0
       WSUM2=0
       WSUM3=0
       WSUM4=0
       VOLT=0.0
*** Wire loop.
       DO 10 I=1,NWIRE
*   Compute the direct contribution.
       ZETA=ZMULT*CMPLX(XPOS-X(I),YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM1=WSUM1-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM1=WSUM1+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM1=WSUM1+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Find the plane nearest to the wire.
       CX=COPLAX-SX*ANINT((COPLAX-X(I))/SX)
*   Mirror contribution from the x plane.
       ZETA=ZMULT*CMPLX(2*CX-XPOS-X(I),YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM2=WSUM2-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM2=WSUM2+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM2=WSUM2+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Find the plane nearest to the wire.
       CY=COPLAY-SY*ANINT((COPLAY-Y(I))/SY)
*   Mirror contribution from the y plane.
       ZETA=ZMULT*CMPLX(XPOS-X(I),2.0*CY-YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM3=WSUM3-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM3=WSUM3+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM3=WSUM3+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT+E(I)*LOG(CABS(ZTERM1))
       ENDIF
*   Mirror contribution from both the x and the y plane.
       ZETA=ZMULT*CMPLX(2.0*CX-XPOS-X(I),2.0*CY-YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM4=WSUM4-E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM4=WSUM4+E(I)*ICONS
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*(ABS(AIMAG(ZETA))-CLOG2)
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM4=WSUM4+E(I)*(ZTERM2/ZTERM1)
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(CABS(ZTERM1))
       ENDIF
10     CONTINUE
*** Convert the two contributions to a real field.
       EX=+REAL(ZMULT*(WSUM1+WSUM2-WSUM3-WSUM4))
       EY=-AIMAG(ZMULT*(WSUM1-WSUM2+WSUM3-WSUM4))
       END
CDECK  ID>, EFCD10.
       SUBROUTINE EFCD10(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCD10 - Subroutine performing the actual field calculations for a
*            cell which has a one circular plane and some wires.
*   VARIABLES : EX, EY, VOLT:Electric field and potential.
*               ETOT, VOLT : Magnitude of electric field, potential.
*               (XPOS,YPOS): The position where the field is calculated.
*               ZI, ZPOS   : Shorthand complex notations.
*   (Last changed on  4/ 9/95.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ZI,ZPOS
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=V0
*   Set the complex position coordinates.
       ZPOS=CMPLX(XPOS,YPOS)
*** Loop over all wires.
       DO 10 I=1,NWIRE
*   Set the complex version of the wire-coordinate for simplicity.
       ZI=CMPLX(X(I),Y(I))
*   Compute the contribution to the potential, if needed.
       IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(ABS(COTUBE*(ZPOS-ZI)/
     -      (COTUBE**2-ZPOS*CONJG(ZI))))
*   Compute the contribution to the electric field, always.
       EX=EX+E(I)*REAL(1/CONJG(ZPOS-ZI)+ZI/(COTUBE**2-CONJG(ZPOS)*ZI))
       EY=EY+E(I)*AIMAG(1/CONJG(ZPOS-ZI)+ZI/(COTUBE**2-CONJG(ZPOS)*ZI))
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, E3DD10.
       SUBROUTINE E3DD10(XXPOS,YYPOS,ZZPOS,EEX,EEY,EEZ,VOLT)
*-----------------------------------------------------------------------
*   E3DD10 - Subroutine adding 3-dimensional charges to tubes with one
*            wire running down the centre.
*            The series expansions for the modified Bessel functions
*            have been taken from Abramowitz and Stegun.
*   VARIABLES : See routine E3DA00 for most of the variables.
*   (Last changed on 25/11/95.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION EXSUM,EYSUM,EZSUM,VSUM,
     -      I0S,I1S,K0S,K0L,K1S,K1L,K0R,K1R,
     -      XX,RR,ZZP,ZZN,RR1,RR2,RM1,RM2,ERR,EZZ
       REAL XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT,RCUT
       PARAMETER(RCUT=1.0)
*** Statement functions for the modified Bessel functions:
       I0S(XX)=1
     -      +3.5156229*(XX/3.75)**2
     -      +3.0899424*(XX/3.75)**4
     -      +1.2067492*(XX/3.75)**6
     -      +0.2659732*(XX/3.75)**8
     -      +0.0360768*(XX/3.75)**10
     -      +0.0045813*(XX/3.75)**12
       I1S(XX)=XX*(
     -      +0.5
     -      +0.87890594*(XX/3.75)**2
     -      +0.51498869*(XX/3.75)**4
     -      +0.15084934*(XX/3.75)**6
     -      +0.02658733*(XX/3.75)**8
     -      +0.00301532*(XX/3.75)**10
     -      +0.00032411*(XX/3.75)**12)
       K0S(XX)=-LOG(XX/2)*I0S(XX)
     -      -0.57721566
     -      +0.42278420*(XX/2)**2
     -      +0.23069756*(XX/2)**4
     -      +0.03488590*(XX/2)**6
     -      +0.00262698*(XX/2)**8
     -      +0.00010750*(XX/2)**10
     -      +0.00000740*(XX/2)**12
       K0L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      -0.07832358*(2/XX)
     -      +0.02189568*(2/XX)**2
     -      -0.01062446*(2/XX)**3
     -      +0.00587872*(2/XX)**4
     -      -0.00251540*(2/XX)**5
     -      +0.00053208*(2/XX)**6)
       K1S(XX)=LOG(XX/2)*I1S(XX)+(1/XX)*(
     -      +1
     -      +0.15443144*(XX/2)**2
     -      -0.67278579*(XX/2)**4
     -      -0.18156897*(XX/2)**6
     -      -0.01919402*(XX/2)**8
     -      -0.00110404*(XX/2)**10
     -      -0.00004686*(XX/2)**12)
       K1L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      +0.23498619*(2/XX)
     -      -0.03655620*(2/XX)**2
     -      +0.01504268*(2/XX)**3
     -      -0.00780353*(2/XX)**4
     -      +0.00325614*(2/XX)**5
     -      -0.00068245*(2/XX)**6)
*** Initialise the sums for the field components.
       EX=0.0
       EEX=0.0
       EY=0.0
       EEY=0.0
       EZ=0.0
       EEZ=0.0
       VOLT=0.0
*** Ensure that the routine can actually work.
       IF(NWIRE.LT.1)THEN
            PRINT *,' Inappropriate potential function.'
            RETURN
       ENDIF
*** Define a periodicity and one plane in the mapped frame.
       SSX=LOG(2*COTUBE/D(1))
       CPL=LOG(D(1)/2)
*** Transform the coordinates to the mapped frame.
       XPOS=0.5*LOG(XXPOS**2+YYPOS**2)
       YPOS=ATAN2(YYPOS,XXPOS)
       ZPOS=ZZPOS
*** Loop over all point charges.
       DO 10 I=1,N3D
       DO 40 II=-1,1
       XX3D=0.5*LOG(X3D(I)**2+Y3D(I)**2)
       YY3D=ATAN2(Y3D(I),X3D(I))+II*2*PI
       ZZ3D=Z3D(I)
*   Skip wires that are on the charge.
       IF(XPOS.EQ.XX3D.AND.YPOS.EQ.YY3D.AND.ZPOS.EQ.ZZ3D)GOTO 40
*** In the far away zone, sum the modified Bessel function series.
       IF((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2.GT.(RCUT*2*SSX)**2)THEN
*   Initialise the per-wire sum.
            EXSUM=0.0
            EYSUM=0.0
            EZSUM=0.0
            VSUM=0.0
*   Loop over the terms in the series.
            DO 20 J=1,NTERMB
*   Obtain reduced coordinates.
            RR=PI*J*SQRT((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)/SSX
            ZZP=PI*J*(XPOS-XX3D)/SSX
            ZZN=PI*J*(XPOS+XX3D-2*CPL)/SSX
*   Evaluate the Bessel functions for this R.
            IF(RR.LT.2)THEN
                 K0R=K0S(RR)
                 K1R=K1S(RR)
            ELSE
                 K0R=K0L(RR)
                 K1R=K1L(RR)
            ENDIF
*   Get the field components.
            VSUM=VSUM+(1/SSX)*K0R*(COS(ZZP)-COS(ZZN))
            ERR=(2*J*PI/SSX**2)*K1R*(COS(ZZP)-COS(ZZN))
            EZZ=(2*J*PI/SSX**2)*K0R*(SIN(ZZP)-SIN(ZZN))
            EXSUM=EXSUM+EZZ
            EYSUM=EYSUM+ERR*(YPOS-YY3D)/
     -           SQRT((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
            EZSUM=EZSUM+ERR*(ZPOS-ZZ3D)/
     -           SQRT((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
20          CONTINUE
*** Direct polynomial summing, obtain reduced coordinates.
       ELSE
*   Loop over the terms.
            DO 30 J=0,NTERMP
*   Simplify the references to the distances.
            RR1=SQRT((XPOS-XX3D+J*2*SSX)**2+(YPOS-YY3D)**2+
     -           (ZPOS-ZZ3D)**2)
            RR2=SQRT((XPOS-XX3D-J*2*SSX)**2+(YPOS-YY3D)**2+
     -           (ZPOS-ZZ3D)**2)
            RM1=SQRT((XPOS+XX3D-J*2*SSX-2*CPL)**2+
     -           (YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
            RM2=SQRT((XPOS+XX3D+J*2*SSX-2*CPL)**2+
     -           (YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
*   Initialisation of the sum: only a charge and a mirror charge.
            IF(J.EQ.0)THEN
                 VSUM=1/RR1-1/RM1
                 EXSUM=(XPOS-XX3D)/RR1**3-
     -                (XPOS+XX3D-2*CPL)/RM1**3
                 EYSUM=(YPOS-YY3D)*(1/RR1**3-1/RM1**3)
                 EZSUM=(ZPOS-ZZ3D)*(1/RR1**3-1/RM1**3)
*   Further terms in the series: 2 charges and 2 mirror charges.
            ELSE
                 VSUM=VSUM+1/RR1+1/RR2-1/RM1-1/RM2
                 EXSUM=EXSUM+
     -                (XPOS-XX3D+J*2*SSX)/RR1**3+
     -                (XPOS-XX3D-J*2*SSX)/RR2**3-
     -                (XPOS+XX3D-J*2*SSX-2*CPL)/RM1**3-
     -                (XPOS+XX3D+J*2*SSX-2*CPL)/RM2**3
                 EYSUM=EYSUM+(YPOS-YY3D)*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                 EZSUM=EZSUM+(ZPOS-ZZ3D)*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
            ENDIF
30          CONTINUE
       ENDIF
*** Convert the double precision sum to single precision.
       EX=EX+E3D(I)*REAL(EXSUM)
       EY=EY+E3D(I)*REAL(EYSUM)
       EZ=EZ+E3D(I)*REAL(EZSUM)
       VOLT=VOLT+E3D(I)*REAL(VSUM)
*** Finish the loop over the charges.
40     CONTINUE
10     CONTINUE
*** Transform the field vectors back to Cartesian coordinates.
       EEX=EXP(-XPOS)*(+EX*COS(YPOS)-EY*SIN(YPOS))
       EEY=EXP(-XPOS)*(+EX*SIN(YPOS)+EY*COS(YPOS))
       EEZ=EZ
       END
CDECK  ID>, EFCD20.
       SUBROUTINE EFCD20(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCD20 - Subroutine performing the actual field calculations for a
*            cell which has a tube and phi periodicity.
*   VARIABLES : EX, EY, VOLT:Electric field and potential.
*               ETOT, VOLT : Magnitude of electric field, potential.
*               (XPOS,YPOS): The position where the field is calculated.
*               ZI, ZPOS   : Shorthand complex notations.
*   (Last changed on 10/ 2/93.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ZI,ZPOS
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=V0
*   Set the complex position coordinates.
       ZPOS=CMPLX(XPOS,YPOS)
*** Loop over all wires.
       DO 10 I=1,NWIRE
*   Set the complex version of the wire-coordinate for simplicity.
       ZI=CMPLX(X(I),Y(I))
*   Case of the wire which is not in the centre.
       IF(ABS(ZI).GT.D(I)/2)THEN
*   Compute the contribution to the potential, if needed.
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(ABS((1/COTUBE**MTUBE)*
     -           (ZPOS**MTUBE-ZI**MTUBE)/
     -           (1-(ZPOS*CONJG(ZI)/COTUBE**2)**MTUBE)))
*   Compute the contribution to the electric field, always.
            EX=EX+E(I)*REAL(MTUBE*CONJG(ZPOS)**(MTUBE-1)*
     -           (1/CONJG(ZPOS**MTUBE-ZI**MTUBE)+ZI**MTUBE/
     -           (COTUBE**(2*MTUBE)-(CONJG(ZPOS)*ZI)**MTUBE)))
            EY=EY+E(I)*AIMAG(MTUBE*CONJG(ZPOS)**(MTUBE-1)*
     -           (1/CONJG(ZPOS**MTUBE-ZI**MTUBE)+ZI**MTUBE/
     -           (COTUBE**(2*MTUBE)-(CONJG(ZPOS)*ZI)**MTUBE)))
       ELSE
*   Case of the central wire.
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(ABS((1/COTUBE)*(ZPOS-ZI)/
     -           (1-ZPOS*CONJG(ZI)/COTUBE**2)))
*   Compute the contribution to the electric field, always.
            EX=EX+E(I)*REAL(1/CONJG(ZPOS-ZI)+ZI/
     -           (COTUBE**2-CONJG(ZPOS)*ZI))
            EY=EY+E(I)*AIMAG(1/CONJG(ZPOS-ZI)+ZI/
     -           (COTUBE**2-CONJG(ZPOS)*ZI))
       ENDIF
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, EFCD30.
       SUBROUTINE EFCD30(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFCD30 - Subroutine performing the actual field calculations for a
*            cell which has a polygon as tube and some wires.
*   VARIABLES : EX, EY, VOLT:Electric field and potential.
*               ETOT, VOLT : Magnitude of electric field, potential.
*               (XPOS,YPOS): The position where the field is calculated.
*               ZI, ZPOS   : Shorthand complex notations.
*   (Last changed on 19/ 2/94.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX WPOS,WDPOS
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=V0
*   Get the mapping of the position.
       CALL EFCMAP(CMPLX(XPOS,YPOS)/COTUBE,WPOS,WDPOS)
*** Loop over all wires.
       DO 10 I=1,NWIRE
*   Compute the contribution to the potential, if needed.
       IF(IOPT.NE.0)VOLT=VOLT-E(I)*LOG(ABS((WPOS-WMAP(I))/
     -      (1-WPOS*CONJG(WMAP(I)))))
*   Compute the contribution to the electric field, always.
       EX=EX+(E(I)/COTUBE)*REAL(WDPOS*(1-ABS(WMAP(I))**2)/
     -      ((WPOS-WMAP(I))*(1-CONJG(WMAP(I))*WPOS)))
       EY=EY-(E(I)/COTUBE)*AIMAG(WDPOS*(1-ABS(WMAP(I))**2)/
     -      ((WPOS-WMAP(I))*(1-CONJG(WMAP(I))*WPOS)))
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, EFCMAP.
       SUBROUTINE EFCMAP(Z,WW,WD)
*-----------------------------------------------------------------------
*   EFCMAP - Maps a the interior part of a regular in the unit circle.
*   Variables: Z     - point to be mapped
*              W     - the image of Z
*              WD    - derivative of the mapping at Z
*              CC1   - coefficients for expansion around centre
*              CC2   - coefficients for expansion around cornre
*   (Last changed on 19/ 2/94.)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX Z,ZZ,WW,WSUM,WD,WDSUM,ZTERM
       REAL CC1(0:15,3:8),CC2(0:15,3:8)
       INTEGER NTERM1(3:8),NTERM2(3:8)
*** Triangle: coefficients for centre and corner expansion.
       DATA (CC1(I,3),I=0,15) /
     -      0.1000000000E+01, -.1666666865E+00, 0.3174602985E-01,
     -      -.5731921643E-02, 0.1040112227E-02, -.1886279933E-03,
     -      0.3421107249E-04, -.6204730198E-05, 0.1125329618E-05,
     -      -.2040969207E-06, 0.3701631357E-07, -.6713513301E-08,
     -      0.1217605794E-08, -.2208327132E-09, 0.4005162868E-10,
     -      -.7264017512E-11/
       DATA (CC2(I,3),I=0,15) /
     -      0.3333333135E+00, -.5555555597E-01, 0.1014109328E-01,
     -      -.1837154618E-02, 0.3332451452E-03, -.6043842586E-04,
     -      0.1096152027E-04, -.1988050826E-05, 0.3605655365E-06,
     -      -.6539443120E-07, 0.1186035448E-07, -.2151069323E-08,
     -      0.3901317047E-09, -.7075676156E-10, 0.1283289534E-10,
     -      -.2327455936E-11/
*** Square: coefficients for centre and corner expansion.
       DATA (CC1(I,4),I=0,15) /
     -      0.1000000000E+01, -.1000000238E+00, 0.8333332837E-02,
     -      -.7051283028E-03, 0.5967194738E-04, -.5049648280E-05,
     -      0.4273189802E-06, -.3616123934E-07, 0.3060091514E-08,
     -      -.2589557457E-09, 0.2191374859E-10, -.1854418528E-11,
     -      0.1569274224E-12, -.1327975205E-13, 0.1123779363E-14,
     -      -.9509817570E-16/
       DATA (CC2(I,4),I=0,15) /
     -      0.1000000000E+01, -.5000000000E+00, 0.3000000119E+00,
     -      -.1750000119E+00, 0.1016666889E+00, -.5916666612E-01,
     -      0.3442307562E-01, -.2002724260E-01, 0.1165192947E-01,
     -      -.6779119372E-02, 0.3944106400E-02, -.2294691978E-02,
     -      0.1335057430E-02, -.7767395582E-03, 0.4519091453E-03,
     -      -.2629216760E-03/
*** Pentagon: coefficients for centre and corner expansion.
       DATA (CC1(I,5),I=0,15) /
     -      0.1000000000E+01, -.6666666269E-01, 0.1212121220E-02,
     -      -.2626262140E-03, -.3322110570E-04, -.9413293810E-05,
     -      -.2570029210E-05, -.7695705904E-06, -.2422486887E-06,
     -      -.7945993730E-07, -.2691839640E-07, -.9361642128E-08,
     -      -.3327319087E-08, -.1204430555E-08, -.4428404310E-09,
     -      -.1650302672E-09/
       DATA (CC2(I,5),I=0,15) /
     -      0.1248050690E+01, -.7788147926E+00, 0.6355384588E+00,
     -      -.4899077415E+00, 0.3713272810E+00, -.2838423252E+00,
     -      0.2174729109E+00, -.1663445234E+00, 0.1271933913E+00,
     -      -.9728997946E-01, 0.7442557812E-01, -.5692918226E-01,
     -      0.4354400188E-01, -.3330700099E-01, 0.2547712997E-01,
     -      -.1948769018E-01/
*** Hexagon: coefficients for centre and corner expansion.
       DATA (CC1(I,6),I=0,15) /
     -      0.1000000000E+01, -.4761904851E-01, -.1221001148E-02,
     -      -.3753788769E-03, -.9415557724E-04, -.2862767724E-04,
     -      -.9587882232E-05, -.3441659828E-05, -.1299798896E-05,
     -      -.5103651119E-06, -.2066504408E-06, -.8578405186E-07,
     -      -.3635090096E-07, -.1567239494E-07, -.6857355572E-08,
     -      -.3038770346E-08/
       DATA (CC2(I,6),I=0,15) /
     -      0.1333333015E+01, -.8888888955E+00, 0.8395061493E+00,
     -      -.7242798209E+00, 0.6016069055E+00, -.5107235312E+00,
     -      0.4393203855E+00, -.3745460510E+00, 0.3175755739E+00,
     -      -.2703750730E+00, 0.2308617830E+00, -.1966916919E+00,
     -      0.1672732830E+00, -.1424439549E+00, 0.1214511395E+00,
     -      -.1034612656E+00/
*** Heptagon: coefficients for centre and corner expansion.
       DATA (CC1(I,7),I=0,15) /
     -      0.1000000000E+01, -.3571428731E-01, -.2040816238E-02,
     -      -.4936389159E-03, -.1446709794E-03, -.4963850370E-04,
     -      -.1877940667E-04, -.7600909157E-05, -.3232265954E-05,
     -      -.1427365532E-05, -.6493634714E-06, -.3026190711E-06,
     -      -.1438593245E-06, -.6953911225E-07, -.3409525462E-07,
     -      -.1692310647E-07/
       DATA (CC2(I,7),I=0,15) /
     -      0.1359752655E+01, -.9244638681E+00, 0.9593217969E+00,
     -      -.8771237731E+00, 0.7490229011E+00, -.6677658558E+00,
     -      0.6196745634E+00, -.5591596961E+00, 0.4905325770E+00,
     -      -.4393517375E+00, 0.4029803872E+00, -.3631100059E+00,
     -      0.3199430704E+00, -.2866140604E+00, 0.2627358437E+00,
     -      -.2368256450E+00/
*** Octagon: coefficients for centre and corner expansion.
       DATA (CC1(I,8),I=0,15) /
     -      0.1000000000E+01, -.2777777612E-01, -.2246732125E-02,
     -      -.5571441725E-03, -.1790652314E-03, -.6708275760E-04,
     -      -.2766949183E-04, -.1219387286E-04, -.5640039490E-05,
     -      -.2706697160E-05, -.1337270078E-05, -.6763995657E-06,
     -      -.3488264610E-06, -.1828456675E-06, -.9718036154E-07,
     -      -.5227070332E-07/
       DATA (CC2(I,8),I=0,15) /
     -      0.1362840652E+01, -.9286670089E+00, 0.1035511017E+01,
     -      -.9800255299E+00, 0.8315343261E+00, -.7592730522E+00,
     -      0.7612683773E+00, -.7132136226E+00, 0.6074471474E+00,
     -      -.5554352999E+00, 0.5699443221E+00, -.5357525349E+00,
     -      0.4329345822E+00, -.3916820884E+00, 0.4401986003E+00,
     -      -.4197303057E+00/
*** Number of terms in each expansion.
       DATA (NTERM1(I),I=3,8) /6*15/
       DATA (NTERM2(I),I=3,8) /6*15/
*** Z coincides with the centre.
       IF(Z.EQ.0)THEN
*   Results are trivial.
            WW=0
            WD=KAPPA
*** Z is close to the centre.
       ELSEIF(ABS(Z).LT.0.75)THEN
*   Series expansion.
            ZTERM=(KAPPA*Z)**NTUBE
            WDSUM=0.0
            WSUM=CC1(NTERM1(NTUBE),NTUBE)
            DO 10 I=NTERM1(NTUBE)-1,0,-1
            WDSUM=WSUM+ZTERM*WDSUM
            WSUM=CC1(I,NTUBE)+ZTERM*WSUM
10          CONTINUE
*   Return the results.
            WW=KAPPA*Z*WSUM
            WD=KAPPA*(WSUM+NTUBE*ZTERM*WDSUM)
*** Z is close to the edge.
       ELSE
*   First rotate Z nearest to 1.
            AROT=-2*PI*NINT(0.5*ATAN2(AIMAG(Z),REAL(Z))*NTUBE/PI)/
     -           REAL(NTUBE)
            ZZ=Z*CMPLX(COS(AROT),SIN(AROT))
*   Expand in a series.
            ZTERM=(KAPPA*(1-ZZ))**(REAL(NTUBE)/REAL(NTUBE-2))
            WDSUM=0
            WSUM=CC2(NTERM2(NTUBE),NTUBE)
            DO 20 I=NTERM2(NTUBE)-1,0,-1
            WDSUM=WSUM+ZTERM*WDSUM
            WSUM=CC2(I,NTUBE)+ZTERM*WSUM
20          CONTINUE
*   And return the results.
            WW=CMPLX(COS(AROT),-SIN(AROT))*(1-ZTERM*WSUM)
            WD=REAL(NTUBE)*KAPPA*(KAPPA*(1-ZZ))**(2.0/REAL(NTUBE-2))*
     -           (WSUM+ZTERM*WDSUM)/REAL(NTUBE-2)
       ENDIF
       END
CDECK  ID>, PH2.
       REAL FUNCTION PH2(XPOS,YPOS)
*-----------------------------------------------------------------------
*   PH2    - Logarithmic contribution to real single-wire potential,
*            for a doubly priodic wire array.
*   PH2LIM - Entry, PH2LIM(r) corresponds to z on the surface of a wire
*            of (small) radius r.
*
*            Clenshaw's algorithm is used for the evaluation of the sum
*            ZTERM = SIN(ZETA) - P1*SIN(3*ZETA) + P2*SIN(5*ZETA).
*
*  (G.A.Erskine/DD, 14.8.1984; some minor modifications (i) common block
*   /EV2COM/ incorporated in /CELDAT/ (ii) large AIMAG(ZETA) corrected)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX ZETA,ZSIN,ZCOF,ZU,ZUNEW,ZTERM
       REAL PH2LIM,RADIUS
*** Start of the main subroutine, off diagonal elements.
       ZETA=ZMULT*CMPLX(XPOS,YPOS)
       IF(ABS(AIMAG(ZETA)).LT.10.0)THEN
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM=(ZUNEW+ZU)*ZSIN
            PH2=-LOG(CABS(ZTERM))
       ELSE
            PH2=-ABS(AIMAG(ZETA))+CLOG2
       ENDIF
       RETURN
*** Start of the entry PH2LIM, used to calculate diagonal terms.
       ENTRY PH2LIM(RADIUS)
       PH2LIM=-LOG(ABS(ZMULT)*RADIUS*(1.0-3.0*P1+5.0*P2))
       END
CDECK  ID>, E2SUM.
       SUBROUTINE E2SUM(XPOS,YPOS,EX,EY)
*-----------------------------------------------------------------------
*   E2SUM  - Components of the elecrostatic field intensity in a doubly
*            periodic wire array.
*            Clenshaw's algorithm is used for the evaluation of the sums
*                  ZTERM1 = SIN(ZETA) - P1*SIN(3*ZETA) + P2*SIN(5*ZETA),
*                  ZTERM2 = COS(ZETA)- 3 P1*COS(3*ZETA)+ 5P2*COS(5*ZETA)
*   VARIABLES : (XPOS,YPOS): Position in the basic cell at which the
*                            field is to be computed.
*  (Essentially by G.A.Erskine/DD, 14.8.1984)
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       COMPLEX WSUM,ZSIN,ZCOF,ZU,ZUNEW,ZTERM1,ZTERM2,ZETA
       WSUM=0
       DO 10 J=1,NWIRE
       ZETA=ZMULT*CMPLX(XPOS-X(J),YPOS-Y(J))
       IF(AIMAG(ZETA).GT.+15.0)THEN
            WSUM=WSUM-E(J)*ICONS
       ELSEIF(AIMAG(ZETA).LT.-15.0)THEN
            WSUM=WSUM+E(J)*ICONS
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4.0*ZSIN**2-2.0
            ZU=-P1-ZCOF*P2
            ZUNEW=1.0-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3.0*P1-ZCOF*5.0*P2
            ZUNEW=1.0-ZCOF*ZU-5.0*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM=WSUM+E(J)*(ZTERM2/ZTERM1)
       ENDIF
10     CONTINUE
       EX=-REAL(-ZMULT*WSUM)
       EY=AIMAG(-ZMULT*WSUM)
       END
CDECK  ID>, EFCMAT.
       SUBROUTINE EFCMAT(X0,Y0,X1,Y1,DX,DY)
*-----------------------------------------------------------------------
*   EFCMAT - Computes the effective distance between points taking the
*            effects of dielectrica into account.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
*** Compute the dielectricum-weighed x-distance.
       DX=ABS(X1-X0)
       DO 10 I=1,NXMATT
       XM0=MAX(MIN(X0,X1),MIN(XMATT(I,1),XMATT(I,2)))
       XM1=MIN(MAX(X0,X1),MAX(XMATT(I,1),XMATT(I,2)))
       IF(XM1.GE.XM0)DX=DX+(XMATT(I,3)-1.0)*ABS(XM1-XM0)
10     CONTINUE
       DX=SIGN(DX,X1-X0)
*** Compute the dielectricum-weighed x-distance.
       DY=ABS(Y1-Y0)
       DO 20 I=1,NYMATT
       YM0=MAX(MIN(Y0,Y1),MIN(YMATT(I,1),YMATT(I,2)))
       YM1=MIN(MAX(Y0,Y1),MAX(YMATT(I,1),YMATT(I,2)))
       IF(YM1.GE.YM0)DY=DY+(YMATT(I,3)-1.0)*ABS(YM1-YM0)
20     CONTINUE
       DY=SIGN(DY,Y1-Y0)
       END

CDECK  ID>, EFCBGF.
*       SUBROUTINE EFCBGF(XIN,YIN,ZIN,EXBGF,EYBGF,EZBGF,VBGF)
*-----------------------------------------------------------------------
*   EFCBGF - Computes the background field.
*   (Last changed on  6/ 4/98.)
*-----------------------------------------------------------------------
*       implicit none
*       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
*    -         MXPSTR
c       PARAMETER (MXWIRE=  2000,MXSW  =  200)
c      PARAMETER (MXMATT=    10)
c     PARAMETER (MX3D  =   100)
c    PARAMETER (MXPOLE=    10)
c       PARAMETER (MXPSTR=   100)
c       PARAMETER (MXLIST=  1000)
c       PARAMETER (MXGRID=    50)
c       CHARACTER*80 CELLID
c       CHARACTER*3 TYPE
c       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
c     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
c       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
c     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
c    -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
c      INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
c    -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
c     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
c     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
c       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
c     -      U(MXWIRE),DENS(MXWIRE),
c     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
c     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
c     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
c     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
c     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
c     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
c    -      COPLAX,COPLAY,COMATX,COMATY,
c     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
c     -      KAPPA
c       COMPLEX ZMULT,WMAP(MXWIRE)
c       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
c     -      COSPH2,SINPH2,AMP2,
c    -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
c    -      PLSTR1,PLSTR2,
c     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
c     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
c     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
c     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
c     -      N3D,NTERMB,NTERMP,IENBGF,
c     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
c     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
c     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
c     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
c       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
c       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
c     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
c    -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
c     -         LSYNCH
c       INTEGER LUNOUT,JFAIL,JEXMEM
c       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
c     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
c     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
c     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
c       REAL VAR(MXVAR),RES(4),XIN,YIN,ZIN,EXBGF,EYBGF,EZBGF,VBGF,
c     -      EXFMP,EYFMP,EZFMP,VFMP
c       INTEGER MODVAR(MXVAR),MODRES(4),IFAIL,I,NREXP,NVAR,ILOC
c*** Check that there is an entry.
c       IF(IENBGF.LE.0)RETURN
c*** Store the location in the variables.
c       IF(POLAR)THEN
c            CALL CFMRTP(XIN,YIN,VAR(1),VAR(2),1)
c            VAR(3)=ZIN
c       ELSE
c            VAR(1)=XIN
c            VAR(2)=YIN
c            VAR(3)=ZIN
c       ENDIF
c       MODVAR(1)=2
c       MODVAR(2)=2
c       MODVAR(3)=2
c*   Interpolate field map.
c       IF(LBGFMP)THEN
c          write(*,*) 'Field map calculations invalid.'
c       ELSE
c            VAR(4)=0
c            VAR(5)=0
c            VAR(6)=0
c            VAR(7)=0
c            MODVAR(4)=0
c            MODVAR(5)=0
c            MODVAR(6)=0
c            MODVAR(7)=0
c       ENDIF
c*   Set number of variables.
c       NVAR=7
c*** Compute the field.
c       NREXP=4
c       CALL AL2EXE(IENBGF,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
c*   Check the error flag and variable types.
c       IF(IFAIL.NE.0.OR.MODRES(1).NE.2.OR.MODRES(2).NE.2.OR.
c     -      MODRES(3).NE.2.OR.MODRES(4).NE.2)THEN
c            EXBGF=0
c            EYBGF=0
c            EZBGF=0
c            VBGF=0
c            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ EFCBGF DEBUG   :'',
c     -           '' Invalid background field result:''/
c     -           26X,''IFAIL='',I2,'', modes: '',4I2)')
c     -           IFAIL,(MODRES(I),I=1,4)
c*   Convert to polar internal field vectors if required.
c       ELSEIF(POLAR)THEN
c            VBGF=RES(1)
c            EXBGF=RES(2)*EXP(XIN)
c            EYBGF=RES(3)*EXP(XIN)
c            EZBGF=RES(4)
*   Or simply store the results.
cc       ELSE
c            VBGF=RES(1)
c            EXBGF=RES(2)
c            EYBGF=RES(3)
c           EZBGF=RES(4)
c       ENDIF
c       END
CDECK  ID>, FFDBG.

       SUBROUTINE EFQA00(IFAIL)
*-----------------------------------------------------------------------
*   EFQA00 - Routine preparing the field calculations by filling the
*            capacitance matrix. This routines handles configurations
*            with not more than one plane in either x or y and not more
*            than one dielectricum in total.
*   VARIABLES : No local variables.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       COMMON /TMPA00/ EPSMT1,EPSMT2
*** Check the configuration of dielectrica is acceptable.
       IF(NXMATT+NYMATT.GT.1.OR.
     -      (NXMATT.EQ.1.AND.XMATT(1,3).EQ.0.AND.XMATT(1,4).EQ.0).OR.
     -      (NYMATT.EQ.1.AND.YMATT(1,3).EQ.0.AND.YMATT(1,4).EQ.0))THEN
            PRINT *,' ###### EFQA00 ERROR   : The configuration of'//
     -           ' dielectrica can not yet be handled ; cell rejected.'
            IFAIL=1
            RETURN
       ELSE
            PRINT *,' ------ EFQA00 MESSAGE : Potentials handled by'//
     -           ' experimental routine.'
       ENDIF
*** Prepare some auxilliary variables for dielectrica.
       YNMATX=.FALSE.
       YNMATY=.FALSE.
       COMATX=0.0
       COMATY=0.0
       EPSMT1=0.0
       EPSMT2=0.0
       IF(NXMATT.EQ.1)THEN
            YNMATX=.TRUE.
            IF(XMATT(1,3).NE.0)COMATX=XMATT(1,2)
            IF(XMATT(1,4).NE.0)COMATX=XMATT(1,1)
            EPSMT1=(1-XMATT(1,5))/(1+XMATT(1,5))
            EPSMT2=2/(1+XMATT(1,5))
       ELSEIF(NYMATT.EQ.1)THEN
            YNMATY=.TRUE.
            IF(YMATT(1,3).NE.0)COMATY=YMATT(1,2)
            IF(YMATT(1,4).NE.0)COMATY=YMATT(1,1)
            EPSMT1=(1-YMATT(1,5))/(1+YMATT(1,5))
            EPSMT2=2/(1+YMATT(1,5))
       ENDIF
*** Loop over all wire combinations.
       DO 10 I=1,NWIRE
       A(I,I)=0.25*D(I)**2
*** Take care of the equipotential planes.
       IF(YNPLAX)A(I,I)=A(I,I)/(2.0*(X(I)-COPLAX))**2
       IF(YNPLAY)A(I,I)=A(I,I)/(2.0*(Y(I)-COPLAY))**2
*** Take care of combinations of equipotential planes.
       IF(YNPLAX.AND.YNPLAY)A(I,I)=4.0*A(I,I)*((X(I)-COPLAX)**2+
     -      (Y(I)-COPLAY)**2)
*** Before adding dielectrica, take the log.
       A(I,I)=-0.5*LOG(A(I,I))
*** One x-dielectricum.
       IF(YNMATX)THEN
*   Dielectricum charge.
            A(I,I)=A(I,I)-EPSMT1*LOG(2*ABS(X(I)-COMATX))
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,I)=A(I,I)+
     -           EPSMT1*LOG(ABS(2*COPLAX-2*COMATX+X(I)))
            IF(YNPLAY)A(I,I)=A(I,I)+
     -           EPSMT1*0.5*LOG((2*COMATX-X(I))**2+(2*COPLAY-Y(I))**2)
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,I)=A(I,I)-
     -           EPSMT1*0.5*LOG((2*COPLAX-2*COMATX+X(I))**2+
     -           (2*COPLAY-Y(I))**2)
*** One y-dielectricum.
       ELSEIF(YNMATY)THEN
*   Dielectricum charge.
            A(I,I)=A(I,I)-EPSMT1*LOG(2*ABS(Y(I)-COMATY))
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,I)=A(I,I)+
     -           EPSMT1*0.5*LOG((2*COPLAX-X(I))**2+(2*COMATY-Y(I))**2)
            IF(YNPLAY)A(I,I)=A(I,I)+
     -           EPSMT1*LOG(ABS(2*COPLAY-2*COMATY+Y(I)))
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,I)=A(I,I)-
     -           EPSMT1*0.5*LOG((2*COPLAX-X(I))**2+
     -           (2*COPLAY-2*COMATY+Y(I))**2)
       ENDIF
*** Loop over all other wires for the off-diagonal elements.
       DO 20 J=I+1,NWIRE
       A(I,J)=(X(I)-X(J))**2+(Y(I)-Y(J))**2
*** Take care of equipotential planes.
       IF(YNPLAX)A(I,J)=A(I,J)/((X(I)+X(J)-2.*COPLAX)**2+(Y(I)-Y(J))**2)
       IF(YNPLAY)A(I,J)=A(I,J)/((X(I)-X(J))**2+(Y(I)+Y(J)-2.*COPLAY)**2)
*** Take care of pairs of equipotential planes in different directions.
       IF(YNPLAX.AND.YNPLAY)A(I,J)=
     -      A(I,J)*((X(I)+X(J)-2.*COPLAX)**2+(Y(I)+Y(J)-2.*COPLAY)**2)
*** Take the log before adding dielectrica.
       A(I,J)=-0.5*LOG(A(I,J))
*** One x-dielectricum.
       IF(YNMATX)THEN
*   Dielectricum charge.
            A(I,J)=A(I,J)-EPSMT1*0.5*
     -           LOG((X(I)+X(J)-2*COMATX)**2+(Y(I)-Y(J))**2)
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((2*COPLAX-2*COMATX+X(I)-X(J))**2+
     -           (Y(I)-Y(J))**2)
            IF(YNPLAY)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((X(I)+X(J)-2*COMATX)**2+
     -           (Y(I)+Y(J)-2*COPLAY)**2)
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,J)=A(I,J)-
     -           EPSMT1*0.5*LOG((2*COPLAX-2*COMATX+X(I)-X(J))**2+
     -           (Y(I)+Y(J)-2*COPLAY)**2)
*** One y-dielectricum.
       ELSEIF(YNMATY)THEN
*   Dielectricum charge.
            A(I,J)=A(I,J)-EPSMT1*0.5*
     -           LOG((X(I)-X(J))**2+(Y(I)+Y(J)-2*COMATY)**2)
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((X(I)+X(J)-2*COPLAX)**2+
     -           (Y(I)+Y(J)-2*COMATY)**2)
            IF(YNPLAY)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((X(I)-X(J))**2+
     -           (2*COPLAY-2*COMATY+Y(I)-Y(J))**2)
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,J)=A(I,J)-
     -           EPSMT1*0.5*LOG((X(I)+X(J)-2*COPLAX)**2+
     -           (2*COPLAY-2*COMATY+Y(I)-Y(J))**2)
       ENDIF
*** Copy this to A(J,I) since the capacitance matrix is symmetric.
       A(J,I)=A(I,J)
20     CONTINUE
10     CONTINUE
*** Call CHARGE to calculate the charges really.
       CALL CHARGE(IFAIL)
       END
CDECK  ID>, EFDA00.
       SUBROUTINE EFDA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFDA00 - Subroutine performing the actual field calculations in case
*            the charges have been prepared by EFQA00.
*   VARIABLES : R2         : Potential before taking -log(sqrt(...))
*               EX, EY     : x,y-component of the electric field.
*               ETOT       : Magnitude of electric field.
*               VOLT       : Potential.
*               EXHELP etc : One term in the series to be summed.
*               (XPOS,YPOS): The position where the field is calculated.
*-----------------------------------------------------------------------
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       COMMON /TMPA00/ EPSMT1,EPSMT2
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
*** Calculate the field in case there are no planes.
       R2=(XPOS-X(I))**2+(YPOS-Y(I))**2
       EXHELP=(XPOS-X(I))/R2
       EYHELP=(YPOS-Y(I))/R2
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=X(I)+(XPOS-2.0*COPLAX)
            R2PLAN=XXMIRR**2+(YPOS-Y(I))**2
            EXHELP=EXHELP-XXMIRR/R2PLAN
            EYHELP=EYHELP-(YPOS-Y(I))/R2PLAN
            R2=R2/R2PLAN
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            YYMIRR=Y(I)+(YPOS-2.0*COPLAY)
            R2PLAN=(XPOS-X(I))**2+YYMIRR**2
            EXHELP=EXHELP-(XPOS-X(I))/R2PLAN
            EYHELP=EYHELP-YYMIRR/R2PLAN
            R2=R2/R2PLAN
       ENDIF
*** Take care of pairs of planes.
       IF(YNPLAX.AND.YNPLAY)THEN
            R2PLAN=XXMIRR**2+YYMIRR**2
            EXHELP=EXHELP+XXMIRR/R2PLAN
            EYHELP=EYHELP+YYMIRR/R2PLAN
            R2=R2*R2PLAN
       ENDIF
*** Calculate the electric field and the potential.
       IF((YNMATX.AND.((XPOS.LT.COMATX.AND.XMATT(1,3).NE.0).OR.
     -      (XPOS.GT.COMATX.AND.XMATT(1,4).NE.0))).OR.
     -      (YNMATY.AND.((YPOS.LT.COMATY.AND.YMATT(1,3).NE.0).OR.
     -      (YPOS.GT.COMATY.AND.YMATT(1,4).NE.0))))THEN
            IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*EPSMT2*LOG(R2)
            EX=EX+E(I)*EPSMT2*EXHELP
            EY=EY+E(I)*EPSMT2*EYHELP
       ELSE
            IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*LOG(R2)
            EX=EX+E(I)*EXHELP
            EY=EY+E(I)*EYHELP
       ENDIF
*** Dielectric mediums, no planes.
       IF(YNMATX.AND.((XPOS.GT.COMATX.AND.XMATT(1,3).NE.0).OR.
     -      (XPOS.LT.COMATX.AND.XMATT(1,4).NE.0)))THEN
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*EPSMT1*0.5*
     -           LOG((XPOS+X(I)-2*COMATX)**2+(YPOS-Y(I))**2)
            EX=EX-E(I)*EPSMT1*0.5*(XPOS+X(I)-2*COMATX)/
     -           SQRT((XPOS+X(I)-2*COMATX)**2+(YPOS-Y(I))**2)
            EY=EY-E(I)*EPSMT1*0.5*(YPOS-Y(I))/
     -           SQRT((XPOS+X(I)-2*COMATX)**2+(YPOS-Y(I))**2)
       ENDIF
       IF(YNMATY.AND.((YPOS.GT.COMATY.AND.YMATT(1,3).NE.0).OR.
     -      (YPOS.LT.COMATY.AND.YMATT(1,4).NE.0)))THEN
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*EPSMT1*0.5*
     -           LOG((YPOS+Y(I)-2*COMATY)**2+(XPOS-X(I))**2)
            EX=EX-E(I)*EPSMT1*0.5*(YPOS+Y(I)-2*COMATY)/
     -           SQRT((YPOS+Y(I)-2*COMATY)**2+(XPOS-X(I))**2)
            EY=EY-E(I)*EPSMT1*0.5*(XPOS-X(I))/
     -           SQRT((YPOS+Y(I)-2*COMATY)**2+(XPOS-X(I))**2)
       ENDIF
*** Finish the loop over the wires.
10     CONTINUE
       END
CDECK  ID>, CFMPTC.
       SUBROUTINE CFMPTC(R,THETA,X,Y,N)
*-----------------------------------------------------------------------
*   CFMPTC - Routine transforming polar to cartesian coordinates.
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL R(*),THETA(*),X(*),Y(*),XI,YI
       INTEGER N,I
*** Loop over the points.
       DO 10 I=1,N
       XI=R(I)*COS(PI*THETA(I)/180.0)
       YI=R(I)*SIN(PI*THETA(I)/180.0)
       X(I)=XI
       Y(I)=YI
10     CONTINUE
       END
CDECK  ID>, CFMRTP.
       SUBROUTINE CFMRTP(RHO,PHI,R,THETA,N)
*-----------------------------------------------------------------------
*   CFMRTP - Routine transforming (r,theta) to (rho,phi) via the map
*            (r,theta)=(exp(rho),180*phi/pi).
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL R(*),THETA(*),RHO(*),PHI(*),RI,THETAI
       INTEGER N,I
*** Loop over the points.
       DO 10 I=1,N
       RI=EXP(RHO(I))
       THETAI=180.0*PHI(I)/PI
       R(I)=RI
       THETA(I)=THETAI
10     CONTINUE
       END
CDECK  ID>, CF2RTC.
       SUBROUTINE CF2RTC(RHO,PHI,X,Y,N)
*-----------------------------------------------------------------------
*   CF2RTC - Routine transforming (rho,phi) to (x,y) via the conformal
*            map (x,y)=exp(rho,phi). This routine may in principle be
*            replaced by any conformal mapping routine.
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X(*),Y(*),RHO(*),PHI(*),XI,YI
       INTEGER I,N
*** Loop over the points.
       DO 10 I=1,N
       XI=EXP(RHO(I))*COS(PHI(I))
       YI=EXP(RHO(I))*SIN(PHI(I))
       X(I)=XI
       Y(I)=YI
10     CONTINUE
       END

CDECK  ID>, CLIP.
       SUBROUTINE CLIP(X0,Y0,X1,Y1,XLL,YLL,XUR,YUR,IFAIL)
*-----------------------------------------------------------------------
*   CLIP   - Routine clipping the line (X0,Y0) to (X1,Y1) to the size of
*            the box formed by (XLL,YLL) (XUR,YUR).
*   VARIABLES : (X0,Y0)    : Begin point of line.
*               (X1,Y1)    : End point of line.
*               (XLL,YLL)  : Lower left hand corner of the box.
*               (XUR,YUR)  : Upper right hand corner of the box.
*-----------------------------------------------------------------------
*** Return on IFAIL=0 if no changes have to be made.
       IFAIL=0
       IF(XLL.LE.X0.AND.X0.LE.XUR.AND.XLL.LE.X1.AND.X1.LE.XUR.AND.
     -    YLL.LE.Y0.AND.Y0.LE.YUR.AND.YLL.LE.Y1.AND.Y1.LE.YUR)RETURN
*** The next few returns are on IFAIL=1.
       IFAIL=1
*** Return with IFAIL=1 if X0 and X1 are out of range.
       IF((X0.LT.XLL.AND.X1.LT.XLL).OR.(X0.GT.XUR.AND.X1.GT.XUR))RETURN
       IF(X0.NE.X1)THEN
*   Adjust X0.
            IF(X0.LT.XLL)THEN
                 Y0=Y0+((Y1-Y0)/(X1-X0))*(XLL-X0)
                 X0=XLL
            ENDIF
            IF(X0.GT.XUR)THEN
                 Y0=Y0+((Y1-Y0)/(X1-X0))*(XUR-X0)
                 X0=XUR
            ENDIF
*   Adjust X1.
            IF(X1.LT.XLL)THEN
                 Y1=Y1+((Y1-Y0)/(X1-X0))*(XLL-X1)
                 X1=XLL
            ENDIF
            IF(X1.GT.XUR)THEN
                 Y1=Y1+((Y1-Y0)/(X1-X0))*(XUR-X1)
                 X1=XUR
            ENDIF
       ENDIF
*** Return with an IFAIL=1 if Y0 and Y1 are out of range.
       IF((Y0.LT.YLL.AND.Y1.LT.YLL).OR.(Y0.GT.YUR.AND.Y1.GT.YUR))RETURN
       IF(Y0.NE.Y1)THEN
*   Adjust Y0.
            IF(Y0.LT.YLL)THEN
                 X0=X0+((X1-X0)/(Y1-Y0))*(YLL-Y0)
                 Y0=YLL
            ENDIF
            IF(Y0.GT.YUR)THEN
                 X0=X0+((X1-X0)/(Y1-Y0))*(YUR-Y0)
                 Y0=YUR
            ENDIF
*   Adjust y1.
            IF(Y1.LT.YLL)THEN
                 X1=X1+((X1-X0)/(Y1-Y0))*(YLL-Y1)
                 Y1=YLL
            ENDIF
            IF(Y1.GT.YUR)THEN
                 X1=X1+((X1-X0)/(Y1-Y0))*(YUR-Y1)
                 Y1=YUR
            ENDIF
        ENDIF
*** If begin and end point coincide, return with IFAIL=1.
        IF(X0.EQ.X1.AND.Y0.EQ.Y1)RETURN
*** All is OK, therefore IFAIL=0.
        IFAIL=0
        END

CDECK  ID>, CFMCTP.
       SUBROUTINE CFMCTP(X,Y,R,THETA,N)
*-----------------------------------------------------------------------
*   CFMCTP - Routine transforming cartesian to polar coordinates.
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL R(*),THETA(*),X(*),Y(*),RI,THETAI
       INTEGER N,I
*** Loop over the points.
       DO 10 I=1,N
       IF(X(I).EQ.0.AND.Y(I).EQ.0)THEN
            RI=0
            THETAI=0
       ELSE
            RI=SQRT(X(I)**2+Y(I)**2)
            THETAI=180*ATAN2(Y(I),X(I))/PI
       ENDIF
       R(I)=RI
       THETA(I)=THETAI
10     CONTINUE
       END
CDECK  ID>, INTUBE.
       SUBROUTINE INTUBE(X,Y,A,N,ILOC)
*-----------------------------------------------------------------------
*   INTUBE - Determines whether a point is located inside a polygon.
*            ILOC is set to +1 if outside, 0 if inside and -1 if the
*            arguments are not valid.
*   (Last changed on 18/ 3/01.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       REAL X,Y,A,PHI
       INTEGER N,ILOC
*** Special case: x=y=0
       IF(X.EQ.0.AND.Y.EQ.0)THEN
            ILOC=0
*** Special case: round tube.
       ELSEIF(N.EQ.0)THEN
            IF(X**2+Y**2.GT.A**2)THEN
                 ILOC=1
            ELSE
                 ILOC=0
            ENDIF
*** Illegal number of edges.
       ELSEIF(N.LT.0.OR.N.EQ.1.OR.N.EQ.2)THEN
            PRINT *,' ###### INTUBE ERROR   : Invalid number of'//
     -           ' edges received (N=',N,').'
            ILOC=-1
*** Truely polygonal tubes.
       ELSE
*   Reduce angle to the first sector.
            PHI=ATAN2(Y,X)
            IF(PHI.LT.0.0)PHI=PHI+2*PI
            PHI=PHI-REAL(2)*PI*INT(0.5*N*PHI/PI)/REAL(N)
*   Compare the length to the local radius.
            IF((X**2+Y**2)*COS(PI/REAL(N)-PHI)**2.GT.
     -           A**2*COS(PI/REAL(N))**2)THEN
                 ILOC=1
            ELSE
                 ILOC=0
            ENDIF
       ENDIF
       END

       SUBROUTINE CELCHK(IFAIL)
*-----------------------------------------------------------------------
*   CELCHK - Subroutine checking the wire positions, The equipotential
*            planes and the periodicity. Two planes having different
*            voltages are not allowed to have a common line, wires are
*            not allowed to be at the same position etc.
*            This routine determines also the cell-dimensions.
*   VARIABLE  : WRONG(I)   : .TRUE. if wire I will be removed
*               IPLAN.     : Number of wires with coord > than plane .
*   (Last changed on 16/ 2/05.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       LOGICAL LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       LOGICAL WRONG(MXWIRE),WRMATX(MXMATT),WRMATY(MXMATT),OK,
     -      SETX,SETY,SETZ,SETV
       REAL CONEW1,CONEW2,CONEW3,CONEW4,COHLP,VTHLP,XNEW,YNEW,
     -      XPRT,YPRT,XPRTI,YPRTI,XPRTJ,YPRTJ,XSEPAR,YSEPAR,
     -      XAUX1,YAUX1,XAUX2,YAUX2,SMIN,SMAX,GAP
       INTEGER IFAIL,I,J,IPLAN1,IPLAN2,IPLAN3,IPLAN4,IWIRE,NXOLD,NYOLD,
     -      IOUT,NELEM,NHLP
       CHARACTER LABHLP
*** Checks on the planes, first move the x planes to the basic cell.
       IF(PERX)THEN
            CONEW1=COPLAN(1)-SX*ANINT(COPLAN(1)/SX)
            CONEW2=COPLAN(2)-SX*ANINT(COPLAN(2)/SX)
*   Check that they are not one on top of the other.
            IF(YNPLAN(1).AND.YNPLAN(2).AND.CONEW1.EQ.CONEW2)THEN
                 IF(CONEW1.GT.0.0)THEN
                      CONEW1=CONEW1-SX
                 ELSE
                      CONEW2=CONEW2+SX
                 ENDIF
            ENDIF
*   Print some warnings if the planes have been moved.
            IF((CONEW1.NE.COPLAN(1).AND.YNPLAN(1)).OR.
     -           (CONEW2.NE.COPLAN(2).AND.YNPLAN(2)))
     -           PRINT *,' ------ CELCHK MESSAGE : The planes in x or'//
     -                ' r are moved to the basic period; this should'//
     -                ' not affect the results.'
            COPLAN(1)=CONEW1
            COPLAN(2)=CONEW2
*   Two planes should now be separated by SX, cancel PERX if not.
            IF(YNPLAN(1).AND.YNPLAN(2).AND.
     -           ABS(COPLAN(2)-COPLAN(1)).NE.SX)THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The separation of'//
     -                ' the x or r planes does not match the period;'//
     -                ' the periodicity is cancelled.'
                 PERX=.FALSE.
                 OK=.FALSE.
            ENDIF
*   If there are two planes left, they should have identical V's.
            IF(YNPLAN(1).AND.YNPLAN(2).AND.VTPLAN(1).NE.VTPLAN(2))THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The voltages of'//
     -                ' the two x (or r) planes differ;'//
     -                ' the periodicity is cancelled.'
                 PERX=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
**  Idem for the y or r planes: move them to the basic period.
       IF(PERY)THEN
            CONEW3=COPLAN(3)-SY*ANINT(COPLAN(3)/SY)
            CONEW4=COPLAN(4)-SY*ANINT(COPLAN(4)/SY)
*   Check that they are not one on top of the other.
            IF(YNPLAN(3).AND.YNPLAN(4).AND.CONEW3.EQ.CONEW4)THEN
                 IF(CONEW3.GT.0.0)THEN
                      CONEW3=CONEW3-SY
                 ELSE
                      CONEW4=CONEW4+SY
                 ENDIF
            ENDIF
*   Print some warnings if the planes have been moved.
            IF((CONEW3.NE.COPLAN(3).AND.YNPLAN(3)).OR.
     -           (CONEW4.NE.COPLAN(4).AND.YNPLAN(4)))
     -           PRINT *,' ------ CELCHK MESSAGE : The planes in y'//
     -                ' are moved to the basic period; this should'//
     -                ' not affect the results.'
            COPLAN(3)=CONEW3
            COPLAN(4)=CONEW4
*   Two planes should now be separated by SY, cancel PERY if not.
            IF(YNPLAN(3).AND.YNPLAN(4).AND.
     -           ABS(COPLAN(4)-COPLAN(3)).NE.SY)THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The separation of'//
     -                ' the two y planes does not match the period;'//
     -                ' the periodicity is cancelled.'
                 PERY=.FALSE.
                 OK=.FALSE.
            ENDIF
*   If there are two planes left, they should have identical V's.
            IF(YNPLAN(3).AND.YNPLAN(4).AND.VTPLAN(3).NE.VTPLAN(4))THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The voltages of'//
     -                ' the two y planes differ;'//
     -                ' the periodicity is cancelled.'
                 PERY=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
**  Check that there is no voltage conflict of crossing planes.
       DO 20 I=1,2
       DO 10 J=3,4
       IF(YNPLAN(I).AND.YNPLAN(J).AND.VTPLAN(I).NE.VTPLAN(J))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Conflicting potential of',
     -           ' 2 crossing planes; one y (or phi) plane is removed.'
            YNPLAN(J)=.FALSE.
            OK=.FALSE.
       ENDIF
10     CONTINUE
20     CONTINUE
**  Make sure the the coordinates of the planes are properly ordered.
       DO 30 I=1,3,2
       IF(YNPLAN(I).AND.YNPLAN(I+1))THEN
            IF(COPLAN(I).EQ.COPLAN(I+1))THEN
                 PRINT *,' !!!!!! CELCHK WARNING : Two planes are on'//
     -                ' top of each other; one of them is removed.'
                 YNPLAN(I+1)=.FALSE.
                 OK=.FALSE.
            ENDIF
            IF(COPLAN(I).GT.COPLAN(I+1))THEN
                 IF(LDEBUG)PRINT *,' ++++++ CELCHK DEBUG   : Planes ',I,
     -                ' and ',I+1,' are interchanged.'
                 COHLP=COPLAN(I)
                 COPLAN(I)=COPLAN(I+1)
                 COPLAN(I+1)=COHLP
                 VTHLP=VTPLAN(I)
                 VTPLAN(I)=VTPLAN(I+1)
                 VTPLAN(I+1)=VTHLP
                 LABHLP=PLATYP(I)
                 PLATYP(I)=PLATYP(I+1)
                 PLATYP(I+1)=LABHLP
                 DO 300 J=1,MXPSTR
                 SMIN=PLSTR1(I,J,1)
                 SMAX=PLSTR1(I,J,2)
                 GAP= PLSTR1(I,J,3)
                 LABHLP=PSLAB1(I,J)
                 PLSTR1(I,J,1)=PLSTR1(I+1,J,1)
                 PLSTR1(I,J,2)=PLSTR1(I+1,J,2)
                 PLSTR1(I,J,3)=PLSTR1(I+1,J,3)
                 PSLAB1(I,J)=PSLAB1(I+1,J)
                 PLSTR1(I+1,J,1)=SMIN
                 PLSTR1(I+1,J,2)=SMAX
                 PLSTR1(I+1,J,3)=GAP
                 PSLAB1(I+1,J)=LABHLP
                 SMIN=PLSTR2(I,J,1)
                 SMAX=PLSTR2(I,J,2)
                 GAP= PLSTR2(I,J,3)
                 LABHLP=PSLAB2(I,J)
                 PLSTR2(I,J,1)=PLSTR2(I+1,J,1)
                 PLSTR2(I,J,2)=PLSTR2(I+1,J,2)
                 PLSTR2(I,J,3)=PLSTR2(I+1,J,3)
                 PSLAB2(I,J)=PSLAB2(I+1,J)
                 PLSTR2(I+1,J,1)=SMIN
                 PLSTR2(I+1,J,2)=SMAX
                 PLSTR2(I+1,J,3)=GAP
                 PSLAB2(I+1,J)=LABHLP
300              CONTINUE
                 NHLP=NPSTR1(I)
                 NPSTR1(I)=NPSTR1(I+1)
                 NPSTR1(I+1)=NHLP
                 NHLP=NPSTR2(I)
                 NPSTR2(I)=NPSTR2(I+1)
                 NPSTR2(I+1)=NHLP
            ENDIF
       ENDIF
30     CONTINUE
*** Checks on the wires, start moving them to the basic x period.
       IF(PERX)THEN
            DO 40 I=1,NWIRE
            XNEW=X(I)-SX*ANINT(X(I)/SX)
            IF(ANINT(X(I)/SX).NE.0)THEN
                 XPRT=X(I)
                 YPRT=Y(I)
                 IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            ENDIF
            X(I)=XNEW
40          CONTINUE
       ENDIF
**  In case of y-periodicity, all wires should be in the first y-period.
       IF(TUBE.AND.PERY)THEN
            DO 55 I=1,NWIRE
            XNEW=X(I)
            YNEW=Y(I)
            CALL CFMCTP(XNEW,YNEW,XNEW,YNEW,1)
            IF(ANINT((PI*YNEW)/(SY*180.0)).NE.0)THEN
                 YNEW=YNEW-180*SY*ANINT((PI*YNEW)/(SY*180.0))/PI
                 CALL CFMPTC(XNEW,YNEW,X(I),Y(I),1)
            ENDIF
55          CONTINUE
       ELSEIF(PERY)THEN
            DO 50 I=1,NWIRE
            YNEW=Y(I)-SY*ANINT(Y(I)/SY)
            IF(ANINT(Y(I)/SY).NE.0)THEN
                 XPRT=X(I)
                 YPRT=Y(I)
                 IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            ENDIF
            Y(I)=YNEW
50          CONTINUE
       ENDIF
*** Make sure the plane numbering is standard: P1 wires P2, P3 wires P4.
       IPLAN1=0
       IPLAN2=0
       IPLAN3=0
       IPLAN4=0
       DO 60 I=1,NWIRE
       IF(YNPLAN(1).AND.X(I).LE.COPLAN(1))IPLAN1=IPLAN1+1
       IF(YNPLAN(2).AND.X(I).LE.COPLAN(2))IPLAN2=IPLAN2+1
       IF(YNPLAN(3).AND.Y(I).LE.COPLAN(3))IPLAN3=IPLAN3+1
       IF(YNPLAN(4).AND.Y(I).LE.COPLAN(4))IPLAN4=IPLAN4+1
60     CONTINUE
*   find out whether smaller (-1) or larger (+1) coord. are to be kept.
       IF(YNPLAN(1).AND.YNPLAN(2))THEN
            IF(IPLAN1.GT.NWIRE/2)THEN
                 YNPLAN(2)=.FALSE.
                 IPLAN1=-1
            ELSE
                 IPLAN1=+1
            ENDIF
            IF(IPLAN2.LT.NWIRE/2)THEN
                 YNPLAN(1)=.FALSE.
                 IPLAN2=+1
            ELSE
                 IPLAN2=-1
            ENDIF
       ENDIF
       IF(YNPLAN(1).AND..NOT.YNPLAN(2))THEN
            IF(IPLAN1.GT.NWIRE/2)THEN
                 IPLAN1=-1
            ELSE
                 IPLAN1=+1
            ENDIF
       ENDIF
       IF(YNPLAN(2).AND..NOT.YNPLAN(1))THEN
            IF(IPLAN2.LT.NWIRE/2)THEN
                 IPLAN2=+1
            ELSE
                 IPLAN2=-1
            ENDIF
       ENDIF
       IF(YNPLAN(3).AND.YNPLAN(4))THEN
            IF(IPLAN3.GT.NWIRE/2)THEN
                 YNPLAN(4)=.FALSE.
                 IPLAN3=-1
            ELSE
                 IPLAN3=+1
            ENDIF
            IF(IPLAN4.LT.NWIRE/2)THEN
                 YNPLAN(3)=.FALSE.
                 IPLAN4=+1
            ELSE
                 IPLAN4=-1
            ENDIF
       ENDIF
       IF(YNPLAN(3).AND..NOT.YNPLAN(4))THEN
            IF(IPLAN3.GT.NWIRE/2)THEN
                 IPLAN3=-1
            ELSE
                 IPLAN3=+1
            ENDIF
       ENDIF
       IF(YNPLAN(4).AND..NOT.YNPLAN(3))THEN
            IF(IPLAN4.LT.NWIRE/2)THEN
                 IPLAN4=+1
            ELSE
                 IPLAN4=-1
            ENDIF
       ENDIF
*   Adapt the numbering of the planes if necessary.
       IF(IPLAN1.EQ.-1)THEN
            YNPLAN(1)=.FALSE.
            YNPLAN(2)=.TRUE.
            COPLAN(2)=COPLAN(1)
            VTPLAN(2)=VTPLAN(1)
            PLATYP(2)=PLATYP(1)
            DO 310 J=1,MXPSTR
            PLSTR1(2,J,1)=PLSTR1(1,J,1)
            PLSTR1(2,J,2)=PLSTR1(1,J,2)
            PLSTR1(2,J,3)=PLSTR1(1,J,3)
            PSLAB1(2,J)=  PSLAB1(1,J)
            PLSTR2(2,J,1)=PLSTR2(1,J,1)
            PLSTR2(2,J,2)=PLSTR2(1,J,2)
            PLSTR2(2,J,3)=PLSTR2(1,J,3)
            PSLAB2(2,J)=  PSLAB2(1,J)
310         CONTINUE
            NPSTR1(2)=    NPSTR1(1)
            NPSTR2(2)=    NPSTR2(1)
            NPSTR1(1)=    0
            NPSTR2(1)=    0
       ENDIF
       IF(IPLAN2.EQ.+1)THEN
            YNPLAN(2)=.FALSE.
            YNPLAN(1)=.TRUE.
            COPLAN(1)=COPLAN(2)
            VTPLAN(1)=VTPLAN(2)
            PLATYP(1)=PLATYP(2)
            DO 320 J=1,MXPSTR
            PLSTR1(1,J,1)=PLSTR1(2,J,1)
            PLSTR1(1,J,2)=PLSTR1(2,J,2)
            PLSTR1(1,J,3)=PLSTR1(2,J,3)
            PSLAB1(1,J)=  PSLAB1(2,J)
            PLSTR2(1,J,1)=PLSTR2(2,J,1)
            PLSTR2(1,J,2)=PLSTR2(2,J,2)
            PLSTR2(1,J,3)=PLSTR2(2,J,3)
            PSLAB2(1,J)=  PSLAB2(2,J)
320         CONTINUE
            NPSTR1(1)=    NPSTR1(2)
            NPSTR2(1)=    NPSTR2(2)
            NPSTR1(2)=    0
            NPSTR2(2)=    0
       ENDIF
       IF(IPLAN3.EQ.-1)THEN
            YNPLAN(3)=.FALSE.
            YNPLAN(4)=.TRUE.
            COPLAN(4)=COPLAN(3)
            VTPLAN(4)=VTPLAN(3)
            PLATYP(4)=PLATYP(3)
            DO 330 J=1,MXPSTR
            PLSTR1(4,J,1)=PLSTR1(3,J,1)
            PLSTR1(4,J,2)=PLSTR1(3,J,2)
            PLSTR1(4,J,3)=PLSTR1(3,J,3)
            PSLAB1(4,J)=  PSLAB1(3,J)
            PLSTR2(4,J,1)=PLSTR2(3,J,1)
            PLSTR2(4,J,2)=PLSTR2(3,J,2)
            PLSTR2(4,J,3)=PLSTR2(3,J,3)
            PSLAB2(4,J)=  PSLAB2(3,J)
330         CONTINUE
            NPSTR1(4)=    NPSTR1(3)
            NPSTR2(4)=    NPSTR2(3)
            NPSTR1(3)=    0
            NPSTR2(3)=    0
       ENDIF
       IF(IPLAN4.EQ.+1)THEN
            YNPLAN(4)=.FALSE.
            YNPLAN(3)=.TRUE.
            COPLAN(3)=COPLAN(4)
            VTPLAN(3)=VTPLAN(4)
            PLATYP(3)=PLATYP(4)
            DO 340 J=1,MXPSTR
            PLSTR1(3,J,1)=PLSTR1(4,J,1)
            PLSTR1(3,J,2)=PLSTR1(4,J,2)
            PLSTR1(3,J,3)=PLSTR1(4,J,3)
            PSLAB1(3,J)=  PSLAB1(4,J)
            PLSTR2(3,J,1)=PLSTR2(4,J,1)
            PLSTR2(3,J,2)=PLSTR2(4,J,2)
            PLSTR2(3,J,3)=PLSTR2(4,J,3)
            PSLAB2(3,J)=  PSLAB2(4,J)
340         CONTINUE
            NPSTR1(3)=    NPSTR1(4)
            NPSTR2(3)=    NPSTR2(4)
            NPSTR1(4)=    0
            NPSTR2(4)=    0
       ENDIF
*** Second pass for the wires, check position relative to the planes.
       DO 70 I=1,NWIRE
       WRONG(I)=.FALSE.
       IF(YNPLAN(1).AND.X(I)-.5*D(I).LE.COPLAN(1))WRONG(I)=.TRUE.
       IF(YNPLAN(2).AND.X(I)+.5*D(I).GE.COPLAN(2))WRONG(I)=.TRUE.
       IF(YNPLAN(3).AND.Y(I)-.5*D(I).LE.COPLAN(3))WRONG(I)=.TRUE.
       IF(YNPLAN(4).AND.Y(I)+.5*D(I).GE.COPLAN(4))WRONG(I)=.TRUE.
       IF(TUBE)THEN
            CALL INTUBE(X(I),Y(I),COTUBE,NTUBE,IOUT)
             IF(IOUT.NE.0)THEN
               WRONG(I)=.TRUE.
               OK=.FALSE.
            ENDIF
       ELSEIF(WRONG(I))THEN
            XPRT=X(I)
            YPRT=Y(I)
            IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            OK=.FALSE.
       ELSEIF((PERX.AND.D(I).GE.SX).OR.(PERY.AND.D(I).GE.SY))THEN
            XPRT=X(I)
            YPRT=Y(I)
            IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            WRONG(I)=.TRUE.
            OK=.FALSE.
       ENDIF
70     CONTINUE
**  Check the wire spacing.
       DO 90 I=1,NWIRE
       IF(WRONG(I))GOTO 90
       DO 80 J=I+1,NWIRE
       IF(WRONG(J))GOTO 80
       IF(TUBE)THEN
            IF(PERY)THEN
                 CALL CFMCTP(X(I),Y(I),XAUX1,YAUX1,1)
                 CALL CFMCTP(X(J),Y(J),XAUX2,YAUX2,1)
                 YAUX1=YAUX1-SY*ANINT(YAUX1/SY)
                 YAUX2=YAUX2-SY*ANINT(YAUX2/SY)
                 CALL CFMPTC(XAUX1,YAUX1,XAUX1,YAUX1,1)
                 CALL CFMPTC(XAUX2,YAUX2,XAUX2,YAUX2,1)
                 XSEPAR=XAUX1-XAUX2
                 YSEPAR=YAUX1-YAUX2
            ELSE
                 XSEPAR=X(I)-X(J)
                 YSEPAR=Y(I)-Y(J)
            ENDIF
       ELSE
            XSEPAR=ABS(X(I)-X(J))
            IF(PERX)XSEPAR=XSEPAR-SX*ANINT(XSEPAR/SX)
            YSEPAR=ABS(Y(I)-Y(J))
            IF(PERY)YSEPAR=YSEPAR-SY*ANINT(YSEPAR/SY)
       ENDIF
       IF(XSEPAR**2+YSEPAR**2.LT.0.25*(D(I)+D(J))**2)THEN
            XPRTI=X(I)
            YPRTI=Y(I)
            XPRTJ=X(J)
            YPRTJ=Y(J)
            IF(POLAR)CALL CFMRTP(XPRTI,YPRTI,XPRTI,YPRTI,1)
            IF(POLAR)CALL CFMRTP(XPRTJ,YPRTJ,XPRTJ,YPRTJ,1)
            WRONG(J)=.TRUE.
            OK=.FALSE.
       ENDIF
80     CONTINUE
90     CONTINUE
**  Remove the wires which are not acceptable for one reason or another.
       IWIRE=NWIRE
       NWIRE=0
       DO 100 I=1,IWIRE
       IF(.NOT.WRONG(I))THEN
            NWIRE=NWIRE+1
            X(NWIRE)=X(I)
            Y(NWIRE)=Y(I)
            D(NWIRE)=D(I)
            V(NWIRE)=V(I)
            WIRTYP(NWIRE)=WIRTYP(I)
       ENDIF
100    CONTINUE
*** Ensure that some elements are left.
       NELEM=NWIRE
       IF(YNPLAN(1))NELEM=NELEM+1
       IF(YNPLAN(2))NELEM=NELEM+1
       IF(YNPLAN(3))NELEM=NELEM+1
       IF(YNPLAN(4))NELEM=NELEM+1
       IF(TUBE) THEN
	NELEM=NELEM+1
       ENDIF
       IF(NELEM.LT.2)THEN
            PRINT *,' ###### CELCHK ERROR   : Neither a field map,'//
     -           ' nor at least 2 elements; cell rejected.'
            OK=.FALSE.
            RETURN
       ENDIF
*** Check dielectrica, initialise the remove flag for the slabs.
       DO 150 I=1,MXMATT
       WRMATX(I)=.FALSE.
       WRMATY(I)=.FALSE.
150    CONTINUE
*   Check overlapping x-slabs and kill slabs outside the planes.
       DO 160 I=1,NXMATT
       IF(WRMATX(I))GOTO 160
       DO 170 J=I+1,NXMATT
       IF(WRMATX(J))GOTO 170
       IF(XMATT(I,3).NE.0.AND.XMATT(J,3).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to -infinity in x ; one is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(XMATT(I,4).NE.0.AND.XMATT(J,4).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to +infinity in x ; one is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ELSEIF((XMATT(I,3).NE.0.AND.XMATT(I,2).GT.XMATT(J,1)).OR.
     -      (XMATT(I,4).NE.0.AND.XMATT(I,1).LT.XMATT(J,2)).OR.
     -      (XMATT(J,3).NE.0.AND.XMATT(J,2).GT.XMATT(I,1)).OR.
     -      (XMATT(J,4).NE.0.AND.XMATT(J,1).LT.XMATT(I,2)))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric'//
     -           ' semi-infinite x-slab overlaps partially'
            PRINT *,'                         with another x-slab.'//
     -           ' One of the slabs is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(XMATT(I,3).EQ.0.AND.XMATT(I,4).EQ.0.AND.
     -      XMATT(J,3).EQ.0.AND.XMATT(J,4).EQ.0.AND.
     -      ((XMATT(I,1)-XMATT(J,1))*(XMATT(J,1)-XMATT(I,2)).GT.0.OR.
     -      (XMATT(I,1)-XMATT(J,2))*(XMATT(J,2)-XMATT(I,2)).GT.0))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two finite dielectric'//
     -           ' x-slabs overlap (in part) ; one is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ENDIF
170    CONTINUE
       IF(WRMATX(I))GOTO 160
       IF((YNPLAN(1).AND.
     -      (XMATT(I,3).NE.0.OR.COPLAN(1).GT.XMATT(I,1))).OR.
     -      (YNPLAN(2).AND.
     -      (XMATT(I,4).NE.0.OR.COPLAN(2).LT.XMATT(I,2))))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric x-slab'//
     -           ' covers a plane ; it is removed.'
            WRMATX(I)=.TRUE.
            OK=.FALSE.
       ENDIF
       IF(WRMATX(I))GOTO 160
       IF(PERX.AND.(XMATT(I,3).NE.0.OR.XMATT(I,4).NE.0.OR.
     -      ABS(XMATT(I,1)-XMATT(I,2)).GT.SX))THEN
            PRINT *,' !!!!!! CELCHK WARNING : The dielectric x-slab'//
     -           ' from (',XMATT(I,1),' to ',XMATT(I,2),')'
            PRINT *,'                         covers more than one'//
     -           ' x-period ; it is removed.'
            WRMATX(I)=.TRUE.
            OK=.FALSE.
       ENDIF
160    CONTINUE
*   Check overlapping y-slabs and kill slabs outside the planes.
       DO 180 I=1,NYMATT
       IF(WRMATY(I))GOTO 180
       DO 190 J=I+1,NYMATT
       IF(WRMATY(J))GOTO 190
       IF(YMATT(I,3).NE.0.AND.YMATT(J,3).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to -infinity in y ; one is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(YMATT(I,4).NE.0.AND.YMATT(J,4).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to +infinity in y ; one is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ELSEIF((YMATT(I,3).NE.0.AND.YMATT(I,2).GT.YMATT(J,1)).OR.
     -      (YMATT(I,4).NE.0.AND.YMATT(I,1).LT.YMATT(J,2)).OR.
     -      (YMATT(J,3).NE.0.AND.YMATT(J,2).GT.YMATT(I,1)).OR.
     -      (YMATT(J,4).NE.0.AND.YMATT(J,1).LT.YMATT(I,2)))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric'//
     -           ' semi-infinite y-slab overlaps partially'
            PRINT *,'                         with another y-slab.'//
     -           ' One of the slabs is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(YMATT(I,3).EQ.0.AND.YMATT(I,4).EQ.0.AND.
     -      YMATT(J,3).EQ.0.AND.YMATT(J,4).EQ.0.AND.
     -      ((YMATT(I,1)-YMATT(J,1))*(YMATT(J,1)-YMATT(I,2)).GT.0.OR.
     -      (YMATT(I,1)-YMATT(J,2))*(YMATT(J,2)-YMATT(I,2)).GT.0))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two finite dielectric'//
     -           ' y-slabs overlap (in part) ; one is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ENDIF
190    CONTINUE
       IF(WRMATY(I))GOTO 180
       IF((YNPLAN(3).AND.
     -      (YMATT(I,3).NE.0.OR.COPLAN(3).GT.YMATT(I,1))).OR.
     -      (YNPLAN(4).AND.
     -      (YMATT(I,4).NE.0.OR.COPLAN(4).LT.YMATT(I,2))))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric y-slab'//
     -           ' covers a plane ; it is removed.'
            WRMATY(I)=.TRUE.
            OK=.FALSE.
       ENDIF
       IF(WRMATY(I))GOTO 180
       IF(PERX.AND.(YMATT(I,3).NE.0.OR.YMATT(I,4).NE.0.OR.
     -      ABS(YMATT(I,1)-YMATT(I,2)).GT.SX))THEN
            PRINT *,' !!!!!! CELCHK WARNING : The dielectric y-slab'//
     -           ' from (',YMATT(I,1),' to ',YMATT(I,2),')'
            PRINT *,'                         covers more than one'//
     -           ' x-period ; it is removed.'
            WRMATY(I)=.TRUE.
            OK=.FALSE.
       ENDIF
180    CONTINUE
*   And finally crossing slabs with different epsilons.
       DO 200 I=1,NXMATT
       IF(WRMATX(I))GOTO 200
       DO 210 J=1,NYMATT
       IF(WRMATY(J))GOTO 210
       IF(ABS(XMATT(I,5)-YMATT(J,5)).GT.1.0E-5*(1.0+ABS(XMATT(I,5))+
     -      ABS(YMATT(J,5))))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric x-slab'//
     -           ' crosses a y-slab but has a'
            PRINT *,'                         different dielectric'//
     -           ' constant; the x-slab is removed.'
            WRMATX(I)=.TRUE.
            OK=.FALSE.
       ENDIF
210    CONTINUE
200    CONTINUE
*   Remove slabs, first x, than y.
       NXOLD=NXMATT
       NXMATT=0
       DO 220 I=1,NXOLD
       IF(WRMATX(I))GOTO 220
       NXMATT=NXMATT+1
       DO 230 J=1,5
       XMATT(NXMATT,J)=XMATT(I,J)
230    CONTINUE
220    CONTINUE
       NYOLD=NYMATT
       NYMATT=0
       DO 240 I=1,NYOLD
       IF(WRMATY(I))GOTO 240
       NYMATT=NYMATT+1
       DO 250 J=1,5
       YMATT(NYMATT,J)=YMATT(I,J)
250    CONTINUE
240    CONTINUE
*** Determine maximum and minimum coordinates and potentials.
       SETX=.FALSE.
       SETY=.FALSE.
       SETZ=.FALSE.
       SETV=.FALSE.
       XMIN=0
       XMAX=0
       YMIN=0
       YMAX=0
       ZMIN=0
       ZMAX=0
       VMIN=0
       VMAX=0
*   Loop over the wires.
       DO 120 I=1,NWIRE
       IF(SETX)THEN
            XMIN=MIN(XMIN,X(I)-D(I)/2)
            XMAX=MAX(XMAX,X(I)+D(I)/2)
       ELSE
            XMIN=X(I)-D(I)/2
            XMAX=X(I)+D(I)/2
            SETX=.TRUE.
       ENDIF
       IF(SETY)THEN
            YMIN=MIN(YMIN,Y(I)-D(I)/2)
            YMAX=MAX(YMAX,Y(I)+D(I)/2)
       ELSE
            YMIN=Y(I)-D(I)/2
            YMAX=Y(I)+D(I)/2
            SETY=.TRUE.
       ENDIF
       IF(SETZ)THEN
            ZMIN=MIN(ZMIN,-U(I)/2)
            ZMAX=MAX(ZMAX,+U(I)/2)
       ELSE
            ZMIN=-U(I)/2
            ZMAX=+U(I)/2
            SETZ=.TRUE.
       ENDIF
       IF(SETV)THEN
            VMIN=MIN(VMIN,V(I))
            VMAX=MAX(VMAX,V(I))
       ELSE
            VMIN=V(I)
            VMAX=V(I)
            SETV=.TRUE.
       ENDIF
120    CONTINUE
*   Consider the planes.
       DO 130 I=1,4
       IF(YNPLAN(I))THEN
            IF(I.LE.2)THEN
                 IF(SETX)THEN
                      XMIN=MIN(XMIN,COPLAN(I))
                      XMAX=MAX(XMAX,COPLAN(I))
                 ELSE
                      XMIN=COPLAN(I)
                      XMAX=COPLAN(I)
                      SETX=.TRUE.
                 ENDIF
            ELSE
                 IF(SETY)THEN
                      YMIN=MIN(YMIN,COPLAN(I))
                      YMAX=MAX(YMAX,COPLAN(I))
                 ELSE
                      YMIN=COPLAN(I)
                      YMAX=COPLAN(I)
                      SETY=.TRUE.
                 ENDIF
            ENDIF
            IF(SETV)THEN
                 VMIN=MIN(VMIN,VTPLAN(I))
                 VMAX=MAX(VMAX,VTPLAN(I))
            ELSE
                 VMIN=VTPLAN(I)
                 VMAX=VTPLAN(I)
                 SETV=.TRUE.
            ENDIF
       ENDIF
130    CONTINUE
*   Consider the dielectrica.
       DO 260 I=1,NXMATT
       IF(XMATT(I,3).EQ.0)THEN
            IF(SETX)THEN
                 XMIN=MIN(XMIN,XMATT(I,1))
                 XMAX=MAX(XMAX,XMATT(I,1))
            ELSE
                 XMIN=XMATT(I,1)
                 XMAX=XMATT(I,1)
                 SETX=.TRUE.
            ENDIF
       ENDIF
       IF(XMATT(I,4).EQ.0)THEN
            IF(SETX)THEN
                 XMIN=MIN(XMIN,XMATT(I,2))
                 XMAX=MAX(XMAX,XMATT(I,2))
            ELSE
                 XMIN=XMATT(I,2)
                 XMAX=XMATT(I,2)
                 SETX=.TRUE.
            ENDIF
       ENDIF
260    CONTINUE
       DO 270 I=1,NYMATT
       IF(YMATT(I,3).EQ.0)THEN
            IF(SETY)THEN
                 YMIN=MIN(YMIN,YMATT(I,1))
                 YMAX=MAX(YMAX,YMATT(I,1))
            ELSE
                 YMIN=YMATT(I,1)
                 YMAX=YMATT(I,1)
                 SETY=.TRUE.
            ENDIF
       ENDIF
       IF(YMATT(I,4).EQ.0)THEN
            IF(SETY)THEN
                 YMIN=MIN(YMIN,YMATT(I,2))
                 YMAX=MAX(YMAX,YMATT(I,2))
            ELSE
                 YMIN=YMATT(I,2)
                 YMAX=YMATT(I,2)
                 SETY=.TRUE.
            ENDIF
       ENDIF
270    CONTINUE
*   Consider the tube.
       IF(TUBE)THEN
            XMIN=-1.1*COTUBE
            XMAX=+1.1*COTUBE
            SETX=.TRUE.
            YMIN=-1.1*COTUBE
            YMAX=+1.1*COTUBE
            SETY=.TRUE.
            VMIN=MIN(VMIN,VTTUBE)
            VMAX=MAX(VMAX,VTTUBE)
            SETV=.TRUE.
       ENDIF
**  In case of x-periodicity, XMAX-XMIN should be SX,
       IF(PERX.AND.SX.GT.(XMAX-XMIN))THEN
            XMIN=-SX/2.0
            XMAX=SX/2.0
            SETX=.TRUE.
       ENDIF
*   in case of y-periodicity, YMAX-YMIN should be SY,
       IF(PERY.AND.SY.GT.(YMAX-YMIN))THEN
            YMIN=-SY/2.0
            YMAX=SY/2.0
            SETY=.TRUE.
       ENDIF
*   in case the cell is polar, the y range should be < 2 pi.
       IF(POLAR.AND.YMAX-YMIN.GE.2.0*PI)THEN
            YMIN=-PI
            YMAX=+PI
            SETY=.TRUE.
       ENDIF
**  Fill in missing dimensions.
       IF(SETX.AND.XMIN.NE.XMAX.AND.(YMIN.EQ.YMAX.OR..NOT.SETY))THEN
            YMIN=YMIN-ABS(XMAX-XMIN)/2
            YMAX=YMAX+ABS(XMAX-XMIN)/2
            SETY=.TRUE.
       ENDIF
       IF(SETY.AND.YMIN.NE.YMAX.AND.(XMIN.EQ.XMAX.OR..NOT.SETX))THEN
            XMIN=XMIN-ABS(YMAX-YMIN)/2
            XMAX=XMAX+ABS(YMAX-YMIN)/2
            SETX=.TRUE.
       ENDIF
       IF(.NOT.SETZ)THEN
            ZMIN=-(ABS(XMAX-XMIN)+ABS(YMAX-YMIN))/4
            ZMAX=+(ABS(XMAX-XMIN)+ABS(YMAX-YMIN))/4
            SETZ=.TRUE.
       ENDIF
*   Ensure that all dimensions are now set.
       IF(.NOT.(SETX.AND.SETY.AND.SETZ))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Unable to establish'//
     -           ' default dimensions in all directions; use AREA.'
            OK=.FALSE.
       ENDIF
*** Check that at least some different voltages are present.
       IF(VMIN.EQ.VMAX.OR..NOT.SETV)THEN
            PRINT *,' ###### CELCHK ERROR   : All potentials in the'//
     -           ' cell are the same; there is no point in going on.'
            OK=.FALSE.
            RETURN
       ENDIF
*** Take action on the warnings if requested.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### CELCHK ERROR   : Cell declared to be'//
     -           ' unuseable because of the above warnings.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### CELCHK ERROR   : Program terminated'//
     -           ' because of the above warnings.'
            RETURN
       ENDIF
*** Cell seems to be alright since it passed all critical tests.
       IFAIL=0
*** Print the amount of CPU time used.
       END

      SUBROUTINE DISPLAYCELDAT(IFAIL)
*-----------------------------------------------------------------------
*     displayCelDat - Debugging subroutine
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXGRID=    50)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       LOGICAL LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      BOLTZ=1.380658E-23)
       INTEGER IFAIL,I,J
       INTEGER K
       WRITE(*,*) ZMULT
       IF(YNPLAN(1)) THEN 
          WRITE(*,*) 'YNPLAN(1) TRUE'
       ELSE 
          WRITE(*,*) 'YNPLAN(1) FALSE' 
       ENDIF
       IF(YNPLAN(2)) THEN 
          WRITE(*,*) 'YNPLAN(2) TRUE'
       ELSE 
          WRITE(*,*) 'YNPLAN(2) FALSE' 
       ENDIF
       IF(YNPLAN(3)) THEN 
          WRITE(*,*) 'YNPLAN(3) TRUE'
       ELSE 
          WRITE(*,*) 'YNPLAN(3) FALSE' 
       ENDIF
       IF(YNPLAN(4)) THEN 
          WRITE(*,*) 'YNPLAN(4) TRUE'
       ELSE 
          WRITE(*,*) 'YNPLAN(4) FALSE' 
       ENDIF
      
       IF(PERX) THEN 
          WRITE(*,*) 'PERX TRUE'
       ELSE 
          WRITE(*,*) 'PERX FALSE' 
       ENDIF

       IF(PERY) THEN 
          WRITE(*,*) 'PERY TRUE'
       ELSE 
          WRITE(*,*) 'PERY FALSE' 
       ENDIF
       
       IF(PERZ) THEN 
          WRITE(*,*)'PERZ TRUE'
       ELSE 
          WRITE(*,*) 'PERZ FALSE' 
       ENDIF
       
       IF(YNPLAX) THEN 
          WRITE(*,*)'YNPLAX TRUE'
       ELSE 
          WRITE(*,*) 'YNPLAX FALSE' 
       ENDIF
       
       IF(YNPLAY) THEN 
          WRITE(*,*)'YNPLAY TRUE'
       ELSE 
          WRITE(*,*) 'YNPLAY FALSE' 
       ENDIF
       
       IF(YNMATX) THEN 
          WRITE(*,*)'YNMATX TRUE'
       ELSE 
          WRITE(*,*) 'YNMATX FALSE' 
       ENDIF
       
       IF(YNMATY) THEN 
          WRITE(*,*)'YNMATY TRUE'
       ELSE 
          WRITE(*,*) 'YNMATY FALSE' 
       ENDIF
       
       IF(POLAR) THEN 
          WRITE(*,*)'POLAR TRUE'
       ELSE 
          WRITE(*,*) 'POLAR FALSE' 
       ENDIF
      
       IF(TUBE) THEN 
          WRITE(*,*)'TUBE TRUE'
       ELSE 
          WRITE(*,*) 'TUBE FALSE' 
       ENDIF
      
       IF(PERMX) THEN 
          WRITE(*,*)'PERMX TRUE'
       ELSE 
          WRITE(*,*) 'PERMX FALSE' 
       ENDIF
      
       IF(PERMY) THEN 
          WRITE(*,*)'PERMY TRUE'
       ELSE 
          WRITE(*,*) 'PERMY FALSE' 
       ENDIF
      
       IF(PERMZ) THEN 
          WRITE(*,*)'PERMZ TRUE'
       ELSE 
          WRITE(*,*) 'PERMZ FALSE' 
       ENDIF
      
       IF(PERAX) THEN 
          WRITE(*,*)'PERAX TRUE'
       ELSE 
          WRITE(*,*) 'PERAX FALSE' 
       ENDIF
      
       IF(PERAY) THEN 
          WRITE(*,*)'PERAY TRUE'
       ELSE 
          WRITE(*,*) 'PERAY FALSE' 
       ENDIF
 
       IF(PERAZ) THEN 
          WRITE(*,*)'PERAZ TRUE'
       ELSE 
          WRITE(*,*) 'PERAZ FALSE' 
       ENDIF
      
       IF(PERRX) THEN 
          WRITE(*,*)'PERRX TRUE'
       ELSE 
          WRITE(*,*) 'PERRX FALSE' 
       ENDIF
      
       IF(PERRY) THEN 
          WRITE(*,*)'PERRY TRUE'
       ELSE 
          WRITE(*,*) 'PERRY FALSE' 
       ENDIF
      
       IF(PERRZ) THEN 
          WRITE(*,*)'PERRZ TRUE'
       ELSE 
          WRITE(*,*) 'PERRZ FALSE' 
       ENDIF
       
       DO 17 I=1,MXWIRE
          IF(CNALSO(I))THEN 
             WRITE(*,*) 'CNALSO (', I,') TRUE'
       ELSE 
          WRITE(*,*) 'CNALSO (', I,') FALSE' 
       ENDIF

 17    CONTINUE
      
       IF(LBGFMP) THEN 
          WRITE(*,*)'LBGFMP TRUE'
       ELSE 
          WRITE(*,*) 'LBGFMP FALSE' 
       ENDIF

       DO 18 J=1, MXWIRE
          WRITE(*,*)INDSW(J)
 18    CONTINUE

       WRITE(*,*) NWIRE
       WRITE(*,*) NSW
       WRITE(*,*) ICTYPE
       WRITE(*,*) MODE
       WRITE(*,*) NTUBE
       WRITE(*,*) MTUBE
       WRITE(*,*) NXMATT
       WRITE(*,*) NYMATT
       WRITE(*,*) N3D
       WRITE(*,*) NTERMB
       WRITE(*,*) NTERMP
       WRITE(*,*) IENBGF

       DO 19 J=1, 5
          WRITE(*,*) INDPLA(J)
          WRITE(*,*) NPSTR1(J)
          WRITE(*,*) NPSTR2(J)
 19    CONTINUE
      
       DO 21 I=1, 5
          DO 33 J=1, MXPSTR
             WRITE(*,*) INDST1(I,J)
             WRITE(*,*) INDST2(I,J)
 33          CONTINUE
 21    CONTINUE

       DO 22 J=1, MXWIRE 
          WRITE(*,*) X(J)
          WRITE(*,*) Y(J)
          WRITE(*,*) V(J)
          WRITE(*,*) E(J)
          WRITE(*,*) D(J)
          WRITE(*,*) W(J)
          WRITE(*,*) U(J)
          WRITE(*,*) DENS(J)
          WRITE(*,*) COSPH2(J)
          WRITE(*,*) SINPH2(J)
          WRITE(*,*) AMP2(J)
 22    CONTINUE

       DO 23 I=1, 4
          WRITE(*,*) COPLAN(I)
          WRITE(*,*) VTPLAN(I)
 23    CONTINUE
     
       DO 24 J=1, MXMATT
          DO 25 I=1, 5
             WRITE(*,*) XMATT(J,I)
             WRITE(*,*) YMATT(J,I)
 25       CONTINUE
 24    CONTINUE
       DO 26, I=1, MX3D     
          WRITE(*,*) X3D(I)
          WRITE(*,*) Y3D(I)
          WRITE(*,*) Z3D(I)
          WRITE(*,*) E3D(I)
 26    CONTINUE
           
       WRITE(*,*) DOWN(1)
       WRITE(*,*) DOWN(2)
       WRITE(*,*) DOWN(3)
      
       DO 27 I=1,5
          DO 28 J=1,MXPSTR
             DO 29 K=1,3
                WRITE(*,*) PLSTR1(I,J,K)
                WRITE(*,*) PLSTR2(I,J,K)
 29          CONTINUE
 28       CONTINUE
 27    CONTINUE
           
       WRITE(*,*) COTUBE
       WRITE(*,*) VTTUBE
       DO 34 I=1, MXWIRE
          WRITE(*,*) B2SIN(I)
 34    CONTINUE

       WRITE(*,*) P1
       WRITE(*,*) P2
       WRITE(*,*) C1
       WRITE(*,*) XMIN
       WRITE(*,*) YMIN
       WRITE(*,*) ZMIN
       WRITE(*,*) XMAX
       WRITE(*,*) YMAX
       WRITE(*,*) ZMAX
       WRITE(*,*) VMIN
       WRITE(*,*) VMAX
       WRITE(*,*) COPLAX
       WRITE(*,*) COPLAY
       WRITE(*,*) COMATX 
       WRITE(*,*) COMATY
       WRITE(*,*) CORVTA
       WRITE(*,*) CORVTB
       WRITE(*,*) CORVTC
       WRITE(*,*) V0
       WRITE(*,*) SX
       WRITE(*,*) SY
       WRITE(*,*) SZ
       WRITE(*,*) KAPPA
       WRITE(*,*) ZMULT
      
       DO 31 J=1,MXWIRE
          WRITE(*,*) WMAP(J)
 31    CONTINUE
       end      
      
*-----------------------------------------------------------------------
*     Routines from Cernlib
*-----------------------------------------------------------------------

       SUBROUTINE DEQINV(N,A,IDIM,R,IFAIL,K,B)
*-----------------------------------------------------------------------
* DEQINV - Replaces B by the solution of X of A*X=B,
*          and replaces A by its inverse.
* CALLS    DFACT, DFEQN, DFINV
*-----------------------------------------------------------------------
       REAL R(N),T1,T2,T3
       DOUBLE PRECISION A(IDIM,N),B(IDIM,K),DET,TEMP,S,
     -                  B1,B2,C11,C12,C13,C21,C22,C23,C31,C32,C33

*** Test for parameter errors
       IF((N.LT.1).OR.(N.GT.IDIM).OR.(K.LT.1)) GO TO 10
*** Test for N.LE.3
       IF(N.GT.3) GO TO 9
       IFAIL=0
       IF(N.LT.3) GO TO 5
*** N=3 case
*** Compute cofactors
       C11=A(2,2)*A(3,3)-A(2,3)*A(3,2)
       C12=A(2,3)*A(3,1)-A(2,1)*A(3,3)
       C13=A(2,1)*A(3,2)-A(2,2)*A(3,1)
       C21=A(3,2)*A(1,3)-A(3,3)*A(1,2)
       C22=A(3,3)*A(1,1)-A(3,1)*A(1,3)
       C23=A(3,1)*A(1,2)-A(3,2)*A(1,1)
       C31=A(1,2)*A(2,3)-A(1,3)*A(2,2)
       C32=A(1,3)*A(2,1)-A(1,1)*A(2,3)
       C33=A(1,1)*A(2,2)-A(1,2)*A(2,1)
       T1=ABS(SNGL(A(1,1)))
       T2=ABS(SNGL(A(2,1)))
       T3=ABS(SNGL(A(3,1)))
*** Set TEMP=pivot and DET=pivot*DET
       IF(T1.GE.T2) GO TO 1
          IF(T3.GE.T2) GO TO 2
*** Pivot is A21
             TEMP=A(2,1)
             DET=C13*C32-C12*C33
             GO TO 3
    1  IF(T3.GE.T1) GO TO 2
*** Pivot is A11
          TEMP=A(1,1)
          DET=C22*C33-C23*C32
          GO TO 3
*** Pivot is A31
    2     TEMP=A(3,1)
          DET=C23*C12-C22*C13
*** Set elements of inverse in A
    3  IF(DET.EQ.0D0) GO TO 11
       S=TEMP/DET
       A(1,1)=S*C11
       A(1,2)=S*C21
       A(1,3)=S*C31
       A(2,1)=S*C12
       A(2,2)=S*C22
       A(2,3)=S*C32
       A(3,1)=S*C13
       A(3,2)=S*C23
       A(3,3)=S*C33
*** Replace B by AINV*B
       DO 4 J=1,K
          B1=B(1,J)
          B2=B(2,J)
          B(1,J)=A(1,1)*B1+A(1,2)*B2+A(1,3)*B(3,J)
          B(2,J)=A(2,1)*B1+A(2,2)*B2+A(2,3)*B(3,J)
          B(3,J)=A(3,1)*B1+A(3,2)*B2+A(3,3)*B(3,J)
    4  CONTINUE
       RETURN
    5  IF(N.LT.2) GO TO 7
*** N=2 case by Cramers rule
       DET=A(1,1)*A(2,2)-A(1,2)*A(2,1)
       IF(DET.EQ.0D0) GO TO 11
       S=1D0/DET
       C11   =S*A(2,2)
       A(1,2)=-S*A(1,2)
       A(2,1)=-S*A(2,1)
       A(2,2)=S*A(1,1)
       A(1,1)=C11
       DO 6 J=1,K
          B1=B(1,J)
          B(1,J)=C11*B1+A(1,2)*B(2,J)
          B(2,J)=A(2,1)*B1+A(2,2)*B(2,J)
    6  CONTINUE
       RETURN
*** N=1 case
    7  IF(A(1,1).EQ.0D0) GO TO 11
       A(1,1)=1D0/A(1,1)
       DO 8 J=1,K
          B(1,J)=A(1,1)*B(1,J)
    8  CONTINUE
       RETURN
*** N.GT.3 cases. Factorize matrix, invert and solve system
    9  CALL DFACT(N,A,IDIM,R,IFAIL,DET,JFAIL)
       IF(IFAIL.NE.0) RETURN
       CALL DFEQN(N,A,IDIM,R,K,B)
       CALL DFINV(N,A,IDIM,R)
       RETURN
*** Error exits
   10  IFAIL=+1
       RETURN
   11  IFAIL=-1
       RETURN
       END
       
       SUBROUTINE DFACT(N,A,IDIM,IR,IFAIL,DET,JFAIL)
*-----------------------------------------------------------------------
* DFACT
*-----------------------------------------------------------------------       
       INTEGER             IR(*),    IPAIRF
       DOUBLE PRECISION    A(IDIM,*),DET,      ZERO,     ONE,X,Y,TF
       REAL                G1,       G2
       REAL                PIVOTF,   P,        Q,        SIZEF,  T
       DOUBLE PRECISION    S11, S12, DOTF
       IPAIRF(J,K)  =  J*2**12 + K
       PIVOTF(X)    =  ABS(SNGL(X))
       SIZEF(X)     =  ABS(SNGL(X))
       DOTF(X,Y,S11)  =  X * Y + S11
* CERNLIB_NUME2465
*       DATA      G1, G2              /  1.E-1232, 1.E1232  /
* CERNLIB_NUME279
*       DATA      G1, G2              /  1.E-139, 1.E139  /
* CERNLIB_NUME75
*       DATA      G1, G2              /  1.E-37,  1.E37  /
* CERNLIB_NUME38
       DATA      G1, G2              /  1.E-19,  1.E19  /
       DATA      ZERO, ONE           /  0.D0, 1.D0  /
       DATA      NORMAL, IMPOSS      /  0, -1  /
       DATA      JRANGE, JOVER, JUNDER  /  0, +1, -1  /
       IF(IDIM .GE. N  .AND.  N .GT. 0)  GOTO 110
       RETURN
 110   IFAIL  =  NORMAL
       JFAIL  =  JRANGE
       NXCH   =  0
       DET    =  ONE
       DO 144    J  =  1, N
 120      K  =  J
          P  =  PIVOTF(A(J,J))
          IF(J .EQ. N)  GOTO 122
          JP1  =  J+1
          DO 121    I  =  JP1, N
             Q  =  PIVOTF(A(I,J))
             IF(Q .LE. P)  GOTO 121
                K  =  I
                P  =  Q
 121         CONTINUE
          IF(K .NE. J)  GOTO 123
 122      IF(P .GT. 0.)  GOTO 130
             DET    =  ZERO
             IFAIL  =  IMPOSS
             JFAIL  =  JRANGE
             RETURN
 123      DO 124    L  =  1, N
             TF      =  A(J,L)
             A(J,L)  =  A(K,L)
             A(K,L)  =  TF
 124         CONTINUE
          NXCH      =  NXCH + 1
          IR(NXCH)  =  IPAIRF(J,K)
 130      DET     =  DET * A(J,J)
          A(J,J)  =  ONE / A(J,J)
          T  =  SIZEF(DET)
          IF(T .LT. G1)  THEN
             DET    =  ZERO
             IF(JFAIL .EQ. JRANGE)  JFAIL  =  JUNDER
          ELSEIF(T .GT. G2)  THEN
             DET    =  ONE
             IF(JFAIL .EQ. JRANGE)  JFAIL  =  JOVER
          ENDIF
          IF(J .EQ. N)  GOTO 144
          JM1  =  J-1
          JP1  =  J+1
          DO 143   K  =  JP1, N
             S11  =  -A(J,K)
             S12  =  -A(K,J+1)
             IF(J .EQ. 1)  GOTO 142
             DO 141  I  =  1, JM1
                S11  =  DOTF(A(I,K),A(J,I),S11)
                S12  =  DOTF(A(I,J+1),A(K,I),S12)
 141            CONTINUE
 142         A(J,K)    =  -S11 * A(J,J)
             A(K,J+1)  =  -DOTF(A(J,J+1),A(K,J),S12)
 143         CONTINUE
 144      CONTINUE
 150   IF(MOD(NXCH,2) .NE. 0)  DET  =  -DET
       IF(JFAIL .NE. JRANGE)   DET  =  ZERO
       IR(N)  =  NXCH
       RETURN
       END

       SUBROUTINE DFEQN(N,A,IDIM,IR,K,B)
*-----------------------------------------------------------------------
* DFEQN 
*-----------------------------------------------------------------------
       INTEGER             IR(*)
       DOUBLE PRECISION    A(IDIM,*),B(IDIM,*),X,Y,TE
       DOUBLE PRECISION    S21, S22, DOTF
       DOTF(X,Y,S21)  =  X*Y + S21
       IF(IDIM .GE. N  .AND.  N .GT. 0  .AND.  K .GT. 0)  GOTO 210
       RETURN
 210   NXCH  =  IR(N)
       IF(NXCH .EQ. 0)  GOTO 220
       DO 212    M  =  1, NXCH
          IJ  =  IR(M)
          I   =  IJ / 4096
          J   =  MOD(IJ,4096)
          DO 211   L  =  1, K
             TE      =  B(I,L)
             B(I,L)  =  B(J,L)
             B(J,L)  =  TE
 211         CONTINUE
 212      CONTINUE
 220   DO 221    L  =  1, K
          B(1,L)  =  A(1,1)*B(1,L)
 221      CONTINUE
       IF(N .EQ. 1)  GOTO 299
       DO 243    L  =  1, K
          DO 232   I  =  2, N
             IM1  =  I-1
             S21  =  - B(I,L)
             DO 231   J  =  1, IM1
                S21  =  DOTF(A(I,J),B(J,L),S21)
 231            CONTINUE
             B(I,L)  =  - A(I,I)*S21
 232         CONTINUE
          NM1  =  N-1
          DO 242   I  =  1, NM1
             NMI  =  N-I
             S22  =  - B(NMI,L)
             DO 241   J  =  1, I
                NMJP1  =  N - J+1
                S22    =  DOTF(A(NMI,NMJP1),B(NMJP1,L),S22)
 241            CONTINUE
             B(NMI,L)  =  - S22
 242         CONTINUE
 243      CONTINUE
 299   CONTINUE
       RETURN
       END
       
       SUBROUTINE DFINV(N,A,IDIM,IR)
*-----------------------------------------------------------------------
* DFINV 
*-----------------------------------------------------------------------
       INTEGER             IR(*)
       DOUBLE PRECISION    A(IDIM,*),ZERO,     X, Y, TI
       DOUBLE PRECISION    S31, S32, S33, S34, DOTF
       DOTF(X,Y,S31)  =  X*Y + S31
       DATA      ZERO      /  0.D0  /
       IF(IDIM .GE. N  .AND.  N .GT. 0)  GOTO 310
       RETURN
 310   IF(N .EQ. 1)  RETURN
       A(2,1)  =  -A(2,2) * DOTF(A(1,1),A(2,1),ZERO)
       A(1,2)  =  -A(1,2)
       IF(N .EQ. 2)  GOTO 330
       DO 314    I  =  3, N
          IM2  =  I-2
          DO 312 J  =  1, IM2
             S31  =  ZERO
             S32  =  A(J,I)
             DO 311  K  =  J, IM2
                S31  =  DOTF(A(K,J),A(I,K),S31)
                S32  =  DOTF(A(J,K+1),A(K+1,I),S32)
 311            CONTINUE
             A(I,J)  =  -A(I,I) * DOTF(A(I-1,J),A(I,I-1),S31)
             A(J,I)  =  -S32
 312         CONTINUE
          A(I,I-1)  =  -A(I,I) * DOTF(A(I-1,I-1),A(I,I-1),ZERO)
          A(I-1,I)  =  -A(I-1,I)
 314      CONTINUE
 330   NM1  =  N-1
       DO 335   I  =  1, NM1
          NMI  =  N-I
          DO 332   J  =  1, I
             S33  =  A(I,J)
             DO 331   K  =  1, NMI
                S33  =  DOTF(A(I+K,J),A(I,I+K),S33)
 331            CONTINUE
             A(I,J)  =  S33
 332         CONTINUE
          DO 334   J  =  1, NMI
             S34  =  ZERO
             DO 333   K  =  J, NMI
                S34  =  DOTF(A(I+K,I+J),A(I,I+K),S34)
 333            CONTINUE
             A(I,I+J)  =  S34
 334         CONTINUE
 335      CONTINUE
       NXCH  =  IR(N)
       IF(NXCH .EQ. 0)  RETURN
         DO 342 M  =  1, NXCH
          K   =  NXCH - M+1
          IJ  =  IR(K)
          I   =  IJ / 4096
          J   =  MOD(IJ,4096)
          DO 341  K  =  1, N
             TI      =  A(K,I)
             A(K,I)  =  A(K,J)
             A(K,J)  =  TI
 341         CONTINUE
 342      CONTINUE
       RETURN
       END
       
       FUNCTION GAMMA(X)
*-----------------------------------------------------------------------
* GAMMA 
*-----------------------------------------------------------------------
       REAL X
       DOUBLE PRECISION C, U, F, H, ALFA, B1, B2
       INTEGER I
       DIMENSION C(0:15)

       DATA C( 0) /3.65738 77250 83382 44D0/
       DATA C( 1) /1.95754 34566 61268 27D0/
       DATA C( 2) /0.33829 71138 26160 39D0/
       DATA C( 3) /0.04208 95127 65575 49D0/
       DATA C( 4) /0.00428 76504 82129 09D0/
       DATA C( 5) /0.00036 52121 69294 62D0/
       DATA C( 6) /0.00002 74006 42226 42D0/
       DATA C( 7) /0.00000 18124 02333 65D0/
       DATA C( 8) /0.00000 01096 57758 66D0/
       DATA C( 9) /0.00000 00059 87184 05D0/
       DATA C(10) /0.00000 00003 07690 81D0/
       DATA C(11) /0.00000 00000 14317 93D0/
       DATA C(12) /0.00000 00000 00651 09D0/
       DATA C(13) /0.00000 00000 00025 96D0/
       DATA C(14) /0.00000 00000 00001 11D0/
       DATA C(15) /0.00000 00000 00000 04D0/
       
       U=DBLE(X)
       IF(U .LE. 0) THEN
         PRINT *,' GAMMA: Argument is negative'
         H=0
         GO TO 9
       ENDIF
    8  F=1
       IF(U .LT. 3) THEN
         DO 1 I = 1,INT(4-U)
           F=F/U
    1      U=U+1
       ELSE
         DO 2 I = 1,INT(U-3)
           U=U-1
    2      F=F*U
       END IF
       H=U+U-7
       ALFA=H+H
       B1=0
       B2=0
       DO 3 I = 15,0,-1
         B0=C(I)+ALFA*B1-B2
         B2=B1
    3    B1=B0
    9  GAMMA =F*(B0-H*B2)
       RETURN
       END       
      
*       SUBROUTINE DEQINV(N,A,IDIM,R,IFAIL,K,B)
*-----------------------------------------------------------------------
* DEQINV - Replacement for the DEQINV (F010) routine from the KERNLIB
*          at CERN using NAG routines. This routine will only work in
*          the Garfield environment. The input matrix is assumed to be
*          symmetric. If it's also positive definite, Choleski's method
*          is used; if a more approximate implementation of Crout's.
*-----------------------------------------------------------------------
c       INTEGER MXWIRE,MXSW,MXLIST,MXGRID,MXMATT,MXPOLE,MX3D,
c     -         MXPSTR
c       PARAMETER (MXWIRE=  2000,MXSW  =  200)
c       PARAMETER (MXMATT=    10)
c       PARAMETER (MX3D  =   100)
c       PARAMETER (MXPOLE=    10)
c       PARAMETER (MXPSTR=   100)
c       PARAMETER (MXLIST=  1000)
c       PARAMETER (MXGRID=    50)

c       DOUBLE PRECISION A(IDIM,*),R(IDIM),B(IDIM)
c       DOUBLE PRECISION C(MXWIRE+1,MXWIRE+1),EPS,X(MXWIRE+1)

*** Check that the declared dimensions are sufficient.
c       IF(N.GE.IDIM.OR.N.GE.MXWIRE+1)THEN
c            PRINT *,' ###### DEQINV ERROR   : Matrix dimension too',
c     -           ' large, recompile with a MXWIRE > ',N+1
c            IFAIL=1
c            RETURN
c       ENDIF
*** Set the precision
*       EPS=X02AAF(DUMMY)
c       EPS=X02AJF()
*** Perform a Choleski inversion.
c       IFAIL=1
c       CALL F01ACF(N,EPS,A,IDIM,C,MXWIRE+1,R,L,IFAIL)
c       IF(IFAIL.EQ.1)THEN
c            PRINT *,' !!!!!! DEQINV WARNING : The matrix is not'//
c     -           ' pos. def., perhaps due to rouding errors (F01ACF);'
c            PRINT *,'                         An attempt will be made'//
c     -           ' to invert using Crout''s method.'
c            GOTO 100
c       ELSEIF(IFAIL.EQ.2)THEN
c            PRINT *,' !!!!!! DEQINV WARNING : The refinement fails to'//
c     -           ' converge, ie the matrix is ill-conditioned (F01ACF);'
c            PRINT *,'                         An attempt will be made'//
c     -           ' to invert using Crout''s method.'
c            GOTO 100
c       ELSEIF(IFAIL.NE.0.AND.IFAIL.NE.1.AND.IFAIL.NE.2)THEN
c            PRINT *,' !!!!!! DEQINV WARNING : Unidentified NAG error'//
c     -           ' error condition from F01ACF: ',IFAIL,';'
c            PRINT *,'                         An attempt will be made'//
c     -           ' to invert using Crout''s method.'
c            GOTO 100
c       ENDIF
*** Set the correct inverse all over the matrix
c      DO 20 I=2,N+1
c       DO 10 J=1,I-1
c       A(I-1,J)=A(I,J)
c       A(J,I-1)=A(I,J)
c10     CONTINUE
c20     CONTINUE
c*** Skip the next part which is only used if Choleski fails.
c       GOTO 200
c*** Try Crout's method if Choleski fails. First restore matrix.
c100    CONTINUE
c       DO 110 I=1,N
c       DO 120 J=I,N
c       A(J,I)=A(I,J)
c120    CONTINUE
c110    CONTINUE
c*** Next call the Crout, approximate, routine.
c       IFAIL=1
c       CALL F01AAF(A,IDIM,N,C,MXWIRE+1,R,IFAIL)
c       IF(IFAIL.EQ.1)THEN
c            PRINT *,' ###### DEQINV ERROR   : The matrix is (almost)',
c     -           ' singular, perhaps due to rounding errors (F01AAF).'
c            RETURN
c       ELSEIF(IFAIL.NE.0.AND.IFAIL.NE.1)THEN
c            PRINT *,' ###### DEQINV WARNING : Unidentified NAG error',
c     -           ' error condition from F01AAF: ',IFAIL
c            RETURN
c       ENDIF
c       PRINT *,' !!!!!! DEQINV WARNING : Crout''s method succeeded'//
c     -      ' but the results are less accurate (F01AAF).'
c*** Copy the inverted matrix to A.
c       DO 130 I=1,N
c       DO 140 J=1,N
c       A(I,J)=C(I,J)
c140    CONTINUE
c130    CONTINUE
c*** Solve the system of equations.
c200    CONTINUE
c       DO 210 I=1,N
c       X(I)=0
c       DO 220 J=1,N
c       X(I)=X(I)+A(I,J)*B(J)
c220    CONTINUE
c210    CONTINUE
c*** Copy X to B.
c       DO 230 I=1,N
c       B(I)=X(I)
c230    CONTINUE
c       END
