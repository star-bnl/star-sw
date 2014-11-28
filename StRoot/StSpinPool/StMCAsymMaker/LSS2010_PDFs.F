**********************************************************************
*                                                                    *
*         POLARIZED NLO QCD PARTON DENSITIES                         *
*                                                                    *
*         E. LEADER, A.V. SIDOROV AND D.B. STAMENOV                  *
*                                                                    *
*         Phys. Rev. D82 (2010) 114018 [arXiv:1010.0574]             *
*                                                                    *
*         PROBLEMS/QUESTIONS TO sidorov@theor.jinr.ru                *
*         OR TO stamenov@inrne.bas.bg                                *
*                                                                    *
*   Two sets of polarized NLO QCD parton densities corresponding to  *
*   positive and sign changing gluon densities are presented in      * 
*   MS-bar scheme.                                                   *
*                                                                    *
*   The sets of PDFs are obtained from a combined NLO QCD analysis   *
*   of the world polarized inclusive and semi-inclusive DIS data.    *
*                                                                    *
*   Heavy quark thresholds Q(H)=M(H) in the BETA function:           *
*              M(c) = 1.43 GeV,   M(b) = 4.3 GeV.                    *
*                                                                    *
*      NLO:  LAMBDA(3) = 0.366,     LAMBDA(4) = 0.311,               *
*            LAMBDA(5) = 0.224                                       *
*   in the BETA function (for details of calculation of strong       *
*   running coupling alpha_s see the paper).                         *
*                                                                    *
*   INPUT:   ISET = number of the parton set                         *
*             (TO BE DEFINED BY THE USER ):                          *
*            ISET = 1   NEXT-TO-LEADING ORDER (xDelta G > 0)         *
*                      (DATA FILE 'NLO_MS_delGpos.grid' UNIT=11)     *
*                                                                    *
*            ISET = 2   NEXT-TO-LEADING ORDER (sign-changing         *
*                       xDelta G)                                    *
*                      (DATA FILE 'NLO_MS_chsign_delG.grid' UNIT=22) *
*                                                                    *
*            X  = Bjorken-x       (between  1.E-5  and  1)           *
*            Q2 = scale in GeV**2 (between  1.0 and 0.58E6)          *
*                                                                    *
*   OUTPUT:  UUB = x *(DELTA u + DELTA ubar)                         *
*            DDB = x *(DELTA d + DELTA dbar)                         *
*            U   = x * DELTA u                                       *
*            D   = x * DELTA d                                       *
*            UB  = x * DELTA ubar                                    *
*            DB  = x * DELTA dbar                                    *
*            ST  = x * DELTA sbar                                    *
*            GL  = x * DELTA GLUON                                   *
*                                                                    *
*          NOTE: The assumption DELTA s = DELTA sbar is used.        *
*                                                                    *
*                                                                    *
*   COMMON:  The main program or the calling routine has to have     *
*            a common block  COMMON / INTINI / IINI , and  IINI      *
*            has always to be zero when LSS2010 is called for the    *
*            first time or when 'ISET' has been changed.             *
*                                                                    *
**********************************************************************

      SUBROUTINE
     1  LSS2010(ISET,X,Q2,UUB,DDB,U,D,UB,DB,ST,GL)


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSET=2, NPART=8, NX=48, NQ=28, NARG=2)
      DIMENSION XUUBF(NX,NQ), XDDBF(NX,NQ),
     1	XUF(NX,NQ), XDF(NX,NQ), XUBF(NX,NQ), XDBF(NX,NQ),
     1          XSF(NX,NQ), XGF(NX,NQ)
     1,PARTON (NSET,NPART,NQ,NX),
     2      QS(NQ), XB(NX), XT(NARG), NA(NARG), ARRF(NX+NQ)
      COMMON / INTINI / IINI
	      CHARACTER*2  STAR
      CHARACTER*1024 FILENAME
      SAVE XUUBF,XDDBF, XUF, XDF, XUBF, XDBF, XSF, XGF,
     1 	 NA, ARRF


*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 1.0D0, 1.25D0, 1.5D0, 2.D0, 2.5D0,
     1           4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2           1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3           3.2D3, 5.8D3, 1.0D4, 1.8D4, 3.2D4, 5.8D4,
     4           1.0D5, 1.8D5, 3.2D5, 5.8D5 /
       DATA XB /
     1           1.D-5, 1.5D-5, 2.2D-5, 3.2D-5, 4.8D-5, 7.D-5,
     2           1.D-4, 1.5D-4, 2.2D-4, 3.2D-4, 4.8D-4, 7.D-4,
     3           1.D-3, 1.5D-3, 2.2D-3, 3.2D-3, 4.8D-3, 7.D-3,
     4           1.D-2, 1.5D-2, 2.2D-2, 3.2D-2, 5.0D-2, 7.5D-2,
     5           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     6           0.3, 0.325, 0.35, 0.375, 0.4, 0.45,  0.5, 0.55,
     7           0.6, 0.65,  0.7,  0.75,  0.8, 0.85,  0.9, 1.0 /



*...CHECK OF X AND Q2 VALUES :

       IF ( (X.LT.1.0D-5) .OR. (X.GT.1.0D0) ) THEN
           WRITE(6,91)
  91       FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
           STOP
       ENDIF
       IF ( (Q2.LT.1.D0) .OR. (Q2.GT.5.8D5) ) THEN
           WRITE(6,92)
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
           STOP
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
*                                  ( third NUMBER IN THE grid)
*   INPUT:   ISET = number of the parton set
*            ISET = 1 NEXT-TO-LEADING ORDER (xDelta G > 0)
*   FILE - NO. = 11                                     8.1044E-05
*
*            ISET = 2 NEXT-TO-LEADING ORDER (sign changing xDelta G)
*   FILE - NO. = 22                                     1.3483E-04


      IF (IINI.NE.0) GOTO 17
      CALL GETENV('STAR',FILENAME)
      FILENAME = FILENAME(1:LEN_TRIM(FILENAME))//'/StRoot/StSpinPool/StMCAsymMaker'
      IF (ISET.EQ.1) THEN

       IIREAD=11
       FILENAME = FILENAME(1:LEN_TRIM(FILENAME))//'/NLO_MS_delGpos.grid'
       OPEN(UNIT=11,FILE=FILENAME(1:LEN_TRIM(FILENAME)),STATUS='OLD')
      ELSE IF (ISET.EQ.2) THEN
       IIREAD=22
       FILENAME = FILENAME(1:LEN_TRIM(FILENAME))//'/NLO_MS_chsign_delG.grid'
       OPEN(UNIT=22,FILE=FILENAME(1:LEN_TRIM(FILENAME)),STATUS='OLD')

      ELSE
        WRITE(6,93)
  93    FORMAT (2X,'PARTON INTERPOLATION: ISET OUT OF RANGE')
        GOTO 60
      END IF
C

      READ(IIREAD,2004) STAR
 2004 FORMAT (A2)
	 DO 15 N = 1, NQ

       DO 15 M = 1, NX

      if(Iset.eq.1.or.Iset.eq.2              ) then
       READ(IIREAD,190) q2gri, xgri,
     1            	 PARTON(ISET,1,N,M), PARTON(ISET,2,N,M),
     1                   PARTON(ISET,3,N,M), PARTON(ISET,4,N,M),
     1                   PARTON(ISET,5,N,M), PARTON(ISET,6,N,M),
     1                   PARTON(ISET,7,N,M), PARTON(ISET,8,N,M)

 190   FORMAT (2d9.3,8(1pd12.4))

            endif
  15   CONTINUE
C
      IINI = 1
 17   CONTINUE
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1


        XB0 = XB(IX)
        XB1 = 1.D0-XB(IX)


        XUUBF(iX,iQ) = PARTON(ISET,1,IQ,IX) / (XB1**3 * XB0**0.5)
        XDDBF(iX,iQ) = PARTON(ISET,2,IQ,IX) / (XB1**3 * XB0**0.5)
        XUF(iX,iQ) =  PARTON(ISET,3,IQ,IX) / (XB1**3 * XB0**0.5)

        XDF(IX,IQ) =  PARTON(ISET,4,IQ,IX) / (XB1**3 * XB0**0.5)
        XUBF(IX,IQ) =  PARTON(ISET,5,IQ,IX) / (XB1**7 * XB0**0.5)
        XDBF(IX,IQ) =  PARTON(ISET,6,IQ,IX) / (XB1**7 * XB0**0.5)
        XSF(IX,IQ)  =  PARTON(ISET,7,IQ,IX) / (XB1**7 * XB0**0.5)
        XGF(IX,IQ)  =  PARTON(ISET,8,IQ,IX) / (XB1**6 * XB0**3.)


 2001   FORMAT (2e9.3,13(1pe12.4))
              


  20  CONTINUE
        XUUBF(nX,iQ) =0.d0
        XDDBF(nX,iQ) =0.d0
        XUF(NX,IQ) = 0.D0
        XDF(NX,IQ) = 0.D0
        XUBF(NX,IQ) = 0.D0
        XDBF(NX,IQ) = 0.D0
        XSF(NX,IQ)  = 0.D0
        XGF(NX,IQ)  = 0.D0


  10  CONTINUE
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
  16  CONTINUE
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      UUB = DFINT(NARG,XT,NA,ARRF,XUUBF)  * (1.D0-X)**3 * X**0.5
      DDB = DFINT(NARG,XT,NA,ARRF,XDDBF)  * (1.D0-X)**3 * X**0.5
      U  = DFINT(NARG,XT,NA,ARRF,XUF)   * (1.D0-X)**3 * X**0.5
      D  = DFINT(NARG,XT,NA,ARRF,XDF)   * (1.D0-X)**3 * X**0.5
      UB  = DFINT(NARG,XT,NA,ARRF,XUBF)   * (1.D0-X)**7 * X**0.5
      DB  = DFINT(NARG,XT,NA,ARRF,XDBF)   * (1.D0-X)**7 * X**0.5
      ST  = DFINT(NARG,XT,NA,ARRF,XSF)    * (1.D0-X)**7 * X**0.5
      GL = DFINT(NARG,XT,NA,ARRF,XGF)    * (1.D0-X)**6 * X**3.

 60   RETURN
      END
