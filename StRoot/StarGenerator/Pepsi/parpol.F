

c********************************************************************
*********************************************************************
*                                                                   *
*    POLARIZED RADIATIVELY GENERATED LO AND NLO PARTON DENSITIES    *
*                                                                   *
*         M. GLUCK, E. REYA, M. STRATMANN AND W. VOGELSANG,         *
*                   DO-TH 95/13, RAL-TR-95-042                      *
*                  (TO APPEAR IN PHYS. REV. D)                      *       
*          PROBLEMS/QUESTIONS TO VOGELSANG@V2.RL.AC.UK              *
*                 OR  TO STRAT@HAL1.PHYSIK.UNI-DORTMUND.DE          *
*                                                                   *
*   INPUT:   ISET = number of the parton set :                      *
*              ISET = 1  'STANDARD' SCENARIO, NEXT-TO-LEADING ORDER *
*                        (DATA FILE 'STDNLO.GRID' UNIT=11, TO BE    *
*                         DEFINED BY THE USER )                     *
*              ISET = 2  'VALENCE' SCENARIO,  NEXT-TO-LEADING ORDER *
*                        (DATA FILE 'VALNLO.GRID' UNIT=22, TO BE    *
*                         DEFINED BY THE USER )                     *
*              ISET = 3  'STANDARD' SCENARIO, LEADING ORDER         *
*                        (DATA FILE 'STDLO.GRID' UNIT=33, TO BE     *
*                         DEFINED BY THE USER )                     *
*              ISET = 4  'VALENCE' SCENARIO,  LEADING ORDER         *
*                        (DATA FILE 'VALLO.GRID' UNIT=44, TO BE     *
*                         DEFINED BY THE USER )                     *
*                                                                   *
*            X  = Bjorken-x       (between  1.E-4  and  1)          *
*            Q2 = scale in GeV**2 (between  0.4  and   1.E4)        *
*             (for values outside the allowed range the program     *
*              writes a warning and extrapolates to the x and       *
*              Q2 values requested)                                 *
*                                                                   *
*   OUTPUT:  UV = x * ( DELTA u - DELTA u(bar) ),                   *
*            DV = x * ( DELTA d - DELTA d(bar) ),                   *        
*            QB = x * POLARIZED LIGHT SEA,                          *   
*                 QB= x * ( (DELTA U(BAR) + DELTA D(BAR))/2 )       * 
*            ST = x * DELTA STRANGE = x * DELTA STRANGE(BAR)        *     
*            GL = x * DELTA GLUON                                   *
*                                                                   *
*            always x times the distribution is returned            *
*                                                                   *
*            The sets are the result of a combined fit to           *
*            data for the spin asymmetries A_1 (p,n,d)              *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINI / IINI , and  IINI     *
*            has always to be zero when PARPOL is called for the    *
*            first time or when 'ISET' has been changed.            *
*                                                                   *
*********************************************************************
*
      SUBROUTINE PARPOL (ISET, X, Q2, UV, DV, QB, ST, GL)

      IMPLICIT NONE
      
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      real*8 xb1,dfint,xb0,q2,uv,x,st,gl,dv,qb,xb,qs,
     &       parton,arrf,xt,xqbf,xuvf,xdvf,xgf,xsf
      integer n,iq,m,ix,iini,iset,npart,nx,nq,narg
     &        ,na
cywu<<
      PARAMETER (NPART=5, NX=42, NQ=21, NARG=2)
      DIMENSION XUVF(NX,NQ), XDVF(NX,NQ), XQBF(NX,NQ), 
     1          XSF(NX,NQ), XGF(NX,NQ), PARTON (NPART,NQ,NX-1), 
     2          QS(NQ), XB(NX), XT(NARG), NA(NARG), ARRF(NX+NQ) 
      COMMON / INTINI / IINI
      SAVE XUVF, XDVF, XQBF, XSF, XGF, NA, ARRF
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.4D0, 0.75D0, 1.0D0, 1.5D0, 2.5D0, 
     1           4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2           1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3           3.2D3, 5.8D3, 1.0D4 /
       DATA XB / 1.D-4, 1.5D-4, 2.2D-4, 3.2D-4, 4.8D-4, 7.D-4,
     2           1.D-3, 1.5D-3, 2.2D-3, 3.2D-3, 4.8D-3, 7.D-3,
     3           1.D-2, 1.5D-2, 2.2D-2, 3.2D-2, 5.0D-2, 7.5D-2,
     4           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     5           0.3, 0.325, 0.35, 0.375, 0.4, 0.45,  0.5, 0.55,
     6           0.6, 0.65,  0.7,  0.75,  0.8, 0.85,  0.9, 1.0 /
*...CHECK OF X AND Q2 VALUES : 
       IF ( (X.LT.1.0D-4) .OR. (X.GT.1.0D0) ) THEN
           WRITE(6,91) 
  91       FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
C          GOTO 60
       ENDIF
       IF ( (Q2.LT.0.4D0) .OR. (Q2.GT.1.D4) ) THEN
cerwin           WRITE(6,92) 
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
cerwin           write(6,*)'q2=',q2
C          GOTO 60
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
*    FILE - NO. = 11 FOR NLO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.040E-03 )     
*    FILE - NO. = 22 FOR NLO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 9.740E-04 )     
*    FILE - NO. = 33 FOR  LO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.731E-03 )     
*    FILE - NO. = 44 FOR  LO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.846E-03 )     
      IF (IINI.NE.0) GOTO 16
      OPEN(IPLST(3),FILE=CPOL,STATUS='OLD')
C
      DO 15 M = 1, NX-1 
         DO 15 N = 1, NQ
       READ(IPLST(3),90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1                 PARTON(4,N,M), PARTON(5,N,M)
 15   CONTINUE
      CLOSE(IPLST(3))
 90   FORMAT (5(1PE10.3))
C
      IINI = 1
*.... ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0 = XB(IX) 
        XB1 = 1.D0-XB(IX)
        XUVF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**3 * XB0**0.7)
        XDVF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0**0.6)
        XQBF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**7 * XB0**0.3) 
        XSF(IX,IQ)  = PARTON(4,IQ,IX) / (XB1**7 * XB0**0.3) 
        XGF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**10 * XB0**0.3)
  20  CONTINUE
        XUVF(NX,IQ) = 0.D0
        XDVF(NX,IQ) = 0.D0
        XQBF(NX,IQ) = 0.D0
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
      UV = DFINT(NARG,XT,NA,ARRF,XUVF) * (1.D0-X)**3 * X**0.7
      DV = DFINT(NARG,XT,NA,ARRF,XDVF) * (1.D0-X)**4 * X**0.6 
      QB = DFINT(NARG,XT,NA,ARRF,XQBF) * (1.D0-X)**7 * X**0.3
      ST = DFINT(NARG,XT,NA,ARRF,XSF)  * (1.D0-X)**7 * X**0.3
      GL = DFINT(NARG,XT,NA,ARRF,XGF)  * (1.D0-X)**10 * X**0.3
 60   RETURN
      END
