*72********************************************************************* 
      SUBROUTINE PARTON(X,Q2,XPQ,XDPQ)
C *********************************************************************  
C This subroutine gives the parton distributions (polarized 
C and unpolarized) for the polarized leptoproduction. The 
C parton distributions are optimized for polarized scattering.
C The following sets are included
C
C       LST(15) = 101  Schaefer, Phys. Lett. B 208,2 (1988) 175
C                      for comparison with older PEPSI versions
C       LST(15) = 102  free
C       LST(15) = 103  free
C       LST(15) = 104  Schaefer  et al hep-ph/9505306
C                      using Glueck et al. Z. Phys. C 53 (1992) 127
C       LST(15) = 105  Bartelski et al hep-ph/9502271 Set II(p,n)
C       LST(15) = 106  Bartelski et al hep-ph/9502271 Set II(P,n)
C       LST(15) = 107  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP  
C       LST(15) = 108  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C       LST(15) = 109  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C       LST(15) = 110  Gehrmann  et al hep-ph/9512406 Gluon A (LO)
C       LST(15) = 111  Gehrmann  et al hep-ph/9512406 Gluon B (LO)
C       LST(15) = 112  Gehrmann  et al hep-ph/9512406 Gluon C (LO)
C       LST(15) = 113  Gehrmann  et al hep-ph/9512406 Gluon A (LO) + (DGLAP)
C       LST(15) = 114  Gehrmann  et al hep-ph/9512406 Gluon B (LO) + (DGLAP)
C       LST(15) = 115  Gehrmann  et al hep-ph/9512406 Gluon C (LO) + (DGLAP)
C...............................................................................      
C       M. Glueck, E. Reya, M. Stratmann and W. Vogelsang,
C       DO-TH 95/13, RAL-TR-95-042                
C       LST(15) = 116  'standard' scenario, next-to-leading order
C       LST(15) = 117  'valence'  scenario, next-to-leading order 
C       LST(15) = 118  'standard' scenario, leading order
C       LST(15) = 119  'valence'  scenario, leading order
C       LST(15) = 120  Stanley J.Brodsky Nucl.Phys. B441(1995)
C       LST(15) = 121  S.Keler & J.F.Owens  Phys.Lett. B266(1991)
C                                         & Phys.Rev. D19(1994)1199
C       LST(15) = 124  D. de Florian et al., hep-ph/9711440  LO set 1      
C       LST(15) = 125  D. de Florian et al., hep-ph/9711440  LO set 2      
C       LST(15) = 126  D. de Florian et al., hep-ph/9711440  LO set 3      
C       LST(15) = 127  D. de Florian et al., hep-ph/9711440 NLO set 1      
C       LST(15) = 128  D. de Florian et al., hep-ph/9711440 NLO set 2      
C       LST(15) = 129  D. de Florian et al., hep-ph/9711440 NLO set 3      
C       LST(15) = 130  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta u(x) = 0.5 * u(x) and Delta d(x) = 0.
C       LST(15) = 131  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta d(x) = 0.5 * d(x) and Delta u(x) = 0.
C       LST(15) = 132  fit routine. (Is outside the official code.)
C
C   CTEQ4 collaboration: http: 
C       UNPOL: Low Q2 parametrization is the only one used here
C       POL:  BOGUS, du=0.5* u(x) dd=-0.3*d(x)  0.0 all else
C       LST(15) = 133  CTEQ4LQ  
C
C   MRS low Q2
C       UNPOL MRS 95 low Q2
C       POL:  BOGUS, du=0.5* u(x) dd=-0.3*d(x)  0.0 all else
C       LST(15)= 137 MRS low Q2
C       LST(15) = 144   grsv2000 hep-ph/0011215  LO standard scenario     
C       LST(15) = 145   grsv2000 hep-ph/0011215  LO valence scenario     
C       LST(15) = 146   grsv2000 hep-ph/0011215 NLO standard scenario     
C       LST(15) = 147   grsv2000 hep-ph/0011215 NLO valence scenario     
C..............................................................................
C       unpolarized distributions
C
C        LST(15) = 150   cteq5l                   LO 
C        LST(15) = 151   cteq5m                   NLO MSBAR
C        LST(15) = 152   cteq5m1                  NLO MSBAR (update)
C
C        LST(15) = 161  mrs99   cor01  central gluon, a_s    
C        LST(15) = 162  mrs99   cor02  higher gluon        
C        LST(15) = 163  mrs99   cor03  lower gluon       
C        LST(15) = 164  mrs99   cor04  lower a_s      
C        LST(15) = 165  mrs99   cor05  higher a_s     
C        LST(15) = 166  mrs99   cor06  quarks up        
C        LST(15) = 167  mrs99   cor07  quarks down            
C        LST(15) = 168  mrs99   cor08  strange up           
C        LST(15) = 169  mrs99   cor09  strange down          
C        LST(15) = 170  mrs99   cor10  charm up         
C        LST(15) = 171  mrs99   cor11  charm down       
C        LST(15) = 172  mrs99   cor12  larger d/u          
C
C        LST(15) = 173  cteq6l LO
C        LST(15) = 174  cteq6d DIS NLO
C        LST(15) = 175  cteq6m NLO MSBAR              
***********************************************************************

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

!#include "mc_set.inc"
#include "mc_set.inc"

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

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

      
      REAL X,Q2,XPQ(-6:6),XDPQ(-6:6),ULMASS
      DOUBLE PRECISION DX,DQ2      
      REAL XUV, XDV, XS, XG  
      REAL XUB, XDB, XC, XBOT
      REAL XDUV,XDDV,XDS,XDG  
      REAL XDUB,XDDB,XDC

C...Variables for set 101
      REAL VALPHAU,VBETAU,GAMMA
      REAL VALPHAD,VBETAD
      REAL VGAMMA,VC3
      REAL VAU0,VAU1,VAD0,VAD1
      REAL FFU0,FFU1,FFD0,FFD1,FG1P,FG1N

C.. Variables for set 104
      REAL VS,VN,VKA,VSW
      REAL VALPHA,VBETA  
      REAL VKLEINA,VKLEINB
      REAL VA,VB,VC,VD,VE,VF
      REAL VA0,VA10
      REAL MNUCL,WWW2

      REAL XD,XU,XDD,XDU,XDDS,XDUS
      
C...Variables for sets 107-109
      DOUBLE PRECISION MRSUPV,MRSDNV,MRSDSEA,MRSUSEA
      DOUBLE PRECISION MRSSTR,MRSCHM,MRSBOT,MRSGLU
      INTEGER MODE

C...Variables for sets 122-129
      DOUBLE PRECISION DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL,
     &       DXDUV,DXDDV,DXDQBAR,DXDSTR,DXDGLU,
     &       DDXUS,DDXDS
C...Variables for sets 105 and 106
      REAL FDELTA,FM
      REAL VA1,VA2,VA3
      REAL VC1,VC2   

C...Variables for set 110-112
      REAL VETAU ,VETAD, VETAQB, VETAG
      REAL VNORMU,VNORMD,VNORMQB,VNORMG
      REAL VAU,   VAD,   VAQB,   VAG
      REAL VBU,   VBD,   VBQB,   VBG
      REAL VGU,   VGD,   VGQB,   VGG
      REAL VRU,   VRD,   VRQB,   VRG

C...Variables for set 113-115
      INTEGER IFLAG
      DOUBLE PRECISION DXDG,DXDUBAR,DXDDBAR,DXDS
      REAL   XUPDB,XDMUB   

      REAL F2N,F2P
      
C...Variables for sets 133-36
      DOUBLE PRECISION DDXPQ(-6:6)
      INTEGER INITCTEQ,CTEQSET
      DOUBLE PRECISION CTQ4PDF
      
C...Variables for set 116-119
      INTEGER ISET

   
C...Variables for set 120-121
      REAL Y,X1,SS,Z
      REAL AJ(4),BJ(4),CJ(4),RJ(4),W(3),XW(3),WW(3),AAA(6,4)
      REAL CONST0(6,4),CONST1(6,4),CONST2(6,4),CONST3(6,4),
     &     SUMR(2),QG(4),QJ(4)
      REAL AQ(4),BQ(4),CQ(4),DQ(4),ST1(4),ST2(4),
     &     QP(4),QM(4),ST3(4),ST4(4)
      DATA XW/-1.775E-02, -1.E-02, -2.2540E-03/,W/5.556E-03,
     &     8.889E-03, 5.556E-03/,WW/0.3872983, 3.1329002E-02,
     &     4.849505/,AJ/ -0.2, -0.2, -0.3, -0.4/,BJ/ 4.7, 3.7,
     &     8., 8./,CJ/ 0., 2.54, 0., 0./,RJ/-0.9582284, 2.068491,
     &     -0.3953224, 14.54266/,AQ/0.757, 3.784, 0.2897, 2./,
     &     BQ/-0.645, -3.672, -0.2637, -1.25/,CQ/3.23, 2.004,
     &     1., 2./,DQ/-3.118, -1.892, -0.9748, -1.25/,ST1/3., 3.,
     &     5., 4./,ST2/4., 4., 6., 5./,ST3/5., 5., 7., 6./,
     &     ST4/6., 6., 8., 7./      
      DATA CONST0/-0.335  , 3.614 , 0.8673 , 0.0 , 0.0 , 0.0  ,
     &     -0.1612 , 4.667 , 0.0    , 0.0 , 0.0 , 0.0  ,
     &     -1.0   , 7.278 , 0.0    , 0.0 , 0.0 , 0.909,
     &     -1.0   , 5.304 , 0.0    , 0.0 , 0.0 , 3.017/
      DATA CONST1/-0.1097 , .8395 ,-1.6637 , 1.1049, 0.0 , 0.0,
     &     -0.2092 , 0.7951,-1.0232 , 0.8616, 0.0 , 0.0,
     &     -0.3823,-0.7904,-1.6629,-0.0133,0.1211,-0.4023,
     &     -0.9342, 1.4654,-3.9141,9.0176,-5.9602,-4.7347/
      DATA CONST2/-0.002442,-0.02186,0.342,-0.2369,0.0,0.0,
     &     0.02657, 0.1081, 0.05799, 0.153,0.0,0.0,
     &     0.02766, 0.8108,0.5719,0.5299,-0.1739,0.0063,
     &     0.5454,  -1.4292,2.8445,-10.426,7.515,3.3594/
      DATA CONST3/ 0.0    , 0.0 , 0.0 , 0.0  , 0.0 , 0.0  ,
     &     0.0    , 0.0 , 0.0 , 0.0  , 0.0 , 0.0  ,
     &     0.0    , 0.0 , 0.0 , 0.0  , 0.0 , 0.0  ,
     &     -0.1668,0.7569,-0.8411,4.0983,-2.7329,-0.9443/
      DATA INITCTEQ/ 1 /
      DATA CTEQSET/10/
C.. Variables for helium amendment
      REAL PERCS,PERCSP,PERCD
      REAL ADMXS,ADMXSP,ADMXD
      REAL FACTS,FACTSP,FACTD
      REAL PARI11
C
C.. Variables for set 144-147
      DOUBLE PRECISION DXDD,DXDU,DUMMYG1P, DUMMYG1N
      DOUBLE PRECISION DXDUB,DXDDB

C.. Variables for set 150-152
      INTEGER IPARTON
      DOUBLE PRECISION CTQ5PDF

C.. Variable for set 173-175
C.. Use IPARTON from definition above 
      DOUBLE PRECISION CTQ6PDF

      INTEGER I,J,K,ICOUNT,ICOUN1,ICERR
      DATA ICOUNT /0/,ICOUN1/0/,ICERR/0/

      ICOUNT = ICOUNT + 1
      


C  Reset all partondensities
      DO I=-6,6
         XPQ(I)  = 0.
         XDPQ(I) = 0.
      ENDDO

C ***************************************************************************
C
C      set 101 Schaefer, Phys. Lett. B 208,2 (1988) 175
C
C ***************************************************************************


      IF(LST(15).EQ.101) THEN

C    unpolarized part  
            
         VALPHAU = 0.588
         VBETAU  = 2.69 
         XUV = GAMMA(VALPHAU)*GAMMA(VBETAU+1.)/GAMMA(VALPHAU+VBETAU+1.)
         XUV = 2./XUV*X**VALPHAU*(1.-X)**VBETAU
         
         VALPHAD = 1.03
         VBETAD  = 6.87
         XDV = GAMMA(VALPHAD)*GAMMA(VBETAD+1.)/GAMMA(VALPHAD+VBETAD+1.)
         XDV = 1./XDV*X**VALPHAD*(1.-X)**VBETAD
         
         VGAMMA = 14.6  
         VC3    = 0.0147
         XUB    = VC3*(1.+VGAMMA)*(1.-X)**VGAMMA
         
         XDB = XUB
         XS  = XUB
         XC  = 0.02*(1.-X)**19

         XPQ( 1)  = XDV + XDB
         XPQ(-1)  = XDB
         XPQ( 2)  = XUV + XUB
         XPQ(-2)  = XUB
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 
         XPQ( 4)  = XC 
         XPQ(-4)  = XC 

C     polarized part

         VAU0 = 0.12
         VAU1 = 0.3*VAU0
         VAD0 = 7.0*VAU0
         VAD1 = 7.0*VAU1
         VALPHAU = 0.588
         VALPHAD = 1.03 
         
         FFU0 = 1./(1.+ VAU0*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFU1 = 1./(1.+ VAU1*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFD0 = 1./(1.+ VAD0*X**(-VALPHAD)*(1.-X)*(1.-X))
         FFD1 = 1./(1.+ VAD1*X**(-VALPHAD)*(1.-X)*(1.-X))
         FG1P =  0.5*(4*XUV*FFU0/9.
     &        - XDV*(4*FFU0+4*FFU1-FFD0+2*FFD1)/27.)
         FG1N =  0.5*( XUV*FFD0/9.
     &        - XDV*(-4*FFU0+8*FFU1+FFD0+FFD1)/27.)
         
         XDUV = 6*(4*FG1P-FG1N)/5.

         VAU0 = 0.12
         VAU1 = 0.3*VAU0
         VAD0 = 7*VAU0  
         VAD1 = 7*VAU1  
         
         VALPHAU = 0.588
         VALPHAD = 1.03 
         
         FFU0 = 1./(1.+ VAU0*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFU1 = 1./(1.+ VAU1*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFD0 = 1./(1.+ VAD0*X**(-VALPHAD)*(1.-X)*(1.-X))
         FFD1 = 1./(1.+ VAD1*X**(-VALPHAD)*(1.-X)*(1.-X))
         FG1P =  0.5*(4*XUV*FFU0/9.
     &        - XDV*( 4*FFU0+4*FFU1-FFD0+2*FFD1)/27.)
         FG1N =  0.5*(XUV*FFD0/9.
     &        - XDV*(-4*FFU0+8*FFU1+FFD0+FFD1)/27.)

         XDDV = 6*(4*FG1N-FG1P)/5.
         XDUB  = 0.
         XDDB  = 0.
         
         XDPQ( 1) = XDDV + XDDB
         XDPQ(-1) = XDDB
         XDPQ( 2) = XDUV + XDUB
         XDPQ(-2) = XDUB

      ELSEIF(LST(15).EQ.104) THEN      
C ***************************************************************************
C 
C      set  104  Schaefer  et al hep-ph/9505306
C 
C ***************************************************************************

C     unpolarized part
         IF (((Q2.LT.0.25 ).OR.(Q2.GT.1.E+8)).OR.
     &        (( X.LT.1.E-5).OR.( X.GT. 1.  )))    THEN
            ICOUN1 = ICOUN1 + 1
            IF(ICOUN1.lt.10) THEN
               WRITE(*,*) 'WARNING: (Q^2,x) exceeds allowed range'
            ENDIF
            IF(LST(22).eq.0.or.LST(25).eq.0) THEN 
               XPQ(2) = 1.
            ENDIF
            RETURN
         ENDIF
        
         MNUCL = ULMASS(2212)
         WWW2  = MNUCL*MNUCL + Q2*(1./X-1.) 
         
         VS  = LOG(LOG(Q2/(0.232*0.232))/LOG(0.25/(0.232*0.232)))
         
         VN  =  0.579 + 0.283*VS + 0.047*VS*VS
         VKA =  0.523 - 0.015*VS
         VA  =  2.22  - 0.59*VS - 0.27*VS*VS
         VB  =  5.95  - 6.19*VS + 1.55*VS*VS
         VD  =  3.57  + 0.94*VS - 0.16*VS*VS
         XDV = VN*X**VKA*(1.+VA*SQRT(X)+ VB*X)*(1.-X)**VD
         
         VN  =  0.663 + 0.191*VS - 0.041*VS*VS + 0.031*VS*VS*VS   
         VKA =  0.326
         VA  = -1.97  + 6.74*VS - 1.96*VS*VS
         VB  = 24.4   -20.7*VS  + 4.08*VS*VS
         VD  = 2.86   + 0.7*VS  - 0.02*VS*VS
         XUV = VN*X**VKA*(1.+VA*SQRT(X)+ VB*X)*(1.-X)**VD - XDV
         
         VALPHA  =  1.396
         VBETA   =  1.331
         VKLEINA =  0.412 - 0.171*VS
         VKLEINB =  0.566 - 0.496*VS
         VA      =  0.363
         VB      = -1.196
         VC      =  1.029 + 1.785*VS - 0.459*VS*VS
         VD      =  4.696 + 2.109*VS
         VE      =  3.838 + 1.944*VS
         VF      =  2.845
         XUB = ( X**VKLEINA*(VA+VB*X+VC*X*X)*(LOG(1./X))**VKLEINB
     &        +VS**VALPHA*EXP(-VE+SQRT(VF*VS**VBETA*LOG(1./X))))  
     &        *(1.-X)**VD
         XDB = XUB
         
         VSW = 0.
         VALPHA  =  0.803
         VBETA   =  0.563
         VKLEINA =  2.082 -  0.577*VS
         VA      = -3.055 +  1.024*VS**0.67
         VB      = 27.4   - 20.0*VS**0.154 
         VD      = 6.22
         VE      = 4.33 + 1.408*VS
         VF      = 8.27 - 0.437*VS
         
         IF (VS.GT.VSW) THEN
            XS = (VS-VSW)**VALPHA/(LOG(1./X)**VKLEINA)
     &           *(1. + VA*SQRT(X) + VB*X)*(1.-X)**VD
     &           *EXP(-VE + SQRT(VF*VS**VBETA*LOG(1./X)))
         ELSE
            XS = 0
         ENDIF
         
         VSW = 0.888
         VALPHA  =  1.01
         VBETA   =  0.37
         VKLEINA =  0.  
         VA      =  0.  
         VB      = 4.24  - 0.804*VS
         VD      = 3.46  + 1.076*VS
         VE      = 4.61  + 1.49*VS 
         VF      = 2.555 + 1.961*VS
         
*MM   >>    if w > 2* m_c = 3GeV
         IF (WWW2.GT.9.AND.VS.GT.VSW) THEN
            XC = (VS-VSW)**VALPHA/(LOG(1./X)**VKLEINA)
     &           *(1. + VA*SQRT(X) + VB*X)*(1.-X)**VD
     &           *EXP(-VE + SQRT(VF*VS**VBETA*LOG(1./X)))
         ELSE
            XC =0.
         ENDIF
         
         VSW = 1.351
         VALPHA  =  1.00
         VBETA   =  0.51
         VKLEINA =  0.  
         VA      =  0.  
         VB      = 1.848
         VD      = 2.929  + 1.396*VS
         VE      = 4.71   + 1.514*VS
         VF      = 4.02   + 1.239*VS
         
*MM   >>   if w > 2*m_b = 10 GeV
         IF (WWW2.GT.100.AND.VS.GT.VSW) THEN
            XBOT = (VS-VSW)**VALPHA/(LOG(1./X)**VKLEINA)
     &           *(1. + VA*SQRT(X) + VB*X)*(1.-X)**VD
     &           *EXP(-VE + SQRT(VF*VS**VBETA*LOG(1./X)))
         ELSE
            XBOT =0.
         ENDIF
         
         VALPHA  =  0.558
         VBETA   =  1.218
         VKLEINA =  1.00  - 0.17*VS
         VKLEINB =  0.
         VA      =          4.879*VS - 1.383*VS*VS
         VB      =  25.92 - 28.97*VS + 5.596*VS*VS
         VC      = -25.69 + 23.68*VS - 1.975*VS*VS
         VD      =  2.537 + 1.718*VS + 0.353*VS*VS
         VE      =  0.595 + 2.138*VS
         VF      =  4.066
         XG = ( X**VKLEINA*(VA+VB*X+VC*X*X)
     &        +VS**VALPHA*EXP(-VE+SQRT(VF*VS**VBETA*LOG(1./X))))
     &        *(1-X)**VD
         
         XPQ( 0)  = XG
         XPQ( 1)  = XDV + XDB
         XPQ(-1)  = XDB
         XPQ( 2)  = XUV + XUB
         XPQ(-2)  = XUB
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 
         XPQ( 4)  = XC 
         XPQ(-4)  = XC 
         XPQ( 5)  = XBOT
         XPQ(-5)  = XBOT
         
C     polarized part
         
         VA0   =  0.225
         VA10  =  0.15 
         VALPHAU = 0.326
         VALPHAD = 0.505
         
         FFU0  = 1./(1.+VA0*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFU1  = 1./(1.+VA0*VA10*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFD0  = 1./(1.+VA0*X**(-VALPHAD)*(1.-X)*(1.-X))
         FFD1  = 1./(1.+VA0*VA10*X**(-VALPHAD)*(1.-X)*(1.-X))
         
         XDUV  =  XUV*FFU0
     &        -XDV*(FFU0+FFU1)/3.
         XDDV  =  XDV*(FFD0-2*FFD1)/3.
         
         XDUB  = 0.
         XDDB  = 0.
         XDG  =  XUV*(1.-FFU0)/2.
     &        +XDV*(2*FFD1-FFD0+FFU1+FFU0-3.)/6.
         
         XDPQ( 0) = XDG
         XDPQ( 1) = XDDV + XDDB
         XDPQ(-1) = XDDB
         XDPQ( 2) = XDUV + XDUB
         XDPQ(-2) = XDUB
      
      ELSEIF((LST(15).EQ.105).OR.(LST(15).EQ.106)) THEN
C***************************************************************************
C     
C       set 105  Bartelski et al hep-ph/9502271 Set II(p,n)
C       set 106  Bartelski et al hep-ph/9502271 Set II(P,n)
C     
C***************************************************************************
 
         XUV =  1.996*X**(-0.462)*(1.-X)**3.96
     &        *(1. - 0.39*SQRT(X) + 5.13*X)*X
         XDV =  0.296*X**(-0.67)*(1.-X)**4.71
     &        *(1. + 5.03*SQRT(X) + 5.56*X)*X
         FDELTA =   0.099*X**(-0.462)*(1.-X)**9.27*(1.+25*X)
         FM     =   0.411*X**(-1.3  )*(1.-X)**9.27
     &        *(1. - 1.15*SQRT(X) + 15.6*X)  
         XUB  = 0.5*X*(0.392*FM - FDELTA)
         XDB  = 0.5*X*(0.392*FM + FDELTA)
         XS   = 0.5*X*0.196*FM
         XC   = 0.5*X*0.020*FM
         XBOT = 0.
         XG   = 0.775*X**(-1.3)*(1.-X)**5.3*(1.+5.2*X)*X
         
         XPQ( 0)  = XG
         XPQ( 1)  = XDV + XDB
         XPQ(-1)  = XDB
         XPQ( 2)  = XUV + XUB
         XPQ(-2)  = XUB
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 
         XPQ( 4)  = XC 
         XPQ(-4)  = XC 
      
C     polarized part
         
         IF (LST(15).EQ.105) THEN
            VA1 =  0.924
            VA2 = -3.237
            VA3 = 11.3  
            XDUV = X**(-0.462)*(1.-X)**3.96*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VA1 = -0.029
            VA2 = -0.163
            VA3 = -1.644
            XDDV = X**(-0.670)*(1.-X)**4.71*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VC1 = 0.
            VC2 = -0.993
            VD  = -0.015
            FDELTA =   X**(-0.462)*(1.-X)**9.27*VD*(1.+25.*X)
            FM     =   X**(-0.8  )*(1.-X)**9.27
     &           *(VC1 + VC2*SQRT(X))

            XDUB = 0.5*X*(0.392*FM - FDELTA)            
            XDDB = 0.5*X*(0.392*FM + FDELTA)
            XDS = 0.5*X*0.196*FM
            XDC = 0.5*X*0.020*FM
            
         ELSEIF(LST(15).EQ.106) THEN
            
            VA1 =  0.929
            VA2 = -3.005
            VA3 = 10.24
            XDUV = X**(-0.462)*(1.-X)**3.96*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VA1 = -0.066
            VA2 = -0.064
            VA3 = -1.644
            XDDV = X**(-0.670)*(1.-X)**4.71*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VC1 = 0.
            VC2 = -0.861
            VD  = -0.013
            FDELTA =   X**(-0.462)*(1.-X)**9.27*VD*(1.+25.*X)
            FM     =   X**(-0.8  )*(1.-X)**9.27
     &           *(VC1+ VC2*SQRT(X))
            XDUB = 0.5*X*(0.392*FM - FDELTA)
            XDDB = 0.5*X*(0.392*FM + FDELTA)
            XDS = 0.5*X*0.196*FM
            XDC = 0.5*X*0.020*FM
         ENDIF

         XDPQ( 1) = XDDV + XDDB
         XDPQ(-1) = XDDB
         XDPQ( 2) = XDUV + XDUB
         XDPQ(-2) = XDUB
         XDPQ( 3) = XDS 
         XDPQ(-3) = XDS 
         XDPQ( 4) = XDC 
         XDPQ(-4) = XDC  
      ELSEIF((LST(15).GE.107).AND.(LST(15).LE.109)) THEN
C ***************************************************************************
C 
C       LST(15) = 107  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP  
C       LST(15) = 108  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C       LST(15) = 109  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C         
C ***************************************************************************
         
         IF (LST(15).EQ.107) IFLAG = 0
         IF (LST(15).EQ.108) IFLAG = 1
         IF (LST(15).EQ.109) IFLAG = 2
        
         MODE = 20       ! MSR(A prime) Parametrization


C  unpolarized part 
*     MRS(A prime)
        
         DX=DBLE(X)
         DQ2=DBLE(Q2)
         
         CALL MRSEB(DX,DQ2,MODE,MRSUPV,MRSDNV,MRSUSEA,
     &        MRSDSEA,MRSSTR,MRSCHM,MRSBOT,MRSGLU)

       
         XPQ( 0) = SNGL(MRSGLU)
         XPQ( 1) = SNGL(MRSDNV + MRSDSEA) 
         XPQ(-1) = SNGL(         MRSDSEA)
         XPQ( 2) = SNGL(MRSUPV + MRSUSEA)
         XPQ(-2) = SNGL(         MRSUSEA)
         XPQ( 3) = SNGL(MRSSTR)
         XPQ(-3) = SNGL(MRSSTR)
         XPQ( 4) = SNGL(MRSCHM)  
         XPQ(-4) = SNGL(MRSCHM)
         XPQ( 5) = SNGL(MRSBOT)   
         XPQ(-5) = SNGL(MRSBOT)
         
C     polarized part (evolved set of Gehrmann hep-ph/9512406 Gluon A,B,C (LO))
         CALL POLNLO(IFLAG,X,Q2,DXDUV,DXDDV,DXDG,DXDUBAR,DXDDBAR,DXDS)
         
         XDPQ( 0) = SNGL(DXDG)
         XDPQ( 1) = SNGL(DXDDV+DXDDBAR)
         XDPQ(-1) = SNGL(DXDDBAR)
         XDPQ( 2) = SNGL(DXDUV+DXDUBAR)
         XDPQ(-2) = SNGL(DXDUBAR)
         XDPQ( 3) = SNGL(DXDS) 
         XDPQ(-3) = SNGL(DXDS) 
         
C ***************************************************************************
C 
C       LST(15) = 110  Gehrmann  et al hep-ph/9512406 Gluon A (LO)
C       LST(15) = 111  Gehrmann  et al hep-ph/9512406 Gluon B (LO)
C       LST(15) = 112  Gehrmann  et al hep-ph/9512406 Gluon C (LO)
C
C       LST(15) = 130  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta u(x) = 0.5 * u(x) and Delta d(x) = 0.
C       LST(15) = 131  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta d(x) = 0.5 * d(x) and Delta u(x) = 0.
C
C ***************************************************************************



      ELSEIF((LST(15).GE.110).AND.(LST(15).LE.112) .or.
     &       (LST(15).GE.130).AND.(LST(15).LE.131) ) THEN

C      unpolarized part

         XUV = 3.221*X**0.564*(1.-X)**3.726
     &        *(1.-0.6889*X**0.200 + 2.254*X + 1.261*SQRT(X)*X)
         XDV = 0.507*X**0.376*(1.-X)**4.476
     &        *(1.+1.615 *X**0.553 + 3.651*X + 1.299*SQRT(X)*X)
         XUPDB = (X**0.158*(0.738 - 0.981*X + 1.063*X*X)
     &          * (-LOG(X))**0.037 
     &          + 0.00285*EXP(SQRT(-4.010*LOG(X))))*(1.-X)**6.356
         XDMUB = 0.1067 * X**0.4036 * (1.+ 0.4680*X**0.8763 + 
     &            X * 18.6760) * (1.- X)**8.6220
          XS = 0.0034*(-LOG(X))**(-1.15)
     &         *(1.-2.392*SQRT(X) + 7.094*X)*(1.-X)**6.166
     &         *EXP(SQRT(-6.719*LOG(X)))
          XG = (X**0.731*(5.110 - 1.204*X - 1.911*X*X)
     &        * (-LOG(X))**(-0.4718)
     &         + 0.0527*EXP(SQRT(-4.584*LOG(X))))*(1.-X)**5.566

         XPQ( 0)  = XG
         XPQ( 1)  = XDV + (XUPDB +  XDMUB)*0.5 
         XPQ(-1)  =       (XUPDB +  XDMUB)*0.5
         XPQ( 2)  = XUV + (XUPDB -  XDMUB)*0.5
         XPQ(-2)  =       (XUPDB -  XDMUB)*0.5
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 

C  polarized part

C  Gluon A        
         IF(LST(15).EQ.110) THEN
            VETAU   =  0.823
            VETAD   = -0.303 
            VETAQB  = -0.0495 
            VETAG   =  1.9
            
            VAU     =  0.578
            VAD     =  0.666
            VAQB    =  0.520
            VAG     =  0.520
            
            VBU     =  3.73
            VBD     =  4.73
            VBQB    = 15.06 
            VBG     =  9.45
            
            VGU     =  9.38
            VGD     = 10.46
            VGQB    =  2.30
            VGG     =  0.0
            
            VRU     = -4.26 
            VRD     = -5.10  
            VRQB    = -2.00
            VRG     =  0.00
            
            VNORMU  =  (1.+ VGU*VAU/(VAU+VBU+1.))
     &           * GAMMA(VAU)*GAMMA(VBU+1.)/GAMMA(VAU+VBU+1.)
     &           + VRU*GAMMA(VAU+0.5)*GAMMA(VBU+1.)
     &           /GAMMA(VAU+VBU+1.5)
            VNORMD  =  (1.+ VGD*VAD/(VAD+VBD+1.))
     &           * GAMMA(VAD)*GAMMA(VBD+1.)/GAMMA(VAD+VBD+1.)
     &           + VRD*GAMMA(VAD+0.5)*GAMMA(VBD+1.)
     &           /GAMMA(VAD+VBD+1.5)
            VNORMQB  =  (1.+ VGQB*VAQB/(VAQB+VBQB+1.))
     &           * GAMMA(VAQB)*GAMMA(VBQB+1.)/GAMMA(VAQB+VBQB+1.)
     &           + VRQB*GAMMA(VAQB+0.5)*GAMMA(VBQB+1.)
     &           /GAMMA(VAQB+VBQB+1.5)
            VNORMG  =  (1.+ VGG*VAG/(VAG+VBG+1.))
     &           * GAMMA(VAG)*GAMMA(VBG+1.)/GAMMA(VAG+VBG+1.)
     &           + VRG*GAMMA(VAG+0.5)*GAMMA(VBG+1.)
     &           /GAMMA(VAG+VBG+1.5)
            VNORMU  = 1./VNORMU
            VNORMD  = 1./VNORMD
            VNORMQB = 1./VNORMQB
            VNORMG  = 1./VNORMG
            
            XDUV  = VETAU*VNORMU*X**VAU*(1.-X)**VBU*(1.+VGU*X
     &           + VRU*SQRT(X))
            XDDV  = VETAD*VNORMD*X**VAD*(1.-X)**VBD*(1.+VGD*X
     &           + VRD*SQRT(X))
            XDUB  = VETAQB*VNORMQB*X**VAQB*(1.-X)**VBQB*
     &           (1.+VGQB*X + VRQB*SQRT(X))       
            XDDB  = XDUB
            XDS   = XDUB
            XDG   = VETAG*VNORMG*X**VAG*(1.-X)**VBG*(1.+VGG*X
     &           + VRG*SQRT(X))

            XDPQ( 0) = XDG
            XDPQ( 1) = XDDV + XDDB
            XDPQ(-1) = XDDB
            XDPQ( 2) = XDUV + XDUB
            XDPQ(-2) = XDUB
            XDPQ( 3) = XDS 
            XDPQ(-3) = XDS 

C Gluon B
         ELSE IF(LST(15).EQ.111) THEN

            VETAU   =  0.823
            VETAD   = -0.303 
            VETAQB  = -0.0495 
            VETAG   =  1.9
            
            VAU     =  0.585
            VAD     =  0.662
            VAQB    =  0.524
            VAG     =  0.524
            
            VBU     =  3.73
            VBD     =  4.73
            VBQB    = 15.96 
            VBG     =  6.87
            
            VGU     =  9.31
            VGD     = 10.91
            VGQB    =  2.42
            VGG     =  1.
            
            VRU     = -4.28 
            VRD     = -5.09  
            VRQB    = -2.00
            VRG     = -2.00
           
            VNORMU  =  (1.+ VGU*VAU/(VAU+VBU+1.))
     &           * GAMMA(VAU)*GAMMA(VBU+1.)/GAMMA(VAU+VBU+1.)
     &             + VRU*GAMMA(VAU+0.5)*GAMMA(VBU+1.)
     &           /GAMMA(VAU+VBU+1.5)
            VNORMD  =  (1.+ VGD*VAD/(VAD+VBD+1.))
     &           * GAMMA(VAD)*GAMMA(VBD+1.)/GAMMA(VAD+VBD+1.)
     &           + VRD*GAMMA(VAD+0.5)*GAMMA(VBD+1.)
     &           /GAMMA(VAD+VBD+1.5)
            VNORMQB  =  (1.+ VGQB*VAQB/(VAQB+VBQB+1.))
     &           * GAMMA(VAQB)*GAMMA(VBQB+1.)/GAMMA(VAQB+VBQB+1.)
     &           + VRQB*GAMMA(VAQB+0.5)*GAMMA(VBQB+1.)
     &           /GAMMA(VAQB+VBQB+1.5)
            VNORMG  =  (1.+ VGG*VAG/(VAG+VBG+1.))
     &           * GAMMA(VAG)*GAMMA(VBG+1.)/GAMMA(VAG+VBG+1.)
     &           + VRG*GAMMA(VAG+0.5)*GAMMA(VBG+1.)
     &           /GAMMA(VAG+VBG+1.5)
            
            VNORMU  = 1./VNORMU
            VNORMD  = 1./VNORMD
            VNORMQB = 1./VNORMQB
            VNORMG  = 1./VNORMG
            
            XDUV  = VETAU*VNORMU*X**VAU*(1.-X)**VBU
     &           *(1.+VGU*X + VRU*SQRT(X))
            XDDV  = VETAD*VNORMD*X**VAD*(1.-X)**VBD
     &           *(1.+VGD*X + VRD*SQRT(X))
            XDUB  = VETAQB*VNORMQB*X**VAQB*(1.-X)**VBQB*
     &           (1.+VGQB*X + VRQB*SQRT(X))       
            XDDB  = XDUB
            XDS   = XDUB
            XDG   = VETAG*VNORMG*X**VAG*(1.-X)**VBG
     &           *(1.+VGG*X + VRG*SQRT(X))

            XDPQ( 0) = XDG
            XDPQ( 1) = XDDV + XDDB
            XDPQ(-1) = XDDB
            XDPQ( 2) = XDUV + XDUB
            XDPQ(-2) = XDUB
            XDPQ( 3) = XDS 
            XDPQ(-3) = XDS 

C Gluon C
         ELSEIF(LST(15).EQ.112) THEN

            VETAU   =  0. 823
            VETAD   = -0.303 
            VETAQB  = -0.0495 
            VETAG   =  1.9
            
            VAU     =  0.582
            VAD     =  0.660
            VAQB    =  0.456
            VAG     =  0.456
            
            VBU     =  3.73
            VBD     =  4.73
            VBQB    = 11.82 
            VBG     =  8.72
            
            VGU     =  9.50
            VGD     = 11.04
            VGQB    =  2.11
            VGG     =  0.0
            
            VRU     = -4.28 
            VRD     = -5.06  
            VRQB    = -1.95
            VRG     = -3.00

            VNORMU  =  (1.+ VGU*VAU/(VAU+VBU+1.))
     &           * GAMMA(VAU)*GAMMA(VBU+1.)/GAMMA(VAU+VBU+1.)
     &           + VRU*GAMMA(VAU+0.5)*GAMMA(VBU+1.)
     &           /GAMMA(VAU+VBU+1.5)
            VNORMD  =  (1.+ VGD*VAD/(VAD+VBD+1.))
     &           * GAMMA(VAD)*GAMMA(VBD+1.)/GAMMA(VAD+VBD+1.)
     &           + VRD*GAMMA(VAD+0.5)*GAMMA(VBD+1.)
     &           /GAMMA(VAD+VBD+1.5)
            VNORMQB  =  (1.+ VGQB*VAQB/(VAQB+VBQB+1.))
     &           * GAMMA(VAQB)*GAMMA(VBQB+1.)/GAMMA(VAQB+VBQB+1.)
     &           + VRQB*GAMMA(VAQB+0.5)*GAMMA(VBQB+1.)
     &           /GAMMA(VAQB+VBQB+1.5)            
            VNORMG  =  (1.+ VGG*VAG/(VAG+VBG+1.))
     &           * GAMMA(VAG)*GAMMA(VBG+1.)/GAMMA(VAG+VBG+1.)
     &           + VRG*GAMMA(VAG+0.5)*GAMMA(VBG+1.)
     &           /GAMMA(VAG+VBG+1.5)
            
            VNORMU  = 1./VNORMU
            VNORMD  = 1./VNORMD
            VNORMQB = 1./VNORMQB
            VNORMG  = 1./VNORMG
            
            XDUV  = VETAU*VNORMU*X**VAU*(1.-X)**VBU
     &           *(1.+VGU*X + VRU*SQRT(X))
            XDDV  = VETAD*VNORMD*X**VAD*(1.-X)**VBD
     &           *(1.+VGD*X + VRD*SQRT(X))
            XDUB  = VETAQB*VNORMQB*X**VAQB*(1.-X)**VBQB*
     &           (1.+VGQB*X + VRQB*SQRT(X))       
            XDDB  = XDUB
            XDS   = XDUB
            XDG   = VETAG*VNORMG*X**VAG*(1.-X)**VBG
     &           *(1.+VGG*X + VRG*SQRT(X))

            XDPQ( 0) = XDG
            XDPQ( 1) = XDDV + XDDB
            XDPQ(-1) = XDDB
            XDPQ( 2) = XDUV + XDUB
            XDPQ(-2) = XDUB
            XDPQ( 3) = XDS 
            XDPQ(-3) = XDS 

	ELSEIF(LST(15).EQ.130) THEN
C unpolarized  Gehrmann  et al hep-ph/9512406
C with Delta u(x) = 0.5 * u(x) and Delta d(x) = 0.
          XDPQ(2) = 0.5 * XPQ(2)

	ELSEIF(LST(15).EQ.131) THEN
C unpolarized  Gehrmann  et al hep-ph/9512406
C with Delta d(x) = 0.5 * d(x) and Delta u(x) = 0. 
          XDPQ(1) = 0.5 * XPQ(1)
            
         ENDIF

C***************************************************************************
C
C       LST(15) = 113  Gehrmann  et al hep-ph/9512406 Gluon A (LO) + (DGLAP)
C       LST(15) = 114  Gehrmann  et al hep-ph/9512406 Gluon B (LO) + (DGLAP)
C       LST(15) = 115  Gehrmann  et al hep-ph/9512406 Gluon C (LO) + (DGLAP)
C
C**************************************************************************

         ELSEIF ((LST(15).GE.113).AND.(LST(15).LE.115)) THEN 
 
         IF (LST(15).EQ.113) IFLAG = 0
         IF (LST(15).EQ.114) IFLAG = 1
         IF (LST(15).EQ.115) IFLAG = 2

C  unpolarized part 
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         CALL GRV94LO (DX,DQ2
     &          ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)
                
         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + (DDXUDB + DDXDEL)*0.5D0)
         XPQ(-1) = SNGL(         (DDXUDB + DDXDEL)*0.5D0)
         XPQ( 2) = SNGL(DDXUV  + (DDXUDB - DDXDEL)*0.5D0)
         XPQ(-2) = SNGL(         (DDXUDB - DDXDEL)*0.5D0)
         XPQ( 3) = SNGL(DDXSB) 
         XPQ(-3) = SNGL(DDXSB) 

C polarized part (evolved set of Gehrmann hep-ph/9512406 Gluon A,B,C (LO))
         CALL POLPAR(IFLAG,DX,DQ2
     &               ,DXDUV,DXDDV,DXDG,DXDQBAR,DXDS)

         XDPQ( 0) = SNGL(DXDG)
         XDPQ( 1) = SNGL(DXDDV + DXDQBAR)
         XDPQ(-1) = SNGL(DXDQBAR)
         XDPQ( 2) = SNGL(DXDUV + DXDQBAR)
         XDPQ(-2) = SNGL(DXDQBAR)
         XDPQ( 3) = SNGL(DXDS) 
         XDPQ(-3) = SNGL(DXDS) 


C ***************************************************************************
C
C       M. Glueck, E. Reya, M. Stratmann and W. Vogelsang,
C       DO-TH 95/13, RAL-TR-95-042
C
C           LST(15) = 116  'standard' scenario, next-to-leading order
C           LST(15) = 117  'valence'  scenario, next-to-leading order
C           LST(15) = 118  'standard' scenario, leading order
C           LST(15) = 119  'valence'  scenario, leading order
C
C ***************************************************************************

      ELSEIF ((LST(15).GE.116).AND.(LST(15).LE.119)) THEN
       IF (LST(15).EQ.116) ISET = 1
       IF (LST(15).EQ.117) ISET = 2
       IF (LST(15).EQ.118) ISET = 3
       IF (LST(15).EQ.119) ISET = 4

C  unpolarized part
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *
         

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         CALL GRV94LO (DX,DQ2
     &          ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)

         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + (DDXUDB + DDXDEL)*0.5D0)
         XPQ(-1) = SNGL(         (DDXUDB + DDXDEL)*0.5D0)
         XPQ( 2) = SNGL(DDXUV  + (DDXUDB - DDXDEL)*0.5D0)
         XPQ(-2) = SNGL(         (DDXUDB - DDXDEL)*0.5D0)
         XPQ( 3) = SNGL(DDXSB)
         XPQ(-3) = SNGL(DDXSB)

C    Polarisierter Teil

      CALL  PARPOL (ISET,DX,DQ2,
     &      DXDUV, DXDDV, DXDQBAR, DXDS, DXDG)

      XDPQ( 0) = SNGL(DXDG)
      XDPQ( 1) = SNGL(DXDDV) + SNGL(DXDQBAR)
      XDPQ(-1) = SNGL(DXDQBAR)
      XDPQ( 2) = SNGL(DXDUV) + SNGL(DXDQBAR)
      XDPQ(-2) = SNGL(DXDQBAR)
      XDPQ( 3) = SNGL(DXDS)
      XDPQ(-3) = SNGL(DXDS)

C*************************************************************************

      ELSEIF(LST(15).EQ.120)THEN

*     This is polarization distribution of Stanley J.Brodsky et al.
*     Nucl.Phys. B441(1995)
         Y=1.-X
         X1=X**(-1.12)
         DO I = 1,4
            QP(I) = X1*(AQ(I)*Y**ST1(I)+BQ(I)*Y**ST2(I))
            QM(I) = X1*(CQ(I)*Y**ST3(I)+DQ(I)*Y**ST4(I))
            QG(I) = QP(I)+QM(I)
            QJ(I) = QP(I)-QM(I)
         ENDDO
         DO I = 1,2
            QG(I) = QG(I)-2.*QG(3)
            QJ(I) = QJ(I)-2.*QJ(3)
         ENDDO
         DO I = 1,4
            XPQ(I-4+INT(I/4.)*3) = QG(3)
            XDPQ(I-4+INT(I/4.)*3) = QJ(3)
         ENDDO
         
         XPQ(0) = QG(4)
         XPQ(1) = QG(1)
         XPQ(2) = QG(2)
         XDPQ(0) = QJ(4)
         XDPQ(1) = QJ(1)
         XDPQ(2) = QJ(2)
         
      ELSEIF(LST(15).EQ.121)THEN

*     This is polarization distribution of S.Keler & J.F.Owens
*     Phys.Lett. B266(1991)126 and Phys.Rev. D19(1994)1199
         
         IF(Q2.LE.4.) Q2 = 4.
         Z=ALOG(ALOG(Q2/WW(2))/WW(3))
         
         DO I = 1,6
            DO J = 1,4
               AAA(I,J) = CONST0(I,J) + CONST1(I,J)*Z +
     &              CONST2(I,J)*Z*Z + CONST3(I,J)*Z*Z*Z
            ENDDO
         ENDDO
         DO I = 1,2
            SUMR(I) = 0.
            DO 1 J = 1,3
               DO 1 K = 1,50
                  X1=XW(J)+K/50.
                  SS=1.+AAA(3,I)*X1+AAA(4,I)*X1*X1
                  SUMR(I)=SUMR(I)+X1**AAA(1,I)*(1.-X1)*
     &                 *AAA(2,I)*SS*W(J)
 1             CONTINUE
               AAA(6,I)=(5.-2.*I)/SUMR(I)
            ENDDO
            DO I = 1,4
               SS = 1.+AAA(3,I)*X+AAA(4,I)*X*X+AAA(5,I)*X*X*X    
               QG(I) = X**AAA(1,I)*(1.-X)**AAA(2,I)*SS*AAA(6,I)
            ENDDO
         SS=QG(2)
         QG(3)=QG(3)/6.
         QG(2)=QG(1)-SS
         QG(1)=SS
         DO I = 1,4
            SS = 1. + CJ(i)*X
            QJ(I) = X**AJ(I)*(1.-X)**BJ(I)*SS*RJ(I)
         ENDDO
         
         DO I = 1,4
            XPQ(I-4+INT(I/4.)*3) = QG(3)
            XDPQ(I-4+INT(I/4.)*3) = QJ(3)
         ENDDO         
         XPQ(0) = QG(4)
         XPQ(1) = QG(1)
         XPQ(2) = QG(2)
         XDPQ(0) = QJ(4)
         XDPQ(1) = QJ(1)
         XDPQ(2) = QJ(2)
C***********************************************************************
C       LST(15) = 124  D. de Florian et al., hep-ph/9711440  LO set 1      
C       LST(15) = 125  D. de Florian et al., hep-ph/9711440  LO set 2      
C       LST(15) = 126  D. de Florian et al., hep-ph/9711440  LO set 3      
C       LST(15) = 127  D. de Florian et al., hep-ph/9711440 NLO set 1      
C       LST(15) = 128  D. de Florian et al., hep-ph/9711440 NLO set 2      
C       LST(15) = 129  D. de Florian et al., hep-ph/9711440 NLO set 3      
C
C       using GRV 1995 as unpolarized reference set
C
C***********************************************************************
      ELSEIF ((LST(15).GE.124).AND.(LST(15).LE.129)) THEN    

c      IF(X.LE.0.0001)  THEN
c      WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0001001'
c      X=0.0001001
c      ENDIF
c      IF(X.GE.0.9)     THEN
c      WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.89999'
c      X=0.89999
c      ENDIF
c      IF(Q2.LE.1.)     THEN
c      WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.0001'
c      Q2 = 1.0001
c      ENDIF
c      IF(Q2.GE.50000.0) THEN
c      WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 49999.9'
c      Q2 = 49999.9
c      ENDIF 

      IF (LST(15).EQ.124) MODE=1
      IF (LST(15).EQ.125) MODE=2
      IF (LST(15).EQ.126) MODE=3
      IF (LST(15).EQ.127) MODE=4
      IF (LST(15).EQ.128) MODE=5
      IF (LST(15).EQ.129) MODE=6
      
C  unpolarized part 
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         IF ((LST(15).GE.124).AND.(LST(15).LE.126)) THEN
         CALL GRV94LO (DX,DQ2
     &          ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)
         ELSEIF ((LST(15).GE.127).AND.(LST(15).LE.129)) THEN
            CALL GRV94HO (DX,DQ2
     &           ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)
         ENDIF
 
         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + (DDXUDB + DDXDEL)*0.5D0)
         XPQ(-1) = SNGL(         (DDXUDB + DDXDEL)*0.5D0)
         XPQ( 2) = SNGL(DDXUV  + (DDXUDB - DDXDEL)*0.5D0)
         XPQ(-2) = SNGL(         (DDXUDB - DDXDEL)*0.5D0)
         XPQ( 3) = SNGL(DDXSB) 
         XPQ(-3) = SNGL(DDXSB) 

*     polarized part

         CALL  POLFIT(MODE,DX,DQ2,DXDUV,DXDDV,DXDQBAR,DXDSTR,DXDGLU)
         XDPQ( 0) = SNGL(DXDGLU)
         XDPQ( 1) = SNGL(DXDDV) + SNGL(DXDQBAR)
         XDPQ(-1) =               SNGL(DXDQBAR) 
         XDPQ( 2) = SNGL(DXDUV) + SNGL(DXDQBAR)
         XDPQ(-2) =               SNGL(DXDQBAR)
         XDPQ( 3) =               SNGL(DXDSTR)
         XDPQ(-3) =               SNGL(DXDSTR)
      ELSEIF(LST(15).EQ.132)THEN         
*72*********************************************************************
         CALL FITPARTON(X,Q2,XPQ,XDPQ)
*72*********************************************************************
      ELSEIF ((LST(15).GE.133).AND.(LST(15).LE.136)) THEN

C  unpolarized part
*     "IMPROVED PARTON DISTRIBUTIONS FROM GLOBAL ANALYSIS OF
*   RECENT DEEP INELASTIC SCATTERING AND INCLUSIVE JET DATA"
*   By: H.L. Lai, J. Huston, S. Kuhlmann, F. Olness, J. Owens, 
*    D. Soper W.K. Tung, H. Weerts
*    Phys. Rev. D55, 1280 (1997) 
*   ( low Q2  set )
*         

*     from data statement above
*       CTEQSET=10  !  Low Q2
         DX=DBLE(X)
         DQ2=DBLE(Q2)
         IF(INITCTEQ.eq.1) THEN
            INITCTEQ=0
            CALL SETCTQ4(CTEQSET)
         ENDIF

*   Note in CTEQ u and d indices are reversed WRT pepsi order:
*   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
*                            for (b, c, s, d, u, g, u_bar, ..., b_bar),


         DDXPQ(-3) = CTQ4PDF(-3,DX,DQ2)
         DDXPQ(-1) = CTQ4PDF(-2,DX,DQ2)
         DDXPQ(-2) = CTQ4PDF(-1,DX,DQ2)
         DDXPQ(0) =  CTQ4PDF(0,DX,DQ2)
         DDXPQ(2) =  CTQ4PDF(1,DX,DQ2)
         DDXPQ(1) =  CTQ4PDF(2,DX,DQ2)
         DDXPQ(3) =  CTQ4PDF(3,DX,DQ2)

* CTEQ routine double precision q(x).  Make single  xq(x)
         DO I=-3,3
            XPQ( I) = X*SNGL(DDXPQ(I))
         ENDDO
         
         XDPQ( 0) = 0.0
         XDPQ( 1) = -0.3*XPQ(1)
         XDPQ(-1) = 0.0
         XDPQ( 2) = 0.5*XPQ(2)
         XDPQ(-2) = 0.0
         XDPQ( 3) = 0.0
         XDPQ(-3) = 0.0

* require F2 non-zero
         IF(XPQ(2).EQ.0.0) XPQ(2)=0.000001

      ELSEIF((LST(15).GE.137).AND.(LST(15).LE.138)) THEN

         MODE = 10       ! MSR(A prime) Low Q2 Parametrization


C  unpolarized part 
*     MRS(A prime)
        
         DX=DBLE(X)
         DQ2=DBLE(Q2)

         CALL MRSEB(DX,DQ2,MODE,MRSUPV,MRSDNV,MRSUSEA,
     &        MRSDSEA,MRSSTR,MRSCHM,MRSBOT,MRSGLU)

       
         XPQ( 0) = SNGL(MRSGLU)
         XPQ( 1) = SNGL(MRSDNV + MRSDSEA) 
         XPQ(-1) = SNGL(         MRSDSEA)
         XPQ( 2) = SNGL(MRSUPV + MRSUSEA)
         XPQ(-2) = SNGL(         MRSUSEA)
         XPQ( 3) = SNGL(MRSSTR)
         XPQ(-3) = SNGL(MRSSTR)
         XPQ( 4) = SNGL(MRSCHM)  
         XPQ(-4) = SNGL(MRSCHM)
         XPQ( 5) = SNGL(MRSBOT)   
         XPQ(-5) = SNGL(MRSBOT)


         
         XDPQ( 0) = 0.0
         XDPQ( 1) = -0.3*XPQ(1)
         XDPQ(-1) = 0.0
         XDPQ( 2) = 0.5*XPQ(2)
         XDPQ(-2) = 0.0
         XDPQ( 3) = 0.0
         XDPQ(-3) = 0.0



C***********************************************************************
C       LST(15) = 144  grsv2000  hep-ph/0011215   LO standard scenario
C       LST(15) = 145  grsv2000  hep-ph/0011215   LO  valence scenario  
C       LST(15) = 146  grsv2000  hep-ph/0011215  NLO standard scenario
C       LST(15) = 147  grsv2000  hep-ph/0011215  NLO  valence scenario

C
C       using GRV 1998 as unpolarized reference set
C
C***********************************************************************
      ELSEIF ((LST(15).GE.144).AND.(LST(15).LE.147)) THEN    

      IF(X.LE.0.00001)  THEN
c$$$      WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0001001'
      X=0.0001001
      ENDIF
      IF(X.GE.1.0)     THEN
c$$$      WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
      X=0.999999
      ENDIF
      IF(Q2.LE.0.8)     THEN
c$$$      WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 0.80001'
      Q2 = 0.80001
      ENDIF
      IF(Q2.GE.1.0E6) THEN
c$$$      WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 999999.9'
      Q2 = 999999.9
      ENDIF 

      
C  unpolarized part 
*                   M. Glueck, E. Reya, A. Vogt :                   *
*        hep-ph/9806404  =  DO-TH 98/07  =  WUE-ITP-98-019          *
*                  (To appear in Eur. Phys. J. C)                   *

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         IF ((LST(15).GE.144).AND.(LST(15).LE.145)) THEN
            ISET = 1 ! LO
         ELSEIF ((LST(15).GE.146).AND.(LST(15).LE.147)) THEN
            ISET = 2 ! NLO
         ENDIF
         CALL GRV98PA (ISET, DX, DQ2 
     &           ,DDXUV,DDXDV,DDXUS, DDXDS, DDXSB,DDXGL)

  
         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + DDXDS)
         XPQ(-1) = SNGL(DDXDS)         
         XPQ( 2) = SNGL(DDXUV  + DDXUS)   
         XPQ(-2) = SNGL(DDXUS)         
         XPQ( 3) = SNGL(DDXSB) 
         XPQ(-3) = SNGL(DDXSB) 

*     polarized part


      IF (LST(15).EQ.144) ISET = 3
      IF (LST(15).EQ.145) ISET = 4
      IF (LST(15).EQ.146) ISET = 1
      IF (LST(15).EQ.147) ISET = 2



         CALL PARPOLNEW (ISET, DX, DQ2, 
     &          DXDU, DXDD, DXDUB, DXDDB, DXDSTR, DXDGLU, 
     &          DUMMYG1P, DUMMYG1N)
         XDPQ( 0) = SNGL(DXDGLU)
         XDPQ( 1) = SNGL(DXDD)
         XDPQ(-1) = SNGL(DXDDB)           
         XDPQ( 2) = SNGL(DXDU)
         XDPQ(-2) = SNGL(DXDUB)           
         XDPQ( 3) = SNGL(DXDSTR)           
         XDPQ(-3) = SNGL(DXDSTR)
             
C*************************************************************************
C        LST(15) = 150   cteq5l                   LO
C        LST(15) = 151   cteq5m                   NLO MSBAR
C        LST(15) = 152   cteq5m1                  NLO MSBAR (update)
C*************************************************************************
  
         ELSEIF((LST(15).GE.150).AND.(LST(15).LE.152)) THEN

         IF(X.LE.0.00001)  THEN
c$$$       WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0000101'
            X=0.0000101
         ENDIF
         IF(X.GE.1.0)     THEN
c$$$       WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
            X=0.999999
         ENDIF
         IF(Q2.LE.1.0)     THEN
c$$$       WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.00001'
            Q2 = 1.00001
         ENDIF
         IF(Q2.GE.1.0E4) THEN
c$$$       WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 9999.9'
            Q2 = 9999.9
         ENDIF 

        XPQ(-5) = X*SNGL( CTQ5PDF (-5, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-4) = X*SNGL( CTQ5PDF (-4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-3) = X*SNGL( CTQ5PDF (-3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-2) = X*SNGL( CTQ5PDF (-1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-1) = X*SNGL( CTQ5PDF (-2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 0) = X*SNGL( CTQ5PDF ( 0, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 1) = X*SNGL( CTQ5PDF ( 2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 2) = X*SNGL( CTQ5PDF ( 1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 3) = X*SNGL( CTQ5PDF ( 3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 4) = X*SNGL( CTQ5PDF ( 4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 5) = X*SNGL( CTQ5PDF ( 5, DBLE(X), DBLE(SQRT(Q2))))

C*****************************************************************************
C        LST(15) = 161  mrs99   cor01  central gluon, a_s    
C        LST(15) = 162  mrs99   cor02  higher gluon        
C        LST(15) = 163  mrs99   cor03  lower gluon       
C        LST(15) = 164  mrs99   cor04  lower a_s      
C        LST(15) = 165  mrs99   cor05  higher a_s     
C        LST(15) = 166  mrs99   cor06  quarks up        
C        LST(15) = 167  mrs99   cor07  quarks down            
C        LST(15) = 168  mrs99   cor08  strange up           
C        LST(15) = 169  mrs99   cor09  strange down          
C        LST(15) = 170  mrs99   cor10  charm up         
C        LST(15) = 171  mrs99   cor11  charm down       
C        LST(15) = 172  mrs99   cor12  larger d/u          
C****************************************************************************

           ELSEIF((LST(15).GE.161).AND.(LST(15).LE.172)) THEN

         IF(X.LE.0.00001)  THEN
c$$$       WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0000101'
            X=0.0000101
         ENDIF
         IF(X.GE.1.0)     THEN
c$$$       WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
            X=0.999999
         ENDIF
         IF(Q2.LE.1.25)     THEN
c$$$        WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.25001'
            Q2 = 1.25001
         ENDIF
        IF(Q2.GE.1.0E4) THEN
c$$$       WRITE(*,*) 'Warning: Q2 exceeds maximum value, ',
c$$$     &                          'Q2 set to 9999999.9'
            Q2 = 9999999.9
         ENDIF 

        MODE = LST(15)-160
        CALL MRS99(DBLE(X),DBLE(SQRT(Q2)),MODE,
     &       MRSUPV,MRSDNV,MRSUSEA,MRSDSEA,MRSSTR,MRSCHM,MRSBOT,
     &       MRSGLU) 

        XPQ(-5) = SNGL(MRSBOT)
        XPQ(-4) = SNGL(MRSCHM)
        XPQ(-3) = SNGL(MRSSTR)
        XPQ(-2) = SNGL(MRSUSEA)
        XPQ(-1) = SNGL(MRSDSEA)
        XPQ( 0) = SNGL(MRSGLU)
        XPQ( 1) = SNGL(MRSDSEA)+SNGL(MRSDNV)
        XPQ( 2) = SNGL(MRSUSEA)+SNGL(MRSUPV)
        XPQ( 3) = SNGL(MRSSTR)
        XPQ( 4) = SNGL(MRSCHM)
        XPQ( 5) = SNGL(MRSBOT)

C Added By Joe Seele for the inclusion of the CTEQ 6 PDF's

      ELSEIF((LST(15).GE.173).AND.(LST(15).LE.175)) THEN

         IF(X.LE.0.00001)  THEN
c$$$       WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0000101'
            X=0.0000101
         ENDIF
         IF(X.GE.1.0)     THEN
c$$$       WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
            X=0.999999
         ENDIF
         IF(Q2.LE.1.0)     THEN
c$$$       WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.00001'
            Q2 = 1.00001
         ENDIF
         IF(Q2.GE.1.0E4) THEN
c$$$       WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 9999.9'
            Q2 = 9999.9
         ENDIF 

        XPQ(-5) = X*SNGL( CTQ6PDF (-5, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-4) = X*SNGL( CTQ6PDF (-4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-3) = X*SNGL( CTQ6PDF (-3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-2) = X*SNGL( CTQ6PDF (-1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-1) = X*SNGL( CTQ6PDF (-2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 0) = X*SNGL( CTQ6PDF ( 0, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 1) = X*SNGL( CTQ6PDF ( 2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 2) = X*SNGL( CTQ6PDF ( 1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 3) = X*SNGL( CTQ6PDF ( 3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 4) = X*SNGL( CTQ6PDF ( 4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 5) = X*SNGL( CTQ6PDF ( 5, DBLE(X), DBLE(SQRT(Q2))))

        IF(mcSet_SFact.eq.2) THEN
         XPQ( 3) = (X*SNGL( CTQ6PDF ( 3, DBLE(X), DBLE(SQRT(Q2)))))*
     1             ((x**(-0.3))*(1-x)*exp(-x/0.05))
        ENDIF

        IF(mcSet_SBFact.eq.2) THEN
         XPQ(-3) = (X*SNGL( CTQ6PDF (-3, DBLE(X), DBLE(SQRT(Q2)))))*
     1             ((x**(-0.3))*(1-x)*exp(-x/0.05))
        ENDIF

C*****************************************************************************



 
C*************************************************************************
      ELSE
         WRITE(*,*) 'NOT KNOWN PARTON DENSITY FUNCTION: STOP',LST(15)
         STOP
      ENDIF
*72*********************************************************************
      DO I=-6,6
        IF(ABS(XDPQ(I)).gt.XPQ(I)) THEN
          IF(ICERR.lt.IPLST(1).and.Q2.gt.1.) THEN 
             WRITE(*,*)'Non proper parton set : ',XDPQ(I),XPQ(I),I,X,Q2
             ICERR=ICERR+1
          ENDIF
          IF(XDPQ(I).gt.0) THEN 
             XDPQ(I)=XPQ(I)
          ELSE
             XDPQ(I)=-XPQ(I)
          ENDIF
       ENDIF
      ENDDO      
*72*********************************************************************      
C   This Amendment is to simulate HeliumIII
      IF(LST(39).NE.0) THEN
         F2P = 4.*(XPQ(2)+XPQ(-2)+XPQ(4)+XPQ(-4))
     &       +XPQ(1)+XPQ(-1)+XPQ(3)+XPQ(-3)
         IF(LST(36).eq.0) THEN 
            XDV=XPQ(1)-XPQ(-1)
            XUV=XPQ(2)-XPQ(-2)
            F2N = 4.*(XDV+2*XPQ(-2)+XPQ(4)+XPQ(-4))
     &          + XUV+2*XPQ(-1)+XPQ(3)+XPQ(-3)
         ELSE
            F2N = 4.*(XPQ(1)+XPQ(-1)+XPQ(4)+XPQ(-4))+
     &           XPQ(2)+XPQ(-2)+XPQ(3)+XPQ(-3)
         ENDIF
         
C     admitxurecoefficient for the unpolarized structure functions 
         PARI11 = 1./3.
 
         IF(LST(39).EQ.3) THEN
C..........................
C  This is a real helium 3.
C..........................
C Percentage of neutron and proton to the helium3 asymmetry
            PERCS   =  0.865
            PERCSP  =  0.
            PERCD   = -0.027
C admixture coefficients for the spin states
            ADMXS   = 1.0
            ADMXSP  = 0.
            ADMXD   = 0.
C factors for the spin state (number of polarized nucleons/total number
C of nucleons
            FACTS   = f2n/(f2n+2*f2p)
            FACTSP  = 0.
            FACTD   = 2*f2p/(f2n+2*f2p)
         ELSEIF(LST(39).EQ.2) THEN
C..........................
C  This is a real helium 3.
C..........................
C Percentage of the spin states
            PERCS   =  0.903
            PERCSP  =  0.014
            PERCD   = -0.083
C admixture coefficients for the spin states
            ADMXS   = 1.0
            ADMXSP  = 1./3.
            ADMXD   = 1./3.
C factors for the spin state (number of polarized nucleons/total number
C of nucleons
            FACTS   = f2n/(f2n+2*f2p)
            FACTSP  = 1./3.
            FACTD   = 1. 
C     admitxurecoefficient for the unpolarized structure functions 
            PARI11 = 1./3.
         ELSEIF(LST(39).EQ.1) THEN
C...........................
C  This is a ideal helium 3.
C...........................
C     Percentage of the spin states
            PERCS   = 1.0
            PERCSP  = 0.0
            PERCD   = 0.0
C     admixturecoefficients for the spin states
            ADMXS   = 1.
            ADMXSP  = 0.
            ADMXD   = 0.
C     factors for the spin state (number of polarized nucleons/total number
C     of nucleons
            FACTS   = f2n/(f2n+2*f2p)
            FACTSP  = 0.
            FACTD   = 0. 
C     admitxurecoefficient for the unpolarized structure functions
            PARI11 = 1./3.
         ENDIF
         
C...  For nuclear target, mix u- and d-valence distributions.
         IF(LST(36).EQ.0) THEN
C...  For nuclear target, mix u- and d-valence distributions.
            XPQ(1)=(1.-PARI11)*XDV+PARI11*XUV+XPQ(-1)
            XPQ(2)=(1.-PARI11)*XUV+PARI11*XDV+XPQ(-2)
C     Admixture in the polarized case         
            XDDV=XDPQ(1)-XDPQ(-1)
            XDUV=XDPQ(2)-XDPQ(-2)
            XDPQ(1)= ((1.-ADMXS )*XDDV+ADMXS *XDUV )
     &           * FACTS * PERCS
     &           +((1.-ADMXSP)*XDDV+ADMXSP*XDUV )
     &           * FACTSP* PERCSP 
     &           +((1.-ADMXD )*XDDV+ADMXD *XDUV )
     &           * FACTD * PERCD
            XDPQ(2)= ((1.-ADMXS )*XDUV+ADMXS *XDDV )
     &           * FACTS * PERCS
     &           +((1.-ADMXSP)*XDUV+ADMXSP*XDDV )
     &           * FACTSP* PERCSP
     &           +((1.-ADMXD )*XDUV+ADMXD *XDDV )
     &           * FACTD * PERCD
            DO I=-6,6
C...  For nuclear target, mix u- and d-valence distributions.
               IF(I.eq.1) THEN
* XDPQ(-1) has to be diluted here by (FACTS*PERCS+FACTSP...
                  XDPQ(1)= XDPQ(1) + XDPQ(-1)
               ELSEIF(I.eq.2) THEN
* XDPQ(-2) has to be diluted here by (FACTS*PERCS+FACTSP...                  
                  XDPQ(2)= XDPQ(2) + XDPQ(-2)
               ELSE
*     now for the sea quarks and gluon distributions                  
                  XDPQ(I)=XDPQ(I)*
     &                 (FACTS*PERCS+FACTSP*PERCSP+FACTD*PERCD)
               ENDIF
            ENDDO            
         ELSEIF(LST(36).EQ.1) THEN
            XD=XPQ(1)
            XU=XPQ(2)
            XPQ(1) =(1.-PARI11)*XD+PARI11*XU
            XPQ(2) =(1.-PARI11)*XU+PARI11*XD
            XD=XPQ(-1)
            XU=XPQ(-2)
            XPQ(-1)=(1.-PARI11)*XD+PARI11*XU
            XPQ(-2)=(1.-PARI11)*XU+PARI11*XD
C     Admixture in the polarized case
*.............................................
*           FOR u and d QUARKS
*.............................................         
            XDD=XDPQ(1)
            XDU=XDPQ(2)
            XDDS=XDPQ(-1)
            XDUS=XDPQ(-2)            
            DO I=-6,6
C...  For nuclear target, mix u- and d-valence distributions.
               IF(I.eq.1) THEN 
                  XDPQ(1)= ((1.-ADMXS )*XDD+ADMXS *XDU )
     &                 * FACTS * PERCS  
     &                 +((1.-ADMXSP)*XDD+ADMXSP*XDU )
     &                 * FACTSP* PERCSP 
     &                 +((1.-ADMXD )*XDD+ADMXD *XDU )
     &                 * FACTD * PERCD
               ELSEIF(I.eq.2) THEN 
                  XDPQ(2)= ((1.-ADMXS )*XDU+ADMXS *XDD )
     &                 * FACTS * PERCS
     &                 +((1.-ADMXSP)*XDU+ADMXSP*XDD )
     &                 * FACTSP* PERCSP
     &                 +((1.-ADMXD )*XDU+ADMXD *XDD )
     &                 * FACTD * PERCD
*     now for the sea quarks and gluon distributions
               ELSEIF(I.eq.-1) THEN 
                  XDPQ(-1)=((1.-ADMXS)*XDDS+ADMXS*XDUS)
     &                 * FACTS * PERCS  
     &                 +((1.-ADMXSP)*XDDS+ADMXSP*XDUS )
     &                 * FACTSP* PERCSP 
     &                 +((1.-ADMXD )*XDDS+ADMXD *XDUS )
     &                 * FACTD * PERCD
               ELSEIF(I.eq.-2) THEN
                  XDPQ(-2)=((1.-ADMXS)*XDUS+ADMXS*XDDS )
     &                 * FACTS * PERCS
     &                 +((1.-ADMXSP)*XDUS+ADMXSP*XDDS )
     &                 * FACTSP* PERCSP
     &                 +((1.-ADMXD )*XDUS+ADMXD *XDDS )
     &                 * FACTD * PERCD
               ELSE
*           now for the sea quarks and gluon distributions                  
                  XDPQ(I)=XDPQ(I)*
     &                 (FACTS*PERCS+FACTSP*PERCSP+FACTD*PERCD)
               ENDIF
            ENDDO
C...  For nuclear target, mix u- and d-valence distributions.
         ENDIF
      ENDIF
         
* polarisation enhancement factor 
      DO I=-6,6
         XDPQ(I) = FLOAT(LST(38))* XDPQ(I)
      ENDDO
      
      RETURN
      END
