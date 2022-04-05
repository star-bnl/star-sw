*.......................................................................
*
      SUBROUTINE POLAR(IDGROUP,DX,DQ2,PDFOUT,NEW)      
*
* DOCU      :  PYTHIA ASYMMETRY STUDIES
*
* COMMON    :
* SEQUENCE  :
* CALLS     :
* CALLED    :
*
* AUTHOR    : R. FATEMI AND B. SURROW
* VERSION   : 1.0
* CREATED   : 11-01-03
* LAST MOD  : 12-01-03
*
* MODIFICATION LOG.
* 11-30-03 RHF ADDED
*
* 11-01-13 : FIRST IMPLEMENTATION
*
*.......................................................................
*
      IMPLICIT NONE
*
      INTEGER IDGROUP,ISET,NEW
      INTEGER SET_PDF,IDG,IINI
      DOUBLE PRECISION DX,DQ2
      DOUBLE PRECISION U,D,G1P,G1N,G1D,DGL
      DOUBLE PRECISION UV,DV,UB,DB,QB,ST,GL
      DOUBLE PRECISION UUB,DDB,SSB,STB,G1PLT,G1NLT
      DOUBLE PRECISION UVERR,DVERR,GLERR,QBERR,G1PERR,G1NERR
      DOUBLE PRECISION PDFOUT(6)
C      IMPLICIT REAL *8(A-H,O-Z)
      DOUBLE PRECISION AACPDFOUT(-3:3)
      COMMON / INTINI / IINI

*     FOR GRSV: The main program or the calling routine has  *
*     to have a common block  COMMON / INTINI / IINI, and    *
*     IINI must be zero when PARPOL2 is called for the       *
*     first time or when 'ISET' has been changed.            *

*     FOR GS: Must call NLOINI first time                    *

*     FOR DSSV: Must call DSSVINI first time                 *

*     FOR DNS: Must call DNSINI first time                   *

      SET_PDF=IDGROUP
      IINI=1

      IF (NEW.eq.0) THEN
         IINI=0
         IF (SET_PDF.gt.200 .AND. SET_PDF.lt.300) THEN
            CALL NLOINI        
         ELSE IF (SET_PDF.gt.300 .AND. SET_PDF.lt.400) THEN
            CALL DSSVINI
         ELSE IF (SET_PDF.gt.700 .AND. SET_PDF.lt.800) THEN
            CALL DNSINI
         ENDIF
      ENDIF
      NEW=1
      
*     
*     PRINT *,"POLAR.F: IDGROUP=",IDGROUP," X=",DX," Q2=",DQ2
*      
*     IF IDG==0 then returns x*PDF 
*     IF IDG!=0 then returns PDF only
      IDG=1
*
* GRSV pdf selection:
*
      IF (SET_PDF.eq.101) THEN 
*   
* GRSV STANDARD LO
*
         ISET=3
         call PARPOL(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.102) THEN 
*   
* GRSV STANDARD NLO 
*        
         ISET=1
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.103) THEN 
*  
* GRSV STANDARD NLO deltag=0
*        
         ISET=3
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.104) THEN 
*   
*  
* GRSV STANDARD NLO deltag=maximum allowed g
*        
         ISET=4
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.105) THEN 
*   
*  
* GRSV STANDARD NLO deltag=minimum allowed g
*        
         ISET=2
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.106) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m015
*        
         ISET=5
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.107) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m030
*        
         ISET=6
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.108) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m045
*        
         ISET=7
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.109) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m060
*        
         ISET=8
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.110) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m075
*        
         ISET=9
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.111) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m090
*        
         ISET=10
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SET_PDF.eq.112) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m105
*        
         ISET=11
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SET_PDF.eq.113) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p030
*        
         ISET=12
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SET_PDF.eq.114) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p045
*        
         ISET=13
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SET_PDF.eq.115) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p060
*        
         ISET=14
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SET_PDF.eq.116) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p070
*        
         ISET=15
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SET_PDF.eq.201) THEN

* Gehrmann-Stirling_NL0_A

         ISET=0
         call POLNLO(ISET,DX,DQ2,UV,DV,GL,UB,DB,ST)
      ELSEIF (SET_PDF.eq.202) THEN

* Gehrmann-Stirling_NL0_B
        
         ISET=1
         call POLNLO(ISET,DX,DQ2,UV,DV,GL,UB,DB,ST)
      ELSEIF (SET_PDF.eq.203) THEN         

* Gehrmann-Stirling_NL0_C
         
         ISET=2
         call POLNLO(ISET,DX,DQ2,UV,DV,GL,UB,DB,ST)
      ELSEIF (SET_PDF.eq.301) THEN

* DSSV
         
         ISET=0
         call DSSVFIT(DX,DQ2,UV,DV,UB,DB,ST,GL)
      ELSEIF (SET_PDF.eq.401) THEN

* LSS Scenario 1
         
         ISET=1
         call LSS2006(ISET,DX,DQ2,UUB,DDB,SSB,GL,UV,DV,UB,DB,STB,
     &        G1PLT,G1P,G1NLT,G1N)         
      ELSEIF (SET_PDF.eq.402) THEN

* LSS Scenario 2
         
         ISET=2
         call LSS2006(ISET,DX,DQ2,UUB,DDB,SSB,GL,UV,DV,UB,DB,STB,
     &        G1PLT,G1P,G1NLT,G1N)
      ELSEIF (SET_PDF.eq.403) THEN

* LSS Scenario 3
         
         ISET=3
         call LSS2006(ISET,DX,DQ2,UUB,DDB,SSB,GL,UV,DV,UB,DB,STB,
     &        G1PLT,G1P,G1NLT,G1N)
      ELSEIF (SET_PDF.eq.501) THEN

* AAC Scenario 1
         
         ISET=1
         call AACPDFE(DQ2,DX,ISET,AACPDFOUT)
      ELSEIF (SET_PDF.eq.502) THEN

* AAC Scenario 2
         
         ISET=2
         call AACPDFE(DQ2,DX,ISET,AACPDFOUT)
      ELSEIF (SET_PDF.eq.503) THEN

* AAC Scenario 3
         
         ISET=3
         call AACPDFE(DQ2,DX,ISET,AACPDFOUT)
      ELSEIF (SET_PDF.eq.601) THEN

* BB Scenario 1
         
         ISET=3
         call PPDF(ISET,DX,DQ2,UV,UVERR,DV,DVERR,GL,GLERR,
     &        QB,QBERR,G1P,G1PERR,G1N,G1NERR)
      ELSEIF (SET_PDF.eq.602) THEN

* BB Scenario 2
         
         ISET=4
         call PPDF(ISET,DX,DQ2,UV,UVERR,DV,DVERR,GL,GLERR,
     &        QB,QBERR,G1P,G1PERR,G1N,G1NERR)
      ELSEIF (SET_PDF.eq.701) THEN

* DNS Scenario 1
         
         ISET=1
         call POLFIT(ISET,DX,DQ2,UV,DV,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SET_PDF.eq.702) THEN

* DNS Scenario 2
         
         ISET=2
         call POLFIT(ISET,DX,DQ2,UV,DV,UB,DB,ST,GL,G1P,G1N)
         
      ENDIF


      IF (SET_PDF.lt.200) THEN

* GRSV Access Converntion
*            OUTPUT:  U = x * DELTA u                               *
*            D = x * DELTA d                                        *        
*            UB = x * DELTA ubar                                    *   
*            DB = x * DELTA dbar                                    * 
*            ST = x * DELTA STRANGE                                 *     
*            GL = x * DELTA GLUON                                   *
*            G1P = g_1^proton                                       *
*            G1N = g_1^neutron

         if (IDG.eq.1) then
            PDFOUT(1)=U/DX
            PDFOUT(2)=D/DX
            PDFOUT(3)=UB/DX
            PDFOUT(4)=DB/DX
            PDFOUT(5)=ST/DX
            PDFOUT(6)=GL/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=U
            PDFOUT(2)=D
            PDFOUT(3)=UB
            PDFOUT(4)=DB
            PDFOUT(5)=ST
            PDFOUT(6)=GL
         endif

      ENDIF

      IF (SET_PDF.gt.200 .AND. SET_PDF.lt.300) THEN

c ---- Gehrmann Stirling access convention         
c ---- OUTPUT parameters (note that always x*distribution is returned)
c ---- uval  = u-ubar
c ---- dval  = d-dbar 
c ---- glue 
c ---- ubar  = 1/2 usea 
c ---- dbar = 1/2 dsea
c ---- str = sbar = 1/2 strsea

         
         if (IDG.eq.1) then
            PDFOUT(1)=UV/DX
            PDFOUT(2)=DV/DX
            PDFOUT(3)=UB/DX
            PDFOUT(4)=DB/DX
            PDFOUT(5)=ST/DX
            PDFOUT(6)=GL/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=UV
            PDFOUT(2)=DV
            PDFOUT(3)=UB
            PDFOUT(4)=DB
            PDFOUT(5)=ST
            PDFOUT(6)=GL
         endif

c         if (ISET.eq.2) then
c            PRINT *,"UV=",PDFOUT(1)," DV=",PDFOUT(2), " UB=",PDFOUT(3)
c     &           ," DB=",PDFOUT(4)," ST=",PDFOUT(5)," GL=",PDFOUT(6)
c         endif

      ENDIF

      IF (SET_PDF.gt.300 .AND. SET_PDF.lt.400) THEN

c ---- DSSV access convention         
C       OUTPUT:                                                              C
C           DUV   :  X * U VALENCE DISTRIBUTION                              C
C           DDV   :  X * D VALENCE DISTRIBUTION                              C
C           DUBAR :  X * UBAR DISTRIBUTION   	                             C
C           DDBAR :  X * DBAR DISTRIBUTION              	             C
C           DSTR  :  X * STRANGE DISTRIBUTION       		             C
C           DGLU  :  X * GLUON DISTRIBUTION 			             C

         
         if (IDG.eq.1) then
            PDFOUT(1)=UV/DX
            PDFOUT(2)=DV/DX
            PDFOUT(3)=UB/DX
            PDFOUT(4)=DB/DX
            PDFOUT(5)=ST/DX
            PDFOUT(6)=GL/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=UV
            PDFOUT(2)=DV
            PDFOUT(3)=UB
            PDFOUT(4)=DB
            PDFOUT(5)=ST
            PDFOUT(6)=GL
         endif


      ENDIF

      IF (SET_PDF.gt.400 .AND. SET_PDF.lt.500) THEN

c ---- LSS access convention         
C   OUTPUT:  UUB = x *(DELTA u + DELTA ubar)                         C
C            DDB = x *(DELTA d + DELTA dbar)                         C
C            SSB = x *(DELTA s + DELTA sbar)                         C
C            GL  = x * DELTA GLUON                                   C
C            UV  = x * DELTA uv                                      C
C            DV  = x * DELTA dv                                      C
C            UB  = x * DELTA ubar                                    C
C            DB  = x * DELTA dbar                                    C
C            ST  = x * DELTA sbar                                    C

c         PRINT *,"POLAR-LSSOUT, UUB: ",UUB," DDB: ",DDB," SSB: ",SSB,
c     &        " GL: ",GL," UV: ",UV," DV: ",DV," UB: ",UB," DB: ",DB,
c     &        " ST: ",ST
c         PRINT *,"Polar.F - LSS, GL/DX: ",(GL/DX),
c     &        " GL: ",GL,
c     &        " ST/DX: ",((SSB-STB)/DX),
c     &        " ST: ",(SSB-STB),
c     &        " DX: ",DX

         if (IDG.eq.1) then
            PDFOUT(1)=(UUB-UB)/DX
            PDFOUT(2)=(DDB-DB)/DX
            PDFOUT(3)=UB/DX
            PDFOUT(4)=DB/DX
            PDFOUT(5)=(SSB-STB)/DX
            PDFOUT(6)=GL/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=(UUB-UB)
            PDFOUT(2)=(DDB-DB)
            PDFOUT(3)=UB
            PDFOUT(4)=DB
            PDFOUT(5)=(SSB-STB)
            PDFOUT(6)=GL
         endif

c         PRINT *,"LSSOUT: ISET: ",ISET," U=",PDFOUT(1)," D=",
c     &        PDFOUT(2), " UB=",PDFOUT(3)," DB=",PDFOUT(4),
c     &        " ST=",PDFOUT(5)," GL=",PDFOUT(6)         


      ENDIF

      IF (SET_PDF.gt.500 .AND. SET_PDF.lt.600) THEN

c ---- AAC access convention         
C     AACPDFOUT(I) --> AAC03 polarized PDFs.
C      I = -3 ... s-bar quark (x Delta s-bar = x Delta s)
C          -2 ... d-bar quark (x Delta d-bar = x Delta d_sea)
C          -1 ... u-bar quark (x Delta u-bar = x Delta u_sea)
C           0 ... gluon (x Delta g)
C           1 ... u quark (x Delta u = x Delta u_v + x Delta u_sea)
C           2 ... d quark (x Delta d = x Delta d_v + x Delta d_sea)
C           3 ... s quark (x Delta s = x Delta s-bar)

c         PRINT *,"POLAR, AACOUT: ISET: ",ISET," U=",AACPDFOUT(1)," D=",
c     &        AACPDFOUT(2), " UB=",AACPDFOUT(-1)," DB=",AACPDFOUT(-2),
c     &        " ST=",AACPDFOUT(3)," GL=",AACPDFOUT(0)
         
         if (IDG.eq.1) then
            PDFOUT(1)=AACPDFOUT(1)/DX
            PDFOUT(2)=AACPDFOUT(2)/DX
            PDFOUT(3)=AACPDFOUT(-1)/DX
            PDFOUT(4)=AACPDFOUT(-2)/DX
            PDFOUT(5)=AACPDFOUT(3)/DX
            PDFOUT(6)=AACPDFOUT(0)/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=AACPDFOUT(1)
            PDFOUT(2)=AACPDFOUT(2)
            PDFOUT(3)=AACPDFOUT(-1)
            PDFOUT(4)=AACPDFOUT(-2)
            PDFOUT(5)=AACPDFOUT(3)
            PDFOUT(6)=AACPDFOUT(0)
         endif


      ENDIF

      IF (SET_PDF.gt.600 .AND. SET_PDF.lt.700) THEN

c ---- BB access convention         
*   OUTPUT:  UV = DELTA u_v    & error: UVERR                      *
*            DV = DELTA d_v    & error: DVERR                      *
*            GL = DELTA gluon  & error: GLERR                      *
*            QB = DELTA qbar   & error: QBERR                      *   
*            G1P = g_1^proton  & error: G1PERR                     *
*            G1N = g_1^neutron & error: G1NERR                     * 
         
c         PRINT *,"polar - out - BB: ISET: ",ISET," U=",(UV+QB),
c     &        " D=",(DV+QB)," GL=",GL," QB=",QB
c         PRINT *,"polar - out - BB: ISET: ",ISET," U=",((UV+QB)/DX),
c     &        " D=",((DV+QB)/DX)," GL=",(GL/DX)," QB=",(QB/DX)

         if (IDG.eq.1) then
            PDFOUT(1)=(UV+QB)/DX
            PDFOUT(2)=(DV+QB)/DX
            PDFOUT(3)=QB/DX
            PDFOUT(4)=QB/DX
            PDFOUT(5)=QB/DX
            PDFOUT(6)=GL/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=UV+QB
            PDFOUT(2)=DV+QB
            PDFOUT(3)=QB
            PDFOUT(4)=QB
            PDFOUT(5)=QB
            PDFOUT(6)=GL
         endif


      ENDIF

      IF (SET_PDF.gt.700 .AND. SET_PDF.lt.800) THEN

c ---- DNS access convention         
C       OUTPUT:                                                              C
C                    UV :    X * U VALENCE DISTRIBUTION		             C
C                    DV :    X * D VALENCE DISTRIBUTION 		     C
C                    UB :  X * UBAR DISTRIBUTION   	                     C
C                    DB :  X * DBAR DISTRIBUTION              	             C
C                    ST :   X * STRANGE DISTRIBUTION       		     C
C                    GL :   X * GLUON DISTRIBUTION 			     C
C                    G1P :    X * POLARIZED STRUCTURE FUNCTION (PROTON)      C
C                    G1N :    X * POLARIZED STRUCTURE FUNCTION(NEUTRON)      C
         
         if (IDG.eq.1) then
            PDFOUT(1)=(UV+UB)/DX
            PDFOUT(2)=(DV+DB)/DX
            PDFOUT(3)=UB/DX
            PDFOUT(4)=DB/DX
            PDFOUT(5)=ST/DX
            PDFOUT(6)=GL/DX
         endif
         
         if (IDG.eq.0) then
            PDFOUT(1)=(UV+UB)
            PDFOUT(2)=(DV+DB)
            PDFOUT(3)=UB
            PDFOUT(4)=DB
            PDFOUT(5)=ST
            PDFOUT(6)=GL
         endif


      ENDIF


      RETURN
      END
*
*.......................................................................
