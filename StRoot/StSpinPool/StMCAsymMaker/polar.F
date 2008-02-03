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
      DOUBLE PRECISION PDFOUT(6)
      COMMON / INTINI / IINI

*     FOR GRSV: The main program or the calling routine has  *
*     to have a common block  COMMON / INTINI / IINI, and    *
*     IINI must be zero when PARPOL2 is called for the       *
*     first time or when 'ISET' has been changed.            *

*     FOR GS: Must call NLOINI first time                    *

      SET_PDF=IDGROUP
      IINI=1

      IF (NEW.eq.0) THEN
         IINI=0
         IF (SET_PDF.gt.200) THEN
            CALL NLOINI
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

      IF (SET_PDF.gt.200) THEN

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

*

      RETURN
      END
*
*.......................................................................
