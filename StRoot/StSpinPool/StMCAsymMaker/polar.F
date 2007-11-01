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
      INTEGER SPECIAL_F2,IDG,IINI
      DOUBLE PRECISION DX,DQ2
      DOUBLE PRECISION POLPDF(-3:3), STRUCT(3)
      DOUBLE PRECISION U,D,UB,DB,ST,GL,G1P,G1N,G1D,DGL
      DOUBLE PRECISION UUB,DDB,SSB,UV,DV,QB,DQB,DG1P,DG1N
      DOUBLE PRECISION DUBAR,DDBAR
      DOUBLE PRECISION DUV,DDV,DSTR,DGLU
      DOUBLE PRECISION DSF2
      DOUBLE PRECISION PDFOUT(6)
      COMMON / INTINI / IINI

*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINI / IINI , and  IINI     *
*            has always to be zero when PARPOL2 is called for the   *
*            first time or when 'ISET' has been changed.            *

      SPECIAL_F2=IDGROUP
*
*      PRINT *,"INSIDE POLAR.f"
*      PRINT *,"IDGROUP=",IDGROUP," X=",DX," Q2=",DQ2
 
*     IF IDG==0 then returns x*PDF 
*     IF IDG!=0 then returns PDF only
      IDG=1
*
      IINI=1
      IF (NEW.eq.0) IINI=0
      NEW=1

* GRSV pdf selection:
*
      IF (SPECIAL_f2.eq.101) THEN 
*   
* GRSV STANDARD LO
*
         ISET=3
         call PARPOL(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.102) THEN 
*   
* GRSV STANDARD NLO 
*        
         ISET=1
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.103) THEN 
*  
* GRSV STANDARD NLO deltag=0
*        
         ISET=3
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.104) THEN 
*   
*  
* GRSV STANDARD NLO deltag=maximum allowed g
*        
         ISET=4
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.105) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag=minimum allowed g
*        
         ISET=2
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.106) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m015
*        
         ISET=5
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.107) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m030
*        
         ISET=6
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.108) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m045
*        
         ISET=7
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.109) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m060
*        
         ISET=8
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.110) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m075
*        
         ISET=9
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.111) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m090
*        
         ISET=10
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.112) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= m105
*        
         ISET=11
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SPECIAL_f2.eq.113) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p030
*        
         ISET=12
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SPECIAL_f2.eq.114) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p045
*        
         ISET=13
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SPECIAL_f2.eq.115) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p060
*        
         ISET=14
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
      ELSEIF (SPECIAL_f2.eq.116) THEN 
*   
*   
*  
* GRSV STANDARD NLO deltag= p070
*        
         ISET=15
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ENDIF
*

*      PRINT *," U=",U," D=",D," UB=",UB," DB=",DB," ST=",ST," GL=",GL

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

*

      RETURN
      END
*
*.......................................................................
