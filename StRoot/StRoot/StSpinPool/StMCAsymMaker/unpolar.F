*.......................................................................
*
      SUBROUTINE UNPOLAR(IDGROUP,DX,DQ2,PDFOUT,UNNEW)      
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
      INTEGER IDGROUP,IDSET,ISET,IINIP
      INTEGER SPECIAL_F2,UNNEW
      DOUBLE PRECISION DX,DQ2
      DOUBLE PRECISION POLPDF(-3:3), STRUCT(3)
      DOUBLE PRECISION U,D,UV,UB,DB,ST,GL,G1P,G1N,G1D,DGL
      DOUBLE PRECISION UUB,DDB,SSB,DV,QB,DQB,DG1P,DG1N
      DOUBLE PRECISION DUBAR,DDBAR
      DOUBLE PRECISION DUV,DDV,DSTR,DGLU
      DOUBLE PRECISION DSF2,DS,US
      DOUBLE PRECISION PDFOUT(6)
      COMMON / INTINIP / IINIP
*
      IINIP=1
      IF (UNNEW.eq.0) IINIP=0
*      PRINT *,"INSIDE UNPOLAR.f"
*      PRINT *,"IDGROUP=",IDGROUP," X=",DX," Q2=",DQ2
      UNNEW=1
*
      SPECIAL_F2=IDGROUP
* GRV pdf selection:
*
      IF (SPECIAL_f2.eq.101) THEN 
*   
* LO 

         ISET=1
         call GRV98PA(ISET,DX,DQ2,UV,DV,US,DS,ST,GL)
*
      ELSEIF (SPECIAL_f2.eq.102) THEN 
*   
* NLO (MS-BAR)

         ISET=2
         call GRV98PA(ISET,DX,DQ2,UV,DV,US,DS,ST,GL)
*
      ELSEIF (SPECIAL_f2.eq.103) THEN 
*   
*   NLO (DIS)
         ISET=3
         call GRV98PA(ISET,DX,DQ2,UV,DV,US,DS,ST,GL)
*
      ENDIF
*
      PRINT *," UV=",UV," DV=",DV," US=",US," DS=",DS," ST=",ST," GL=",GL
      PDFOUT(1)=UV
      PDFOUT(2)=DV
      PDFOUT(3)=US
      PDFOUT(4)=DS
      PDFOUT(5)=ST
      PDFOUT(6)=GL

*
      RETURN
      END
*
*.......................................................................
