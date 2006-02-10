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
*            has always to be zero when PARPOL2 is called for the    *
*            first time or when 'ISET' has been changed.            *

      SPECIAL_F2=IDGROUP
*
      PRINT *,"INSIDE POLAR.f"
      PRINT *,"IDGROUP=",IDGROUP," X=",DX," Q2=",DQ2
 
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
* STANDARD SCENARIO, NLO (MS-BAR)
*
         ISET=1
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.102) THEN 
*   
* VALENCE SCENARIO, NLO (MS-BAR)
*        
         ISET=2
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.103) THEN 
*   
* STANDARD SCENARIO, LO 
*  
         ISET=3
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.104) THEN 
*   
* VALENCE SCENARIO, LO 
*   
         ISET=4
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
      ELSEIF (SPECIAL_f2.eq.105) THEN 
*   
* MAXIMUM SCENARIO, NLO 
*   
         ISET=5
         call PARPOL2(ISET,DX,DQ2,U,D,UB,DB,ST,GL,G1P,G1N)
*
* GS pdf selection:
*
      ELSEIF (SPECIAL_f2.eq.201) THEN 
*   
* NLO, SET A
*   
         ISET=0
C         call polnlo(ISET,DX,DQ2,U,D,GL,UB,DB,ST)

*
      ELSEIF (SPECIAL_f2.eq.202) THEN 
*   
* NLO, SET B
*   
         ISET=1
C         call polnlo(ISET,DX,DQ2,U,D,GL,UB,DB,ST)

*
      ELSEIF (SPECIAL_f2.eq.203) THEN 
*   
* NLO, SET C
*   
         ISET=2
C         call polnlo(ISET,DX,DQ2,U,D,GL,UB,DB,ST)
*
      ELSEIF (SPECIAL_f2.eq.204) THEN 
*   
* LO, SET A
*
         ISET=0
C         call polpar(ISET,DX,DQ2,U,D,GL,UB,DB,ST)
*
      ELSEIF (SPECIAL_f2.eq.205) THEN 
*   
* LO, SET B
*   
         ISET=1
C         call polpar(ISET,DX,DQ2,U,D,GL,UB,DB,ST)
*
      ELSEIF (SPECIAL_f2.eq.206) THEN 
*   
* LO, SET C
*   
         ISET=2
C         call polpar(ISET,DX,DQ2,U,D,GL,UB,DB,ST)
*
* LSS pdf selection:
*
      ELSEIF (SPECIAL_f2.eq.301) THEN
*   
* SET 1, NLO (JET)
*
         ISET=1
C         call LSS2001(ISET,DX,DQ2,UUB,DDB,SSB,GL,UV,DV,UB,DB,ST)
*
      ELSEIF (SPECIAL_f2.eq.302) THEN
*   
* SET 2, NLO (MS-bar)
*
         ISET=2
C         call LSS2001(ISET,DX,DQ2,UUB,DDB,SSB,GL,UV,DV,UB,DB,ST)
*
      ELSEIF (SPECIAL_f2.eq.303) THEN
*   
* SET 3, LO 
*
         ISET=3
C         call LSS2001(ISET,DX,DQ2,UUB,DDB,SSB,GL,UV,DV,UB,DB,ST)
*
* ACC pdf selection:
*
      ELSEIF (SPECIAL_f2.eq.401) THEN
*   
* SET 1, LO 
*
         ISET=1
C         call AACPDF(ISET,DQ2,DX,POLPDF,STRUCT)
*
      ELSEIF (SPECIAL_f2.eq.402) THEN
*   
* SET 2, NLO SET1
*
         ISET=2
C         call AACPDF(ISET,DQ2,DX,POLPDF,STRUCT)
*
      ELSEIF (SPECIAL_f2.eq.403) THEN
*   
* SET 3, NLO SET2
*
         ISET=3
C         call AACPDF(ISET,DQ2,DX,POLPDF,STRUCT)
*
* BB pdf selection:
*
      ELSEIF (SPECIAL_f2.eq.501) THEN
*   
* SET 1, LO (SET1) 
*
         ISET=1
C         call PPDF(ISET,DX,DQ2,UV,DUV,DV,DDV,GL,DGL,
C     &             QB,DQB,G1P,DG1P,G1N,DG1N)
*
      ELSEIF (SPECIAL_f2.eq.502) THEN
*   
* SET 2, LO (SET2) 
*
         ISET=2
C         call PPDF(ISET,DX,DQ2,UV,DUV,DV,DDV,GL,DGL,
C     &             QB,DQB,G1P,DG1P,G1N,DG1N)
*
      ELSEIF (SPECIAL_f2.eq.503) THEN
*   
* SET 3, NLO (SET1) 
*
         ISET=3
C         call PPDF(ISET,DX,DQ2,UV,DUV,DV,DDV,GL,DGL,
C     &             QB,DQB,G1P,DG1P,G1N,DG1N)
*
      ELSEIF (SPECIAL_f2.eq.504) THEN
*   
* SET 4, NLO (SET2) 
*
         ISET=4
C         call PPDF(ISET,DX,DQ2,UV,DUV,DV,DDV,GL,DGL,
C     &             QB,DQB,G1P,DG1P,G1N,DG1N)
*
* DS pdf selection:
*
      ELSEIF (SPECIAL_f2.ge.601.and.SPECIAL_f2.le.612) THEN 
*   
* STANDARD SCENARIO, NLO (MS-BAR)
*
         ISET=SPECIAL_f2-600
C         call DS2000(ISET,DX,DQ2,DUV,DDV,DUBAR,DDBAR,
C     &                               DSTR,DGLU,G1P,G1N,G1D)

*
      ENDIF
*

      PRINT *," U=",U," D=",D," UB=",UB," DB=",DB," ST=",ST," GL=",GL

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
