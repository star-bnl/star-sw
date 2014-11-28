*72*********************************************************************
      SUBROUTINE INIDEFLO
************************      
*HI    Speed optimized *
************************      
       IMPLICIT NONE

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

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


      INTEGER K,J,LX1,LQ
      DOUBLE PRECISION XX,QQ
      COMMON/ XARRAY / XARRAY
      COMMON/ GRID / GU,GD,GL,GS,GG,GP,GN
      DOUBLE PRECISION XARRAY(102), GU(76,26), GD(76,26), GL(76,26),
     &              GS(76,26), GG(76,26), GP(76,26), GN(76,26) 

      OPEN(IPLST(3),FILE=CPOL,STATUS='OLD')
      DO  K = 1, 76 
         DO  J = 1, 26
            READ(IPLST(3),40)GU(K,J),GD(K,J),GL(K,J),GS(K,J), 
     &                 GG(K,J),GP(K,J),GN(K,J)
         END DO
      END DO
      CLOSE(IPLST(3))
 40   FORMAT (7(1PE15.7))

      DO LX1=0,25
         XX=10.D0**(-4.D0+LX1/25.D0*3.D0)
         XARRAY(LX1+1) = DLOG(XX)
      ENDDO
      DO LX1=1,50
         XX=0.1D0+LX1/50.D0*0.9D0
         XARRAY(LX1+26) = DLOG(XX)
      ENDDO
      DO LQ=0,25
         QQ=0.6D0*10**(5.D0*LQ/25.D0)
         XARRAY(LQ+77) = DLOG(QQ)
      ENDDO
 
      RETURN
      END 
