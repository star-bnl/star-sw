
C*********************************************************************
      subroutine nloini
      implicit real*8(a-h,o-z)
      common/pdist/arraya(151,20,6)
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

      
      open(IPLST(3),file=CPOL,status='UNKNOWN')
      do i=1,20
         do j=1,151
            read(IPLST(3),901) arraya(j,i,1),arraya(j,i,2),
     &        arraya(j,i,3),arraya(j,i,4),arraya(j,i,5),arraya(j,i,6)
         enddo
      enddo
      close(IPLST(3))

 901  format(6f14.9)

      return
      end
