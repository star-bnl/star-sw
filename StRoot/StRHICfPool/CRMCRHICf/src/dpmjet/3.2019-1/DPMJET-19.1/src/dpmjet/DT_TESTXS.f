
      SUBROUTINE DT_TESTXS
 
      IMPLICIT NONE
      DOUBLE PRECISION adp , adplab , aplabh , aplabl , dumecm , p , 
     &                 plabh , plabl , xsela , xstot
      INTEGER i , j , k , nbins
      SAVE 
 
      DIMENSION xstot(26,2) , xsela(26,2)
 
      OPEN (10,FILE='testxs_ptot.out',STATUS='UNKNOWN')
      OPEN (11,FILE='testxs_pela.out',STATUS='UNKNOWN')
      OPEN (12,FILE='testxs_ntot.out',STATUS='UNKNOWN')
      OPEN (13,FILE='testxs_nela.out',STATUS='UNKNOWN')
      dumecm = 0.0D0
      plabl = 0.01D0
      plabh = 10000.0D0
      nbins = 120
      aplabl = LOG10(plabl)
      aplabh = LOG10(plabh)
      adplab = (aplabh-aplabl)/DBLE(nbins)
      DO i = 1 , nbins + 1
         adp = aplabl + DBLE(i-1)*adplab
         p = 10.0D0**adp
         DO j = 1 , 26
            CALL DT_XSHN(j,1,p,dumecm,xstot(j,1),xsela(j,1))
            CALL DT_XSHN(j,8,p,dumecm,xstot(j,2),xsela(j,2))
         END DO
         WRITE (10,99010) p , (xstot(k,1),k=1,26)
         WRITE (11,99010) p , (xsela(k,1),k=1,26)
         WRITE (12,99010) p , (xstot(k,2),k=1,26)
         WRITE (13,99010) p , (xsela(k,2),k=1,26)
      END DO
99010 FORMAT (F8.3,26F9.3)
 
      END SUBROUTINE
