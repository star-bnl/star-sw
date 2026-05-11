
      SUBROUTINE DT_DHADDE
 
      IMPLICIT NONE
      INTEGER i , iretur , l
      SAVE 
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C HADRIN: decay channel information
      INCLUDE 'inc/hndech'
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
      INCLUDE 'inc/hnspli'
C decay channel information for HADRIN
      INCLUDE 'inc/hnaddh'
 
      DATA iretur/0/
 
      iretur = iretur + 1
      AMH(31) = 0.48D0
      IF ( iretur.GT.1 ) RETURN
      DO i = 1 , 94
         AMH(i) = AAM(i)
         GAH(i) = GA(i)
         TAUh(i) = TAU(i)
         ICHh(i) = IICh(i)
         IBArh(i) = IIBar(i)
         K1H(i) = K1(i)
         K2H(i) = K2(i)
      END DO
C*sr
C     AMH(1)=0.93828D0
      AMH(1) = 0.9383D0
C*
      AMH(2) = AMH(1)
      DO i = 26 , 30
         K1H(i) = 452
         K2H(i) = 452
      END DO
      DO i = 1 , 307
         WTI(i) = WT(i)
         NZKi(i,1) = NZK(i,1)
         NZKi(i,2) = NZK(i,2)
         NZKi(i,3) = NZK(i,3)
      END DO
      DO i = 1 , 16
         l = i + 94
         AMH(l) = AMZ(i)
         GAH(l) = GAZ(i)
         TAUh(l) = TAUz(i)
         ICHh(l) = ICHz(i)
         IBArh(l) = IBArz(i)
         K1H(l) = K1Z(i)
         K2H(l) = K2Z(i)
      END DO
      DO i = 1 , 153
         l = i + 307
         WTI(l) = WTZ(i)
         NZKi(l,3) = NZK3(i)
         NZKi(l,2) = NZK2(i)
         NZKi(l,1) = NZK1(i)
      END DO
      END SUBROUTINE
