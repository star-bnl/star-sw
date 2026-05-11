
      SUBROUTINE PHO_PRSTRG
C**********************************************************************
C
C     print information of /POSTRG/
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I5)') 
     &                        'PHO_PRSTRG: number of strings soft+hard:'
     &                        , ISTr
      IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A/,1X,A)') 'COMMON /POSTRG/:' , 
     &           ' NOBAM  ID1  ID2  ID3  ID4     NPO1/2/3/4        MASS'
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &        ' ======================================================='
      DO i = 1 , ISTr
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,9I5,1P,E11.3)') NCOde(i) , 
     &        IPAr1(i) , IPAr2(i) , IPAr3(i) , IPAr4(i) , NPOs(1,i) , 
     &        NPOs(2,i) , NPOs(3,i) , NPOs(4,i) , PHEp(5,NPOs(1,i))
      END DO
 
      END SUBROUTINE
