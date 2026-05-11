
      SUBROUTINE PHO_ABORT
C**********************************************************************
C
C     top MC event generation due to fatal error,
C     print all information of event generation and history
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i , i3 , ii , k
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
C  hard scattering data
      INCLUDE 'inc/pohslt'
 
      WRITE (LO,'(//,1X,A,/,1X,A)')
     &        'PHO_ABORT: program execution stopped' , 
     &       '===================================='
      WRITE (LO,'(/,1X,A,/,1X,A)') 'listing of available data follows:'
C
      CALL PHO_SETMDL(0,0,-2)
      CALL PHO_PREVNT(-1)
      CALL PHO_ACTPDF(0,-2)
C  print selected parton flavours
      WRITE (LO,'(1X,A,I4)') 'selected soft flavours: ' , KSOft
      DO i = 1 , KSOft
         WRITE (LO,'(10X,2I5)') IJSi1(i) , IJSi2(i)
      END DO
      WRITE (LO,'(1X,A,I4)') 'selected hard flavours: ' , KHArd
      DO k = 1 , KHArd
         i = LSIdx(k)
         WRITE (LO,'(10X,A,I5)') 'process:' , NPRohd(i)
         WRITE (LO,'(10X,A,2I4,7X,A,2I4)') 'initial:' , NINhd(i,1) , 
     &          NINhd(i,2) , 'final:' , NOUthd(i,1) , NOUthd(i,2)
      END DO
C  print selected parton momenta
      WRITE (LO,'(1X,A,I4)') 'selected soft momenta: ' , KSOft
      DO i = 1 , KSOft
         WRITE (LO,'(10X,A,4E12.3)') 'par.1' , (PSOft1(ii,i),ii=1,4)
         WRITE (LO,'(10X,A,4E12.3)') 'par.2' , (PSOft2(ii,i),ii=1,4)
      END DO
      WRITE (LO,'(1X,A,I4)') 'selected hard momenta: ' , KHArd
      DO k = 1 , KHArd
         i = LSIdx(k)
         i3 = 8*i - 4
         WRITE (LO,'(10X,A,4E12.3)') 'par.1' , (PPH(i3+ii,1),ii=1,4)
         WRITE (LO,'(10X,A,4E12.3)') 'par.2' , (PPH(i3+ii,2),ii=1,4)
      END DO
 
C  print /POEVT1/
      CALL PHO_PREVNT(0)
 
C  fragmentation process
      IF ( ISTr.GT.0 ) THEN
C  print /POSTRG/
         CALL PHO_PRSTRG
 
         IF ( ISWmdl(6).GE.0 ) CALL PYLIST(1)
 
      END IF
 
C  last message
      WRITE (LO,'(////5X,A,///5X,A,///)')
     &        'PHO_ABORT: execution terminated due to fatal error' , 
     &'*** Simulating division by zero to get traceback information ***'
      ISTr = 100/IPAmdl(100)
 
      END SUBROUTINE
