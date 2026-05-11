
      SUBROUTINE PHO_PRESEL(Mode,Irej)
C**********************************************************************
C
C     user specific function to pre-select events during generation
C
C     input:   MODE  5  electron and photon kinematics
C                   10  process and number of cut Pomerons
C                   15  partons without construction of strings
C                   20  partons assigned to strings
C                   25  after fragmentation, complete final state
C
C     output:  IREJ  0  event accepted
C                   50  event rejected
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER Irej , Mode
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  hard scattering data
      INCLUDE 'inc/pohslt'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      Irej = 0
 
C     XBJ = GQ2(2)/(GGECM**2+GQ2(2))
C     IF(XBJ.LT.0.002D0) IREJ = 1
 
      END SUBROUTINE
