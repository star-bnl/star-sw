
      SUBROUTINE PHO_MEMASS(I,J,Amps,Amps2,Amve,Amve2,Ips,Ive)
C**********************************************************************
C
C     determine meson masses corresponding to the input flavours
C
C     input: I,J,K     quark flavours (PDG convention)
C
C     output: AMPS     pseudo scalar meson mass
C             AMPS2    next possible two particle configuration
C                      (two pseudo scalar  mesons)
C             AMVE     vector meson mass
C             AMVE2    next possible two particle configuration
C                      (two vector mesons)
C             IPS,IVE  meson numbers in CPC
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER I , J , Ips , Ive
      DOUBLE PRECISION Amps , Amps2 , Amve , Amve2
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  local variables
      INTEGER ii , jj
 
      IF ( I.GT.0 ) THEN
         ii = I
         jj = -J
      ELSE
         ii = J
         jj = -I
      END IF
 
C  particle ID's
      Ips = ID_psm_list(ii,jj)
      Ive = ID_vem_list(ii,jj)
C  masses
      IF ( Ips.NE.0 ) THEN
         Amps = XM_list(ABS(Ips))
      ELSE
         Amps = 0.D0
      END IF
      IF ( Ive.NE.0 ) THEN
         Amve = XM_list(ABS(Ive))
      ELSE
         Amve = 0.D0
      END IF
 
C  next possible two-particle configurations (add phase space)
      Amps2 = XM_psm2_list(ii,jj)*1.5D0
      Amve2 = XM_vem2_list(ii,jj)*1.1D0
 
      END SUBROUTINE
