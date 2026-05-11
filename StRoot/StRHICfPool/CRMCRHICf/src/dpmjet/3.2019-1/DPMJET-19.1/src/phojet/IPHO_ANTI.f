
      INTEGER FUNCTION IPHO_ANTI(Id)
C**********************************************************************
C
C     determine antiparticle for given ID
C
C     input:  ID gives CPC particle number
C
C     output: ipho_anti antiparticle code
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  external functions
      INTEGER IPHO_ID2PDG , IPHO_PDG2ID
 
C  local variables
      INTEGER idabs , idpdg , i_anti , l
 
      IPHO_ANTI = -Id
      idabs = ABS(Id)
 
C  baryons
      IF ( IBA3_list(idabs).NE.0 ) RETURN
 
C  charged particles
      IF ( ICH3_list(idabs).NE.0 ) RETURN
 
C  K0_s and K0_l
      idpdg = IPHO_ID2PDG(Id)
      IF ( idpdg.EQ.310 ) THEN
         Id = IPHO_PDG2ID(130)
         RETURN
      ELSE IF ( idpdg.EQ.130 ) THEN
         Id = IPHO_PDG2ID(310)
         RETURN
      END IF
 
C  neutral mesons with open strangeness, charm, or beauty
      i_anti = 0
      DO l = 1 , 3
         i_anti = i_anti + IQ_list(l,idabs)
      END DO
      IF ( i_anti.NE.0 ) RETURN
 
C  neutrinos
      idpdg = ABS(idpdg)
      IF ( (idpdg.EQ.12) .OR. (idpdg.EQ.14) .OR. (idpdg.EQ.16) ) RETURN
 
      IPHO_ANTI = Id
 
      END FUNCTION
