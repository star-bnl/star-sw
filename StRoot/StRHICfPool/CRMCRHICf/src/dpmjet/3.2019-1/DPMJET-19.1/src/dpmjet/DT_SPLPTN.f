
      SUBROUTINE DT_SPLPTN(Nn)
 
C***********************************************************************
C SamPLing of ParToN momenta and flavors.                              *
C This version dated 15.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ecm
      INTEGER Nn
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
 
C sample flavors of sea-quarks
      CALL DT_SPLFLA(Nn,1)
 
C sample x-values of partons at chain ends
      ecm = UMO
      CALL DT_XKSAMP(Nn,ecm)
 
C samle flavors
      CALL DT_SPLFLA(Nn,2)
 
      END SUBROUTINE
