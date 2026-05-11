
      INTEGER FUNCTION IPHO_ID2PDG(Idcpc)
C**********************************************************************
C
C     conversion of internal particle code to PDG standard
C
C     input:     IDcpc        internal particle number
C     output:    ipho_id2pdg  PDG particle number
C                             (0 for invalid IDcpc)
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Idcpc
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  particle ID translation table
      INCLUDE 'inc/popar1'
 
      INTEGER idabs
 
      idabs = ABS(Idcpc)
      IF ( (idabs.LT.1) .OR. (idabs.GT.ID_pdg_max) ) THEN
         IPHO_ID2PDG = 0
         RETURN
      END IF
 
      IPHO_ID2PDG = SIGN(ID_pdg_list(idabs),Idcpc)
 
      END FUNCTION
