
      INTEGER FUNCTION IPHO_PDG2ID(Idpdg)
C**********************************************************************
C
C     calculation internal particle code using the particle index i
C     according to the PDG proposal.
C
C     input:  IDpdg          PDG particle number
C     output: ipho_pdg2id    internal particle code
C                            (0 for invalid IDpdg)
C
C     the hash algorithm is based on a program by Gerry Lynch
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Idpdg
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  particle ID translation table
      INCLUDE 'inc/popar1'
 
      INTEGER nin , nout
 
      nin = ABS(Idpdg)
 
      IF ( (nin.GT.99999) .OR. (nin.EQ.0) ) THEN
C  invalid particle number
         IF ( LPRi.GT.4 .AND. IDEb(71).GT.5 ) WRITE (LO,'(1x,A,I10)')
     &         'ipho_pdg2id: invalid PDG ID number ' , Idpdg
         IPHO_PDG2ID = 0
         RETURN
      ELSE IF ( nin.LE.577 ) THEN
C  simple case
         nout = nin
      ELSE
C  use hash algorithm
         nout = MOD(nin,577)
      END IF
 
 
C  particle not in table
 100  IF ( ID_list(nout).EQ.0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(71).GE.0 ) WRITE (LO,'(1x,A,I10)')
     &         'ipho_pdg2id: particle not in table ' , Idpdg
         IPHO_PDG2ID = 0
         RETURN
      END IF
 
      IF ( ID_pdg_list(ID_list(nout)).EQ.nin ) THEN
C  particle ID found
         IPHO_PDG2ID = SIGN(ID_list(nout),Idpdg)
         RETURN
      ELSE
C  increment and try again
         nout = nout + 5
         IF ( nout.GT.577 ) nout = MOD(nout,577)
         GOTO 100
      END IF
 
      END FUNCTION
