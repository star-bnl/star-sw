
      INTEGER FUNCTION IPHO_CHR3(Id,Mode)
C**********************************************************************
C
C     output of three times the electric charge
C
C     input:  mode
C             0   ID gives CPC particle number
C             1   ID gives PDG particle number
C             2   ID gives position of particle in /POEVT1/
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id , Mode
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  external functions
      INTEGER IPHO_PDG2ID
 
C  local variables
      INTEGER i , idpdg
 
      IPHO_CHR3 = 0
 
      IF ( Mode.EQ.0 ) THEN
         i = Id
      ELSE IF ( Mode.EQ.1 ) THEN
         i = IPHO_PDG2ID(Id)
         IF ( i.EQ.0 ) RETURN
         idpdg = Id
      ELSE IF ( Mode.EQ.2 ) THEN
         IF ( ISThep(Id).GT.11 ) RETURN
         i = IMPart(Id)
         idpdg = IDHep(Id)
         IF ( (idpdg.EQ.90) .OR. (idpdg.EQ.91) .OR. (idpdg.EQ.92) ) THEN
            IPHO_CHR3 = ICOlor(1,Id)
            RETURN
         END IF
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,2i4)')
     &         'ipho_chr3: invalid mode (ID,mode): ' , Id , Mode
         RETURN
      END IF
 
      IF ( (i.EQ.0) .OR. (ABS(i).GT.ID_pdg_max) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,3i8)')
     &         'ipho_chr3: invalid arguments (ID,mode,i): ' , Id , 
     &        Mode , i
         STOP
      END IF
 
      IPHO_CHR3 = ICH3_list(ABS(i))*SIGN(1,i)
 
      END FUNCTION
