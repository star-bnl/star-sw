
      DOUBLE PRECISION FUNCTION PHO_PMASS(Id,Mode)
C***********************************************************************
C
C     particle mass
C
C     input:  mode  -1   initialization
C                    0   ID gives CPC particle number
C                    1   ID gives PDG particle number,
C                        (for quarks current masses are returned)
C                    2   ID gives position of particle in /POEVT1/
C                    3   ID gives PDG parton number,
C                        (for quarks constituent masses are returned)
C
C     output: average particle mass (in GeV)
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id , Mode , mstj24
 
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
 
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
      INCLUDE 'inc/pydat1'
 
C  external functions
      INTEGER IPHO_PDG2ID , IPHO_ID2PDG
 
      DOUBLE PRECISION PYMASS
 
C  local variables
      INTEGER i , idpdg
 
      PHO_PMASS = 0.D0
 
      IF ( Mode.EQ.0 ) THEN
         i = Id
      ELSE IF ( Mode.EQ.1 ) THEN
         i = IPHO_PDG2ID(Id)
         IF ( i.EQ.0 ) RETURN
      ELSE IF ( Mode.EQ.2 ) THEN
         IF ( ISThep(Id).GT.11 ) RETURN
         i = IMPart(Id)
         idpdg = IDHep(Id)
         IF ( (idpdg.EQ.90) .OR. (idpdg.EQ.91) .OR. (idpdg.EQ.92) ) THEN
            PHO_PMASS = PHEp(5,Id)
            RETURN
         END IF
      ELSE IF ( Mode.EQ.3 ) THEN
         i = ABS(Id)
         IF ( (i.GT.0) .AND. (i.LE.6) ) THEN
            PHO_PMASS = PARmdl(150+i)
            RETURN
         ELSE
            i = IPHO_PDG2ID(Id)
            IF ( i.EQ.0 ) RETURN
         END IF
      ELSE IF ( Mode.EQ.-1 ) THEN
C  initialization: take masses for quarks and di-quarks from JETSET
         mstj24 = MSTj(24)
         MSTj(24) = 0
         DO i = 1 , 22
            idpdg = IPHO_ID2PDG(i)
 
            XM_list(i) = PYMASS(idpdg)
 
         END DO
         MSTj(24) = mstj24
         RETURN
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,2i4)')
     &         'pho_pmass: invalid arguments (ID,mode): ' , Id , Mode
         RETURN
      END IF
 
      IF ( (i.EQ.0) .OR. (ABS(i).GT.ID_pdg_max) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,2i8)')
     &         'pho_pmass: invalid arguments (ID,mode): ' , Id , Mode
         PHO_PMASS = 1.D0/DBLE(i)
         RETURN
      END IF
 
      PHO_PMASS = XM_list(ABS(i))
 
      END FUNCTION
