
      CHARACTER*15 FUNCTION PHO_PNAME(Id,Mode)
C***********************************************************************
C
C     returns particle name for given ID number
C
C     input:  ID      particle ID number
C             mode    0:   ID treated as compressed particle code
C                     1:   ID treated as PDG number
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id , Mode
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  external functions
      INTEGER IPHO_ID2PDG , IPHO_PDG2ID
 
C  local variables
      INTEGER idpdg , i , ii , k , l , ichar , i_anti
      CHARACTER*15 name
 
      PHO_PNAME = '(?????????????)'
 
      IF ( Mode.EQ.0 ) THEN
         i = Id
         idpdg = IPHO_ID2PDG(Id)
         IF ( idpdg.EQ.0 ) RETURN
      ELSE IF ( Mode.EQ.1 ) THEN
         i = IPHO_PDG2ID(Id)
         IF ( i.EQ.0 ) RETURN
         idpdg = Id
      ELSE IF ( Mode.EQ.2 ) THEN
         IF ( ISThep(Id).GT.11 ) THEN
            IF ( ISThep(Id).EQ.20 ) THEN
               PHO_PNAME = 'hard ini. part.'
            ELSE IF ( ISThep(Id).EQ.21 ) THEN
               PHO_PNAME = 'hard fin. part.'
            ELSE IF ( ISThep(Id).EQ.25 ) THEN
               PHO_PNAME = 'hard scattering'
            ELSE IF ( ISThep(Id).EQ.30 ) THEN
               PHO_PNAME = 'diff. diss.    '
            ELSE IF ( ISThep(Id).EQ.35 ) THEN
               PHO_PNAME = 'elastic scatt. '
            ELSE IF ( ISThep(Id).EQ.40 ) THEN
               PHO_PNAME = 'central scatt. '
            END IF
            RETURN
         END IF
         idpdg = IDHep(Id)
         i = IMPart(Id)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,2i4)')
     &         'pho_pname: invalid arguments (ID,mode): ' , Id , Mode
         RETURN
      END IF
 
      ii = ABS(i)
      IF ( (ii.EQ.0) .OR. (ii.GT.ID_pdg_max) ) RETURN
 
      name = NAMe_list(ii)
      ichar = ICH3_list(ii)*SIGN(1,i)
      IF ( MOD(ichar,3).NE.0 ) THEN
         ichar = 0
      ELSE
         ichar = ichar/3
      END IF
 
C  find position of first blank character
      k = 1
 100  k = k + 1
      IF ( name(k:k).NE.' ' ) GOTO 100
 
C  append anti-particle sign
      IF ( i.LT.0 ) THEN
         i_anti = 0
         DO l = 1 , 3
            i_anti = i_anti + IQ_list(l,ii)
         END DO
         IF ( IBA3_list(ii).NE.0 ) THEN
            name(k:k) = '~'
            k = k + 1
         ELSE IF ( ((i_anti.NE.0) .AND. (ichar.EQ.0)) .OR. 
     &             (idpdg.EQ.-12) .OR. (idpdg.EQ.-14) .OR. 
     &             (idpdg.EQ.-16) ) THEN
            name(k:k) = '~'
            k = k + 1
         END IF
      END IF
 
C  append charge sign
      IF ( ichar.EQ.-2 ) THEN
         name(k:k+1) = '--'
      ELSE IF ( ichar.EQ.-1 ) THEN
         name(k:k) = '-'
      ELSE IF ( ichar.EQ.1 ) THEN
         name(k:k) = '+'
      ELSE IF ( ichar.EQ.2 ) THEN
         name(k:k+1) = '++'
      END IF
 
      PHO_PNAME = name
 
      END FUNCTION
