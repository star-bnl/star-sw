
      SUBROUTINE PHO_BAMASS(I,J,K,Am8,Am82,Am10,Am102,I8,I10)
C**********************************************************************
C
C     determine baryon masses corresponding to the input flavours
C
C     input: I,J,K     quark flavours (PDG convention)
C
C     output: AM8      octett baryon mass
C             AM82     next possible two particle configuration
C                      (octett baryon and meson)
C             AM10     decuplett baryon mass
C             AM102    next possible two particle configuration
C                      (decuplett baryon and meson,
C                       baryon built up from first two quarks)
C             I8,I10   internal baryon numbers
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER I , J , K , I8 , I10
      DOUBLE PRECISION Am8 , Am82 , Am10 , Am102
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  local variables
      INTEGER ii , jj , kk
 
C  find particle ID's
      ii = ABS(I)
      jj = ABS(J)
      kk = ABS(K)
      I8 = ID_b8_list(ii,jj,kk)
      I10 = ID_b10_list(ii,jj,kk)
 
C  masses (if combination possible)
      IF ( I8.NE.0 ) THEN
         Am8 = XM_list(I8)
         I8 = SIGN(I8,I)
      ELSE
         Am8 = 0.D0
      END IF
      IF ( I10.NE.0 ) THEN
         Am10 = XM_list(I10)
         I10 = SIGN(I10,I)
      ELSE
         Am10 = 0.D0
      END IF
 
C  next possible two-particle configurations (add phase space)
      Am82 = XM_b82_list(ii,jj,kk)*1.5D0
      Am102 = XM_b102_list(ii,jj,kk)*1.1D0
 
      END SUBROUTINE
