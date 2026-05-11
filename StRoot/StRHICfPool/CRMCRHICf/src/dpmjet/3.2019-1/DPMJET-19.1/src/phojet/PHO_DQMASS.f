
      SUBROUTINE PHO_DQMASS(I,J,K,L,Am82,Am102)
C**********************************************************************
C
C     determine minimal masses corresponding to the input flavours
C     (diquark a-diquark string system)
C
C     input: I,J,K,L   quark flavours (PDG convention)
C
C     output: AM82     mass of two octett baryons
C             AM102    mass of two decuplett baryons
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER I , J , K , L
      DOUBLE PRECISION Am82 , Am102
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  local variables
      INTEGER ii , jj , kk , ll
 
      ii = ABS(I)
      kk = ABS(K)
      jj = ABS(J)
      ll = ABS(L)
 
      Am82 = XM_bb82_list(ii,jj,kk,ll)
      Am102 = XM_bb102_list(ii,jj,kk,ll)
 
      END SUBROUTINE
