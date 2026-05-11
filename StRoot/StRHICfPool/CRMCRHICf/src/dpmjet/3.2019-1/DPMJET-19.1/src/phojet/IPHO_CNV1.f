
      INTEGER FUNCTION IPHO_CNV1(Ipart)
C*********************************************************************
C
C     conversion of quark numbering scheme to PARTICLE DATA GROUP
C                                             convention
C
C     input:   old internal particle code of hard scattering
C                    0   gluon
C                    1   d
C                    2   u
C                    3   s
C                    4   c
C     valence quarks changed to standard numbering
C
C     output:  standard particle codes
C
C*********************************************************************
      IMPLICIT NONE
      INTEGER ii , Ipart
      SAVE 
C
      ii = ABS(Ipart)
C  change gluon number
      IF ( ii.EQ.0 ) THEN
         IPHO_CNV1 = 21
C  change valence quark
      ELSE IF ( (ii.GT.6) .AND. (ii.LT.13) ) THEN
         IPHO_CNV1 = SIGN(ii-6,Ipart)
      ELSE
         IPHO_CNV1 = Ipart
      END IF
      END FUNCTION
