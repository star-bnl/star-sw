
      SUBROUTINE DT_SPLFLA(Nn,Mode)
 
C***********************************************************************
C SamPLing of FLAvors of partons at chain ends.                        *
C This subroutine replaces FLKSAA/FLKSAM.                              *
C            NN            number of nucleon-nucleon interactions      *
C            MODE = 1      sea-flavors                                 *
C                 = 2      valence-flavors                             *
C Based on the original version written by J. Ranft/H.-J. Moehring.    *
C This version dated 16.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM
      INTEGER i , MAXINT , MAXNCL , MAXSQU , MAXVQU , Mode , Nn
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C flavors of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmf'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmi'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpm0'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
      IF ( Mode.EQ.1 ) THEN
C sea-flavors
         DO i = 1 , Nn
            IPSq(i) = INT(1.0D0+DT_RNDM(CROnco)*(2.0D0+SEAsq))
            IPSaq(i) = -IPSq(i)
         END DO
         DO i = 1 , Nn
            ITSq(i) = INT(1.0D0+DT_RNDM(CROnco)*(2.0D0+SEAsq))
            ITSaq(i) = -ITSq(i)
         END DO
      ELSE IF ( Mode.EQ.2 ) THEN
C valence flavors
         DO i = 1 , IXPv
            CALL DT_FLAHAD(KKProj(IFRovp(i)),IPVq(i),IPPv1(i),IPPv2(i))
         END DO
         DO i = 1 , IXTv
            CALL DT_FLAHAD(KKTarg(IFRovt(i)),ITVq(i),ITTv1(i),ITTv2(i))
         END DO
      END IF
 
      END SUBROUTINE
