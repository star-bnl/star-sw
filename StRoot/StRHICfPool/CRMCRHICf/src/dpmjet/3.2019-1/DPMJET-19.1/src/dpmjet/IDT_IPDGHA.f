
      INTEGER FUNCTION IDT_IPDGHA(Mcind)
 
C***********************************************************************
C Conversion of particle index BAMJET-index scheme --> PDG proposal    *
C Adopted from the original by S. Roesler. This version dated 12.5.95  *
C Renamed to be not in conflict with the modified PHOJET-version       *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Mcind
      SAVE 
 
C hadron index conversion (BAMJET <--> PDG)
      INCLUDE 'inc/dthaic'
 
      IDT_IPDGHA = IAMcin(Mcind)
 
      END FUNCTION
