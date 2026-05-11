
      INTEGER FUNCTION IDT_MCHAD(Itdtu)
 
C***********************************************************************
C Conversion of particle index BAMJET-index scheme --> HADRIN index s. *
C Adopted from the original by S. Roesler. This version dated 6.5.95   *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Itdtu , itrans
      SAVE 
 
      DIMENSION itrans(210)
      DATA itrans/1 , 2 , -1 , -1 , -1 , -1 , -1 , 8 , 9 , -1 , -1 , 
     &     24 , 13 , 14 , 15 , 16 , 8 , 9 , 25 , 8 , 1 , 8 , 23 , 24 , 
     &     25 , -1 , -1 , -1 , -1 , -1 , 23 , 13 , 23 , 14 , 23 , 15 , 
     &     24 , 16 , 25 , 15 , 24 , 16 , 25 , 15 , 24 , 16 , 25 , 1 , 
     &     8 , 8 , 8 , 1 , 1 , 1 , 8 , 8 , 1 , 1 , 8 , 8 , 1 , 8 , 1 , 
     &     8 , 1 , 8 , 2 , 2 , 9 , 9 , 2 , 2 , 9 , 9 , 2 , 9 , 1 , 13 , 
     &     23 , 14 , 1 , 1 , 8 , 8 , 1 , 1 , 23 , 14 , 1 , 8 , 1 , 8 , 
     &     1 , 8 , 23 , 23 , 8 , 8 , 2 , 9 , 9 , 9 , 9 , 1 , 8 , 8 , 8 , 
     &     8 , 8 , 2 , 9 , 9 , 9 , 9 , 9 , 85* - 1 , 7* - 1 , 1 , 8 , 
     &     -1/
 
      IF ( Itdtu.GT.0 ) THEN
         IDT_MCHAD = itrans(Itdtu)
      ELSE
         IDT_MCHAD = -1
      END IF
 
      END FUNCTION
