
      DOUBLE PRECISION FUNCTION DT_EBIND(Ia,Iz)
 
C***********************************************************************
C Binding energy for nuclei.                                           *
C (Shirokov & Yudin, Yad. Fizika, Nauka, Moskva 1972)                  *
C                 IA        mass number                                *
C                 IZ        atomic number                              *
C This version dated 5.5.95   is updated by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION a1 , a2 , a3 , a4 , a5 , aa , ZERO
      INTEGER Ia , ia5 , Iz
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0)
 
      DATA a1 , a2 , a3 , a4 , a5/0.01575D0 , 0.0178D0 , 0.000710D0 , 
     &     0.0237D0 , 0.034D0/
 
      IF ( (Ia.LE.1) .OR. (Iz.EQ.0) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,2I5)')
     &         'DT_EBIND IA,IZ set EBIND=0.  ' , Ia , Iz
         DT_EBIND = ZERO
         RETURN
      END IF
      aa = Ia
      DT_EBIND = a1*aa - a2*aa**0.666667D0 - a3*Iz*Iz*aa**(-0.333333D0)
     &           - a4*(Ia-2*Iz)**2/aa
      IF ( MOD(Ia,2).EQ.1 ) THEN
         ia5 = 0
      ELSE IF ( MOD(Iz,2).EQ.1 ) THEN
         ia5 = 1
      ELSE
         ia5 = -1
      END IF
      DT_EBIND = DT_EBIND - ia5*a5*aa**(-0.75D0)
 
      END FUNCTION
