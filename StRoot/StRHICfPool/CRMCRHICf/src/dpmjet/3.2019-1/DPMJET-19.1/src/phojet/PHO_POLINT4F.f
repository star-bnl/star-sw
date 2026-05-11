
      SUBROUTINE PHO_POLINT4F(Xa,Ya,X,Y)
 
      IMPLICIT NONE
      DOUBLE PRECISION c1 , c2 , c3 , cc1 , cc2 , cd1 , cd2 , d1 , d2 , 
     &                 d3 , dc1 , dd1 , den , h1 , h2 , h3 , h4 , w , 
     &                 X , Xa
      DOUBLE PRECISION Y , Ya
C  The POLINT4 routine is based on the POLINT routine from "Numerical Recipes",
C  but assuming N=4, and ignoring the error estimation.
C  suggested by Z. Sullivan.
      DIMENSION Xa(*) , Ya(*)
 
      h1 = Xa(1) - X
      h2 = Xa(2) - X
      h3 = Xa(3) - X
      h4 = Xa(4) - X
 
      w = Ya(2) - Ya(1)
      den = w/(h1-h2)
      d1 = h2*den
      c1 = h1*den
 
      w = Ya(3) - Ya(2)
      den = w/(h2-h3)
      d2 = h3*den
      c2 = h2*den
 
      w = Ya(4) - Ya(3)
      den = w/(h3-h4)
      d3 = h4*den
      c3 = h3*den
 
      w = c2 - d1
      den = w/(h1-h3)
      cd1 = h3*den
      cc1 = h1*den
 
      w = c3 - d2
      den = w/(h2-h4)
      cd2 = h4*den
      cc2 = h2*den
 
      w = cc2 - cd1
      den = w/(h1-h4)
      dd1 = h4*den
      dc1 = h1*den
 
      IF ( (h3+h4).LT.0D0 ) THEN
         Y = Ya(4) + d3 + cd2 + dd1
      ELSE IF ( (h2+h3).LT.0D0 ) THEN
         Y = Ya(3) + d2 + cd1 + dc1
      ELSE IF ( (h1+h2).LT.0D0 ) THEN
         Y = Ya(2) + c2 + cd1 + dc1
      ELSE
         Y = Ya(1) + c1 + cc1 + dc1
      END IF
 
C               *************************
      END SUBROUTINE
