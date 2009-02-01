*
* $Id: dsmt5t.F,v 1.1.1.2 2009/02/01 17:10:34 fisyak Exp $
*
* $Log: dsmt5t.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:34  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:56  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:57  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:43  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE DSMT5T(A,S,R)
C *** ------------------------------------------------------            05/07/81
C *** TRANSFORMATION OF SYMMETRIC 5X5 MATRIX S: A*S*AT -> R.
C *** A IS A GENERAL 5X5 MATRIX WHEREAS S,R ARE SYMMETRIC
C *** MATRICES STORED IN TRIANGULAR FORM. FOR:I,J=1,5
C *** R(I,J) = SUM:K=1,5(SUM:L=1,5(A(I,L)*S(L,K)*A(J,K))).
C *** NB: S AND R MAY WELL BE THE SAME MATRIX.
C
C        Author: A. Haas (Freiburg University)
C
C *** ------------------------------------------------------
      DOUBLE PRECISION A,S,R,Q,T1,T2,T3,T4,T5
      DIMENSION A(25),S(15),R(15),Q(15)
      DO 5 I=1,15
         Q(I)=S(I)
    5 CONTINUE
      K=1
      DO 15 J=1,5
         T1=A(J   )
         T2=A(J+ 5)
         T3=A(J+10)
         T4=A(J+15)
         T5=A(J+20)
         DO 10 I=J,5
            R(K)=A(I   )*(Q(1)*T1+Q(2)*T2+Q( 3)*T3+Q( 4)*T4+Q( 5)*T5)
     +          +A(I+ 5)*(Q(2)*T1+Q(6)*T2+Q( 7)*T3+Q( 8)*T4+Q( 9)*T5)
     +          +A(I+10)*(Q(3)*T1+Q(7)*T2+Q(10)*T3+Q(11)*T4+Q(12)*T5)
     +          +A(I+15)*(Q(4)*T1+Q(8)*T2+Q(11)*T3+Q(13)*T4+Q(14)*T5)
     +          +A(I+20)*(Q(5)*T1+Q(9)*T2+Q(12)*T3+Q(14)*T4+Q(15)*T5)
            K=K+1
   10    CONTINUE
   15 CONTINUE
*
      END
