* $Id: zerov.F,v 1.1 1998/02/13 14:14:41 fisyak Exp $
* $Log: zerov.F,v $
* Revision 1.1  1998/02/13 14:14:41  fisyak
* move in zerov.F from former uti
*
      SUBROUTINE ZEROV(IA,N)
      IMPLICIT NONE
C Input Arguments
      INTEGER IA(*)     ! Vector to be erased
      INTEGER N         ! Number of elements to erase
C     ZEROV erases to zero a vector.
      INTEGER I
*
      DO I = 1,N
        IA(I) = 0
      ENDDO
*
      RETURN
      END
