* $Id: dzelin.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzelin.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:24  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:11  nevski
* dzdoc for agi
*
      SUBROUTINE DZELIN(LUNIND,LO)
#include "dzedit/dzeindex.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      NIND=0
      DO 10 I=1,MAXBNK
         CALL DZDCRD(LUNIND,CINDEX(I),IRC)
         IF(IRC.NE.0)GOTO 20
*         READ(LUNIND,'(A4)',END=20)CINDEX(I)
         IORD(I)=I
10       NIND=NIND+1
      WRITE(*,*)'More than ',MAXBNK,' banks, cant make index'
      GOTO 100
20    CONTINUE
      IF(NIND .LE. 0)GOTO 100
      DO 30  I=1,NIND-1
         DO 30  K=I+1,NIND
            I1 = IORD(I)
            K1 = IORD(K)
            IF(LLE(CINDEX(I1),CINDEX(K1)))GOTO 30
            IORD(K) = I1
            IORD(I) = K1
   30 CONTINUE
      WRITE(LO,'(A)')BS//'twocolumn[Index]'
      DO 40 I=1,NIND
        WRITE(LO,'(A,A,A,I4.4,A)')BS//'parbox{4cm}{',CINDEX(IORD(I)),
     &  ' '//BS//'dotfill '//BS//'pageref{',IORD(i),'}}'//BS//BS
40    CONTINUE
100   CONTINUE
      RETURN
      END

