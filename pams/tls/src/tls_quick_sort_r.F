* $Id: tls_quick_sort_r.F,v 1.2 1998/04/09 15:18:33 fisyak Exp $
* $Log: tls_quick_sort_r.F,v $
* Revision 1.2  1998/04/09 15:18:33  fisyak
* tls bug fixes from Duncan J. Prindle
*
      INTEGER FUNCTION tls_Quick_Sort_r( nrows, el1, el2, table )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C
C It would be nice to clean this program up.
C Actual sort of table assuming element to be sorted on is real.
C Taken from Numerical Recipes.
C        Return Values
C           tls_sort_normal_cv (S):        Successful.
C           tls_sort_qs_longrow_cv (E):    temp. row space not long enough.
C           tls_sort_qs_stackshort_cv (E): stack in quicksort too short.
C
C 9/ 8/95 Add code to handle 0,1 row tables   djp
C 4/ 8/98 Changed method of choosing pivot.
C         Made a start on commenting the code. djp

      INTEGER M, NSTACK
      REAL    FM, FA, FC, FMI
      PARAMETER (M=7,NSTACK=50)
C Inputs:
      INTEGER nrows         ! Number of input rows.
      REAL    el1(*)        ! Element to sort on (from first row of table)
      REAL    el2           ! Element to sort on (from second row of table)
      REAL    table(*)      ! table to be sorted.
C
      INTEGER tls_loc
      INTEGER len, JSTACK, L, LL, I, IR, IQ, J
      REAL    t(200), tel1
      INTEGER ISTACK(NSTACK)

C Make sure there are at least two rows. (else table can be considered sorted)

      IF (nrows.LT.2) THEN
          tls_Quick_Sort_r = tls_sort_normal_cv
          RETURN
          END IF

C Make sure our temporary array is long enough for one row of the table.

      len = (tls_loc(el2) - tls_loc(el1(1)))/4
      IF (len.GT.200) THEN
          tls_Quick_Sort_r = tls_sort_qs_longrow_cv
          RETURN
          END IF

C Initialize partition pointers to beginning and end or table.

      JSTACK = 0
      L  = 1
      IR = nrows

C Main loop starts here.

10    IF(IR-L.LT.M)THEN

C Current partition is seven rows (or shorter)
C Sort this by `straight insertion'

        DO 13 J=L+1,IR
          tel1 = el1(1+len*(J-1))
          DO LL = 1,len
             t(LL) = table(LL+len*(J-1))
             END DO
          DO 11 I=J-1,1,-1
            IF(el1(1+len*(I-1)).LE.tel1)GO TO 12
            DO LL = 1,len
               table(LL+len*I) = table(LL+len*(I-1))
               END DO
11        CONTINUE
          I = 0
12        CONTINUE
          DO LL = 1,len
             table(LL+len*I) = t(LL)
             END DO
13      CONTINUE

C If this was the last partition we are done.

        IF (JSTACK.EQ.0) THEN
            tls_Quick_Sort_r = tls_sort_normal_cv
            RETURN
            END IF

C Remove partition we have just finished from the stack.

        IR = ISTACK(JSTACK)
        L  = ISTACK(JSTACK-1)
        JSTACK = JSTACK-2

      ELSE

C 
        I=L
        J=IR
        IQ=(L+IR)/2.0
        tel1 = el1(1+len*(IQ-1))
        DO LL = 1,len
           t(LL)=table(LL+len*(IQ-1))
           table(LL+len*(IQ-1))=table(LL+len*(L-1))
           END DO
20      CONTINUE
21        IF ((J.GT.0) .AND. (tel1.LT.el1(1+len*(J-1)))) THEN
              J=J-1
              GO TO 21
              ENDIF
          IF(J.LE.I)THEN
              DO LL = 1,len
                 table(LL+len*(I-1))=t(LL)
                 END DO
              GO TO 30
              ENDIF
          DO LL = 1,len
             table(LL+len*(I-1))=table(LL+len*(J-1))
             END DO
          I=I+1
22        IF ((I.LE.nrows) .AND. (tel1.GT.el1(1+len*(I-1)))) THEN
              I=I+1
              GO TO 22
              ENDIF
          IF(J.LE.I)THEN
              I=J
              DO LL = 1,len
                 table(LL+len*(I-1))=t(LL)
                 END DO
              GO TO 30
              ENDIF
          DO LL = 1,len
             table(LL+len*(J-1))=table(LL+len*(I-1))
             END DO
          J=J-1
        GO TO 20
30      JSTACK=JSTACK+2
        IF (JSTACK.GT.NSTACK) THEN
            tls_Quick_Sort_r = tls_sort_qs_stackshort_cv
            RETURN
            END IF
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1
        ELSE
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1
        ENDIF
      ENDIF
      GO TO 10
      END
