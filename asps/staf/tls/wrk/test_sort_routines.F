      PROGRAM Test_Sort_Routines
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C
C This program generates a few arrays of random numbers and then
C sorts them in various ways. We should be able to make sure there
C are no grevious errors in the tls sort routines.
C
      INTEGER tls_Zero_Row, tls_Index_Sort_r, tls_Index_Sort_i
      INTEGER tls_Quick_Sort_r, tls_Quick_Sort_i
      INTEGER tls_Search_r, tls_Search_i
      INTEGER tls_Search_Index_r, tls_Search_Index_i
      INTEGER tls_Search_Near_r, tls_Search_Near_i
      INTEGER tls_Search_Near_Index_r, tls_Search_Near_Index_i
C
      INTEGER size
      PARAMETER (size=10000)
      INTEGER iseed, i, j, k, ok, index(size)
      INTEGER pass1, pass2, pass3, krows, j_table_off(10), irandom
      INTEGER j_index_off(10)
      REAL    RAN, random
      REAL    save_first
      INTEGER save_last
C
      REAL    ETIME, times(2), tstart, tend, del_time
C
      INTEGER i1(size), i2(size)
      REAL    r1(size), r2(size)
      STRUCTURE / Table1 /
         INTEGER i1
         REAL    r1
         REAL    r2
         INTEGER i2
         REAL    dummy(6)
         END STRUCTURE
      RECORD/ Table1 / table(size)
C
      iseed = 971
      DO i = 1,size
         i1(i) = size * RAN(iseed)
         i2(i) = size * RAN(iseed)
         r1(i) = 100.0 * RAN(iseed)
         r2(i) = 100.0 * RAN(iseed)
         table(i).i1 = i1(i)
         table(i).i2 = i2(i)
         table(i).r1 = r1(i)
         table(i).r2 = r2(i)
         END DO
C***************************************************************
C First set of checks is for Quick_Sort and Index_Sort.
C***************************************************************
C Test routines for real arrays and tables.
      ok = tls_Quick_Sort_r( size, r1(1), r1(2), r1 )
      pass1 = 0
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' Unexpected return code from tls_Quick_Sort_r '
          pass1 = pass1 + 1
          END IF
      DO i = 2,size
         IF (r1(i-1).GT.r1(i)) THEN
             WRITE(6,*)' tls_Quick_Sort_r not sorting',
     +                 ' in ascending order '
             pass1 = pass1 + 1
             END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Quick_Sort_r passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Quick_Sort_r FAILED validation<<<<< '
        END IF

      ok = tls_Index_Sort_r( size, table(1).r1, table(2).r1,
     +                       index, size )
      pass1 = 0
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' Unexpected return code from tls_Index_Sort_r '
          pass1 = pass1 + 1
          END IF
      DO i = 1,size
         IF (table(index(i)).r1.NE.r1(i)) THEN
             WRITE(6,*)' Problem in tls_Quick_Sort_r',
     +                 ' or tls_Index_Sort_r '
             pass1 = pass1 + 1
             END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)'  tls_Index_Sort_r passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Quick_Sort_r FAILED validation<<<<< '
        END IF

C Test routines for integer arrays and tables.
      ok = tls_Quick_Sort_i( size, i1(1), i1(2), i1 )
      pass1 = 0
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' Unexpected return code from tls_Quick_Sort_i '
          pass1 = pass1 + 1
          END IF
      DO i = 2,size
         IF (i1(i-1).GT.i1(i)) THEN
             WRITE(6,*)' tls_Quick_Sort_i not sorting',
     +                 ' in ascending order '
             pass1 = pass1 + 1
             END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Quick_Sort_i passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Quick_Sort_r FAILED validation<<<<< '
        END IF

      ok = tls_Index_Sort_i( size, table(1).i1, table(2).i1,
     +                       index, size )
      pass1 = 0
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' Unexpected return code from tls_Index_Sort_i '
          pass1 = pass1 + 1
          END IF
      DO i = 1,size
         IF (table(index(i)).i1.NE.i1(i)) THEN
             WRITE(6,*)' Problem in tls_Quick_Sort_i',
     +                 ' or tls_Index_Sorti '
             pass1 = pass1 + 1
             END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Index_Sort_i passed validation  '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Index_Sort_r FAILED validation<<<<< '
        END IF
C
C***************************************************************
C Second set of checks is for Search.
C***************************************************************
C We simply generate some random numbers and search table.r1 for them.
C This part tests tls_Search_r in the case of a table input.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      ok = tls_Quick_Sort_r( size, table(1).r1, table(2).r1, table )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Quick_Sort_r failed. What happended? '
          END IF
      DO i = 1,size
         IF (i.EQ.1) THEN
             random = -1.0
           ELSE IF (i.EQ.size) THEN
             random = size + 1.0
           ELSE
             random = 100.0 * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_r( size, table(1).r1, table(2).r1,
     +                      random, j_table_off, krows )
         IF (ok.EQ.tls_sort_s_listshort_cv) THEN
             WRITE(6,*)' Too many entries to fit ',
     +                 'in j_table_off were found by tls_Search_r '
             DO j = 1,krows
                IF (table(j_table_off(j)).r1.NE.random) THEN
                    WRITE(6,*)' False match from tls_Search_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF (ok.EQ.tls_sort_s_nomatch_cv) THEN
             IF (j_table_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (random.GE.table(1).r1) THEN
                     WRITE(6,*)' j_table_off in tls_Search_r',
     +                         ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_table_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (random.LE.table(size).r1) THEN
                   WRITE(6,*)' j_table_off in tls_Search_r',
     +                       ' should not be',size
                     pass1 = pass1 + 1
                     END IF
               ELSE IF ((random.LT.table(j_table_off(1)).r1) .OR.
     +                  (random.GE.table(j_table_off(1)+1).r1)) THEN
                 WRITE(6,*)' j_table_off in tls_Search_r',
     +                     ' in wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(j_table_off(1)-1).r1.EQ.random) THEN
                 WRITE(6,*)' tls_Search_r did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).r1.NE.random) THEN
                    WRITE(6,*)' False match from tls_Search_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from tls_Search_r '
             pass1 = pass1 + 1
           END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************* '
          WRITE(6,*)' tls_Search_r passed validation '
          WRITE(6,*)' ********************************************* '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_r FAILED validation<<<<< '
        END IF
C Now test i1.
C This tests tls_Search_i in the case of a 1-d array input.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      DO i = 1,size
         IF (i.EQ.1) THEN
             irandom = -1
           ELSE IF (i.EQ.size) THEN
             irandom = size + 1
           ELSE
             irandom = size * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_i( size, i1(1), i1(2), irandom,
     +                      j_table_off, krows )
         IF (ok.EQ.tls_sort_s_listshort_cv) THEN
             WRITE(6,*)' Too many entries to fit',
     +                 ' j_table_off were found by tls_Search_i '
             DO j = 1,krows
                IF (i1(j_table_off(j)).NE.irandom) THEN
                    WRITE(6,*)' False match from tls_Search_i '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF (ok.EQ.tls_sort_s_nomatch_cv) THEN
             IF (j_table_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (irandom.GE.i1(1)) THEN
                     WRITE(6,*)' j_table_off in tls_Search_i',
     +                         ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_table_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (irandom.LE.i1(size)) THEN
                   WRITE(6,*)' j_table_off in tls_Search_i',
     +                       ' should not be',size
                     pass1 = pass1 + 1
                     END IF
               ELSE IF ((irandom.LT.i1(j_table_off(1))) .OR.
     +                  (irandom.GE.i1(j_table_off(1)+1))) THEN
                 WRITE(6,*)' j_table_off in tls_Search_i',
     +                     ' in wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF ((j_table_off(1).GT.1) .AND.
     +           (i1(j_table_off(1)-1).EQ.irandom)) THEN
                 WRITE(6,*)' tls_Search_i did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (i1(j_table_off(j)).NE.irandom) THEN
                    WRITE(6,*)' False match from tls_Search_i '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from tls_Search_i '
           END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_i passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_i FAILED validation<<<<< '
        END IF
C
C***************************************************************
C Third set of checks is for Search_Index.
C***************************************************************
C Now test Index_Search 
C test tls_Search_Index_i for table input.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      ok = tls_Index_Sort_i( size, table(1).i2, table(2).i2,
     +                       index, size )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Index_Sort_i failed. What happended? '
          END IF
      DO i = 1,size
         IF (i.EQ.1) THEN
             irandom = -1
           ELSE IF (i.EQ.size) THEN
             irandom = size + 1
           ELSE
             irandom = size * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_Index_i( size, table(1).i2, table(2).i2,
     +                            irandom, j_table_off, krows,
     +                            index, j_index_off )
         IF (ok.EQ.tls_sort_si_listshort_cv) THEN
             WRITE(6,*)' Too many entries to fit in j_table_off',
     +                 ' were found by tls_Search_Index_i '
             DO j = 1,krows
                IF (table(j_table_off(j)).i2.NE.irandom) THEN
                    WRITE(6,*)' False match from tls_Search_Index_i '
                    pass1 = pass1 + 1
                    END IF
                IF (j_table_off(j).NE.index(j_index_off(j))) THEN
                    WRITE(6,*)' j_table_off and j_index_off',
     +                        ' disagree in tls_Search_Index_i '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF (ok.EQ.tls_sort_si_nomatch_cv) THEN
             IF (j_index_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (irandom.GE.table(index(1)).i2) THEN
                     WRITE(6,*)' j_index_off in tls_Search_Index_i',
     +                         ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_index_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (irandom.LE.table(index(size)).i2) THEN
                   WRITE(6,*)' j_index_off in tls_Search_Index_i',
     +                       ' should not be',size
                     pass1 = pass1 + 1
                     END IF
          ELSE IF ((irandom.LT.table(index(j_index_off(1))).i2) .OR.
     +             (irandom.GE.table(index(j_index_off(1)+1)).i2)) THEN
                 WRITE(6,*)' j_index_off in tls_Search_Index_i',
     +                     ' in wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(index(j_index_off(1)-1)).i2.EQ.irandom) THEN
                 WRITE(6,*)' tls_Search_Index_i did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).i2.NE.irandom) THEN
                    WRITE(6,*)' False match from tls_Search_Index_i '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from',
     +                 ' tls_Search_Index_i '
             pass1 = pass1 + 1
           END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_Index_i passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_Index_i FAILED validation<<<<< '
        END IF
C test tls_Search_Index_r for table input.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      ok = tls_Index_Sort_r( size, table(1).r2, table(2).r2,
     +                       index, size )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Index_Sort_r failed. What happended? '
          END IF
      DO i = 1,size
         IF (i.EQ.1) THEN
             random = -1.0
           ELSE IF (i.EQ.size) THEN
             random = size + 1.0
           ELSE
             random = 100.0 * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_Index_r( size, table(1).r2, table(2).r2,
     +                            random, j_table_off, krows,
     +                            index, j_index_off )
         IF (ok.EQ.tls_sort_si_listshort_cv) THEN
             WRITE(6,*)' Too many entries to fit in j_table_off',
     +                 ' were found by tls_Search_Index_r '
             DO j = 1,krows
                IF (table(j_table_off(j)).r2.NE.random) THEN
                    WRITE(6,*)' False match from tls_Search_Index_r '
                    pass1 = pass1 + 1
                    END IF
                IF (j_table_off(j).NE.index(j_index_off(j))) THEN
                    WRITE(6,*)' j_table_off and j_index_off',
     +                        ' disagree in tls_Search_Index_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF (ok.EQ.tls_sort_si_nomatch_cv) THEN
             IF (j_index_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (random.GE.table(index(1)).r2) THEN
                     WRITE(6,*)' j_index_off in tls_Search_Index_r',
     +                         ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_index_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (random.LE.table(index(size)).r2) THEN
                   WRITE(6,*)' j_index_off in tls_Search_Index_r',
     +                       ' should not be',size
                     pass1 = pass1 + 1
                     END IF
          ELSE IF ((random.LT.table(index(j_index_off(1))).r2) .OR.
     +             (random.GE.table(index(j_index_off(1)+1)).r2)) THEN
                 WRITE(6,*)' j_index_off in tls_Search_Index_r',
     +                     ' in wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(index(j_index_off(1)-1)).r2.EQ.random) THEN
                 WRITE(6,*)' tls_Search_Index_r did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).r2.NE.random) THEN
                    WRITE(6,*)' False match from tls_Search_Index_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from',
     +                 ' tls_Search_Index_r '
             pass1 = pass1 + 1
           END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_Index_r passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_Index_r FAILED validation<<<<< '
        END IF
C
C***************************************************************
C Fourth set of checks is for Search_Near.
C***************************************************************
C We simply generate some random numbers and search table.r1 for them.
C This part tests tls_Search_Near_r in the case of a table input
C  and the case where we are wrong about the starting position.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      ok = tls_Quick_Sort_r( size, table(1).r1, table(2).r1, table )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Quick_Sort_r failed. What happended? '
          END IF
      j_table_off(1) = 1
      DO i = 1,size
         IF (i.EQ.1) THEN
             random = -1.0
           ELSE IF (i.EQ.size) THEN
             random = size + 1.0
           ELSE
             random = 100.0 * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_Near_r( size, table(1).r1, table(2).r1,
     +                      random, j_table_off, krows )
         IF ((ok.EQ.tls_sort_sn_listshort_cv) .OR.
     +       (ok.EQ.tls_sort_s_listshort_cv)) THEN
             WRITE(6,*)' Too many entries to fit in j_table_off were',
     +                 ' found by tls_Search_Near_r '
             DO j = 1,krows
                IF (table(j_table_off(j)).r1.NE.random) THEN
                    WRITE(6,*)' False match from tls_Search_Near_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF ((ok.EQ.tls_sort_sn_nomatch_cv) .OR.
     +              (ok.EQ.tls_sort_s_nomatch_cv)) THEN
             IF (j_table_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (random.GE.table(1).r1) THEN
                     WRITE(6,*)' j_table_off in tls_Search_Near_r',
     +                         ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_table_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (random.LE.table(size).r1) THEN
                   WRITE(6,*)' j_table_off in tls_Search_Near_r',
     +                       ' should not be ',size
                     pass1 = pass1 + 1
                     END IF
               ELSE IF ((random.LT.table(j_table_off(1)).r1) .OR.
     +                  (random.GE.table(j_table_off(1)+1).r1)) THEN
                 WRITE(6,*)' j_table_off in tls_Search_Near_r in',
     +                     ' wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(j_table_off(1)-1).r1.EQ.random) THEN
                 WRITE(6,*)' tls_Search_Near_r did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).r1.NE.random) THEN
                    WRITE(6,*)' False match from tls_Search_Near_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from',
     +                 ' tls_Search_Near_r '
             pass1 = pass1 + 1
           END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_Near_r passed validation '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_Near_r FAILED validation<<<<< '
        END IF
C
C Compare speed of simple tls_Search_i with tls_Search_Near_i.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      WRITE(6,*)' Table of 10 elements per row, number of rows =',size
      tstart = ETIME( times )
      ok = tls_Quick_Sort_i( size, table(1).i1, table(2).i1, table )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Quick_Sort_r failed. What happended? '
          END IF
      tend = ETIME( times )
      del_time = tend - tstart
      WRITE(6,*)' tls_Quick_Sort_r took ',del_time,' to order table. '
      tstart = ETIME( times )
      j_table_off(1) = 1
      DO i = 1,size
         irandom = i
         krows = 10
         ok = tls_Search_i( size, table(1).i1, table(2).i1,
     +                      irandom, j_table_off, krows )
         IF (ok.EQ.tls_sort_s_listshort_cv) THEN
             DO j = 1,krows
                IF (table(j_table_off(j)).i1.NE.irandom) THEN
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF (ok.EQ.tls_sort_s_nomatch_cv) THEN
             IF (j_table_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (irandom.GE.table(1).i1) THEN
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_table_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (irandom.LE.table(size).i1) THEN
                     pass1 = pass1 + 1
                     END IF
               ELSE IF ((irandom.LT.table(j_table_off(1)).i1) .OR.
     +                  (irandom.GE.table(j_table_off(1)+1).i1)) THEN
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(j_table_off(1)-1).i1.EQ.irandom) THEN
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).i1.NE.irandom) THEN
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from tls_Search_i '
             pass1 = pass1 + 1
           END IF
         END DO
      tend     = ETIME( times )
      del_time = tend - tstart
      WRITE(6,*)size,' calls to tls_Search_i of ordered table took',
     +          del_time
C
      pass1 = 0
      pass2 = 0
      pass3 = 0
      ok = tls_Quick_Sort_i( size, table(1).i2, table(2).i2, table )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Quick_Sort_i failed. What happended? '
          END IF
      tstart = ETIME( times )
      j_table_off(1) = 1
      DO i = 1,size
         IF (i.EQ.1) THEN
             irandom = -1
           ELSE IF (i.EQ.size) THEN
             irandom = size + 1
           ELSE
             irandom = i
           END IF
         krows = 10
         ok = tls_Search_Near_i( size, table(1).i2, table(2).i2,
     +                           irandom, j_table_off, krows )
         IF ((ok.EQ.tls_sort_sn_listshort_cv) .OR.
     +       (ok.EQ.tls_sort_s_listshort_cv)) THEN
             DO j = 1,krows
                IF (table(j_table_off(j)).i2.NE.irandom) THEN
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF ((ok.EQ.tls_sort_sn_nomatch_cv) .OR.
     +              (ok.EQ.tls_sort_s_nomatch_cv)) THEN
             IF (j_table_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (irandom.GE.table(1).i2) THEN
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_table_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (irandom.LE.table(size).i2) THEN
                     pass1 = pass1 + 1
                     END IF
               ELSE IF ((irandom.LT.table(j_table_off(1)).i2) .OR.
     +                  (irandom.GE.table(j_table_off(1)+1).i2)) THEN
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(j_table_off(1)-1).i2.EQ.irandom) THEN
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).i2.NE.irandom) THEN
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from',
     +                 ' tls_Search_Near_i '
             pass1 = pass1 + 1
           END IF
         END DO
      tend     = ETIME( times )
      del_time = tend - tstart
      WRITE(6,*)size,' calls to tls_Search_Near_i of',
     +          ' ordered table took',del_time
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_Near_i passed validation  '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_Near_i FAILED validation<<<<< '
        END IF
C
C***************************************************************
C The fifth set of checks is for Search_Near_Index.
C***************************************************************
C
C We simply generate some random numbers and search table.r1 for them.
C This part tests tls_Search_Near_Index_r in the case of a table input
C  and the case where we are wrong about the starting position.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      ok = tls_Index_Sort_r( size, table(1).r1, table(2).r1,
     +                       index, size )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Index_Sort_r failed. What happended? '
          END IF
      j_table_off(1) = 1
      DO i = 1,size
         IF (i.EQ.1) THEN
             random = -1.0
           ELSE IF (i.EQ.size) THEN
             random = size + 1.0
           ELSE
             random = 100.0 * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_Near_Index_r( size, table(1).r1, table(2).r1,
     +                      random, j_table_off, krows,
     +                      index, j_index_off )
         IF ((ok.EQ.tls_sort_sni_listshort_cv) .OR.
     +       (ok.EQ.tls_sort_si_listshort_cv)) THEN
             WRITE(6,*)' Too many entries to fit in j_table_off',
     +                 ' were found by tls_Search_Near_Index_r '
             DO j = 1,krows
                IF (table(j_table_off(j)).r1.NE.random) THEN
                    WRITE(6,*)' False match from',
     +                        ' tls_Search_Near_Index_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF ((ok.EQ.tls_sort_sni_nomatch_cv) .OR.
     +              (ok.EQ.tls_sort_si_nomatch_cv)) THEN
             IF (j_index_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (random.GE.table(index(1)).r1) THEN
                   WRITE(6,*)' j_index_off in tls_Search_Near_Index_r',
     +                       ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_index_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (random.LE.table(index(size)).r1) THEN
                   WRITE(6,*)' j_index_off in tls_Search_Near_Index_r',
     +                       ' should not be ',size
                     pass1 = pass1 + 1
                     END IF
          ELSE IF ((random.LT.table(index(j_index_off(1))).r1) .OR.
     +             (random.GE.table(index(j_index_off(1)+1)).r1)) THEN
                 WRITE(6,*)' j_index_off in tls_Search_Near_Index_r',
     +                     ' in wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(j_table_off(1)-1).r1.EQ.random) THEN
                 WRITE(6,*)' tls_Search_Near_Index_r did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).r1.NE.random) THEN
                    WRITE(6,*)' False match from',
     +                        ' tls_Search_Near_Index_r '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from',
     +                 ' tls_Search_Near_Index_r '
             pass1 = pass1 + 1
           END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_Near_Index_r passed validation  '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_Near_Index_r FAILED validation',
     +              '<<<<< '
        END IF
C
C We do a little timing test comparing sorting methods here.
      pass1 = 0
      pass2 = 0
      pass3 = 0
      tstart = ETIME( times )
      ok = tls_Index_Sort_i( size, table(1).i1, table(2).i1,
     +                       index, size )
      IF (ok.NE.tls_sort_normal_cv) THEN
          WRITE(6,*)' tls_Index_Sort_i failed. What happended? '
          END IF
      tend = ETIME( times )
      del_time = tend - tstart
      WRITE(6,*)' tls_Index_Sort_i took ',del_time,' to order table '
      tstart = ETIME( times )
      j_table_off(1) = 1
      DO i = 1,size
         IF (i.EQ.1) THEN
             irandom = -1
           ELSE IF (i.EQ.size) THEN
             irandom = size + 1
           ELSE
             irandom = size * RAN(iseed)
           END IF
         krows = 10
         ok = tls_Search_Near_Index_i( size, table(1).i1, table(2).i1,
     +                      irandom, j_table_off, krows,
     +                      index, j_index_off )
         IF ((ok.EQ.tls_sort_sni_listshort_cv) .OR.
     +       (ok.EQ.tls_sort_si_listshort_cv)) THEN
             WRITE(6,*)' Too many entries to fit in j_table_off',
     +                 ' were found by tls_Search_Near_Index_i '
             DO j = 1,krows
                IF (table(j_table_off(j)).i1.NE.irandom) THEN
                    WRITE(6,*)' False match from',
     +                        ' tls_Search_Near_Index_i '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE IF ((ok.EQ.tls_sort_sni_nomatch_cv) .OR.
     +              (ok.EQ.tls_sort_si_nomatch_cv)) THEN
             IF (j_index_off(1).EQ.0) THEN
                 pass2 = pass2 + 1
                 IF (irandom.GE.table(index(1)).i1) THEN
                   WRITE(6,*)' j_index_off in tls_Search_Near_Index_i',
     +                       ' should not be 0 '
                     pass1 = pass1 + 1
                     END IF
               ELSE IF (j_index_off(1).EQ.size) THEN
                 pass3 = pass3 + 1
                 IF (irandom.LE.table(index(size)).i1) THEN
                   WRITE(6,*)' j_index_off in tls_Search_Near_Index_i',
     +                       ' should not be ',size
                     pass1 = pass1 + 1
                     END IF
          ELSE IF ((irandom.LT.table(index(j_index_off(1))).i1) .OR.
     +             (irandom.GE.table(index(j_index_off(1)+1)).i1)) THEN
                 WRITE(6,*)' j_index_off in tls_Search_Near_Index_i',
     +                     ' in wrong position. '
                 pass1 = pass1 + 1
               END IF
           ELSE IF (ok.EQ.tls_sort_normal_cv) THEN
             IF (table(j_table_off(1)-1).i1.EQ.irandom) THEN
                 WRITE(6,*)' tls_Search_Near_Index_i did not return',
     +                     ' first occurence '
                 pass1 = pass1 + 1
                 END IF
             DO j = 1,krows
                IF (table(j_table_off(j)).i1.NE.irandom) THEN
                    WRITE(6,*)' False match from',
     +                        ' tls_Search_Near_Index_i '
                    pass1 = pass1 + 1
                    END IF
                END DO
           ELSE
             WRITE(6,*)' Unexpected return code from',
     +                 ' tls_Search_Near_Index_i '
             pass1 = pass1 + 1
           END IF
         END DO
      tend = ETIME( times )
      del_time = tend - tstart
      WRITE(6,*)size,' calls to tls_Search_Near_Index_i of ordered',
     +          ' table took',del_time

      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Search_Near_Index_i passed validation  '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Search_Near_Index_i FAILED validation',
     +              '<<<<< '
        END IF

C
C***************************************************************
C The sixth set of checks is for Zero_Row
C***************************************************************
C
C Assume table has some random values.
C Generate random index into table and zero that row.
C We chack that all elements in row are in fact zeroed and the last
C element of previous row and first element of next row are unchanged.
C Do this 1000 times, just for kicks.
      pass1 = 0
      DO i = 1,size
         j = size * RAN(iseed) + 1
         IF (j.NE.1) THEN
            save_first = table(j-1).dummy(6)
            END IF
         IF (j.NE.size) THEN
            save_last  = table(j+1).i1
            END IF
         ok = tls_Zero_Row( table(j), table(j+1) )
         IF (ok.EQ.tls_sort_id_short_cv) THEN
             WRITE(6,*)' tls_Zero_Row thinks table(j+1) is before',
     +                 ' table(j); j = ',j
             pass1 = pass1 + 1
             END IF
C
C Now tediously check element by element that row is 0.
          IF (table(j).i1.NE.0) THEN
             pass1 = pass1 + 1
             END IF
          IF (table(j).r1.NE.0.0) THEN
             pass1 = pass1 + 1
             END IF
          IF (table(j).r2.NE.0.0) THEN
             pass1 = pass1 + 1
             END IF
          IF (table(j).i2.NE.0) THEN
             pass1 = pass1 + 1
             END IF
          DO k = 1,6
             IF (table(j).dummy(k).NE.0.0) THEN
                pass1 = pass1 + 1
                END IF
             END DO
C
C Also check pre and post-cursors.
         IF (j.NE.1) THEN
            IF (save_first .NE. table(j-1).dummy(6)) THEN
               pass1 = pass1 + 1
               END IF
            END IF
         IF (j.NE.size) THEN
            IF (save_last  .NE. table(j+1).i1) THEN
               pass1 = pass1 + 1
               END IF
            END IF
         END DO
      IF (pass1.EQ.0) THEN
          WRITE(6,*)' ********************************************** '
          WRITE(6,*)' tls_Zero_Row passed validation  '
          WRITE(6,*)' ********************************************** '
        ELSE
          WRITE(6,*)' >>>>>tls_Zero_Row FAILED validation',
     +              '<<<<< '
        END IF
      END
