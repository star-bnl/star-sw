* $Id: tls_quick_sort_ii.F,v 1.1 1999/12/21 01:22:03 perev Exp $
* $Log: tls_quick_sort_ii.F,v $
* Revision 1.1  1999/12/21 01:22:03  perev
* Add _ii functions, remove QSortTable.c & tls_sort.c
*
* Revision 1.4  1998/08/13 16:16:37  didenko
* updated END statment
*
* Revision 1.3  1998/08/13 02:09:01  perev
* cleanup+speedup+leaks
*
* Revision 1.2  1998/04/09 15:18:33  fisyak
* tls bug fixes from Duncan J. Prindle
*
      INTEGER FUNCTION tls_Quick_Sort_ii( nrows, el1, el2, table )
      INTEGER Quick_Sort_ii
      tls_Quick_Sort_ii = Quick_Sort_ii( nrows, el1, el2, table )
      return
      END
