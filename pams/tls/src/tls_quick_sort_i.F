* $Id: tls_quick_sort_i.F,v 1.3 1998/08/13 02:09:01 perev Exp $
* $Log: tls_quick_sort_i.F,v $
* Revision 1.3  1998/08/13 02:09:01  perev
* cleanup+speedup+leaks
*
* Revision 1.2  1998/04/09 15:18:33  fisyak
* tls bug fixes from Duncan J. Prindle
*
      INTEGER FUNCTION tls_Quick_Sort_i( nrows, el1, el2, table )
      INTEGER Quick_Sort
      tls_Quick_Sort_i = Quick_Sort( nrows, el1, el2, table )
      return
