* $Id: tls_quick_sort_r.F,v 1.3 1998/08/13 02:09:01 perev Exp $
* $Log: tls_quick_sort_r.F,v $
* Revision 1.3  1998/08/13 02:09:01  perev
* cleanup+speedup+leaks
*
* Revision 1.2  1998/04/09 15:18:33  fisyak
* tls bug fixes from Duncan J. Prindle
*
      INTEGER FUNCTION tls_Quick_Sort_r( nrows, el1, el2, table )
      integer Quick_Sort_r
      tls_Quick_Sort_r = Quick_Sort_r( nrows, el1, el2, table )
      return
