* $Id: tls_index_sort_r.F,v 1.2 1998/08/13 02:09:00 perev Exp $
* $Log: tls_index_sort_r.F,v $
* Revision 1.2  1998/08/13 02:09:00  perev
* cleanup+speedup+leaks
*
* Revision 1.1  1998/03/24 00:42:08  fisyak
* Clean
*
      INTEGER FUNCTION tls_Index_Sort_r( nrows, el1, el2,
     +                                   index, ind_size )
      tls_Index_Sort_r  = Index_Sort_r( nrows, el1, el2, index, ind_size ) 
      end
