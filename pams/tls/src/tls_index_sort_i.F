* $Id: tls_index_sort_i.F,v 1.4 1998/08/17 14:48:55 fisyak Exp $
* $Log: tls_index_sort_i.F,v $
* Revision 1.4  1998/08/17 14:48:55  fisyak
* add type for Index_Sort
*
* Revision 1.3  1998/08/17 14:39:22  fisyak
* clean up
*
* Revision 1.2  1998/08/13 02:09:00  perev
* cleanup+speedup+leaks
*
* Revision 1.1  1998/03/24 00:42:08  fisyak
* Clean
*
      INTEGER FUNCTION tls_Index_Sort_i( nrows, el1, el2,
     +                                   index, ind_size )
      
      implicit none
      integer nrows(1),el1(1),el2(1),index(*),ind_size(1)
      integer tls_Index_Sort_i, Index_Sort
      tls_Index_Sort_i  = Index_Sort( nrows, el1, el2, index, ind_size ) 
      end
         
