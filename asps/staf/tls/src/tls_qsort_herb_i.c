#include <stdio.h>
#include "PAM.h"

#define tls_qsort_herb_i_ F77_NAME(tls_qsort_herb_i,TLS_QSORT_HERB_I)

#define TLS_SORT_NORMAL_CV      3585
void tls_swap_two_rows(int row1,int row2,int rowsize,int *table) {
  register int ii,offset1,offset2,swap;
  if(row1==row2) return;
  offset1=row1*rowsize; offset2=row2*rowsize;
  for(ii=rowsize-1;ii>=0;ii--) {
                 swap=table[ii+offset1];
    table[ii+offset1]=table[ii+offset2];
    table[ii+offset2]=swap;
  }
}
void tls_qsort(int *sortCol,int rsize,int *table,int left,int rite) {
  register int ileft,irite,rowsize,comparator;

  ileft=left; irite=rite; rowsize=rsize;
  comparator=sortCol[((ileft+irite)/2)*rowsize]; 
  
  /*
  printf("top if tls_qsort left/rite = %6d %6d, call number %d, com=%d\n",
    left,rite,++BBB,comparator);
  */
  do {
    while(sortCol[ileft*rowsize]<comparator&&ileft<rite) ileft++;
    while(sortCol[irite*rowsize]>comparator&&irite>left) irite--;
    if(ileft<=irite) tls_swap_two_rows(ileft++,irite--,rowsize,table);
  } while(ileft<=irite);
  if(left<irite) {
    tls_qsort(sortCol,rsize,table,left,irite);
  }
  if(ileft<rite) {
    tls_qsort(sortCol,rsize,table,ileft,rite);
  }
}
int type_of_call tls_qsort_herb_i_(int *nrows,int *sortCol,
      int *sortColRow2,int *table) {
  int rowsize;
  rowsize=(sortColRow2-sortCol);

/*
  printf("Begin: Herb's C version of tls quick sort.\n");
  printf("Row size = %d x %d = %d bytes.\n",rowsize,sizeof(int),
      rowsize*sizeof(int));
  printf("Number of rows = %d.\n",*nrows);
*/
  if(*nrows<2) return TLS_SORT_NORMAL_CV;
  tls_qsort(sortCol,rowsize,table,0,(*nrows)-1);
  
/*  printf("End:   Herb's C version of tls quick sort.\n");*/
  return TLS_SORT_NORMAL_CV;
}
