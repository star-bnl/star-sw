/*void ami(){}*/
#include "table_header.h"
void *ReAllocate(TABLE_HEAD_ST *h, int newsize);
void *ami;
void hbook1_(){printf("***DUMMY HBOOK1****");}
void hbook2_(){printf("***DUMMY HBOOK2****");}
void hbookn_(){printf("***DUMMY HBOOKN****");}
void hfill_(){printf("***DUMMY HFILL****");}
void hfn_(){printf("***DUMMY HFN****");}
void hrend_(){printf("***DUMMY HREND****");}
void hropen_(){printf("***DUMMY HROPEN****");}
void hrout_(){printf("***DUMMY HROUT****");}
void sig_die(){printf("***DUMMY SIG_DIE****");}
void gufld_ (float *x, float *b){
printf("***DUMMY GUFLD 0 0 5****");
  b[0] = 0.;
  b[1] = 0.;
  b[2] = 5.;
}
int ds2ReallocTable (TABLE_HEAD_ST **h, char** ppData, int newsize){
  *ppData = (char *) ReAllocate(*h,newsize); return 1;
}
int ds2realloctable_(TABLE_HEAD_ST **h, char** ppData, int newsize){
  *ppData = (char *) ReAllocate(*h,newsize); return -1;
}
