/*void ami(){}*/
#include "table_header.h"
#include "fortranc.h"
void *ReAllocate(TABLE_HEAD_ST *h, int newsize);
void *ami;

void type_of_call F77_NAME(hbook1,HBOOK1)(){printf("***DUMMY HBOOK1****");}
void type_of_call F77_NAME(hbook2,HBOOK2)(){printf("***DUMMY HBOOK2****");}
void type_of_call F77_NAME(hbookn,HBOOKN)(){printf("***DUMMY HBOOKN****");}
void type_of_call F77_NAME(hfill,HFILL)(){printf("***DUMMY HFILL****");}
void type_of_call F77_NAME(hfn,HFN)(){printf("***DUMMY HFN****");}
void type_of_call F77_NAME(hrend,HREND)(){printf("***DUMMY HREND****");}
void type_of_call F77_NAME(hropen,HROPEN)(){printf("***DUMMY HROPEN****");}
void type_of_call F77_NAME(hrout,HROUT)(){printf("***DUMMY HROUT****");}

void sig_die(){printf("***DUMMY SIG_DIE****");}

void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b){
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
