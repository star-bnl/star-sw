/* $Id: St_staf_dummies.c,v 1.16 2000/01/18 23:06:56 fine Exp $ */
/*void ami(){}*/
#include "table_header.h"
#include "fortranc.h"
void *ReAllocate(TABLE_HEAD_ST *h, int newsize);
#if 0
void *ami;
void sig_die(){printf("***DUMMY SIG_DIE****");}
void type_of_call F77_NAME(hbook1,HBOOK1)(int i1,int i2,int i3,int i4,int i5,int i6,int i7){printf("***DUMMY HBOOK1****");}
void type_of_call F77_NAME(hbook2,HBOOK2)(){printf("***DUMMY HBOOK2****");}
void type_of_call F77_NAME(hbookn,HBOOKN)(){printf("***DUMMY HBOOKN****");}
void type_of_call F77_NAME(hdelet,HDELET)(int i1){printf("***DUMMY HDELET****");}
void type_of_call F77_NAME(hexist,HEXIST)(int i1){printf("***DUMMY HEXIST****");}
void type_of_call F77_NAME(hf1,HF1)(int i1,int i2,int i3){printf("***DUMMY HF1****");}
void type_of_call F77_NAME(hfill,HFILL)(){printf("***DUMMY HFILL****");}
void type_of_call F77_NAME(hfn,HFN)(){printf("***DUMMY HFN****");}
void type_of_call F77_NAME(hreset,HRESET)(int i1,int i2,int i3){printf("***DUMMY HRESET****");}
void type_of_call F77_NAME(hrend,HREND)(){printf("***DUMMY HREND****");}
void type_of_call F77_NAME(hropen,HROPEN)(){printf("***DUMMY HROPEN****");}
void type_of_call F77_NAME(hrout,HROUT)(){printf("***DUMMY HROUT****");}
void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b){
printf("***DUMMY GUFLD 0 0 5****");
  b[0] = 0.;
  b[1] = 0.;
  b[2] = 5.;
}
#endif
int ds2ReallocTable (TABLE_HEAD_ST **h, char** ppData, int newsize){
  *ppData = (char *) ReAllocate(*h,newsize); return 1;
}
int ds2realloctable_(TABLE_HEAD_ST **h, char** ppData, int newsize){
  *ppData = (char *) ReAllocate(*h,newsize); return -1;
}
#if 0
#if WIN32
void	Message( const char *msg, int *ID ){}
void	MessageOut( const char *msg ){}
#define rannor_ F77_NAME(rannor,RANNOR)
extern void type_of_call rannor_(float *a, float *b);
float type_of_call RG32(int *c){
  float a,b;
  rannor_(&a, &b);
  return a;
}
#endif
/* 
  $Log: St_staf_dummies.c,v $
  Revision 1.16  2000/01/18 23:06:56  fine
  fix comment

 
  Revision 1.12  2000/01/12 18:07:24  fine  
  cvs symbols have been added and copyright class introduced  

*/
#endif
