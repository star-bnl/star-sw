/* 	Sort of tables			*/

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
void QSortTable(void *key,void *base, size_t count, size_t size, int flag);

int main()
{
typedef  struct {int i;float f;double d; short s; short ss;} myTab;
  myTab tab[500];
  int i,idx[500];
  for (i=0;i<500;i++) {
    tab[i].i = rand();
    tab[i].f = rand();
    tab[i].d = rand();
    tab[i].s = rand();
    tab[i].ss = rand();
  }

QSortTable(&tab[0].s,tab, 500, sizeof(myTab), 0x00);
for(i=0;i<499;i++) { printf("%hd \n",tab[i].s); assert(tab[i].s<=tab[i+1].s);}

QSortTable(&tab[0].i,tab, 500, sizeof(myTab), 0x01);
for(i=0;i<499;i++) { printf("%d  \n",tab[i].i); assert(tab[i].i<=tab[i+1].i);}

QSortTable(&tab[0].f,tab, 500, sizeof(myTab), 0x02);
for(i=0;i<499;i++) { printf("%g  \n",tab[i].f); assert(tab[i].f<=tab[i+1].f);}

QSortTable(&tab[0].d,tab, 500, sizeof(myTab), 0x03);
for(i=0;i<499;i++) { printf("%g  \n",tab[i].d); assert(tab[i].d<=tab[i+1].d);}


QSortTable(&tab[0].s,idx, 500, sizeof(myTab), 0x10);
for(i=0;i<499;i++) { printf("%hd \n",tab[idx[i]].s); assert(tab[idx[i]].s<=tab[idx[i+1]].s);}

QSortTable(&tab[0].i,idx, 500, sizeof(myTab), 0x11);
for(i=0;i<499;i++) { printf("%d \n",tab[idx[i]].i); assert(tab[idx[i]].i<=tab[idx[i+1]].i);}

QSortTable(&tab[0].f,idx, 500, sizeof(myTab), 0x12);
for(i=0;i<499;i++) { printf("%g \n",tab[idx[i]].f); assert(tab[idx[i]].f<=tab[idx[i+1]].f);}

QSortTable(&tab[0].d,idx, 500, sizeof(myTab), 0x13);
for(i=0;i<499;i++) { printf("%g \n",tab[idx[i]].d); assert(tab[idx[i]].d<=tab[idx[i+1]].d);}


QSortTable(&tab[0].s,idx, 500, sizeof(myTab), 0x30);
for(i=0;i<499;i++) { printf("%d %hd \n",idx[i],tab[idx[i]-1].s); assert(tab[idx[i]-1].s<=tab[idx[i+1]-1].s);}

QSortTable(&tab[0].i,idx, 500, sizeof(myTab), 0x31);
for(i=0;i<499;i++) { printf("%d \n",tab[idx[i]-1].i); assert(tab[idx[i]-1].i<=tab[idx[i+1]-1].i);}

QSortTable(&tab[0].f,idx, 500, sizeof(myTab), 0x32);
for(i=0;i<499;i++) { printf("%g \n",tab[idx[i]-1].f); assert(tab[idx[i]-1].f<=tab[idx[i+1]-1].f);}

QSortTable(&tab[0].d,idx, 500, sizeof(myTab), 0x33);
for(i=0;i<499;i++) { printf("%g \n",tab[idx[i]-1].d); assert(tab[idx[i]-1].d<=tab[idx[i+1]-1].d);}




return 0;
}





size_t keyOffset = 0;
size_t rowSize   = 0;
char * keyStart = 0;

static int compare_short	(const void* p1, const void* p2);
static int compare_int  	(const void* p1, const void* p2);
static int compare_float	(const void* p1, const void* p2);
static int compare_double	(const void* p1, const void* p2);
static int compare_idx_short	(const void* p1, const void* p2);
static int compare_idx_int	(const void* p1, const void* p2);
static int compare_idx_float	(const void* p1, const void* p2);
static int compare_idx_double	(const void* p1, const void* p2);

void QSortTable(void *key,void *base, size_t count, size_t size, int flag)
{
/*	flag =  isIdx 	+ itype
		isIdx 	0x00 =sort table itself
			0x10 =sort by index 0=first
			0x30 =sort by index 1=first
		itype	0 = short
			1 = long (32bit)
			2 = float
			3 = double
*/
  int i,*idx,iStart;
  keyOffset = (char*)key - (char*)base;
  assert(keyOffset>=0);
  rowSize   = size;
  assert(rowSize>0);
  keyStart = (char*)key;
  idx = (int*)base;
  if (flag & 0x10) for (i=0;i<count;i++) {idx[i] = i*size;}


  switch (flag&0x1f) {
  
    case 0x00: /*no idx, short*/
      qsort(base,count,rowSize,compare_short);		return;

    case 0x01: /*no idx, int*/
      qsort(base,count,rowSize,compare_int  ); 		return;     

    case 0x02: /*no idx, float*/
      qsort(base,count,rowSize,compare_float); 		return;

    case 0x03: /*no idx, double*/
      qsort(base,count,rowSize,compare_double); 	return;

    case 0x10: /*idx, short*/
      qsort(base,count,sizeof(int),compare_idx_short);	break;

    case 0x11: /*idx, int*/
      qsort(base,count,sizeof(int),compare_idx_int);	break;

    case 0x12: /*idx, float*/
      qsort(base,count,sizeof(int),compare_idx_float);	break;

    case 0x13: /*idx, double*/
      qsort(base,count,sizeof(int),compare_idx_double);	break;
      
    default: assert(0);
  }
  iStart = (flag&0x20) ? 1:0;
  for (i=0;i<count;i++){ idx[i] = idx[i]/size+iStart;}
  return;

}
static int compare_short(const void* p1, const void* p2)
{
  short k1 = *((short*)((char*)p1+keyOffset));
  short k2 = *((short*)((char*)p2+keyOffset));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_int(const void* p1, const void* p2)
{
  int k1 = *((int*)((char*)p1+keyOffset));
  int k2 = *((int*)((char*)p2+keyOffset));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_float(const void* p1, const void* p2)
{
  float k1 = *((float*)((char*)p1+keyOffset));
  float k2 = *((float*)((char*)p2+keyOffset));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_double(const void* p1, const void* p2)
{
  double k1 = *((double*)((char*)p1+keyOffset));
  double k2 = *((double*)((char*)p2+keyOffset));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_idx_short(const void* p1, const void* p2)
{
  short k1 = *((short*)(*((int*)p1)+keyStart));
  short k2 = *((short*)(*((int*)p2)+keyStart));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_idx_int(const void* p1, const void* p2)
{
  int k1 = *((int*)(*((int*)p1)+keyStart));
  int k2 = *((int*)(*((int*)p2)+keyStart));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_idx_float(const void* p1, const void* p2)
{
  float k1 = *((float*)(*((int*)p1)+keyStart));
  float k2 = *((float*)(*((int*)p2)+keyStart));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
static int compare_idx_double(const void* p1, const void* p2)
{
  double k1 = *((double*)(*((int*)p1)+keyStart));
  double k2 = *((double*)(*((int*)p2)+keyStart));
  if (k1<k2) 		return -1;
  else  if (k1>k2) 	return  1;
  return 0;
}
