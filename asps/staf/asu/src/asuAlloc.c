#include <stdio.h>
#include <string.h>
#include "asuAlloc.h"
static asuAllocMgr_t gAsuMgr;
static asuAlloc_t gAsuMain;
void asuWau();

void asuPrintBlok(asuAlloc_t *p);


/*--------------------------------------------------------------------*/
void asuMallocInit()
{
    
    memset(&gAsuMgr,0,sizeof(asuAllocMgr_t));
    memset(&gAsuMain,0,sizeof(asuAlloc_t));
}

/*--------------------------------------------------------------------*/
void asuMallocInitTrace()
{
}

void asuMallocLevel(int level)
{
if (level == -1){ printf("ASU/MALLOC/LEVEL=%d\n",gAsuMgr.fLogLevel);
} else 		{ gAsuMgr.fLogLevel=level;}
}

/*--------------------------------------------------------------------*/
void asuMallocStats(level)
{
asuAlloc_t *pa; char* pc;
float Mega;
int numb,accu,corr,size;
  if (level >=0) gAsuMgr.fLogLevel = level;
  printf("\nASU_MALLOC: Memory allocation for Event:%d \n",gAsuMgr.fEvent);

  Mega = gAsuMgr.fSize; Mega = Mega/1000000;
  printf(" Size %10.3fM  Blocks%8d  Allocs%8d  Free   %8d  Errors %d Warnings %d\n"
         ,Mega
         ,gAsuMgr.fNumBlok 
         ,gAsuMgr.fNumAlloc 
         ,gAsuMgr.fNumFree 
         ,gAsuMgr.fNumErr
         ,gAsuMgr.fNumWar);
  
  numb = 0; accu = 0;
  for (pa = gAsuMain.next; pa; pa = pa->next) {/*loop over all*/ 
    pc = (char*)pa; size = pa->size;
    corr = (pa->magic!=kAsuMagic || pc[size + sizeof(asuAlloc_t)]!=kAsuLast);
    corr = corr & (pa->begin != kAsuBegin);
    if ( !corr && !gAsuMgr.fLogLevel) continue;
    numb++; accu += size;

    printf("%4d - %10d  ",numb,accu); asuPrintBlok(pa);
  } 

  gAsuMgr.fEvent++;

}

/*--------------------------------------------------------------------*/
void asuMallocPrintTrace(void *p, size_t size, const char* file, int line)
{
}

/*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*/
void asuMallocReset()
{
}

/*--------------------------------------------------------------------*/
void asuMallocAdd(void *pp, size_t size, const char* file, int line)
{
asuAlloc_t *p,*pold;
int i;
   p = (asuAlloc_t*)pp;

   if (!p) {/* No Memory */
     gAsuMgr.fNumErr++;
     printf("asuMalloc. ***Error***: Can not allocate %d bytes\n",size);
     printf("asuMalloc. ***Error***: in File: %s Line %d \n",file,line);
     return;}

   pold = gAsuMain.next; gAsuMain.next = p;
   p->next = pold; p->back = &gAsuMain;  
   if (pold) pold->back = p;
/* 	Fill new block 	*/
   i=strlen(file)-15; if (i<0) i=0; strcpy(p->file,file+i);
   p->size = size; p->line=line; p->event = gAsuMgr.fEvent;
   p->status=0; p->magic = kAsuMagic; p->summ = 0;
   p->begin = kAsuBegin;
   *((char*)pp + sizeof(asuAlloc_t)+size)=kAsuLast;
/*	Update Mgr	*/
   gAsuMgr.fNumBlok++;
   gAsuMgr.fNumAlloc++;
   gAsuMgr.fSize +=size;
}

/*--------------------------------------------------------------------*/
int asuMallocRemove(void *pv, const char* file, int line){

static int NumGiveUp=50;
asuAlloc_t *pa,*pnex,*pbak;
char *pc; int size,idel,inew,i;

  pc = (char*)pv; 
  pa = (asuAlloc_t*)(pc - sizeof(asuAlloc_t)); 

  if (pa->magic != kAsuMagic) {/* block was not created by asuMalloc*/
    if ( gAsuMgr.fLogLevel || NumGiveUp) {
      printf("asuFree. ***Warning***: block %p was not made by asuAlloc\n",pa); 
      printf("asuFree. ***-------***: File: %s Line %d \n",file,line);
      if (NumGiveUp) NumGiveUp--;}
    gAsuMgr.fNumWar++; return 1;}

  if (pa->status == kAsuDead) {/* block is dead */
      printf("asuFree. ***Error***: attempt delete dead block \n"); 
      asuPrintBlok(pa);
      printf("asuFree. ***Error***: File: %s Line %d \n",file,line);
      gAsuMgr.fNumErr++; return 2;}
  size = pa->size;

  if(*(pc+size)!=kAsuLast){ ;
      gAsuMgr.fNumErr++;
      printf("asuFree. ***Error***: Block corrupted \n"); 
      asuPrintBlok(pa);
      printf("asuFree. ***Error***: File: %s Line %d \n",file,line);}

  inew = (pa->line <0 );
  idel = (line < 0 );
  if (idel != inew) {/* new/new mess */
      gAsuMgr.fNumErr++;
      printf("asuFree. ***Error***: New/Delete mess \n"); 
      printf("         block made by "); 
      if (inew) {printf("new");} else {printf("malloc");}
      printf(" but deleted by ");
      if (idel) {printf("delete");} else {printf("free");}
      printf("\n");
      asuPrintBlok(pa);
      printf("asuFree. ***Error***: File: %s Line %d \n",file,line);}
 
  pnex = pa->next; pbak = pa->back;
  pbak->next = pnex; if (pnex) pnex->back = pbak;
  pa->next = NULL; pa->back = NULL; pa->status = kAsuDead;
  
  gAsuMgr.fNumBlok--;
  gAsuMgr.fNumFree++;
  gAsuMgr.fSize-=pa->size;

  return 0;      
}

/*--------------------------------------------------------------------*/
void *asuCalloc(size_t nobj, size_t size, const char* file, int line)
{
   asuAlloc_t *p,*pold;

   size = size*nobj;
   p = (asuAlloc_t*)malloc(size+sizeof(asuAlloc_t)+1);
   if (p) memset(p,0,size+sizeof(asuAlloc_t));
   asuMallocAdd(p,size,file,line);
   if (!p) return NULL;
   return (void*)((char*)p+sizeof(asuAlloc_t));
}

/*--------------------------------------------------------------------*/
void *asuMalloc(size_t size, const char* file, int line)
{
   void *p;
   int i;

   p = malloc(size+sizeof(asuAlloc_t)+1);
   
   asuMallocAdd(p,size,file,line);
   if (!p) return NULL;
   p = (void*)((char*)p+sizeof(asuAlloc_t));
   return p;
}

/*--------------------------------------------------------------------*/
void *asuRealloc(void *p, size_t size, const char* file, int line)
{
char *pc = (char*)p;

int ierr  = asuMallocRemove(p,file,line);
  switch (ierr) {
  
  case 0:
    p = realloc(pc-sizeof(asuAlloc_t),size+sizeof(asuAlloc_t)+1);
    pc = (char*)p;
    asuMallocAdd(p,size,file,line);
    if (!p) return NULL;
    return (void*)(pc+sizeof(asuAlloc_t));
    break;
  case 1:
    return realloc(p,size);
  default:
    return NULL;
  }
}

/*--------------------------------------------------------------------*/
void asuFree(void *p, const char* file, int line)
{
int ierr;
  if(!p) return;
  ierr = asuMallocRemove(p,file,line);
  if (ierr==2) return;
  if (ierr==1) free(p);
  if (ierr==0) free((char*)p-sizeof(asuAlloc_t)); 
}

/*--------------------------------------------------------------------*/

  
int asuStack(void *p) 
{
  static unsigned long MinStack=(unsigned long)(-1),MaxStack=0,MinHeap=0;
  static long init=0,ncall=0;
  unsigned long NowStack,pp;

  ncall++;
  if (!MinHeap) {
    MinHeap = (unsigned long)malloc(8); 
    init = (! p);
    }

  NowStack = (unsigned long)&NowStack ;

  if (NowStack > MaxStack) MaxStack = NowStack;
  if (NowStack < MinStack) MinStack = NowStack;
  pp = (unsigned long)p;
  if (! pp) return 0;

  if (MaxStack >= pp && pp > NowStack) 	return  1;
  if (NowStack >= pp && pp > MinStack) 	return -1;
  if (! init) return 0;
/*if (pp < MinHeap)  			return -2;*/
  return 0;    
}

void asuPrintBlok(asuAlloc_t *p){
  int corr;
  int size = p->size;
  corr =(*((char*)p + sizeof(asuAlloc_t)+size)!=kAsuLast) ;
  corr = corr & (p->begin != kAsuBegin);
  gAsuMgr.fNumErr +=corr;
  printf("%p(%6d) \tEvt=%d \tFile=%s:%d "
    ,p,size,p->event,p->file,p->line);

  if (corr) printf(" CORRUPTED"); printf("\n");

}
unsigned short int asuSumm(void *blok,int size) {
 unsigned long int *data = (unsigned long int *)blok;
 int n = size/sizeof(long);
 unsigned long int sum = 0;
 int i;
 for (i=0;i<n;i++) { sum = sum ^ data[i];}
 return (sum & 0xFFFF ) ^ (sum >> 16);
} 
void asuWau() {return;};
