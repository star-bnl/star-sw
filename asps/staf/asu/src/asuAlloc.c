
#include <stdio.h>
#include "asuAlloc.h"

static ASU_MALLOCLEVEL_T asu_mallocLevel=ASU_MALLOC_INIT;

static size_t asu_mallocCalls = 0;
static size_t asu_freeCalls = 0;
static size_t asu_mallocSize = 0;
static size_t asu_freeSize = 0;

static size_t asu_nTrace = 0;
static size_t asu_maxTrace = 1;
static ASU_MALLOCTRACE_T asu_mallocTrace0;
static ASU_MALLOCTRACE_T *asu_mallocTrace=&asu_mallocTrace0;


/*--------------------------------------------------------------------*/
void asuMallocInit()
{
   if( !(ASU_MALLOC_INIT == asu_mallocLevel) ){
      fprintf(stderr,"ASU_MALLOC: use Reset \n");fflush(0);
      return;
   }
   fprintf(stderr,"ASU_MALLOC: Initializing ");fflush(0);

   asuMallocLevel(ASU_MALLOC_LEVEL);

   switch(asu_mallocLevel) {
   case ASU_MALLOC_VERBOSE:	/* ...and print every time */
      fprintf(stderr,",VERBOSE ");
   case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
      fprintf(stderr,",FILL ");
   case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
      fprintf(stderr,",TRACE ");
      asuMallocInitTrace();
   case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
      fprintf(stderr,",COUNT ");
   case ASU_MALLOC_FAST:	/* only call m&f */
   default:
      fprintf(stderr,"\n");
      break;
   }
}

/*--------------------------------------------------------------------*/
void asuMallocInitTrace()
{
   asu_nTrace = 0;
   asuMallocGrowTrace(1024);	/* reserve space for 1024 calls */
}

/*--------------------------------------------------------------------*/
void asuMallocGrowTrace(size_t m)
{
   ASU_MALLOCTRACE_T* oldTrace = asu_mallocTrace;
   size_t oldMaxTrace = asu_maxTrace;

   asu_maxTrace = m;
   asu_mallocTrace = (ASU_MALLOCTRACE_T*)MALLOC(asu_maxTrace*sizeof(
		ASU_MALLOCTRACE_T));
   memcpy(asu_mallocTrace, (void*)oldTrace
		, oldMaxTrace*sizeof(ASU_MALLOCTRACE_T));
}

/*--------------------------------------------------------------------*/
void asuMallocStats()
{
   int i;

   fprintf(stderr,"\nASU_MALLOC: Memory allocation statistics: ");

   switch(asu_mallocLevel) {
   case ASU_MALLOC_VERBOSE:	/* ...and print every time */
   case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
   case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
      for( i=0;i<asu_nTrace;i++ ){
	 if( asu_mallocTrace[i].p ){
	    fprintf(stderr,"\n");
            asuMallocPrintTrace(
			  asu_mallocTrace[i].p
			, asu_mallocTrace[i].size
			, asu_mallocTrace[i].file
			, asu_mallocTrace[i].line
	    );
	 }
      }
   case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
      fprintf(stderr,"\n\tmallocCalls %d, freeCalls %d, diff %d"
      		, asu_mallocCalls, asu_freeCalls
		, asu_mallocCalls - asu_freeCalls);
      fprintf(stderr,"\n\tmallocSize %d, freeSize %d, diff %d"
      		, asu_mallocSize, asu_freeSize
		, asu_mallocSize - asu_freeSize);
      fprintf(stderr,"\n\tasuMallocSize %d"
		, (1==asu_maxTrace? 0:
		asu_maxTrace*sizeof(ASU_MALLOCTRACE_T)));
   case ASU_MALLOC_FAST:	/* only call m&f */
   default:
      fprintf(stderr,"\n");
      break;
   }
}

/*--------------------------------------------------------------------*/
void asuMallocPrintTrace(void *p, size_t size, char* file, int line)
{
   long v;
   memcpy(&v,p,4);
   fprintf(stderr,"(%p:%d) %s.%d [%x]", p, size, file, line, v);
}

/*--------------------------------------------------------------------*/
ASU_MALLOCLEVEL_T asuMallocLevel(ASU_MALLOCLEVEL_T level)
{
   ASU_MALLOCLEVEL_T oldLevel = asu_mallocLevel;

   switch(level) {
   case ASU_MALLOC_VERBOSE:	/* ...and print every time */
   case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
   case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
      switch(asu_mallocLevel) {
      case ASU_MALLOC_INIT:	/* UNINITIALIZED */
      case ASU_MALLOC_FAST:	/* only call m&f */
      case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
	 asuMallocInitTrace();
      case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
      case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
      case ASU_MALLOC_VERBOSE:	/* ...and print every time */
      default:
      break;
      }
   case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
   case ASU_MALLOC_FAST:	/* only call m&f */
   case ASU_MALLOC_INIT:	/* UNINITIALIZED */
   default:
   break;
   }

   asu_mallocLevel = level;
   return oldLevel;
}

/*--------------------------------------------------------------------*/
void asuMallocReset()
{
   ASU_MALLOCLEVEL_T tmpLevel;;

   asu_mallocCalls = 0;
   asu_freeCalls = 0;
   asu_mallocSize = 0;
   asu_freeSize = 0;
   FREE(asu_mallocTrace);
   tmpLevel = asu_mallocLevel;
   asuMallocLevel(ASU_MALLOC_INIT);
   asuMallocInit();
   asuMallocLevel(tmpLevel);
}

/*--------------------------------------------------------------------*/
void asuMallocAdd(void *p, size_t size, char* file, int line)
{
   int patt=ASU_MALLOC_FILLPATTERN;
   int *c = &patt;

   switch(asu_mallocLevel) {
   case ASU_MALLOC_VERBOSE:	/* ...and print every time */
      fprintf(stderr,"ASU_MALLOC: alloc ");
      asuMallocPrintTrace(p,size,file,line);
      fprintf(stderr,"\n");
   case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
      memset(p,'*',size);	/* HACK ? should be many memcpy's */
      if(size >= 4) memcpy(p,c,4);
   case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
      if( asu_nTrace >= asu_maxTrace ){
	 asuMallocGrowTrace((int)(1.5*asu_maxTrace));
      }
      asu_mallocTrace[asu_nTrace].p = p;
      asu_mallocTrace[asu_nTrace].size = size;
      strncpy(asu_mallocTrace[asu_nTrace].file,file,127);
      asu_mallocTrace[asu_nTrace].line = line;
      asu_nTrace++;
   case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
      asu_mallocCalls++;
      asu_mallocSize += size;
   case ASU_MALLOC_FAST:	/* only call m&f */
   case ASU_MALLOC_INIT:	/* UNINITIALIZED */
   default:
   break;
   }


}

/*--------------------------------------------------------------------*/
void asuMallocRemove(void *p, char* file, int line)
{
   int i;
   int patt=ASU_MALLOC_FREEPATTERN;
   int *c = &patt;
   size_t size;

   switch(asu_mallocLevel) {
   case ASU_MALLOC_VERBOSE:	/* ...and print every time */
   case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
   case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
      for( i=0;i<asu_nTrace;i++ ){
	 if( p == asu_mallocTrace[i].p ){
	    asu_mallocTrace[i].p = NULL;
	    asu_freeSize += asu_mallocTrace[i].size;

	    size = asu_mallocTrace[i].size;
	    switch(asu_mallocLevel) {
	    case ASU_MALLOC_VERBOSE:	/* ...and print every time */
	       fprintf(stderr,"ASU_MALLOC: free ");
	       asuMallocPrintTrace(p,size,file,line);
	       fprintf(stderr,"\n");
	    case ASU_MALLOC_FILL:	/* ...and fill w/ pattern */
	       memset(p,'-',size); /* HACK ? should be many memcpy's */
	       if(size >= 4) memcpy(p,c,4);
	    case ASU_MALLOC_TRACE:	/* ...and keep trace of m&f */
	    case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
	    case ASU_MALLOC_FAST:	/* only call m&f */
	    case ASU_MALLOC_INIT:	/* UNINITIALIZED */
	    default:
	       break;
	    }

	    break;
	 }
      }
   case ASU_MALLOC_COUNT:	/* ...and count calls to m&f */
      asu_freeCalls++;
   case ASU_MALLOC_FAST:	/* only call m&f */
   case ASU_MALLOC_INIT:	/* UNINITIALIZED */
   default:
      break;
   }

}

/*--------------------------------------------------------------------*/
void *asuCalloc(size_t nobj, size_t size, char* file, int line)
{
   void *p=NULL;
   p = calloc(nobj,size);
   asuMallocAdd(p,size,file,line);
   return p;
}

/*--------------------------------------------------------------------*/
void *asuMalloc(size_t size, char* file, int line)
{
   void *p=NULL;
   p = malloc(size);
   asuMallocAdd(p,size,file,line);
   return p;
}

/*--------------------------------------------------------------------*/
void *asuRealloc(void *p, size_t size, char* file, int line)
{
   asuMallocRemove(p,file,line);
   p = realloc(p,size);
   asuMallocAdd(p,size,file,line);
   return p;
}

/*--------------------------------------------------------------------*/
void asuFree(void *p, char* file, int line)
{
   asuMallocRemove(p,file,line);
   free(p);
}

/*--------------------------------------------------------------------*/
