#ifndef ASU_ALLOC_H
#define ASU_ALLOC_H 1
#include <stdlib.h>
#ifdef __ROOT__
#undef ASU_MALLOC_ON 
#endif /**__ROOT__**/

#ifdef ASU_MALLOC_ON
#ifdef __cplusplus
void * operator new(size_t sz,const char *file,int line);
void * operator new(size_t sz);

#ifdef NEW_ARRAY_ON
void * operator new[](size_t sz,const char *file,int line);
void * operator new[](size_t sz);
#endif /**NEW_ARRAY_ON**/

#define new new(__FILE__,__LINE__)
#endif /**__cplusplus**/
#endif /** ASU_MALLOC_ON **/


#ifndef CC_P
        #ifdef __cplusplus
                #define CC_P "C"
        #else
                #define CC_P
        #endif
#endif /*CC_P*/


enum eAsu { kAsuMagic = 0xBEDA1980, kAsuDead = 0xDEAD
          , kAsuBegin = 0xBABA1998, kAsuLast = 123};

typedef struct asuAlloc_t {
        struct asuAlloc_t *next;
        struct asuAlloc_t *back;
	char file[16];
	size_t size;
	short int line;
        unsigned short int event;
        unsigned short int status;        
        unsigned short int summ;        
        unsigned int magic;        
        unsigned int begin;
}asuAlloc_t;

typedef struct asuAllocMgr_t {
int fNumBlok;
int fNumAlloc;
int fNumFree;
unsigned long fSize;
int fEvent;
int fNumErr;
int fNumWar;
int fLogLevel;
}asuAllocMgr_t;





extern CC_P int  asuStack(void *p);
extern CC_P void asuMallocInit();
extern CC_P void asuMallocInitTrace();
extern CC_P void asuMallocReset();
extern CC_P void asuMallocStats(int level);
extern CC_P void asuMallocPrintTrace(void *p, size_t size,const char* file,int line);
extern CC_P void asuFree(void *p,const char* file, int line);
extern CC_P void asuMallocLevel(int level);

extern CC_P void asuMallocAdd   (void *p, size_t size, const char* file, int line);
extern CC_P int  asuMallocRemove(void *p, const char* file, int line);

extern CC_P void *asuCalloc(size_t nobj, size_t size, const char* file, int line);
extern CC_P void *asuMalloc(size_t size, const char* file, int line);
extern CC_P void *asuRealloc(void *p, size_t size, const char* file, int line);


#ifdef ASU_MALLOC_ON
#define new new(__FILE__,__LINE__)
#define CALLOC(NOBJ,SIZE) asuCalloc(NOBJ,SIZE,__FILE__,__LINE__)
#define MALLOC(SIZE) asuMalloc(SIZE,__FILE__,__LINE__)
#define REALLOC(P,SIZE) asuRealloc(P,SIZE,__FILE__,__LINE__)
#define FREE(P) { asuFree((P),__FILE__,__LINE__); (P) = NULL;}
#else
#define CALLOC(NOBJ,SIZE) calloc(NOBJ,SIZE)
#define MALLOC(SIZE) malloc(SIZE)
#define REALLOC(P,SIZE) realloc(P,SIZE)
#define FREE(P) { free(P); (P) = NULL;}
#endif
#endif /*ASU_ALLOC_H*/
