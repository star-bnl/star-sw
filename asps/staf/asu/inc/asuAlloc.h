#ifndef NULL
	#define NULL 0
#endif

#ifndef CC_P
        #ifdef __cplusplus
                #define CC_P "C"
        #else
                #define CC_P
        #endif
#endif /*CC_P*/

#include <stdlib.h>

typedef enum asu_malloclevel_t {
	ASU_MALLOC_FAST,		/* only call m&f */
	ASU_MALLOC_COUNT,		/* ...and count calls to m&f */
	ASU_MALLOC_TRACE,		/* ...and keep trace of m&f */
	ASU_MALLOC_FILL,		/* ...and fill w/ pattern */
	ASU_MALLOC_VERBOSE,		/* ...and print every time */
	ASU_MALLOC_INIT			/* UNINITIALIZED */
}ASU_MALLOCLEVEL_T;

#define ASU_MALLOC_FILLPATTERN 0xbabe
#define ASU_MALLOC_FREEPATTERN 0xdead

#ifndef ASU_MALLOC_LEVEL
#define ASU_MALLOC_LEVEL ASU_MALLOC_FAST
#endif					/* default is fastest */

typedef struct asu_malloctrace_t {
	void *p;
	size_t size;
	char file[128];
	int line;
}ASU_MALLOCTRACE_T;

extern CC_P void asuMallocInit();
extern CC_P void asuMallocInitTrace();
extern CC_P void asuMallocGrowTrace(size_t m);
extern CC_P void asuMallocReset();
extern CC_P void asuMallocStats();
extern CC_P void asuMallocPrintTrace(void *p, size_t size, char* file
		, int line);
extern CC_P void asuMallocLevel(ASU_MALLOCLEVEL_T level);

extern CC_P void asuMallocAdd(void *p, size_t size
		, char* file, int line);
extern CC_P void asuMallocRemove(void *p
		, char* file, int line);

extern CC_P void *asuCalloc(size_t nobj, size_t size
		, char* file, int line);
extern CC_P void *asuMalloc(size_t size
		, char* file, int line);
extern CC_P void *asuRealloc(void *p, size_t size
		, char* file, int line);
extern CC_P void asuFree(void *p
		, char* file, int line);


#ifndef ASU_MALLOC_OFF
#define CALLOC(NOBJ,SIZE) asuCalloc(NOBJ,SIZE,__FILE__,__LINE__)
#define MALLOC(SIZE) asuMalloc(SIZE,__FILE__,__LINE__)
#define REALLOC(P,SIZE) asuRealloc(P,SIZE,__FILE__,__LINE__)
#define FREE(P) asuFree(P,__FILE__,__LINE__)
#else
#define CALLOC(NOBJ,SIZE) calloc(NOBJ,SIZE)
#define MALLOC(SIZE) malloc(SIZE)
#define REALLOC(P,SIZE) realloc(P,SIZE)
#define FREE(P) free(P)
#endif

