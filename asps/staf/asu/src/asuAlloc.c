#include "asuAlloc.h"

static int asuAllocCalls = 0;
static int asuFreeCalls = 0;
static int asuAllocSize = 0;
static int asuFreeSize = 0;
/*--------------------------------------------------------------------*/
void asuAllocStats(void)
{
	printf("asuAllocStats: allocCalls %d, freeCalls %d, diff %d\n"
		, asuAllocCalls, asuFreeCalls
		, asuAllocCalls - asuFreeCalls);
	printf("asuAllocStats: allocSize %d, freeSize -, diff -\n"
		, asuAllocSize);
		/*, asuFreeSize , asuAllocSize - asuFreeSize);*/
}

/*--------------------------------------------------------------------*/
void *asuAlloc(size_t size)
{
	return asuRealloc(NULL, size);
}

/*--------------------------------------------------------------------*/
void *asuCalloc(size_t nobj, size_t size)
{
	asuAllocSize += size*nobj;
	asuAllocCalls++;
	return calloc(nobj, size);
}

/*--------------------------------------------------------------------*/
void asuFree(void *ptr)
{
	if (ptr != NULL) {
		asuFreeSize += sizeof *ptr;
		free((char *)ptr);
		asuFreeCalls++;
	}
}

/*--------------------------------------------------------------------*/
void *asuRealloc(char *old, size_t size)
{
	char *ptr;

	if (size == 0){
		printf("ZERO_LENGTH_ALLOC\n"); /* HACK */
	}
	if ((ptr = (old == NULL ? malloc(size) : realloc(old, size))) != NULL) {
		if (old == NULL) {
			asuAllocSize += size;
			asuAllocCalls++;
		}
		return ptr;
	}
	else {
		printf("NOT_ENOUGH_MEMORY\n");
		}
	return NULL;
}

