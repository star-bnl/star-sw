#include <stdio.h>
#include <stdlib.h>
#include <new.h>
#include "asuAlloc.h"
#ifdef new
#undef new 
#endif

void* operator new   (size_t sz) { return asuMalloc(sz,"NEW:",-1);}
void* operator new   (size_t sz,const char *file,int line) {
 return asuMalloc(sz,file,-line);
}


void  operator delete(void *ptr) { asuFree (ptr,"DEL:",-1);}

