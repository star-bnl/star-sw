
#include <stdlib.h>
#include <stdio.h>
#include "asuAlloc.h"

int main()
{
   char *a;
   char *b;
   char *c;
   char *d;

   asuMallocInit();
   asuMallocLevel(ASU_MALLOC_TRACE);
   a = (char*)MALLOC(128);
   asuMallocStats();
   b = (char*)MALLOC(1024);
   asuMallocLevel(ASU_MALLOC_FILL);
   c = (char*)MALLOC(2048);
   asuMallocStats();
   asuMallocLevel(ASU_MALLOC_VERBOSE);
   d = (char*)MALLOC(32);
   asuMallocStats();
   asuMallocLevel(ASU_MALLOC_FILL);
   FREE(a);
   asuMallocStats();
   FREE(c);
   asuMallocStats();
   FREE(b);
   asuMallocStats();
   asuMallocLevel(ASU_MALLOC_VERBOSE);
   FREE(d);
   asuMallocStats();
}
