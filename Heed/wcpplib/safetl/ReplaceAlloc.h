#ifndef REPLACE_ALLOC_H
#define REPLACE_ALLOC_H

/* This macro allows to replace standard new and delete for objects
of particular class to any non-standard operators, in the example below
it is ordinary malloc and free.
*/
#include <stdlib.h>

#define macro_alloc \
public: \
  void* operator new(size_t size, void* adr = NULL) \
  { \
    if(adr == NULL) return malloc(size); \
    return adr; \
  } \
  void* operator new[](size_t size, void* adr = NULL) \
  { \
    if(adr == NULL) return malloc(size); \
    return adr; \
  } \
  void operator delete (void *f) \
  { \
    free(f); \
  } \
  void operator delete[] (void *f) \
  { \
    free(f); \
  }




#endif
