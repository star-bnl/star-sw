#ifndef __CDL_H__
#define __CDL_H__

#include <stdlib.h>
#if defined(__hpux)
#include <dl.h>
void perror();
#elif defined(AIX)
#include "dlfcn.h"
#elif !defined(WIN32)
#include <dlfcn.h>
#endif

typedef enum {
  UNDEF_T,
  VOID_T,
  VOID_P,
  CHAR_T,
  CHAR_P,
  LOGICAL_T,
  LOGICAL_P,
  INTEGER_T,
  INTEGER_P,
  REAL_T,
  REAL_P,
  DOUBLE_T,
  DOUBLE_P,
  COMPLEX_T,
  COMPLEX_P,
  LAST_T
} ArgType;

typedef 
#if defined(__hpux)
   shl_t 
#elif defined(SUN) || defined(SGI) || defined(SGI64)
   int *
#else
   void *
#endif
  FileHandle;

typedef struct files {
   struct files  *next;
   char          *filename;
   FileHandle     file_handle;
} Files, *FilesPtr;

typedef void (*GenFuncPtr)();

typedef struct FuncDesc {
   struct FuncDesc   *next;
   struct files      *file;
   char              *name;
   GenFuncPtr         funcptr;
   ArgType            type;
   short              narg;
   ArgType           *arglist;
} FuncDesc, *FuncPtr;

struct cdldefs{
  char path[72], f77[72], cc[72], CC[72], ld[72], lang[4];
};

#ifdef __cplusplus
extern "C" {
#endif

  extern void  cdl_init(void);
  extern void  cdl_set_opt(char *text, char *chvar);
  extern int   cdl_create(char *filename);
  extern int   cdl_load(char *lib_name);
  extern int   cdl_unload(char *lib_name);
  extern void *cdl_func_addr(char *func_name);
  extern char *cdl_get_lib(int *ns );
  extern void  cdl_symbols(char *lib_name, int *ns, char *symbol);
  extern FuncPtr cdl_get_func(char *func_name);
  extern int cdl_proto(char *func_proto);
  
#ifdef __cplusplus
}
#endif

#endif /* __CDL_H__ */
