#ifndef _CDL_INCLUDED
#define _CDL_INCLUDED

#ifdef HPUX
#include <stdlib.h>
#include <dl.h>
void perror();
#endif
#if defined(SUN) || defined(SGI) || defined(ALPHA_OSF)
#include <dlfcn.h>
#endif
#ifdef AIX
#include "dlfcn.h"
#endif

#include <string.h>
#include <stdio.h>

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
#ifdef HPUX
   shl_t 
#endif
#if defined(SUN) || defined(SGI) || defined(SGI64)
   int *
#endif
#if defined(ALPHA_OSF) || defined(AIX)
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

static FuncPtr procList = NULL;

static FilesPtr fileList = NULL;

static int debug_level = 0;

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

#endif
