#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "asuAlloc.h"
#include "cdl.h"
#include "emlLib.h"

/* Dynamically load a shared library. */
int 
soc_dl_load (char *pkgName)
{
  char *solibName;
  
  solibName = (char *) MALLOC(strlen(pkgName) + 7);
#if defined(__hpux)
  sprintf(solibName, "lib%s.sl", pkgName);
#else
  sprintf(solibName, "lib%s.so", pkgName);
#endif
  if (cdl_load(solibName)) {
    FREE(solibName); /*fix memory leak -akio*/
    EML_ERROR(NO_FUNCTION_LOADED);
  }
  FREE(solibName); /*fix memory leak -akio*/

  return STAFCV_OK;
}

static int
soc_dl_call (char *pkgName, char *suffix)
{
  char *funcName;
  int (*theFunction)();
  
  funcName = (char *) MALLOC(strlen(pkgName) + strlen(suffix) + 1);
  sprintf(funcName, "%s%s", pkgName, suffix);

  theFunction = (int(*)()) cdl_func_addr(funcName);
  FREE(funcName);   
  if (!theFunction) 
    EML_ERROR(NO_FUNCTION_LOADED);
  
  return theFunction();   
}

/* Call the ???_init entrypoint of a package */
int 
soc_dl_init (char *pkgName) 
{
  return soc_dl_call(pkgName, "_init");
}

/* Call the ???_start entrypoint of a package */
int 
soc_dl_start (char *pkgName) 
{
  return soc_dl_call(pkgName, "_start");
}

/* Call the ???_stop entrypoint of a package */
int 
soc_dl_stop (char *pkgName) 
{
  return soc_dl_call(pkgName, "_stop");
}

