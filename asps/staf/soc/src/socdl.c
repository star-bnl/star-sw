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
  char solibName[500];
  
 sprintf(solibName, "lib%s.so", pkgName);
 if (! cdl_load(solibName)) goto OK;
 sprintf(solibName, "lib%s.sl", pkgName);
 if (! cdl_load(solibName)) goto OK;
 sprintf(solibName, "%s.sl", pkgName);
 if (! cdl_load(solibName)) goto OK;
 sprintf(solibName, "%s.so", pkgName);
 if (! cdl_load(solibName)) goto OK;

 printf ("soc_dl_load can not load %s\n",pkgName);
 EML_ERROR(NO_FUNCTION_LOADED);

OK: printf("%s loaded\n",solibName);
  return STAFCV_OK;
}

static int
soc_dl_call (char *pkgName, char *suffix)
{
  char funcName[500];
  int (*theFunction)();
  
  sprintf(funcName, "%s%s", pkgName, suffix);

  theFunction = (int(*)()) cdl_func_addr(funcName);
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

