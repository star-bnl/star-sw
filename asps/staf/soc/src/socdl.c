#include <stdio.h>
#include <stdlib.h>
#include "cdl.h"
#include "emlLib.h"

/*--------------------------------------------------------------------*/
int soc_dl_init(char *pkgName) {
  int returnValue;
  char *solibName;
  char *funcName;
  int (*theFunction)();

  solibName = (char*)malloc(strlen(pkgName) +7);
  sprintf(solibName,"lib%s.so",pkgName);

  if(cdl_load(solibName)) {
    fprintf(stderr,"Cannot load '%s'.\n",solibName);
    fprintf(stderr,"Check that your LD_LIBRARY_PATH contains '.'.\n");
    exit(2);
  }
  
  funcName = (char*)malloc(strlen(pkgName) +6);
  sprintf(funcName,"%s_init",pkgName);
  theFunction = (int(*)())cdl_func_addr(funcName);
  if(!theFunction) EML_ERROR(NO_FUNCTION_LOADED);
  free(funcName);

  returnValue=theFunction();

  return returnValue;
  
}

/*--------------------------------------------------------------------*/
int soc_dl_start(char *pkgName) {
  int returnValue;
  char *funcName;
  int (*theFunction)();

  funcName = (char*)malloc(strlen(pkgName) +7);
  sprintf(funcName,"%3s_start",pkgName);
  theFunction = (int(*)())cdl_func_addr(funcName);
  if(!theFunction) EML_ERROR(NO_FUNCTION_LOADED);
  free(funcName);

  returnValue=theFunction();

  return returnValue;
}

/*--------------------------------------------------------------------*/
int soc_dl_stop(char *pkgName) {
  int returnValue;
  char *funcName;
  int (*theFunction)();

  funcName = (char*)malloc(strlen(pkgName) +6);
  sprintf(funcName,"%3s_stop",pkgName);
  theFunction = (int(*)())cdl_func_addr(funcName);
  if(!theFunction) EML_ERROR(NO_FUNCTION_LOADED);
  free(funcName);

  returnValue=theFunction();

  return returnValue;
}

