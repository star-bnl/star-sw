/*:
*:>---------------------------------------------------------------------
*:FILE:         ami_cstubs.cc
*:DESCRIPTION:  C KUIP Action Modules for AMI
*:AUTHOR:       
*:BUGS:         
*:HISTORY:      
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "asuAlloc.h"

#include "ami_macros.h"
#include "ami_types.h"
#include "ami_globals.h"

STAFCV_T 
ami_count()
{
   printf("AMI:\tObject count = %ld \n",ami->count());
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
ami_list()
{
   char *herb980615;
   char* amilist;
   amilist = ami->list();
   herb980615=strtok(amilist,"\n");
   while(herb980615) {
     printf("%s\n",herb980615);
     herb980615=strtok(NULL,"\n");
   }
   FREE(amilist);  /*fix memory leak -akio*/
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
ami_call(char* name,long ntabs,char **tnames)
{
   STRING_SEQ_T tbls;		/* table names */

//- Get table names. -**
   tbls._length = tbls._maximum = ntabs;
   tbls._buffer = tnames;

//- Tell the AMI Broker to invoke the PAM.
   if( !ami->callInvoker(name, tbls) ){
      //- WARNING!!! - PAM status already recorded!!!
      EML_POPSTACK();
      return FALSE;
//    EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);

}

STAFCV_T 
amimodule_rank(char* name)
{
   amiInvoker* pam;		/* amiInvoker object */

   if( NULL == (pam = ami->findInvoker(name)) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("AMI:\tAnalysis module rank = %ld \n",pam->rank());
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
amimodule_show(char* name)
{
   amiInvoker* pam;		/* amiInvoker object */

   if( NULL == (pam = ami->findInvoker(name)) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   int rank = pam->rank();
   char *c;
   for( int i=0;i<rank;i++ ){
      printf("AMI:\tTable Specification = ...\n%s .\n"
		,c = pam->tableSpec(i));
      FREE(c);
   }
   EML_SUCCESS(STAFCV_OK);
}

/***********************************************************************
 ***********************************************************************
 ********	THE FOLLOWING COMMANDS ARE NOT IMPLEMENTED.	********
 ********	SHOULD THEY BE?					********
 ***********************************************************************
 ***********************************************************************/

STAFCV_T 
amimodule_init(char* name)
{
  // Just to hush pedantic compilers
  static void *pn = name;
  EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
amimodule_start(char* name)
{
  // Just to hush pedantic compilers
  static void *pn = name;
  EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
amimodule_stop(char* name)
{
  // Just to hush pedantic compilers
  static void *pn = name;
  EML_FAILURE(NOT_YET_IMPLEMENTED);
}

