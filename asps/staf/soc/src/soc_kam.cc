/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:         soc_kam.c
*:DESCRIPTION:  C KUIP Action Modules for Error & Message Logger
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      26jul95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

#include "asuAlloc.h"
#include "asuLib.h"
#include "emlLib.h"
#include "socLib.h"

#include "kuip.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_bind_
*:DESCRIPTION:  KUIP Action Module to bind to SOC server
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/BIND ASP SOLIB
*:<---------------------------------------------------------------------
*/

void kam_soc_bind_(){kam_soc_bind();}
int kam_soc_bind()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* aspName = ku_gets();	/* ASP name */
   char* solibName = ku_gets();	/* shared library */

   void *solibHandle;
   char funcName[9];
// void **funcHandle;
   int (*asp_init)();
   int (*asp_start)();

   if( 0 == strcmp(solibName,"-") ){
      solibName = (char*)ASUALLOC(9);
      sprintf(solibName,"lib%3s.so",aspName);
   }

   solibHandle = dlopen(solibName,RTLD_LAZY);
   sprintf(funcName,"%3s_init",aspName);
   asp_init = (int(*)())dlsym(solibHandle,funcName);
   sprintf(funcName,"%3s_start",aspName);
   asp_start = (int(*)())dlsym(solibHandle,funcName);

   asp_init();
   asp_start();

   ASUFREE(solibName);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_release_
*:DESCRIPTION:  KUIP Action Module to release bound SOC server
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/RELEASE ASP SOLIB
*:<---------------------------------------------------------------------
*/

void kam_soc_release_(){kam_soc_release();}
int kam_soc_release()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* aspName = ku_gets();	/* ASP name */
   char* solibName = ku_gets();	/* shared library */

   if( 0 == strcmp(solibName,"-") ){
      solibName = (char*)ASUALLOC(9);
      sprintf(solibName,"lib%3s.so",aspName);
   }

   void *solibHandle;
   char funcName[9];
// void **funcHandle;
   int (*asp_stop)();

   solibHandle = dlopen(solibName,RTLD_LAZY);
   sprintf(funcName,"%3s_stop",aspName);
   asp_stop = (int(*)())dlsym(solibHandle,funcName);

   asp_stop();

   ASUFREE(solibName);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_name_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/OBJECT/NAME IDREF
*:<---------------------------------------------------------------------
*/
void kam_socobject_name_(){kam_socobject_name();}
int kam_socobject_name()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long idref = ku_geti();	/* idRef of object */

   if( !VALID_IDREF(idref) ){
      EML_ERROR(KAM_INVALID_IDREF);
   }

   socObject* p;
   if( !soc->getObject(idref,p) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

   char *n;
   printf("SOC:\tObject name = %s \n",n=p->name());
   delete[] n;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_type_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/OBJECT/TYPE IDREF
*:<---------------------------------------------------------------------
*/
void kam_socobject_type_(){kam_socobject_type();}
int kam_socobject_type()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long idref = ku_geti();	/* idRef of object */

   if( !VALID_IDREF(idref) ){
      EML_ERROR(KAM_INVALID_IDREF);
   }

   socObject* p;
   if( !soc->getObject(idref,p) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

   char *t;
   printf("SOC:\tObject type = %s \n",t=p->type());
   delete[] t;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_count_
*:DESCRIPTION:  KUIP Action Module to show number of registered objects
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/COUNT
*:<---------------------------------------------------------------------
*/
void kam_soc_count_(){kam_soc_count();}
int kam_soc_count()
{
   long npars = ku_npar();	/* number of KUIP parameters */

   long i = soc->count();
   printf("SOC:\tObject count = %d \n",i);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_deleteid_
*:DESCRIPTION:  KUIP Action Module to delete object #id
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/DELETEOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_soc_deleteid_(){kam_soc_deleteid();}
int kam_soc_deleteid()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long id = ku_geti();		/* object id */

   soc->deleteID(id);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_deleteobject_
*:DESCRIPTION:  KUIP Action Module to delete registered object
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/DELETEOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_soc_deleteobject_(){kam_soc_deleteobject();}
int kam_soc_deleteobject()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   soc->deleteObject(name,type);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_idobject_
*:DESCRIPTION:  KUIP Action Module to identify object
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/FINDOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_soc_idobject_(){kam_soc_idobject();}
int kam_soc_idobject()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   IDREF_T id;
   if( !soc->idObject(name,type,id) ){
      EML_ERROR(KAM_INVALID_IDREF);
   }
   printf("SOC:\tObject idRef =  %d \n",id);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_list_
*:DESCRIPTION:  KUIP Action Module to list all registered objects
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/LIST
*:<---------------------------------------------------------------------
*/
void kam_soc_list_(){kam_soc_list();}
int kam_soc_list()
{
   long npars = ku_npar();	/* number of KUIP parameters */

   soc->list();
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_newobject_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/NEWOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_soc_newobject_(){kam_soc_newobject();}
int kam_soc_newobject()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */

   soc->newObject(name);

   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

