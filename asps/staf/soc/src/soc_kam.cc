/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:         soc_kam.c
*:DESCRIPTION:  C KUIP Action Modules for Error & Message Logger
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      11nov96-v001a-cet- seperate KAM and non-KAM func.s
*:HISTORY:      26jul95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#define KUIP
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
void kam_soc_bind_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* aspName = ku_gets();	/* ASP name */
   char* solibName = ku_gets();	/* shared library */

   STAFCV_T status = soc_bind(aspName,solibName);
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
void kam_soc_release_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* aspName = ku_gets();	/* ASP name */

   STAFCV_T status = soc_release(aspName);
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
void kam_socobject_name_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long idref = ku_geti();	/* idRef of object */

   STAFCV_T status = socobject_name(idref);
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
void kam_socobject_type_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long idref = ku_geti();	/* idRef of object */

   STAFCV_T status = socobject_type(idref);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_version_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/OBJECT/VERSION IDREF
*:<---------------------------------------------------------------------
*/
void kam_socobject_version_()
{
   long npars = ku_npar();	// number of KUIP parameters 
   long idref = ku_geti();	// idRef of object 

   STAFCV_T status = socobject_version(idref);
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
void kam_soc_count_(){
   long npars = ku_npar();	/* number of KUIP parameters */

   STAFCV_T status = soc_count();
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
void kam_soc_deleteid_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long id = ku_geti();		/* object id */

   EML_CONTEXT("ERROR: This is a functional, though obsolete command.\n"
   "Please use SOC/DELETEOID instead.\n");
   EML_WARNING(OBSOLETE_COMMAND);

   STAFCV_T status = soc_deleteid(id);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_deleteoid_
*:DESCRIPTION:  KUIP Action Module to delete object #id
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/DELETEOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_soc_deleteoid_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long id = ku_geti();		/* object id */

   STAFCV_T status = soc_deleteoid(id);
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
void kam_soc_deleteobject_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   EML_CONTEXT("ERROR: This is a functional, though obsolete command.\n"
   "Please use SOC/OBJECT/DELETE instead.\n");
   EML_WARNING(OBSOLETE_COMMAND);

   STAFCV_T status = soc_deleteobject(name, type);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_soc_idobject_
*:DESCRIPTION:  KUIP Action Module to identify object
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*: * SOC/IDOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_soc_idobject_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   STAFCV_T status = soc_idobject(name, type);
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
void kam_soc_list_()
{
   long npars = ku_npar();	/* number of KUIP parameters */

   STAFCV_T status = soc_list();
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
void kam_soc_newobject_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */

   STAFCV_T status = soc_newobject(name);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_lock_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/OBJECT/LOCK IDREF [ LOCK ]
*:<---------------------------------------------------------------------
*/
void kam_socobject_lock_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long idref = ku_geti();	/* idRef of object */
   char* l = ku_gets();		/* lock value */

   STAFCV_T status = socobject_lock(idref,l[0]);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_implements_
*:DESCRIPTION:  KUIP Action Module to 
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/OBJECT/LOCK IDREF [ LOCK ]
*:<---------------------------------------------------------------------
*/
void kam_socobject_implements_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   long idref = ku_geti();	/* idRef of object */
   char* iface = ku_gets();	/* interface name */

   STAFCV_T status = socobject_implements(idref,iface);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_delete_
*:DESCRIPTION:  KUIP Action Module to delete object
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/OBJECT/DELETE NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_socobject_delete_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   STAFCV_T status = socobject_delete(name, type);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_socobject_oid_
*:DESCRIPTION:  KUIP Action Module to identify object
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* SOC/CATALOG/FINDOBJECT NAME [ TYPE ]
*:<---------------------------------------------------------------------
*/
void kam_socobject_oid_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   STAFCV_T status = socobject_oid(name, type);
}


