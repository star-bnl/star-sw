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
  char* pkgName = ku_gets();	/* ASP name */
  char* solibName = ku_gets();	/* Library name */

   soc_bind(pkgName, solibName);
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
   char* pkgName = ku_gets();	/* ASP name */

   soc_release(pkgName);
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
void 
kam_socobject_name_()
{
   long idref = ku_geti();	/* idRef of object */

   socobject_name(idref);
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
void 
kam_socobject_type_()
{
   long idref = ku_geti();	/* idRef of object */

   socobject_type(idref);
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
void 
kam_socobject_version_()
{
   long idref = ku_geti();	// idRef of object 

   socobject_version(idref);
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
void 
kam_soc_count_()
{
   soc_count();
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
   long id = ku_geti();		/* object id */

   EML_CONTEXT("ERROR: This is a functional, though obsolete command.\n"
   "Please use SOC/DELETEOID instead.\n");
   EML_WARNING(OBSOLETE_COMMAND);

   soc_deleteid(id);
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
void 
kam_soc_deleteoid_()
{
   long id = ku_geti();		/* object id */

   soc_deleteoid(id);
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
void 
kam_soc_deleteobject_()
{
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   EML_CONTEXT("ERROR: This is a functional, though obsolete command.\n"
   "Please use SOC/OBJECT/DELETE instead.\n");
   EML_WARNING(OBSOLETE_COMMAND);

   soc_deleteobject(name, type);
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
void 
kam_soc_idobject_()
{
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   soc_idobject(name, type);
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
void 
kam_soc_list_()
{
  soc_list();
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
void 
kam_soc_newobject_()
{
  char* name = ku_gets();	/* object name */
  
  soc_newobject(name);
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
void
kam_socobject_lock_()
{
  long idref = ku_geti();	/* idRef of object */
  char* l = ku_gets();		/* lock value */
  
  socobject_lock(idref,l[0]);
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
void 
kam_socobject_implements_()
{
   long idref = ku_geti();	/* idRef of object */
   char* iface = ku_gets();	/* interface name */

   socobject_implements(idref,iface);
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
void 
kam_socobject_delete_()
{
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   socobject_delete(name, type);
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
void 
kam_socobject_oid_()
{
   char* name = ku_gets();	/* object name */
   char* type = ku_gets();	/* object type */

   socobject_oid(name, type);
}


