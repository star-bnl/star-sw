/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:	topClasses.hh
**:DESCRIPTION:	TOP-TEMPLATE ASP C++ classes.
**:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:	-- STILL IN DEVELOPMENT --
**:HISTORY:	13jun96-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifndef TOPCLASSES_HH
#define TOPCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "asuAlloc.h"
#include "asuLib.h"
#include "socLib.h"
#include "tdmLib.h"
#include "top_macros.h"
#include "top_types.h"

//:#####################################################################
//:=============================================== CLASS              ==
class topProject : public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   topProject();
   topProject(const char* name, const char* spec="-");
   virtual ~topProject();
//:----------------------------------------------- ATTRIBUTES         --
char* selectionSpecification();
void selectionSpecification(const char* spec);

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T project(tdmTable * table1, tdmTable *& table2);
tdmTable * pTarget(tdmTable * table1, const char * name);
STAFCV_T reset();

protected:
//:----------------------------------------------- PROT VARIABLES     --
char * mySelectSpec;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class topJoin : public virtual topProject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   topJoin();
   topJoin(const char* name, const char* spec="-"
		, const char* where="-");
   virtual ~topJoin();
//:----------------------------------------------- ATTRIBUTES         --
char* whereClause();
void whereClause(const char* clause);

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T join(tdmTable * table1, tdmTable * table2, tdmTable *& table3);
tdmTable * jTarget(tdmTable * table1, tdmTable * table2
		, const char * name);
STAFCV_T reset();

protected:
//:----------------------------------------------- PROT VARIABLES     --
char * myWhereClause;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class topFactory : public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   topFactory();
   topFactory(const char* name);
   virtual ~topFactory();
//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * list ();

//:- Project -----------------------
   virtual STAFCV_T deleteProject (const char * name);
   virtual STAFCV_T findProject (const char * name
                , topProject*& fileStream);
   virtual STAFCV_T getProject (IDREF_T id
                , topProject*& fileStream);
   virtual STAFCV_T newProject (const char * name
                , const char * spec);

//:- Join --------------------------
   virtual STAFCV_T deleteJoin (const char * name);
   virtual STAFCV_T findJoin (const char * name
                , topJoin*& fileStream);
   virtual STAFCV_T getJoin (IDREF_T id
                , topJoin*& fileStream);
   virtual STAFCV_T newJoin (const char * name
                , const char * spec, const char * clause);

protected:
//:----------------------------------------------- PROT VARIABLES     --
//:**NONE**
//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:---------------------------------------------------------------------
CORBA_TIE(topProject)
CORBA_TIE(topJoin)
CORBA_TIE(topFactory)

#endif /* TOPCLASSES_HH */

