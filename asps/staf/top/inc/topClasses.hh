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

//- OVERRIDE VIRTUAL
virtual char * listing();

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
class topSort : public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   topSort();
   topSort(const char* name, const char* whichColumn);
   virtual ~topSort();
//:----------------------------------------------- ATTRIBUTES         --
char* whichColumn();
void  whichColumn(char *);

//- OVERRIDE VIRTUAL
// Craig virtual char * listing();

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T sort(tdmTable *table);
STAFCV_T SortTheTable(tdmTable *table);
float Value(int *errorFlag,DS_DATASET_T* dsPtr,int rownum,size_t colNum);
void     SwapRows(void *tableDataAddress,size_t rowsize,void *tmp,
    DS_DATASET_T* dsPtr,int row1,int row2);
void top_qsort(void *beginningOfData,size_t rowsize,void *temp,
    DS_DATASET_T* dsPtr,int left,int right,size_t colNum,int *errorFlag);

protected:
//:----------------------------------------------- PROT VARIABLES     --
char * myWhichColumn;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};
//:######################################################################
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

//- OVERRIDE VIRTUAL
virtual char * listing();

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T fastjoin(tdmTable * table1, tdmTable * table2, tdmTable *& table3);
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
class topCut : public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   topCut();
   topCut(const char* name, const char* spec);
   virtual ~topCut();
//:----------------------------------------------- ATTRIBUTES         --
char* cutFunction();

//- OVERRIDE VIRTUAL
virtual char * listing();

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T filter(tdmTable * table1, tdmTable * table2);
STAFCV_T cut(tdmTable * table1);

protected:
//:----------------------------------------------- PROT VARIABLES     --
char * myCutFunction;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
STAFCV_T DoCutTable(tdmTable *tbl,char *func,long *irig,
     long *percentPass);
STAFCV_T DoFilterTable(tdmTable *src,tdmTable *tgt,char *func
	, long *irig, long *percentPass);
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

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * list ();
   STAFCV_T topOperator(tdmTable*,char*,char*,char*);

//:- Project -----------------------
   virtual STAFCV_T deleteProject (const char * name);
   virtual STAFCV_T findProject (const char * name
                , topProject*& agent);
   virtual STAFCV_T getProject (IDREF_T id
                , topProject*& agent);
   virtual STAFCV_T newProject (const char * name
                , const char * spec);

//:- Join --------------------------
   virtual STAFCV_T deleteJoin (const char * name);
   virtual STAFCV_T findJoin (const char * name
                , topJoin*& agent);
   virtual STAFCV_T getJoin (IDREF_T id
                , topJoin*& agent);
   virtual STAFCV_T newJoin (const char * name
                , const char * spec, const char * clause);
//:-  Sort---------------------------
   virtual STAFCV_T newSort (const char * name , const char * whichColumn);
   virtual STAFCV_T findSort (const char * name , topSort*& agent);
//:-  Cut ---------------------------
   virtual STAFCV_T deleteCut (const char * name);
   virtual STAFCV_T findCut (const char * name
                , topCut*& agent);
   virtual STAFCV_T getCut (IDREF_T id
                , topCut*& agent);
   virtual STAFCV_T newCut (const char * name
                , const char * spec);
//:-  Mask --------------------------
/* NOT YET IMPLEMENTED
   virtual STAFCV_T deleteMask (const char * name);
   virtual STAFCV_T findMask (const char * name
                , topMask*& agent);
   virtual STAFCV_T getMask (IDREF_T id
                , topMask*& agent);
   virtual STAFCV_T newMask (const char * name
                , const char * spec);
*/

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
CORBA_TIE(topCut)
CORBA_TIE(topFactory)

#endif /* TOPCLASSES_HH */

/* This is a picture of Adolph:    //:-O   */
