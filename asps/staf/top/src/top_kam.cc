/*Copyright 1996, Lawrence Berkeley National Laboratory
*:>--------------------------------------------------------------------
**:FILE:         top_kam.c
**:DESCRIPTION:  C++ KUIP Action Modules for TOP-Table OPerators
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      23oct96-v001a-cet- make it work
**:HISTORY:      13jun96-v000a-cet- creation
**:<-------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES            --*/
#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"

#include "emlLib.h"
#include "topLib.h"

/*-------------------------------------------- TYPEDEFS            --*/
/*-------------------------------------------- GLOBALS             --*/
/*-------------------------------------------- PROTOTYPES          --*/


/*-------------------------------------------------------------------*/
void kam_top_count_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */

        STAFCV_T status = top_count();
}
STAFCV_T top_count()
{
   printf("TOP:\tObject count = %d \n",top->count());
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_top_list_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */

        STAFCV_T status = top_list();
}
STAFCV_T top_list()
{
   printf("%s",top->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_top_newproject_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *select = ku_gets(); /* selection specification */

        STAFCV_T status = top_newproject(agent, select);
}
STAFCV_T top_newproject(char* agent, char* select)
{
   if( !top->newProject(agent, select) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_top_newjoin_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *select = ku_gets(); /* selection specification */
   char *where = ku_gets(); /* where clause */

        STAFCV_T status = top_newjoin(agent, select, where);
}
STAFCV_T top_newjoin(char* agent, char* select, char* where)
{
   if( !top->newJoin(agent, select, where) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_top_newcut_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of cut agent */
   char *func = ku_gets(); /* cut function */

        STAFCV_T status = top_newcut(agent, func);
}
STAFCV_T top_newcut(char* agent, char* func)
{
   if( !top->newCut(agent, func) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_top_newsort_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of sort agent */
   char *sfunc = ku_gets(); /* sort function */
        STAFCV_T status = top_newsort();
}
STAFCV_T top_newsort() {
EML_ERROR(NOT_YET_IMPLEMENTED);
}
 
/*-------------------------------------------------------------------*/
void kam_topproject_selectspec_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *select = ku_gets(); /* selection specification */

        STAFCV_T status = topproject_selectspec(agent, select);
}
STAFCV_T topproject_selectspec(char* agent, char* select)
{
   topProject* proj=NULL;
   if( !top->findProject(agent, proj) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   switch (select[0]) {
   case '-':
      printf("TOP:\tSelection Specification = (%s) \n"
		,s=proj->selectionSpecification());
      ASUFREE(s);
      break;
   default:
      proj->selectionSpecification(select);
      break;
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topproject_project_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */

        STAFCV_T status = topproject_project(agent, table1, table2
		, select);
}
STAFCV_T topproject_project(char* agent, char* table1, char* table2
	, char* select)
{
//- Find Mandatory Input Objects
   topProject* proj=NULL;
   if( !top->findProject(agent, proj) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   tdmTable* tbl1=NULL;
   if( !tdm->findTable(table1, tbl1) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
//- Handle Optional Selection Specification
   switch (select[0]) {
   case '-':
      break;
   default:
      proj->selectionSpecification(select);
      break;
   }
//- Find or Create Optional Output Object
   tdmTable* tbl2=NULL;
   if( !tdm->findTable(table2, tbl2) ){
      tbl2 = proj->pTarget(tbl1, table2);
   }
//- Project table
   if( !proj->project(tbl1,tbl2) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topproject_reset_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */

        STAFCV_T status = topproject_reset(agent);
}
STAFCV_T topproject_reset(char* agent)
{
//- Find Mandatory Input Objects
   topProject* proj=NULL;
   if( !top->findProject(agent, proj) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !proj->reset() ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_selectspec_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *select = ku_gets(); /* selection specification */

        STAFCV_T status = topjoin_selectspec(agent, select);
}
STAFCV_T topjoin_selectspec(char* agent, char* select)
{
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   switch (select[0]) {
   case '-':
      printf("TOP:\tSelection Specification = (%s) \n"
                ,s=join->selectionSpecification());
      ASUFREE(s);
      break;
   default:
      join->selectionSpecification(select);
      break;
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_whereclause_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *where = ku_gets(); /* where clause */

        STAFCV_T status = topjoin_whereclause(agent, where);
}
STAFCV_T topjoin_whereclause(char* agent, char* where)
{
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   switch (where[0]) {
   case '-':
      printf("TOP:\tWhere Clause = (%s) \n"
                ,s=join->whereClause());
      ASUFREE(s);
      break;
   default:
      join->whereClause(where);
      break;
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_join_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* second input table */
   char *table3 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */
   char *where = ku_gets(); /* where clause */

        STAFCV_T status = topjoin_join(agent, table1, table2, table3
		, select, where);
}
STAFCV_T topjoin_join(char* agent, char* table1, char* table2
	, char* table3, char* select, char* where)
{
//- Find Mandatory Input Objects
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   tdmTable* tbl1=NULL;
   if( !tdm->findTable(table1, tbl1) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   tdmTable* tbl2=NULL;
   if( !tdm->findTable(table2, tbl2) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
//- Handle Optional Selection Specification
   switch (select[0]) {
   case '-':
      break;
   default:
      join->selectionSpecification(select);
      break;
   }
//- Handle Optional Where Clause
   switch (where[0]) {
   case '-':
      break;
   default:
      join->whereClause(where);
      break;
   }
//- Find or Create Optional Output Object
   tdmTable* tbl3=NULL;
   if( !tdm->findTable(table3, tbl3) ){
      tbl3 = join->jTarget(tbl1, tbl2, table3);
   }
//- Join tables
   if( !join->join(tbl1,tbl2,tbl3) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_reset_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */

        STAFCV_T status = topjoin_reset(agent);
}
STAFCV_T topjoin_reset(char* agent)
{
//- Find Mandatory Input Objects
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !join->reset() ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_topcut_function_()
{
  char *agent = ku_gets();
  STAFCV_T status = topcut_function(agent);
}
STAFCV_T topcut_function(char* agent)
{
  topCut* cut=NULL;
  if( !top->findCut(agent, cut) ){
    printf("I can't find agent %s.\n",agent);
    EML_ERROR(OBJECT_NOT_FOUND);
  }
  printf("%s\n",cut->cutFunction());
}
/*-------------------------------------------------------------------*/
void kam_topcut_filter_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of cut agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* output table */
   char *func = ku_gets(); /* cut function */

        STAFCV_T status = topcut_filter(agent, table1, table2, func);
}
STAFCV_T topcut_filter(char* agent, char* table1, char* table2
	, char* func)
{
//- Find or create Mandatory Objects
   topCut* cut=NULL;
   if( !top->findCut(agent, cut) ){
      if(0 == strcmp(".",func))EML_ERROR(KAM_OBJECT_NOT_FOUND);
      if( !top->newCut(agent, func)
      ||  !top->findCut(agent,cut)
      ){
	 EML_ERROR(KAM_OBJECT_NOT_CREATED);
      }
   }
   tdmTable* tbl1=NULL;
   if( !tdm->findTable(table1, tbl1) ){
      printf("I can't find table '%s'.\n",table1);
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   char *spec1 = tbl1->typeSpecifier();
   tdmTable* tbl2=NULL;
   if( !tdm->newTable(table2,spec1,0) ) {
      if( !tdm->findTable(table2, tbl2) ){
         free(spec1);
         printf("Could not create new table '%s' with spec '%s'.\n",
         table2,spec1);
         EML_ERROR(KAM_CREATE_FAILED);
      } else {
         if( !tdm->findTable(table2,tbl2) ) {
	   free(spec1);
	   EML_ERROR(KAM_OBJECT_NOT_FOUND);
         }
      }
   }
   else {
      if( !tbl2->isType(spec1) ){
	 EML_ERROR(KAM_BAD_TABLE_TYPE);
      }
      tbl2->maxRowCount(0);
   }

//- filter table
   if( !cut->filter(tbl1,tbl2) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topcut_cut_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of cut agent */
   char *table1 = ku_gets(); /* first input table */
   char *func = ku_gets(); /* cut function */

  
        STAFCV_T status = topcut_cut(agent, table1, func);
}
STAFCV_T topcut_cut(char* agent, char* table1,
	char* func)
{
//- Find or create Mandatory Objects
   topCut* cut=NULL;
   if( !top->findCut(agent, cut) ){
      if(0 == strcmp(".",func))EML_ERROR(KAM_OBJECT_NOT_FOUND);
      if( !top->newCut(agent, func)
      ||  !top->findCut(agent,cut)
      ){
	 EML_ERROR(KAM_OBJECT_NOT_CREATED);
      }
   }
   tdmTable* tbl1=NULL;
   if( !tdm->findTable(table1, tbl1) ){
      printf("I can't find table '%s'.\n",table1);
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

//- cut table
   if( !cut->cut(tbl1) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
