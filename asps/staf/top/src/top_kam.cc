/*Copyright 1996, Lawrence Berkeley National Laboratory
*:>--------------------------------------------------------------------
**:FILE:         top_kam.c
**:DESCRIPTION:  C++ KUIP Action Modules for TOP-Table OPerators
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      17jan98-v003b-cet- SPEC,WHERE,CUTF,etc. READONLY
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
   char *s;
   printf("%s",s=top->list() );
   FREE(s);  /*fix memory leak -akio*/
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
      EML_FAILURE(METHOD_FAILURE); // done
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
      EML_FAILURE(METHOD_FAILURE);
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
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topsort_agent_sort_() {
  char *agent = ku_gets(); /* name of sort agent */
  char *whichTable = ku_gets(); /* which table to sort */
  STAFCV_T status = topsort_agent_sort(agent,whichTable);
}
STAFCV_T topsort_agent_sort(char *agent,char *whichTable) {
  topSort* sort=NULL;
  if(!top->findSort(agent,sort)) {
    EML_CONTEXT("ERROR: Did not find SOREF '%s'.\n",agent);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  tdmTable* tbl=NULL;
  if( NULL == (tbl = tdm->findTable(whichTable)) ){
    EML_CONTEXT("ERROR: Did not find table '%s'.\n",whichTable);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  if(!sort->sort(tbl)) EML_FAILURE(METHOD_FAILURE);
  EML_SUCCESS(STAFCV_OK);
}
void kam_top_newsort_()
{
  // long npars = ku_npar(); /* no. of KUIP param.s */
  char *agent = ku_gets(); /* name of sort agent */
  char *whichCol = ku_gets(); /* which column to sort on */
  STAFCV_T status = top_newsort(agent,whichCol);
}
STAFCV_T top_newsort(char *agent,char *whichCol) {
  if( !top->newSort(agent, whichCol) ){
     EML_FAILURE(METHOD_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topsort_agent_column_()
{
   // long npars = ku_npar();
   char *agent = ku_gets(); /* name of project agent */

   STAFCV_T status = topsort_agent_column(agent);
}
STAFCV_T topsort_agent_column(char* agent)
{
   topSort* sort=NULL;
   if( !top->findSort(agent, sort) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   printf("TOP:\tThis agent sorts on column '%s'\n",s=sort->whichColumn());
   FREE(s);
   EML_SUCCESS(STAFCV_OK);
}
void kam_topproject_agent_selectspec_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *select;/*17jan98 = ku_gets(); selection specification */

        STAFCV_T status = topproject_agent_selectspec(agent, select);
}
STAFCV_T topproject_agent_selectspec(char* agent, char* select)
{
   topProject* proj=NULL;
   if( !top->findProject(agent, proj) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   printf("TOP:\tSelection Specification = (%s) \n"
		,s=proj->selectionSpecification());
   FREE(s);
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topproject_agent_project_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */

        STAFCV_T status = topproject_agent_project(agent, table1, table2
		, select);
}
STAFCV_T topproject_agent_project(char* agent, char* table1, char* table2
	, char* select)
{
//- Find Mandatory Input Objects
   topProject* proj=NULL;
   if( !top->findProject(agent, proj) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   tdmTable* tbl1=NULL;
   if( NULL == (tbl1 = tdm->findTable(table1)) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",table1);
      EML_FAILURE(OBJECT_NOT_FOUND);
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
   if( NULL == (tbl2 = tdm->findTable(table2)) ){
      tbl2 = proj->pTarget(tbl1, table2);
   }
//- Project table
   if( !proj->project(tbl1,tbl2) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topproject_agent_reset_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */

        STAFCV_T status = topproject_agent_reset(agent);
}
STAFCV_T topproject_agent_reset(char* agent)
{
//- Find Mandatory Input Objects
   topProject* proj=NULL;
   if( !top->findProject(agent, proj) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   if( !proj->reset() ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_agent_selectspec_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *select;/*17jan98 = ku_gets(); selection specification */

        STAFCV_T status = topjoin_agent_selectspec(agent, select);
}
STAFCV_T topjoin_agent_selectspec(char* agent, char* select)
{
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   printf("TOP:\tSelection Specification = (%s) \n"
                ,s=join->selectionSpecification());
   FREE(s);
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_agent_whereclause_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *where;/*17jan98 = ku_gets(); where clause */

        STAFCV_T status = topjoin_agent_whereclause(agent, where);
}
STAFCV_T topjoin_agent_whereclause(char* agent, char* where)
{
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *s=NULL;
   printf("TOP:\tWhere Clause = (%s) \n"
                ,s=join->whereClause());
   FREE(s);
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_agent_fastjoin_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* second input table */
   char *table3 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */
   char *where = ku_gets(); /* where clause */

        STAFCV_T status = topjoin_agent_fastjoin(agent, table1, table2, table3
		, select, where);
}
STAFCV_T topjoin_agent_fastjoin(char* agent, char* table1, char* table2
	, char* table3, char* select, char* where)
{
//- Find Mandatory Input Objects
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   tdmTable* tbl1=NULL;
   if( NULL == (tbl1 = tdm->findTable(table1)) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",table1);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   tdmTable* tbl2=NULL;
   if( NULL == (tbl2 = tdm->findTable(table2)) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",table2);
      EML_FAILURE(OBJECT_NOT_FOUND);
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
   if( NULL == (tbl3 = tdm->findTable(table3)) ){
      tbl3 = join->jTarget(tbl1, tbl2, table3);
   }
//- Join tables
   if( !join->fastjoin(tbl1,tbl2,tbl3) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
/*-------------------------------------------------------------------*/
void kam_topjoin_agent_join_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* second input table */
   char *table3 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */
   char *where = ku_gets(); /* where clause */

        STAFCV_T status = topjoin_agent_join(agent, table1, table2, table3
		, select, where);
}
STAFCV_T topjoin_agent_join(char* agent, char* table1, char* table2
	, char* table3, char* select, char* where)
{
//- Find Mandatory Input Objects
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   tdmTable* tbl1=NULL;
   if( NULL == (tbl1 = tdm->findTable(table1)) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",table1);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   tdmTable* tbl2=NULL;
   if( NULL == (tbl2 = tdm->findTable(table2)) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",table2);
      EML_FAILURE(OBJECT_NOT_FOUND);
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
   if( NULL == (tbl3 = tdm->findTable(table3)) ){
      tbl3 = join->jTarget(tbl1, tbl2, table3);
   }
//- Join tables
   if( !join->join(tbl1,tbl2,tbl3) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topjoin_agent_reset_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */

        STAFCV_T status = topjoin_agent_reset(agent);
}
STAFCV_T topjoin_agent_reset(char* agent)
{
//- Find Mandatory Input Objects
   topJoin* join=NULL;
   if( !top->findJoin(agent, join) ){
      EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",agent);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   if( !join->reset() ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_topcut_agent_function_()
{
  char *agent = ku_gets();
  STAFCV_T status = topcut_agent_function(agent);
}
STAFCV_T topcut_agent_function(char* agent)
{
  topCut* cut=NULL;
  if( !top->findCut(agent, cut) ){
    EML_CONTEXT("ERROR: I can't find %s.\n",agent);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  char *s;
  printf("%s\n",s=cut->cutFunction());
  FREE(s); /*fix memory leak -akio*/
  EML_SUCCESS(STAFCV_OK);
}
/*-------------------------------------------------------------------*/
void kam_topcut_agent_filter_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of cut agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* output table */
   char *func = ku_gets(); /* cut function */

        STAFCV_T status = topcut_agent_filter(agent, table1, table2, func);
}
STAFCV_T topcut_agent_filter(char* agent, char* table1, char* table2
	, char* func)
{
//- Find or create Mandatory Objects
   topCut* cut=NULL;
   if( !top->findCut(agent, cut) ){
      if(0 == strcmp(".",func)) EML_FAILURE(OBJECT_NOT_FOUND);
      if( !top->newCut(agent, func)
      ||  !top->findCut(agent,cut)
      ){
	 EML_FAILURE(OBJECT_NOT_CREATED);
      }
   }
   tdmTable* tbl1=NULL;
   if( NULL == (tbl1 = tdm->findTable(table1)) ){
      EML_CONTEXT("ERROR: I can't find table '%s'.\n",table1);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *spec1 = tbl1->typeSpecifier();
   tdmTable* tbl2=NULL;
   if( !tdm->newTable(table2,spec1,0) ) {
      if( NULL == (tbl2 = tdm->findTable(table2)) ){
         FREE(spec1);
         EML_CONTEXT("ERROR: Couldn't create new table '%s' with spec '%s'.\n",
         table2,spec1);
         EML_FAILURE(CREATE_FAILED);
      } else {
         if( NULL == (tbl2 = tdm->findTable(table2)) ) {
	   FREE(spec1);
           EML_CONTEXT("ERROR: Are you sure you defined '%s'?\n",table2);
	   EML_FAILURE(OBJECT_NOT_FOUND);
         }
      }
   } else {
      tbl2 = tdm->findTable(table2);
      if( !tbl2->isType(spec1) ){
	 FREE(spec1);  /*fix memory leak -akio*/
         EML_CONTEXT("ERROR: Bad table type '%s'?\n",spec1);
	 EML_FAILURE(BAD_TABLE_TYPE);
      }
      FREE(spec1);
      tbl2->maxRowCount(0);
   }

//- filter table
   if( !cut->filter(tbl1,tbl2) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_topcut_agent_cut_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of cut agent */
   char *table1 = ku_gets(); /* first input table */
   char *func = ku_gets(); /* cut function */
  
        STAFCV_T status = topcut_agent_cut(agent, table1, func);
}
STAFCV_T topcut_agent_cut(char* agent, char* table1,
	char* func)
{
//- Find or create Mandatory Objects
   topCut* cut=NULL;
   if( !top->findCut(agent, cut) ){
      if(0 == strcmp(".",func)) EML_FAILURE(OBJECT_NOT_FOUND);
      if( !top->newCut(agent, func)
      ||  !top->findCut(agent,cut)
      ){
	 EML_FAILURE(OBJECT_NOT_CREATED);
      }
   }
   tdmTable* tbl1=NULL;
   if( NULL == (tbl1 = tdm->findTable(table1)) ){
      EML_CONTEXT("ERROR: I can't find table '%s'.\n",table1);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

//- cut table
   if( !cut->cut(tbl1) ){
      EML_FAILURE(METHOD_FAILURE)
   }
   EML_SUCCESS(STAFCV_OK);
}
