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
void kam_top_count_(){kam_top_count();}
int kam_top_count()
{
   long npars = ku_npar(); /* no. of KUIP param.s */

   printf("TOP:\tObject count = %d \n",top->count());
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_top_list_(){kam_top_list();}
int kam_top_list()
{
   long npars = ku_npar(); /* no. of KUIP param.s */

   printf("%s",top->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_top_newproject_(){kam_top_newproject();}
int kam_top_newproject() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *select = ku_gets(); /* selection specification */

   if( !top->newProject(agent, select) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_top_newjoin_(){kam_top_newjoin();}
int kam_top_newjoin() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *select = ku_gets(); /* selection specification */
   char *where = ku_gets(); /* where clause */

   if( !top->newJoin(agent, select, where) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_top_newfilter_(){kam_top_newfilter();}
int kam_top_newfilter() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of mask agent */
   char *cfunc = ku_gets(); /* cut function */
EML_ERROR(NOT_YET_IMPLEMENTED);
}
 
/*-------------------------------------------------------------------*/
void kam_top_newsort_(){kam_top_newsort();}
int kam_top_newsort() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of sort agent */
   char *sfunc = ku_gets(); /* sort function */
EML_ERROR(NOT_YET_IMPLEMENTED);
}
 
/*-------------------------------------------------------------------*/
void kam_topproject_selectspec_(){kam_topproject_selectspec();}
int kam_topproject_selectspec() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *select = ku_gets(); /* selection specification */

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
void kam_topproject_project_(){kam_topproject_project();}
int kam_topproject_project() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */

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
void kam_topproject_reset_(){kam_topproject_reset();}
int kam_topproject_reset() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of project agent */

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
void kam_topjoin_selectspec_(){kam_topjoin_selectspec();}
int kam_topjoin_selectspec() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *select = ku_gets(); /* selection specification */

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
void kam_topjoin_whereclause_(){kam_topjoin_whereclause();}
int kam_topjoin_whereclause() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *where = ku_gets(); /* where clause */

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
void kam_topjoin_join_(){kam_topjoin_join();}
int kam_topjoin_join() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */
   char *table1 = ku_gets(); /* first input table */
   char *table2 = ku_gets(); /* second input table */
   char *table3 = ku_gets(); /* output table */
   char *select = ku_gets(); /* selection specification */
   char *where = ku_gets(); /* where clause */

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
void kam_topjoin_reset_(){kam_topjoin_reset();}
int kam_topjoin_reset() {
   long npars = ku_npar(); /* no. of KUIP param.s */
   char *agent = ku_gets(); /* name of join agent */

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

