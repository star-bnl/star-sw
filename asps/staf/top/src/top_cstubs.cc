/*
 *:>--------------------------------------------------------------------
 **:FILE:         top_cstubs.cc
 **:DESCRIPTION:  C++ KUIP Action Modules for TOP-Table OPerators
 **:AUTHOR:       Dave Morrison
 **:BUGS:        
 **:<-------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>
#include "emlLib.h"
#include "topLib.h"

STAFCV_T 
top_count()
{
  printf("TOP:\tObject count = %ld \n",top->count());
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
top_list()
{
  char *herb980615;
  char *s;
  s=top->list();
  herb980615=strtok(s,"\n");
  while(herb980615) {
     printf("%s\n",herb980615);    // You can't write more than BUFSIZ at
     herb980615=strtok(NULL,"\n");
  }
  FREE(s);  /*fix memory leak -akio*/
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
top_newproject(char* agent, char* select)
{
  if( !top->newProject(agent, select) ){
    EML_FAILURE(METHOD_FAILURE); // done
  }
  EML_SUCCESS(STAFCV_OK);
}
 
STAFCV_T 
top_newjoin(char* agent, char* select, char* where)
{
  if( !top->newJoin(agent, select, where) ){
    EML_FAILURE(METHOD_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
top_newcut(char* agent, char* func)
{
  if( !top->newCut(agent, func) ){
    EML_FAILURE(METHOD_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}
 
STAFCV_T 
topsort_agent_sort(char *agent,char *whichTable) {
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

STAFCV_T 
top_operator(char *table,char *column,char *topOperator,char *value) {
  tdmTable* tbl=NULL;
  if( NULL == (tbl = tdm->findTable(table)) ){
    EML_CONTEXT("ERROR: Did not find table '%s'.\n",table);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  if(!top->topOperator(tbl,column,topOperator,value)) 
      EML_FAILURE(METHOD_FAILURE);
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
top_newsort(char *agent,char *whichCol) {
  if( !top->newSort(agent, whichCol) ){
    EML_FAILURE(METHOD_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}
 
STAFCV_T 
topsort_agent_column(char* agent)
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

STAFCV_T 
topproject_agent_selectspec(char* agent, char *select)
{
  // Just to hush pedantic compilers
  static void *ps = &select;

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
 
STAFCV_T 
topproject_agent_project(char* agent, char* table1, char* table2
			 , char* select)
{
  //- Find Mandatory Input Objects
  if(strstr(table2,"/")) {
    EML_CONTEXT("ERROR: No slashes, please:  '%s'.\n",table2);
    EML_FAILURE(SLASHES_NOT_SUPPORTED);
  }
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
 
STAFCV_T 
topproject_agent_reset(char* agent)
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
 
STAFCV_T 
topjoin_agent_selectspec(char* agent, char *select)
{
  // Just to hush pedantic compilers
  static void *ps = &select;

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
 
STAFCV_T 
topjoin_agent_whereclause(char* agent, char* where)
{
  // Just to hush pedantic compilers
  static void *pw = &where;

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
 
STAFCV_T 
topjoin_agent_fastjoin(char* agent, char* table1, char* table2
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

STAFCV_T 
topjoin_agent_join(char* agent, char* table1, char* table2
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
 
STAFCV_T 
topjoin_agent_reset(char* agent)
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

STAFCV_T 
topcut_agent_function(char* agent)
{
  topCut* cut=NULL;
  if( !top->findCut(agent, cut) ){
    EML_CONTEXT("ERROR: I can't find %s.\n",agent);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  char *s;
  s=cut->cutFunction();
  fputs(s,stdout); printf("\n");
  FREE(s); /*fix memory leak -akio*/
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
topcut_agent_filter(char* agent, char* table1, char* table2
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
 
STAFCV_T 
topcut_agent_cut(char* agent, char* table1, char* func)
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
