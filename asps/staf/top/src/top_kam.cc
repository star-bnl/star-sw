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

#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"

#include "emlLib.h"
#include "topLib.h"

void 
kam_top_count_()
{
  top_count();
}

void 
kam_top_list_()
{
  top_list();
}

void 
kam_top_newproject_()
{
  char *agent = ku_gets(); /* name of project agent */
  char *select = ku_gets(); /* selection specification */

  top_newproject(agent, select);
}
 
void 
kam_top_newjoin_()
{
  char *agent = ku_gets(); /* name of join agent */
  char *select = ku_gets(); /* selection specification */
  char *where = ku_gets(); /* where clause */

  top_newjoin(agent, select, where);
}
 
void 
kam_top_newcut_()
{
  char *agent = ku_gets(); /* name of cut agent */
  char *func = ku_gets(); /* cut function */

  top_newcut(agent, func);
}
 
void 
kam_topsort_agent_sort_() {
  char *agent = ku_gets(); /* name of sort agent */
  char *whichTable = ku_gets(); /* which table to sort */

  topsort_agent_sort(agent,whichTable);
}

void 
kam_operate_() {
  char *table = ku_gets(); 
  char *column = ku_gets(); 
  char *operation = ku_gets(); 
  char *value = ku_gets(); 

  top_operator(table,column,operation,value);
}

void
kam_top_newsort_() {
  char *agent = ku_gets(); /* name of sort agent */
  char *whichCol = ku_gets(); /* which column to sort */

  top_newsort(agent,whichCol);
}  

void
kam_topsort_agent_column_() {
  char *agent = ku_gets(); /* name of sort agent */

  topsort_agent_column(agent);
}  

void 
kam_topproject_agent_selectspec_()
{
  char *agent = ku_gets(); /* name of project agent */
  char *select=NULL; /*17jan98 = ku_gets(); selection specification */
  
  topproject_agent_selectspec(agent, select);
}
 
void 
kam_topproject_agent_project_()
{
  char *agent = ku_gets(); /* name of project agent */
  char *table1 = ku_gets(); /* first input table */
  char *table2 = ku_gets(); /* output table */
  char *select = ku_gets(); /* selection specification */

  topproject_agent_project(agent, table1, table2, select);
}
 
void 
kam_topproject_agent_reset_()
{
  char *agent = ku_gets(); /* name of project agent */

  topproject_agent_reset(agent);
}
 
void 
kam_topjoin_agent_selectspec_()
{
  char *agent = ku_gets(); /* name of join agent */
  char *select=NULL; /*17jan98 = ku_gets(); selection specification */

  topjoin_agent_selectspec(agent, select);
}
 
void 
kam_topjoin_agent_whereclause_()
{
  char *agent = ku_gets(); /* name of join agent */
  char *where=NULL; /*17jan98 = ku_gets(); where clause */

  topjoin_agent_whereclause(agent, where);
}
 
void 
kam_topjoin_agent_fastjoin_()
{
  char *agent = ku_gets(); /* name of join agent */
  char *table1 = ku_gets(); /* first input table */
  char *table2 = ku_gets(); /* second input table */
  char *table3 = ku_gets(); /* output table */
  char *select = ku_gets(); /* selection specification */
  char *where = ku_gets(); /* where clause */

  topjoin_agent_fastjoin(agent, table1, table2, table3, select, where);
}

void 
kam_topjoin_agent_join_()
{
  char *agent = ku_gets(); /* name of join agent */
  char *table1 = ku_gets(); /* first input table */
  char *table2 = ku_gets(); /* second input table */
  char *table3 = ku_gets(); /* output table */
  char *select = ku_gets(); /* selection specification */
  char *where = ku_gets(); /* where clause */

  topjoin_agent_join(agent, table1, table2, table3, select, where);
}
 
void 
kam_topjoin_agent_reset_()
{
  char *agent = ku_gets(); /* name of join agent */

  topjoin_agent_reset(agent);
}

void 
kam_topcut_agent_function_()
{
  char *agent = ku_gets();

  topcut_agent_function(agent);
}

void 
kam_topcut_agent_filter_()
{
  char *agent = ku_gets(); /* name of cut agent */
  char *table1 = ku_gets(); /* first input table */
  char *table2 = ku_gets(); /* output table */
  char *func = ku_gets(); /* cut function */

  topcut_agent_filter(agent, table1, table2, func);
}
 
void 
kam_topcut_agent_cut_()
{
  char *agent = ku_gets(); /* name of cut agent */
  char *table1 = ku_gets(); /* first input table */
  char *func = ku_gets(); /* cut function */
  
  topcut_agent_cut(agent, table1, func);
}
