/***************************************************************************
 *
 * $Id: TableIter.hh,v 1.6 2000/01/10 20:37:55 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Iterates of table-list in a given StDbConfigNode class
 *
 ***************************************************************************
 *
 * $Log: TableIter.hh,v $
 * Revision 1.6  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.5  1999/09/30 02:06:13  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef TABLEITR_HH
#define TABLEITR_HH

#ifndef __CINT__
#include "StDbConfigNode.hh" // also includes StDbTable.h
typedef TableList::iterator ListIter;
#else
class ListIter;
#endif

#include "StDbTable.h"

class TableIter {

  ListIter itr;
  StDbConfigNode* mnode;

public:

  TableIter() : mnode(0){};
  TableIter(StDbConfigNode* node){ init(node);};
  ~TableIter(){};

  void init(StDbConfigNode* node);

  StDbTable* next();
  StDbTable* operator++();
  bool done();

  // will also want a method nextAndRelease() which does the
  // same as next() but removes the table from the StDbConfigNode

  //ClassDef(TableIter,0)

};


#endif






