/***************************************************************************
 *
 * $Id: StDbTableIter.hh,v 1.1 2000/01/19 20:20:07 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Iterates of table-list in a given StDbConfigNode class
 *
 ***************************************************************************
 *
 * $Log: StDbTableIter.hh,v $
 * Revision 1.1  2000/01/19 20:20:07  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
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

class StDbTableIter {

  ListIter itr;
  StDbConfigNode* mnode;

public:

  StDbTableIter() : mnode(0){};
  StDbTableIter(StDbConfigNode* node){ init(node);};
  ~StDbTableIter(){};

  void init(StDbConfigNode* node);

  StDbTable* next();
  StDbTable* operator++();
  bool done();

  // will also want a method nextAndRelease() which does the
  // same as next() but removes the table from the StDbConfigNode

  //ClassDef(StDbTableIter,0)

};


#endif






