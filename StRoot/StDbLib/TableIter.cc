/***************************************************************************
 *
 * $Id: TableIter.cc,v 1.7 2000/01/10 20:37:55 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Iterates of table-list in a given StDbConfigNode class
 *
 ***************************************************************************
 *
 * $Log: TableIter.cc,v $
 * Revision 1.7  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.6  1999/09/30 02:06:13  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "TableIter.hh"
#include "StDbTable.h"


////////////////////////////////////////////////////////

void
TableIter::init(StDbConfigNode* node){
mnode=node; itr = mnode->mTables.begin();
}

////////////////////////////////////////////////////////

StDbTable*
TableIter::next(){
StDbTable* ret = 0;
if(!done()) { 
 ret = *itr;
 char* tableName = (*itr)->getName();
 // cout << "Table [" << tableName <<"] is available" << endl;
 delete [] tableName;
 itr++;
}
return ret;
}

////////////////////////////////////////////////////////


StDbTable*
TableIter::operator++(){
return next();
}

////////////////////////////////////////////////////////

bool
TableIter::done(){
bool retVal = true;
if(itr != mnode->mTables.end())retVal = false;
return retVal;
}










