/***************************************************************************
 *
 * $Id: TableIter.cc,v 1.6 1999/09/30 02:06:13 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Iterates of table-list in a given StDbConfigNode class
 *
 ***************************************************************************
 *
 * $Log: TableIter.cc,v $
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

StDbTableI*
TableIter::next(){
StDbTableI* ret = 0;
if(!done()) { 
 ret = *itr;
 char* tableName = (*itr)->getTableName();
 // cout << "Table [" << tableName <<"] is available" << endl;
 delete [] tableName;
 itr++;
}
return ret;
}

////////////////////////////////////////////////////////


StDbTableI*
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









