#include "TableIter.hh"
#include "StDbTableComponent.h"

void
TableIter::init(StDbConfigNode* node){
itr = node->mTables.begin();
// cout << " will try here first " << endl;
// cout << "Table [" << (*itr)->getTableName()<<"] is available" << endl;
}


StDbTableComponent*
TableIter::next(char*& name){
StDbTableComponent* ret = *itr;
name = (*itr)->getTableName();
 cout << "Table [" << (*itr)->getTableName()<<"] is available" << endl;
itr++;
// cout << " will try here " << endl;
// cout << (*itr)->getTableName() << endl;

return ret;
}


bool
TableIter::done(StDbConfigNode* node){

bool retVal = true;
if(itr != node->mTables.end())retVal = false;
return retVal;

}
