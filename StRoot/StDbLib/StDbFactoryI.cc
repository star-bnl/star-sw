#include "StDbFactoryI.hh"



StDbTableComponent*
StDbFactoryI::getDbTable(const char* tableName, int option){

if(!isloaded)initTableList();


StDbTableComponent* table = 0;

for(TableList::iterator itr = mTableList.begin();
    itr != mTableList.end(); ++itr){
  if(!(*itr)) cout << "Look out " << endl;
  if(strcmp(tableName,(*itr)->getTableName())==0){
     table = (*itr)->duplicate();
     break;
  }
}

return table;
}
