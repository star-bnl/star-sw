#ifndef STDBFACTORYI_HH
#define STDBFACTORYI_HH

#include "StDbDefs.hh"
#include "StDbTableComponent.h"

//class StDbTableComponent;

#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbTableComponent*, allocator<StDbTableComponent*> > TableList;
#else
typedef list<StDbTableComponent*> TableList;
#endif


class StDbFactoryI {

protected:

  bool isloaded;
  StDbDomain mdbDomain;
  StDbType mdbType;
  TableList mTableList;

public:

  StDbFactoryI() : isloaded(false) {};
  virtual ~StDbFactoryI(){};
  virtual StDbType getDbType() { return mdbType; }
  virtual StDbDomain getDbDomain() { return mdbDomain; }  
  virtual StDbTableComponent* getDbTable(const char* tableName, int option);
  
  virtual void initTableList() {};// = 0; // Here's all the work
  //  virtual void deleteTableList();

};

/*
void
StDbFactoryI::deleteTableList(){

  StDbTableComponent* table;
  TableList::iterator itr;

  do {
      for(itr = mTableList.begin(); itr != mTableList.end(); ++itr){
         table = *itr;
         mTableList.erase(itr);
         delete table;
         break;
        }
     } while( mTableList.begin() != mTableList.end() );
}
*/
#endif







