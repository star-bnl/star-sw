#ifndef STDBFACTORYI_HH
#define STDBFACTORYI_HH

#include "StDbDefs.hh"
#include "StDbTableID.h"
#include <fstream.h>

class StDbTable;

#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbTableID*, allocator<StDbTableID*> > IDList;
#else
typedef list<StDbTableID*> IDList;
#endif


class StDbFactoryI {

protected:

  bool isloaded;
  StDbType mdbType;
  IDList mIDList;

  virtual void initIDList(); // Here's all the work in derived classes

public:

  StDbFactoryI() : isloaded(false), mdbType(0) {};
  virtual ~StDbFactoryI(){ deleteIDList();};
  virtual StDbType getDbType() { return mdbType; };

  virtual int findTableID(const char* tableName);
  virtual StDbTable* getDbTable(const char* tableName, int option);
  virtual void deleteIDList();
  virtual void initIDList(ifstream& is);

};

inline void
StDbFactoryI::initIDList() {
 cerr<< "StDbFactoryI:: Has No default Tables in Base Class"<< endl;
}


#endif







