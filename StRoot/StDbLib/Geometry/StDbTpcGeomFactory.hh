#ifndef STDBTPCGEOMFACTORY_HH
#define STDBTPCGEOMFACTORY_HH

#include "StDbFactoryI.hh"


class StDbTpcGeomFactory : public StDbFactoryI {

protected:

  StDbTpcGeomFactory() {
    mdbDomain = Tpc;
    mdbType = Geometry;
  }

static StDbTpcGeomFactory* mInstance;

public:

  static StDbTpcGeomFactory* Instance(){
    if(!mInstance)mInstance = new StDbTpcGeomFactory();
  return mInstance;
  };

  ~StDbTpcGeomFactory(){};//{deleteTableList();};
  //  StDbTableComponent* getDbTable(const char* name, int option);
  void initTableList();

};

#endif








