#ifndef STDBTPCCONDFACTORY_HH
#define STDBTPCCONDFACTORY_HH

#include "StDbFactoryI.hh"


class StDbTpcCondFactory : public StDbFactoryI {


protected:

  StDbTpcCondFactory(){
    mdbDomain = Tpc;
    mdbType = Conditions;
  }

static StDbTpcCondFactory* mInstance;

public:

  static StDbTpcCondFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbTpcCondFactory;
    }
  return mInstance;
  }
  
  //  virtual StDbTableComponent* getDbTable(const char* name, int option);
  virtual void initTableList();

};

#endif




