#ifndef STDBFACTORIESI_HH
#define STDBFACTORIESI_HH

#include "StDbDefs.hh"
#include "StDbTable.h"
#include "StDbFactoryI.hh"


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbFactoryI*, allocator<StDbFactoryI*> > Factories;
#else
typedef list<StDbFactoryI*> Factories;
#endif


class StDbFactories {

protected:

  Factories mfactories;

  StDbFactories() { initList(); };
  void initList();
  static StDbFactories* mInstance;

public:

  static StDbFactories* Instance() {
    if(!mInstance){
      mInstance = new StDbFactories();
    }
  return mInstance;
  }

  StDbFactoryI* getFactory(StDbType type);
 
};


#endif






