#include "StDbFactories.hh"

StDbFactories* StDbFactories::mInstance=0;

StDbFactoryI*
StDbFactories::getFactory(StDbType type, StDbDomain domain){


StDbFactoryI* factory;

for(Factories::iterator itr = mfactories.begin();
    itr != mfactories.end(); ++itr){
  if( (*itr)->getDbType()==type && (*itr)->getDbDomain()==domain){
     factory = *itr;
     break;
  }
}

return factory;
}

void
StDbFactories::initList(){

mfactories.push_back(StDbTpcCalibFactory::Instance());
mfactories.push_back(StDbTpcCondFactory::Instance());
mfactories.push_back(StDbTpcGeomFactory::Instance());

}





