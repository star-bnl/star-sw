#include "StDbFactories.hh"
#include "StDbCalibFactory.hh"
#include "StDbGeomFactory.hh"
#include "StDbCondFactory.hh"

StDbFactories* StDbFactories::mInstance=0;

StDbFactoryI*
StDbFactories::getFactory(StDbType type){


StDbFactoryI* factory = 0;

for(Factories::iterator itr = mfactories.begin();
    itr != mfactories.end(); ++itr){
  if( (*itr)->getDbType()==type){
     factory = *itr;
     break;
  }
}

return factory;
}

void
StDbFactories::initList(){

mfactories.push_back(StDbCalibFactory::Instance());
mfactories.push_back(StDbCondFactory::Instance());
mfactories.push_back(StDbGeomFactory::Instance());

}





