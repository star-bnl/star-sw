#ifndef STDBRunParamsFACTORY_HH
#define STDBRunParamsFACTORY_HH

#include "StDbFactoryI.hh"

class StDbRunParamsFactory : public StDbFactoryI {

protected:

  StDbRunParamsFactory(){ mdbType = RunParams; };
  void initIDList();

static StDbRunParamsFactory* mInstance;

public:

  static StDbRunParamsFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbRunParamsFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbRunParamsFactory(){ deleteIDList(); };

};

#endif









