#ifndef STDBCondFACTORY_HH
#define STDBCondFACTORY_HH

#include "StDbFactoryI.hh"

class StDbCondFactory : public StDbFactoryI {

protected:

  StDbCondFactory(){ mdbType = Conditions; };
  void initIDList();

static StDbCondFactory* mInstance;

public:

  static StDbCondFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbCondFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbCondFactory(){ deleteIDList(); };

};

#endif









