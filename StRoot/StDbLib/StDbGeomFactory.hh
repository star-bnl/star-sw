#ifndef STDBGeomFACTORY_HH
#define STDBGeomFACTORY_HH

#include "StDbFactoryI.hh"

class StDbGeomFactory : public StDbFactoryI {

protected:

  StDbGeomFactory(){ mdbType = Geometry; };
  void initIDList();

static StDbGeomFactory* mInstance;

public:

  static StDbGeomFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbGeomFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbGeomFactory(){ deleteIDList(); };

};

#endif









