#ifndef STDBCalIBFACTORY_HH
#define STDBCalIBFACTORY_HH

#include "StDbFactoryI.hh"

class StDbCalibFactory : public StDbFactoryI {

protected:

  StDbCalibFactory(){ mdbType = Calibrations; };
  void initIDList();

static StDbCalibFactory* mInstance;

public:

  static StDbCalibFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbCalibFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbCalibFactory(){ deleteIDList(); };

};

#endif









