#ifndef STDBTPCCALIBFACTORY_HH
#define STDBTPCCALIBFACTORY_HH

#include "StDbFactoryI.hh"


class StDbTpcCalibFactory : public StDbFactoryI {

protected:

  StDbTpcCalibFactory();
  //  {
  //  mdbDomain = Tpc;
  //  mdbType = Calibrations;
  // }

static StDbTpcCalibFactory* mInstance;

public:

  static StDbTpcCalibFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbTpcCalibFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbTpcCalibFactory(){};
  //StDbTableComponent* getDbTable(const char* tableName, int option);
  void initTableList();

};

#endif




