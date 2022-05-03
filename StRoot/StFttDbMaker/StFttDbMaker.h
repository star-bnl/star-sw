#ifndef STFTTDBMAKER_H
#define STFTTDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StFttDb;

class StFttDbMaker : public StMaker {
public: 
  StFttDbMaker(const char *name="fttDbMkr");
  virtual ~StFttDbMaker();
  virtual int  Init();
  int  InitRun(int runNumber);
  void Clear(Option_t *option);
  int  Make();
  void setDbAccess(int v){mDbAccess=v;}
 
private:
  StFttDb *mFttDb;
  int mDbAccess=1;

  ClassDef(StFttDbMaker,1)   //StAF chain virtual base class for Makers        
};

#endif