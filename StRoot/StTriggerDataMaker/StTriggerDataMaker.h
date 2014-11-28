#ifndef STAR_StTriggerDataMaker
#define STAR_StTriggerDataMaker
//#include "StMaker.h"
#include "StRtsTable.h"
#include "StRTSBaseMaker.h"

class TH1F;
class StTriggerData;
class StDAQReader;

// class definition
class StTriggerDataMaker : public StRTSBaseMaker {
public: 
  StTriggerDataMaker(const char *name="trgd");
  virtual ~StTriggerDataMaker(){};
  virtual Int_t Make();
  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  
  void setDebug(int v) {mDebug=v;}
  
protected:
  
private:
  int mDebug;

  ClassDef(StTriggerDataMaker, 0)   //StAF chain virtual base class for Makers
};
#endif
