#ifndef ST_FMS_BITCHECK_MAKER_H
#define ST_FMS_BITCHECK_MAKER_H

#include "StMaker.h"

class StFmsBitCheckMaker : public StMaker {
public:
  StFmsBitCheckMaker(const char* name = "bitcheck");

  void Clear(Option_t* option = "");
  int Init();
  int InitRun(int runNumber);
  int Make();
  int Finish();

private:

  ClassDef(StFmsBitCheckMaker,0);
};

#endif 

