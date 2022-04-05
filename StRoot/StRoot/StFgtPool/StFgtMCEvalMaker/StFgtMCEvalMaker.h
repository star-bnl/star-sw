/////
#ifndef _ST_FGT_MCEVAL_MAKER_
#define _ST_FGT_MCEVAL_MAKER_

#include "StMaker.h"
class StFgtMCEvalMaker : public StMaker {
 public:
  StFgtMCEvalMaker(const Char_t* name="FgtMCEvalMaker");

  virtual ~StFgtMCEvalMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();


};

#endif
