//\class StFgtBaseMaker
//\author Anselm Vossen (avossen@indiana.edu)
// :
//
// :
//

#ifndef STAR_StFgtBaseMaker_HH
#define STAR_StFgtBaseMaker_HH


#include <StMessMgr.h>
#include "StRoot/StChain/StMaker.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"


class StFgtBaseMaker : virtual public StMaker
{

 public:
  StFgtBaseMaker(const Char_t* makerName, const Char_t* detName="fgt");

 protected:
  StFgtEvent* mFgtEvent;
  
 private:      
  ClassDef(StFgtBaseMaker,1);

};


#endif
