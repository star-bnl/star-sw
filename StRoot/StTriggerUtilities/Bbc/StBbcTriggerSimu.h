
#ifndef STAR_StBbcTriggerMaker
#define STAR_StBbcTriggerMaker

/*!
 *                                                                     
 * \class  StBbcTriggerSimu
 * \author rfatemi
 * \date   05/23/07
 * 
 * Emulates BBC trigger using only ADC and not TDC
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StChain;
class StMuDst;
class StMuDstMaker;
class StEvent;
class StMuEvent;
class StBbcTriggerDetector;
class StBbcTriggerSimu {

 private:
 StMuDstMaker *muDstMaker;
 StMuDst *muDst;
 StMuEvent *muEvent;
 int BBCadcNum;
 static const int AdcTrigThresh=5;
 protected:

 public: 
  StBbcTriggerSimu();
  virtual ~StBbcTriggerSimu();
  void Clear();
  void Make();

  int Wbbc, Ebbc, bbcTrig, BBCadc[48];

  ClassDef(StBbcTriggerSimu, 1)
};

#endif


