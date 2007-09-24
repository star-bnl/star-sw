
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

#include "StTriggerSimu.h"

class StChain;
class StMuDst;
class StMuDstMaker;
class StEvent;
class StMuEvent;
class StBbcTriggerDetector;

class StBbcTriggerSimu : public StTriggerSimu {

 private:
 StMuDstMaker *muDstMaker;
 StMuDst *muDst;
 StMuEvent *muEvent;
 int BBCadcNum;
 static const int AdcTrigThresh=5;
 //int mMCflag; // set yo 0 for real data

 protected:

 public: 
  StBbcTriggerSimu();
  virtual ~StBbcTriggerSimu();
  void Init();
  void Clear();
  void Make();
  
  short isTrigger(int trigId) { return bbcTrig; }

  int Wbbc, Ebbc, bbcTrig, BBCadc[48];
  bool getEandW() {return bbcTrig;} // bbc trigger decision
  //void setMC(int x) {mMCflag=x;}

  ClassDef(StBbcTriggerSimu, 1)
};

#endif

//
// $Log: StBbcTriggerSimu.h,v $
// Revision 1.4  2007/09/24 18:08:30  kocolosk
// added inheritance from ABC clss StTriggerSimu
//
// Revision 1.3  2007/07/23 02:59:56  balewski
// cleanup, bbc for M-C still not working
//

