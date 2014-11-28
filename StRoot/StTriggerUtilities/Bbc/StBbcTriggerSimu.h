// -*- mode:c++ -*-

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

#include "StTriggerUtilities/StVirtualTriggerSimu.h"

class StMuDst;
class StEvent;
class StBbcTriggerDetector;

class StBbcTriggerSimu : public StVirtualTriggerSimu {

 private:
 void Make(StMuDst*);
 void Make(StEvent*);
  void Make(StBbcTriggerDetector&);

 int BBCadcNum;
 static const int AdcTrigThresh=5;
 //int mMCflag; // set yo 0 for real data

 protected:

 public:
  StBbcTriggerSimu();
  virtual ~StBbcTriggerSimu();
  void Init();
  void InitRun(int runnumber){}
  void Clear();
  void Make();

  StTriggerSimuDecision triggerDecision(int trigId) { return bbcTrig; }

  int Wbbc, Ebbc, BBCadc[48];
  StTriggerSimuDecision bbcTrig;
  bool getEandW() {return (Wbbc && Ebbc);} // bbc trigger decision
  //void setMC(int x) {mMCflag=x;}

  ClassDef(StBbcTriggerSimu, 1)
};

#endif

//
// $Log: StBbcTriggerSimu.h,v $
// Revision 1.8  2009/12/22 18:11:01  pibero
// Added ability to set input source (MuDst or StEvent) for BBC trigger simulator.
//
// Revision 1.7  2007/11/08 20:59:43  kocolosk
// subdet isTrigger returns a bool
// triggerDecision returns enumerator including kDoNotCare
//
// Revision 1.6  2007/10/22 23:09:51  balewski
// split L2 to generic and year specific, not finished
//
// Revision 1.5  2007/10/12 17:12:38  kocolosk
// rename ABC class for subdetector trigger simulators
// StTriggerSimu => StVirtualTriggerSimu
//
// Revision 1.4  2007/09/24 18:08:30  kocolosk
// added inheritance from ABC clss StTriggerSimu
//
// Revision 1.3  2007/07/23 02:59:56  balewski
// cleanup, bbc for M-C still not working
//

