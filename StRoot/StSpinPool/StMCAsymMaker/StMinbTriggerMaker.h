
#ifndef STAR_StMinbTriggerMaker
#define STAR_StMinbTriggerMaker

/*!
 *                                                                     
 * \class  StMinbTriggerMaker
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
class StEventInfo;
class StMinbTriggerMaker : public StMaker {

 private:
 StMuDstMaker *muDstMaker;
 StMuEvent *muEvent;
 int BBCadcNum;
 
 protected:

 public: 
  StMinbTriggerMaker(const char *name="BbcTrig");
  virtual       ~StMinbTriggerMaker();
  virtual Int_t Init();
  virtual Int_t  Make();

  int EVTid;
  int Wbbc, Ebbc, bbcTrig;
  int BBCadc[48];

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMinbTriggerMaker.h,v 1.1 2007/07/10 15:19:57 rfatemi Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StMinbTriggerMaker, 0)
};

#endif


