// -*- mode: c++;-*-
// $Id: StJetTriggerMaker.h,v 1.1 2008/07/11 23:32:20 tai Exp $
#ifndef STJETTRIGGERMAKER_HH
#define STJETTRIGGERMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StJetTrgWriter;

class StMuDstMaker;
class StEmcTriggerMaker;

class StJetTriggerMaker : public StMaker {

public:

  StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker);
  virtual ~StJetTriggerMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetTriggerMaker.h,v 1.1 2008/07/11 23:32:20 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:


  TDirectory* _file;

  StMuDstMaker* _uDstMaker;
  StEmcTriggerMaker* _emcTrigMaker;

  StJetTrgWriter* _minbWriter;
  StJetTrgWriter* _bht1Writer;
  StJetTrgWriter* _bht2Writer;
  StJetTrgWriter* _bjp1Writer;
  StJetTrgWriter* _bjp2Writer;

  ClassDef(StJetTriggerMaker, 0)

};

#endif // STJETTRIGGERMAKER_HH
