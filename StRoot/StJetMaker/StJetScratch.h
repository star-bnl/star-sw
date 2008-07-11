// -*- mode: c++;-*-
// $Id: StJetScratch.h,v 1.1 2008/07/11 23:32:18 tai Exp $
#ifndef STJETSCRATCH_HH
#define STJETSCRATCH_HH

#include "StMaker.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StJetTrgWriter;

class StMuDstMaker;
class StEmcTriggerMaker;

class StJetScratch : public StMaker {

public:

  StJetScratch(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker);
  virtual ~StJetScratch() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetScratch.h,v 1.1 2008/07/11 23:32:18 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:


  TDirectory* _file;

  StMuDstMaker* _uDstMaker;
  StEmcTriggerMaker* _emcTrigMaker;

  StJetTrgWriter* _minbWriter;
  StJetTrgWriter* _bht1Writer;
  StJetTrgWriter* _bht2Writer;
  StJetTrgWriter* _bjp1Writer;
  StJetTrgWriter* _bjp2Writer;

  ClassDef(StJetScratch, 0)

};

#endif // STJETSCRATCH_HH
