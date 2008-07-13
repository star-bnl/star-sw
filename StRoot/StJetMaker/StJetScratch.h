// -*- mode: c++;-*-
// $Id: StJetScratch.h,v 1.4 2008/07/13 05:36:39 tai Exp $
#ifndef STJETSCRATCH_HH
#define STJETSCRATCH_HH

#include "StMaker.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StJetTrgWriter;

class StMuDstMaker;

namespace StSpinJet {

}

class StJetScratch : public StMaker {

public:

  StJetScratch(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StJetScratch() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetScratch.h,v 1.4 2008/07/13 05:36:39 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;


  ClassDef(StJetScratch, 0)

};

#endif // STJETSCRATCH_HH
