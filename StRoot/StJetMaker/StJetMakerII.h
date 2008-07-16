// -*- mode: c++;-*-
// $Id: StJetMakerII.h,v 1.1 2008/07/16 21:53:20 tai Exp $
#ifndef STJETMAKERII_HH
#define STJETMAKERII_HH

#include "StMaker.h"

#include <vector>

class TTree;

class StJetPars;
class StJetTreeEntryMaker;
class StBET4pMakerImp;

namespace StSpinJet {

}

class StJetMakerII : public StMaker {

public:

  StJetMakerII(const Char_t *name, StJetTreeEntryMaker* entryMaker);
  virtual ~StJetMakerII();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMakerII.h,v 1.1 2008/07/16 21:53:20 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  StJetTreeEntryMaker* _entryMaker;
  StBET4pMakerImp* _dataSource;

  ClassDef(StJetMakerII, 0)

};

#endif // STJETMAKER_HH
