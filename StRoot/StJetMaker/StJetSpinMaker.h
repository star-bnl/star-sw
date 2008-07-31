// -*- mode: c++;-*-
// $Id: StJetSpinMaker.h,v 1.1 2008/07/31 22:47:26 tai Exp $
#ifndef STJETSPINMAKER_HH
#define STJETSPINMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StMuDstMaker;

class StJetSpinMaker : public StMaker {

public:

  StJetSpinMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StJetSpinMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetSpinMaker.h,v 1.1 2008/07/31 22:47:26 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;


  ClassDef(StJetSpinMaker, 0)

};

#endif // STJETSPINMAKER_HH
