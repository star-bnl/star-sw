// -*- mode: c++;-*-
// $Id: StjSpinMaker.h,v 1.2 2008/08/02 19:22:27 tai Exp $
#ifndef STJETSPINMAKER_HH
#define STJETSPINMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StMuDstMaker;

class StjSpinMaker : public StMaker {

public:

  StjSpinMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StjSpinMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjSpinMaker.h,v 1.2 2008/08/02 19:22:27 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;


  ClassDef(StjSpinMaker, 0)

};

#endif // STJETSPINMAKER_HH
