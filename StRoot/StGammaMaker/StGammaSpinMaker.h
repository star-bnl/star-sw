#ifndef __StGammaSpinMaker_h__
#define __StGammaSpinMaker_h__

#include "StMaker.h"
#include "TString.h"

class TFile;
class StGammaEvent;
class StSpinDbMaker;

class StGammaSpinMaker : public StMaker
{

 public:
  StGammaSpinMaker( const Char_t *name="gspmaker" );
  ~StGammaSpinMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");
  Int_t Finish();

 private:
 protected:

  StSpinDbMaker *mSpinDb;

  ClassDef(StGammaSpinMaker,1);

};

#endif
