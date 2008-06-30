#ifndef __StGammaSpinMaker_h__
#define __StGammaSpinMaker_h__

#include "StMaker.h"
#include "TString.h"

class TFile;
class StGammaEvent;
class StSpinDbMaker;

#include <map>

class StGammaSpinMaker : public StMaker
{

 public:
  StGammaSpinMaker( const Char_t *name="gspmaker" );
  ~StGammaSpinMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");
  Int_t Finish();

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGammaSpinMaker.h,v 1.2 2008/06/30 14:58:43 jwebb Exp $ built "__DATE__" "__TIME__; return cvs;}


 private:
 protected:

  StSpinDbMaker *mSpinDb;

  ClassDef(StGammaSpinMaker,1);

};

#endif
