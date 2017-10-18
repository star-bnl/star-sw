#ifndef __StEEmcMixTreeMaker_h__
#define __StEEmcMixTreeMaker_h__

#include "StEEmcMixMaker.h"
#include <TTree.h>
#include "StEEmcPair.h"
#include "StEEmcPoint.h"
#include "TObjArray.h"
#include "TString.h"
#include "TFile.h"
#include "StEEmcMixEvent.h"

class StEEmcMixTreeMaker : public StEEmcMixMaker {

 public:

  StEEmcMixTreeMaker( const Char_t *name );
  ~StEEmcMixTreeMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  void setFilename( const Char_t *f="out.root" ){ mFilename=f; };

  Int_t Finish();

 private:
 protected:

  TTree *mTree;
  StEEmcMixEvent *mMixEvent;

  TString mFilename;
  TFile *mFile;

  ClassDef(StEEmcMixTreeMaker,1);

};

#endif
