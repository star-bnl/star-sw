#ifndef __StEEmcPointTreeMaker_h__
#define __StEEmcPointTreeMaker_h__

#include "StEEmcPointMaker.h"
#include <TTree.h>
#include "StEEmcMixEvent.h"
#include <TString.h>
#include <TFile.h>

class StEEmcPointTreeMaker : public StEEmcPointMaker {

 public: 
  
  StEEmcPointTreeMaker( const Char_t *name );
  ~StEEmcPointTreeMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  
  void  Clear(Option_t *opts="");

  void setFilename( const Char_t *f="out.root" ){ mFilename=f; }

  Int_t Finish();

 private:
 protected:

  TTree *mTree;
  StEEmcMixEvent *mMixEvent;
  
  TString mFilename;
  TFile *mFile;

  ClassDef(StEEmcPointTreeMaker,1);

};

#endif
