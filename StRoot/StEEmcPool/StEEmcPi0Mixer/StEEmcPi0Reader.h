#ifndef __StEEmcPi0Reader_h__
#define __StEEmcPi0Reader_h__

#include "StMaker.h"
#include "StEEmcMixEvent.h"

#include "TChain.h"

class StEEmcPi0Reader : public StMaker
{

 public:

  StEEmcPi0Reader(const Char_t *name="mRealTree");
  ~StEEmcPi0Reader(){ /* nada */ };

  void chainFile( const Char_t *name );

  StEEmcMixEvent *event(){ return mEvent; }

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  Long64_t getNumberOfEvents(){ return mChain->GetEntries(); }
  Int_t    getEvent(Int_t event);

 private:
 protected:

  Int_t index;
  
  TChain *mChain; /**< real events chain */
  StEEmcMixEvent *mEvent; /**< pi0 event */

  ClassDef(StEEmcPi0Reader,1);


};

#endif
