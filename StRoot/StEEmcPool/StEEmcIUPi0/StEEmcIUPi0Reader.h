#ifndef __StEEmcIUPi0Reader_h__
#define __StEEmcIUPi0Reader_h__

#include "StMaker.h"
#include "StEEmcIUMixEvent.h"

#include "TChain.h"

class StEEmcIUPi0Reader : public StMaker
{

 public:

  StEEmcIUPi0Reader(const Char_t *name="mRealTree");
  ~StEEmcIUPi0Reader(){ /* nada */ };

  void chainFile( const Char_t *name );

  StEEmcIUMixEvent *event(){ return mEvent; }

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  Long64_t getNumberOfEvents(){ return mChain->GetEntries(); }
  Int_t    getEvent(Int_t event);

 private:
 protected:

  Int_t index;
  
  TChain *mChain; /**< real events chain */
  StEEmcIUMixEvent *mEvent; /**< pi0 event */

  ClassDef(StEEmcIUPi0Reader,1);


};

#endif
