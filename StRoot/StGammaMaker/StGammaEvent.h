#ifndef __StGammaEvent_h__
#define __StGammaEvent_h__

#include "TObject.h"
#include "TVector3.h"

#include "StGammaCandidate.h"
//#include "StGammaIsolation.h"
//#include "StGammaDistribution.h"
//#include "StGammaFit.h"

#include "StGammaTrack.h"
#include "StGammaTower.h"
#include "StGammaStrip.h"

#include "TClonesArray.h"

class StMuTrack;

#define TPC_VERTEX 0x0001

class StGammaEvent : public TObject {
  
 public:
  StGammaEvent();
  ~StGammaEvent(){ /* nada */ };

  void Clear(Option_t *opts="");
  UShort_t mFlags;  /// Event flags (see above)

  StGammaTrack *newTrack( StMuTrack *mutr=0 );
  StGammaTower *newTower();
  StGammaTower *newPreshower1();
  StGammaTower *newPreshower2();
  StGammaTower *newPostshower();
  StGammaStrip *newStrip();
  StGammaCandidate *newCandidate();

  Int_t numberOfTracks(){ return nTracks; }
  Int_t numberOfTowers(){ return nTowers; }
  Int_t numberOfPreshower1(){ return nPreshower1; }
  Int_t numberOfPreshower2(){ return nPreshower2; }
  Int_t numberOfPostshower(){ return nPostshower; }
  Int_t numberOfStrips(){ return nStrips; }
  Int_t numberOfCandidates(){ return nCandidates; }

  StGammaTrack *track( Int_t i ){ return (StGammaTrack*)mTracks->At(i); }
  StGammaTower *tower( Int_t i ){ return (StGammaTower*)mTowers->At(i); }
  StGammaTower *preshower1( Int_t i ){ return (StGammaTower*)mPreshower1->At(i); }
  StGammaTower *preshower2( Int_t i ){ return (StGammaTower*)mPreshower2->At(i); }
  StGammaTower *postshower( Int_t i ){ return (StGammaTower*)mPostshower->At(i); }
  StGammaStrip *strip( Int_t i ){ return (StGammaStrip*)mStrips->At(i); }
  StGammaCandidate *candidate( Int_t i ){ return (StGammaCandidate*)mCandidates->At(i); }
  
  //                                                     Run, event and vertex
 protected:
  Int_t mRunNumber;   /// Run number
  Int_t mEventNumber; /// Event number  
  TVector3 mVertex; /// Event vertex (TPC)


 public:
  void SetRunNumber( Int_t run ){ mRunNumber=run; }
  void SetEventNumber( Int_t event ){ mEventNumber=event; }
  void SetVertex( TVector3 vertex ){ mVertex=vertex; }

  Int_t runNumber(){ return mRunNumber; }
  Int_t eventNumber(){ return mEventNumber; }
  TVector3 vertex(){ return mVertex; }

  
 private:
  //                                                 Towers, tracks and strips
  Int_t InitArrays();

  Int_t nTracks;
  Int_t nTowers;
  Int_t nPreshower1;
  Int_t nPreshower2;
  Int_t nPostshower;
  Int_t nStrips;
  Int_t nCandidates;

  TClonesArray *mTracks;    //[nTracks]
  TClonesArray *mTowers;    //[nTowers]
  TClonesArray *mPreshower1;//[nPreshower1]
  TClonesArray *mPreshower2;//[nPreshower2]
  TClonesArray *mPostshower;//[nPostshower]
  TClonesArray *mStrips;    //[nStrips]
  TClonesArray *mCandidates;//[nCandidates]
 
  ClassDef(StGammaEvent,1);

};

#endif
