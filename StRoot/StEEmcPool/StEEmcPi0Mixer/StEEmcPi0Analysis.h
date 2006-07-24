#ifndef __StEEmcPi0Analysis_h__
#define __StEEmcPi0Analysis_h__

#include "StMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h" 
#include "StEvent/StTriggerId.h" 

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

#include "StEEmcMixEvent.h" 

class StEEmcMixMaker;
class StEEmcPointMaker;
class StRFEmcTrigMaker;
class TH1F;
class TH2F;
class TFile; 
class SpinCuts; 
class EEmcGeomSimple;
class StEEmcA2EMaker; 
class TTree; 

#include <vector>
#include "StEEmcPair.h"
#include "SpinHistos.h"
#include "TString.h" 

class StEEmcPi0Analysis : public StMaker
{

 public:
  /// constructor
  StEEmcPi0Analysis(const Char_t *name);
  /// destructor
  ~StEEmcPi0Analysis(){ /* nada */ };

  /// initializes the maker
  Int_t Init();
  /// processes a single event
  Int_t Make();
  /// init run
  Int_t InitRun(Int_t);
  /// clears the maker
  void  Clear(Option_t *opts="");
  /// specifies the name of the mixer and the mass range for 
  /// gated histograms.
  void mixer(const Char_t *name);
  /// specifies the name of the point maker
  void points(const Char_t *name);
  /// specifies the name of the mudst maker
  void mudst(const Char_t *name); 
  /// specifies the name of the analysis maker
  void analysis(const Char_t *name); 
  /// specifies the name of the spin db maker
  void spin(const Char_t *name);

  /// Add trigger ID to process
  void trigger(Int_t trig){ mTriggerList.push_back(trig); }

  /// Sets the appropriate minbias trig id
  void minbias(Int_t trig){ mMinBias = trig; }

  SpinCuts *cuts(){ return mCuts; } 

  void filename(const Char_t *fname){ mFilename=fname; } 

  void triggerSim( const Char_t *name, Int_t t ) { mTrigSim=(StRFEmcTrigMaker*)GetMaker(name); mTrigSimThreshold=t; }

 private:
 protected:

  /// Verify that the pi0 candidate is the only pair of reconstructed
  /// points matching the contiguous group of towers. 
  Bool_t twoBodyCut( StEEmcPair &p );

  /// Pointer to the pi0 mixer
  StEEmcMixMaker   *mEEmixer;  //!
  /// pointer to the point maker
  StEEmcPointMaker *mEEpoints; //!
  /// pointer to mudst
  StMuDstMaker *mMuDst; 
  /// pointer to analysis maker
  StEEmcA2EMaker *mEEanalysis; 
  /// pointer to the spin database
  StSpinDbMaker  *mSpinDb;

  std::vector<Int_t> mTriggerList;   /**<-- list of triggers to process */
  Int_t mMinBias;                    /**<-- id of the min bias trigger */

  enum SpinStates { kAny=0, kUpUp, kUpDn, kDnUp, kDnDn };

  /// Spin-sorted pi0 histograms
  SpinHistos *mHistograms[100];
  SpinHistos *mBackgrounds[100];
  SpinCuts   *mCuts; 
  TH1F       *hFillPatternI;    /**<-- intended fill pattern */
  TH1F       *hSpin4;           /**<-- number of times code sees spin state*/
  TH1F       *hBx7;             /**<--  7-bit bunch crossing */
  TH1F       *hBx48;            /**<-- 48-bit bunch crossing */
  TH2F       *hBx7diffBx48;     /**<--  7-bit bunch crossing - 48bit */
  TH1F       *hBxStar;          /**<-- star beam crossing */
  TH1F       *hBxStarPi0;       /**<-- gated on pi0 */ 

  TH2F       *hMassBx;          /**<-- mass vs bunch crossing */
  TH2F       *hZvertBx;         /**<-- z vertex (gated) vs bunch crossing */
  TH2F       *hZggBx;           /**<-- zgg vs bunch crossing */
  TH2F       *hEtaBx;           /**<-- eta vs bunch crossing */ 

  TH1F       *hSpin4mb;         /**<-- spin state w/ mb */ 


  enum EventCutTypes { 
      kEvent=0,
      kMinBias, 
      kTrig,
      kSoftTrig, 
      kSoftTrig36, 
      kVertex,
      kCandidate 
  }; 

  enum PairCutTypes {
      kPair=1, 
      kTwoBody,
      kAdcTow,
      kEtTow,
      kKinematics 
  }; 
      
  /// histogram to keep track of where events get cut 
  TH1F *hEventCounter; 

  /// histogram to keep track of where candidates get cut 
  TH1F *hPairCounter; 

  /// method to cut events
  Bool_t accept(StMuDst *mu); 
  Bool_t accept(StEEmcPair pair, Bool_t fill=true ); 

  /// method to retrieve 4bit spin state 
  Int_t getSpinState(StMuDst *mu, Int_t &bxs);

  TString mFilename;
  TFile *mFile; 

  /// Trigger simulation for MC 
  StRFEmcTrigMaker *mTrigSim;
  Int_t mTrigSimThreshold;

  Int_t mRunNumber;
  Bool_t mSpinSort;

  /// EEMC tower geometry
  EEmcGeomSimple *mEEgeom;

  StEEmcMixEvent *mRealEvent;
  StEEmcMixEvent *mMixedEvent; 

  TTree *mRealTree;
  TTree *mMixedTree; 

  /// Makes class availabel to root
  ClassDef(StEEmcPi0Analysis,1);  

};

#endif
