#ifndef __StEEmcIUPi0Analysis_h__
#define __StEEmcIUPi0Analysis_h__

#include "StMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h" 
#include "StEvent/StTriggerId.h" 

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

#include "StEEmcIUMixEvent.h" 
//#include "StEEmcMcReadMaker/StMcOutputMaker.h"

class StEEmcIUMixMaker;
class StEEmcIUPointMaker;
class StRFEmcTrigMaker;
class TH1F;
class TH2F;
class TH2D;
class TFile; 
class SpinCutsIU; 
class EEmcGeomSimple;
class StEEmcA2EMaker; 
class TTree; 
//class StMcOutputMaker;
#include <vector>
#include "StEEmcIUPair.h"
#include "SpinIUHistos.h"
#include "TString.h" 

class StEEmcIUPi0Analysis : public StMaker
{

 public:
  /// constructor
  StEEmcIUPi0Analysis(const Char_t *name);
  /// destructor
  ~StEEmcIUPi0Analysis(){ /* nada */ };

  /// initializes the maker
  Int_t Init();
  /// processes a single event
  Int_t Make();
  /// init run
  Int_t InitRun(Int_t);

  Int_t numbofcutCandidate();
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

  SpinCutsIU *cuts(){ return mCuts; } 

  void filename(const Char_t *fname){ mFilename=fname; } 

  void triggerSim( const Char_t *name, Int_t t ) { mTrigSim=(StRFEmcTrigMaker*)GetMaker(name); mTrigSimThreshold=t; }

 private:
 protected:

  /// Verify that the pi0 candidate is the only pair of reconstructed
  /// points matching the contiguous group of towers. 
  Bool_t twoBodyCut( StEEmcIUPair &p );

  /// Pointer to the pi0 mixer
  StEEmcIUMixMaker   *mEEmixer;  //!
  /// pointer to the point maker
  StEEmcIUPointMaker *mEEpoints; //!
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
  SpinIUHistos *mHistograms[5];
  SpinIUHistos *mBackgrounds[5];
  SpinCutsIU   *mCuts; 
  TH1F       *hFillPatternI;    /**<-- intended fill pattern */
  TH1F       *hSpin4;           /**<-- number of times code sees spin state*/
  TH1F       *hBx7;             /**<--  7-bit bunch crossing */
  TH1F       *hBx48;            /**<-- 48-bit bunch crossing */
  TH2F       *hBx7diffBx48;     /**<--  7-bit bunch crossing - 48bit */
  TH1F       *hBxStar;          /**<-- star beam crossing */
  TH1F       *hBxStarPi0;       /**<-- gated on pi0 */ 
  TH1F       *hPi0Mass;
  TH1F       *hPTvsPiNum;       // pi0# vs pt
  TH1F       *hMcEta; 
  TH1F       *hMcPhi; 
  TH1F       *hMcPt;
  TH2F       *hREtaPhi;
  TH1F       *hReconEta;
  TH1F       *hReconPhi; 
  TH1F       *hEEmcEta;
  TH1F       *hResoEta;
  TH1F       *hResoPhi;
  TH1F       *hResoPt;
  TH2F       *hResoPtvsG;
  TH1F       *hMcEnergy;
  TH1F       *hResoEnergy;
  TH2F       *hResoEnergyvsG;
  TH1F       *hRecoEnergy; 
  TH1F       *hZvert;   
  TH2F       *hMassBx;          /**<-- mass vs bunch crossing */
  TH2F       *hZvertBx;         /**<-- z vertex (gated) vs bunch crossing */

  TH2F       *hZggBx;           /**<-- zgg vs bunch crossing */
  TH2F       *hEtaBx;           /**<-- eta vs bunch crossing */ 

  TH1F       *hSpin4mb;         /**<-- spin state w/ mb */ 
  TH1F       *hRealMcPD;
  TH1F       *hRealMcPR;
  TH2F       *McEvsMass;
  TH2F       *DEtaMass;
  TH2F       *DPhiMass;

  TH2D       *dUvsdV;
  TH2D       *dUvsdVGood;
  TH1F       *dUVzeroE;
  TH1F       *dUVzeroEta;
  TH2F       *dUvsRatio;
  TH2F       *dUvsRatioGood;
  TH2F       *dVvsRatio;
  TH2F       *dVvsRatioGood;

  TH2F       *GoodPiGeo;
  TH1F       *GoodPiPt;
  TH1F       *EventCounter;
  TH1F       *hPi0Counter;
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
  Bool_t accept(StEEmcIUPair pair, Bool_t fill=true ); 

  /// method to retrieve 4bit spin state 
  Int_t getSpinState(StMuDst *mu, Int_t &bxs);

  TString mFilename;
  TFile *mFile; 

  /// Trigger simulation for MC 
  StRFEmcTrigMaker *mTrigSim;
  Int_t mTrigSimThreshold;

  Int_t mRunNumber;
  Bool_t mSpinSort;
  Int_t numcutCan;
  Int_t numoacceptedpair;
  /// EEMC tower geometry
  EEmcGeomSimple *mEEgeom;

  StEEmcIUMixEvent *mRealEvent;
  StEEmcIUMixEvent *mMixedEvent; 


  TTree *mRealTree;
  TTree *mMixedTree; 

  /// Makes class availabel to root
  ClassDef(StEEmcIUPi0Analysis,1);  

};
inline Int_t StEEmcIUPi0Analysis::numbofcutCandidate(){return (Int_t) numcutCan;}
#endif
