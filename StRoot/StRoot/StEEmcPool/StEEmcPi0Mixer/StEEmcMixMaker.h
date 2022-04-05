#ifndef __StEEmcMixMaker_h__
#define __StEEmcMixMaker_h__

#include <StMaker.h>
#include <vector>
#include <algorithm>
#include "TString.h"

class TH1F;
class TH2F;

#include "StEEmcPool/StEEmcPointMaker/StEEmcPoint.h"
#include "StEEmcPair.h"

class TRandom;
class StEEmcPointMaker;
class StMuDstMaker;
class StEEmcA2EMaker;
class StMuEvent;
class EEmcGeomSimple;

class StEEmcMixMaker : public StMaker {

 public:

  /// Default constructor
  /// \param name Name of the maker, passed to StMaker constructor
  /// \param size Size of the mixed-event pool 
  StEEmcMixMaker( const Char_t *name, Int_t size=20 );
  ~StEEmcMixMaker(){ /* nada */ };

  /// sets the name of the muDst maker
  void mudst( const Char_t *name );
  /// sets the name of the point maker
  void points( const Char_t *name );
  /// sets the name of the adc-->energy maker
  void analysis( const Char_t *name );

  /// returns the number of candidates
  Int_t numberOfCandidates();
  /// returns the number of mixed-background candidates
  Int_t numberOfMixedCandidates(); 

  /// Return a specified candidate pair
  StEEmcPair candidate(Int_t c){ return mCandidates[c]; }
  /// Returns the specified mixed candidate pair
  StEEmcPair mixedCandidate(Int_t m) { return mBackground[m]; }

  /// Initialize
  Int_t Init();
  /// Process
  Int_t Make();
  /// Clear
  void  Clear( Option_t *opts="" );

  /// add a sector to the list of sectors to process
  /// \param sector An EEmc sector, [0,11]
  void sector( Int_t sector );
  /// add a trigger to the list of triggers to process
  /// \param trigger An offline trigger ID
  void trigger( Int_t trigger );

  /// set minimum ET for pair of points
  void minET( Float_t et );
  /// set maximum Zgg for pair of points
  void maxZ( Float_t z );
  /// minimum energy for a given point
  void minEpoint( Float_t m );


  /// Sets the trigger mode for combinatoric background mixing.
  /// Points will be saved from event-to-event in a "pool".  By
  /// default, any or all points in an event can get into the pool
  /// (default=0, minbias).  If high-tower is selected, we do not
  /// allow points beneath the highest tower in the event (the 
  /// triggered tower) to enter the pool.  If jet-patch is selected...
  /// well, we don't know how to handle this yet so it behaves
  /// like minbias.
  /// \param m: 0=minbias, 1=high-tower, 2=jet patch
  void setTrigMode(Int_t m){ mTrigMode=m; }


  /// create 1d and 2d histograms
  void book();
  /// fill 1d and 2d histograms
  void fill();
  /// Fill pairwise distributions
  void fill ( std::vector<TH1F *> &h, StEEmcPair pair ); 
  /// Fill qa distributions
  void fillQA( std::vector<TH2F *> &h, StEEmcPair pair ); 
  /// Mass range for qa histograms
  void range( Float_t min, Float_t max ); 

  /// Fix vertex for simple MC
  void fixedVertex(Float_t x, Float_t y, Float_t z){ mFixedVertex=TVector3(x,y,z); }
  /// Smear vertex
  void floatVertex(Float_t x, Float_t y, Float_t z, Float_t s){ fixedVertex(x,y,z); mSigmaVertex=s; }

  /// By default, only points in the same sector are mixed toghether.
  /// Setting mMixLimits>0 allow points to be mixed which are mMixLimits
  /// sectors away.
  void mixLimit(Int_t m){ mMixLimits=m; }

 private:
 protected:

  TVector3 mFixedVertex; /**<-- fixed vertex */
  Float_t  mSigmaVertex; /**<-- smear fixed vertex */

  Float_t mETcut;  /**<-- min ET */
  Float_t mZcut;   /**<-- min Z */
  Float_t mEpoint; /**<-- min epoint */

  Int_t mTrigMode; /**<-- trigger mode for mixing */
  void fillPool(); /**<-- fills mixed event pool */

  Int_t mMixLimits; /**<-- Allow mixing of points mMixLimits sectors away */

  std::vector<Int_t> mSectorList;    /**<-- list of sectors to process */
  std::vector<Int_t> mTriggerList;   /**<-- list of triggers to process */

  /// 1D histos
  std::vector<TH1F*> mH1; //!
  /// 2D histos
  std::vector<TH2F*> mH2; //!
  /// 1D real histos
  std::vector<TH1F*> mH1real; //!
  /// 2D real histos
  std::vector<TH2F*> mH2real; //!
  /// 1D mixed histos
  std::vector<TH1F*> mH1mix; //!
  /// 2D mixed histos
  std::vector<TH2F*> mH2mix; //!
  /// Pointer to MuDst
  StMuDstMaker     *mMuDstMaker; //!
  /// Pointer to points
  StEEmcPointMaker *mEEpoints;   //!
  /// Pointer to ADC 2 energy
  StEEmcA2EMaker   *mEEanalysis; //!
  /// MuDst name
  TString mMuDstMakerName;
  /// Point maker name
  TString mPointMakerName;
  /// Analaysis name
  TString mAnalysisName;
  /// Pointer to tower geom
  EEmcGeomSimple *mEEmcTow;
  
  /// Vector of points to mix into X-->gamma gamma
  StEEmcPointVec_t mPoints;


  /// Pool of points saved from event to event for background mixing.
  /// We rank each event by the adc response of the high tower, in
  /// bins 100 adc counts wide.  
  std::vector< StEEmcPointVec_t > mPool;

  /// Point pairs mixed on each event
  StEEmcPairVec_t mCandidates;
  /// Background pairs mixed on each event
  StEEmcPairVec_t mBackground;


  /// Accept or reject this event (trigger, qa, etc...)
  Bool_t accept(StMuEvent *);

  /// Accept or reject point pairs
  Bool_t accept( StEEmcPoint &p1, StEEmcPoint &p2 );

  /// Min and max mass for gated quantities
  Float_t mMinMass, mMaxMass; 
  /// Size of mixed event pool
  Int_t mPoolSize; 

  /// Mix real pairs
  void mixReal();
  /// Mix combinatoric pairs
  void mixBackground();

  /// Random number generator for event mixing
  TRandom *mRandom;

  /// Makes class visible to root
  ClassDef(StEEmcMixMaker,1);

};

inline void StEEmcMixMaker::sector(Int_t s){ mSectorList.push_back(s); }
inline void StEEmcMixMaker::trigger(Int_t t){ mTriggerList.push_back(t); }
inline void StEEmcMixMaker::minET(Float_t et){ mETcut=et; }
inline void StEEmcMixMaker::maxZ(Float_t z){ mZcut=z; }
inline void StEEmcMixMaker::minEpoint(Float_t m){ mEpoint=m; } 

inline void StEEmcMixMaker::mudst( const Char_t *n ){mMuDstMakerName=n;}
inline void StEEmcMixMaker::analysis(const Char_t *a){mAnalysisName=a;}
inline void StEEmcMixMaker::points(const Char_t *n){mPointMakerName=n; }

inline void StEEmcMixMaker::range( Float_t mn, Float_t mx ){ mMinMass=mn; mMaxMass=mx; } 

inline Int_t StEEmcMixMaker::numberOfCandidates(){ return (Int_t)mCandidates.size(); } 
inline Int_t StEEmcMixMaker::numberOfMixedCandidates(){ return (Int_t)mBackground.size(); } 
#endif
