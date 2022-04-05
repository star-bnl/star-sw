#ifndef __StEEmcIUMixMaker_h__
#define __StEEmcIUMixMaker_h__

#include <StMaker.h>
#include <vector>
#include <algorithm>
#include "TString.h"

class TH1F;
class TH2F;

#include "StEEmcIUPoint.h"
#include "StEEmcIUPair.h"

class TRandom;
class StEEmcIUPointMaker;
class StMuDstMaker;
class StEEmcA2EMaker;
class StMuEvent;
class EEmcGeomSimple;

class StEEmcIUMixMaker : public StMaker {

 public:

  /// Default constructor
  /// \param name Name of the maker, passed to StMaker constructor
  /// \param size Size of the mixed-event pool 
  StEEmcIUMixMaker( const Char_t *name, Int_t size=20 );
  ~StEEmcIUMixMaker(){ /* nada */ };

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
  Int_t numberofFpoint();
  /// Return a specified candidate pair
  StEEmcIUPair candidate(Int_t c){ return mCandidates[c]; }
  /// Returns the specified mixed candidate pair
  StEEmcIUPair mixedCandidate(Int_t m) { return mBackground[m]; }

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
  void fill ( std::vector<TH1F *> &h, StEEmcIUPair pair ); 
  /// Fill qa distributions
  void fillQA( std::vector<TH2F *> &h, StEEmcIUPair pair ); 
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
  std::vector<Int_t> numberofFailpoint;
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
  StEEmcIUPointMaker *mEEpoints;   //!
  
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


  TH1F       *hZ1;
  TH1F       *hZ2;
  
  /// Vector of points to mix into X-->gamma gamma
  StEEmcIUPointVec_t mPoints;


  /// Pool of points saved from event to event for background mixing.
  /// We rank each event by the adc response of the high tower, in
  /// bins 100 adc counts wide.  
  std::vector< StEEmcIUPointVec_t > mPool;

  /// Point pairs mixed on each event
  StEEmcIUPairVec_t mCandidates;
  /// Background pairs mixed on each event
  StEEmcIUPairVec_t mBackground;


  /// Accept or reject this event (trigger, qa, etc...)
  Bool_t accept(StMuEvent *);

  /// Accept or reject point pairs
  Bool_t accept( StEEmcIUPoint &p1, StEEmcIUPoint &p2 );

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
  ClassDef(StEEmcIUMixMaker,1);

};

inline void StEEmcIUMixMaker::sector(Int_t s){ mSectorList.push_back(s); }
inline void StEEmcIUMixMaker::trigger(Int_t t){ mTriggerList.push_back(t); }
inline void StEEmcIUMixMaker::minET(Float_t et){ mETcut=et; }
inline void StEEmcIUMixMaker::maxZ(Float_t z){ mZcut=z; }
inline void StEEmcIUMixMaker::minEpoint(Float_t m){ mEpoint=m; } 

inline void StEEmcIUMixMaker::mudst( const Char_t *n ){mMuDstMakerName=n;}
inline void StEEmcIUMixMaker::analysis(const Char_t *a){mAnalysisName=a;}
inline void StEEmcIUMixMaker::points(const Char_t *n){mPointMakerName=n; }

inline void StEEmcIUMixMaker::range( Float_t mn, Float_t mx ){ mMinMass=mn; mMaxMass=mx; } 

inline Int_t StEEmcIUMixMaker::numberOfCandidates(){ return (Int_t)mCandidates.size(); } 
inline Int_t StEEmcIUMixMaker::numberOfMixedCandidates(){ return (Int_t)mBackground.size(); } 
#endif
inline Int_t StEEmcIUMixMaker::numberofFpoint(){return (Int_t) numberofFailpoint.size();}
