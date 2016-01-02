#ifndef STPICOCUTSBASE_H
#define STPICOCUTSBASE_H

/* **************************************************
 *  Cut base class for pico analysis
 *  - Base class for cuts 
 *  
 * **************************************************
 *  Inhertit from it if needed 
 *
 * **************************************************
 *
 *
 *  Initial Authors:  
 *            Xin Dong        (xdong@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *          **Jochen Thaeder  (jmthader@lbl.gov)   
 *
 *  Contributing Authors
 *            Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Guannan Xie     (guannanxie@lbl.gov)
 *
 *  ** Code Maintainer
 *
 * **************************************************
 */

#include <vector>

#include "TNamed.h"
#include "TString.h"
#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StBTofUtil/StV0TofCorrection.h"

class StPicoTrack;
class StPicoEvent;
class StPicoDst;
class StPicoBTofPidTraits;

class StPicoCutsBase : public TNamed
{
 public:
  
  StPicoCutsBase();
  StPicoCutsBase(const Char_t *name);
  ~StPicoCutsBase();
  
  void initBase();

  // -- overload init in inhertited class - but call initBase there
  virtual void init() { initBase(); }

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   

  bool isGoodEvent(StPicoDst const * const picoDst, int *aEventCuts = NULL);

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   

  bool isGoodRun(StPicoEvent const *picoEvent) const;

  bool isGoodTrack(StPicoTrack const *trk) const;
   
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  // -- PID
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  
  enum ePicoPID {kPion, kKaon, kProton, kElectron, kMuon, kK0Short, kLambda, kPicoPIDMax};

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  // -- TOF PID
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  StPicoBTofPidTraits* hasTofPid(StPicoTrack const * const trk) const;

  bool isTPCHadron(StPicoTrack const * const trk, int pidFlag) const;

  bool isTPCPion(StPicoTrack const *trk) const;
  bool isTPCKaon(StPicoTrack const *trk) const;
  bool isTPCProton(StPicoTrack const *trk) const;

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  // -- TOF PID
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  bool isTOFHadronPID(StPicoTrack const *trk, float const & tofBeta, int pidFlag) const;
  bool isTOFHadron(StPicoTrack const *trk, float const & tofBeta, int pidFlag) const;
  bool isHybridTOFHadron(StPicoTrack const *trk, float const & tofBeta, int pidFlag) const;

  // -- Is TOF particle in ptot range
  //    if track has no TOF information - return false
  //    use for 
  //      - primary hadrons 
  //      - secondarys from charm decays (as an approximation)
  bool isTOFPion(StPicoTrack const *trk) const;
  bool isTOFKaon(StPicoTrack const *trk) const;
  bool isTOFProton(StPicoTrack const *trk) const;

  bool isTOFPion(StPicoTrack const *trk,   float const & tofBeta) const;
  bool isTOFKaon(StPicoTrack const *trk,   float const & tofBeta) const;
  bool isTOFProton(StPicoTrack const *trk, float const & tofBeta) const;

  // -- Is TOF particle in ptot range
  //    if track has no TOF information - return true
  //    use for 
  //      - primary hadrons 
  //      - secondarys from charm decays (as an approximation)
  bool isHybridTOFPion(StPicoTrack const *trk) const;
  bool isHybridTOFKaon(StPicoTrack const *trk) const;
  bool isHybridTOFProton(StPicoTrack const *trk) const;

  bool isHybridTOFPion(StPicoTrack const *trk,   float const & tofBeta) const;
  bool isHybridTOFKaon(StPicoTrack const *trk,   float const & tofBeta) const;
  bool isHybridTOFProton(StPicoTrack const *trk, float const & tofBeta) const;

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  
  const unsigned int&  eventStatMax()  const { return mEventStatMax; }

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  // -- SETTER for CUTS
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

  void setBadRunListFileName(const char* fileName);

  void setCutVzMax(float f);
  void setCutVzVpdVzMax(float f);
  void setCutTriggerWord(UShort_t us);

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

  void setCutNHitsFitMax(int i);
  void setCutRequireHFT(bool b);
  void setCutNHitsFitnHitsMax(float f);

  void setCutPrimaryDCAtoVtxMax(float f);

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

  void setCutPtRange(float min, float max, int pidFlag);
  void setCutTPCNSigma(float f, int pidFlag);
  void setCutTOFNSigma(float f, int pidFlag);
  void setCutTOFDeltaOneOverBeta(float f, int pidFlag);
  void setCutPtotRangeTOF(float min, float max, int pidFlag);
  void setCutPtotRangeHybridTOF(float min, float max, int pidFlag);

  void setCutPionPtRange(float min, float max);
  void setCutTPCNSigmaPion(float f);
  void setCutTOFNSigmaPion(float f);
  void setCutTOFDeltaOneOverBetaPion(float f);
  void setCutPionPtotRangeTOF(float min, float max);
  void setCutPionPtotRangeHybridTOF(float min, float max);

  void setCutKaonPtRange(float min, float max);
  void setCutTPCNSigmaKaon(float f);
  void setCutTOFNSigmaKaon(float f);
  void setCutTOFDeltaOneOverBetaKaon(float f);
  void setCutKaonPtotRangeTOF(float min, float max);
  void setCutKaonPtotRangeHybridTOF(float min, float max);

  void setCutProtonPtRange(float min, float max);
  void setCutTPCNSigmaProton(float f);
  void setCutTOFNSigmaProton(float f);
  void setCutTOFDeltaOneOverBetaProton(float f);
  void setCutProtonPtotRangeTOF(float min, float max);
  void setCutProtonPtotRangeHybridTOF(float min, float max);

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

  // -- calculate beta of track -- basic calculation
  float getTofBetaBase(StPicoTrack const* const trk) const;

  // -- calculate beta of track -- for primary particles
  float getTofBeta(StPicoTrack const* const trk) const;

  // -- calculate corrected beta of track -- for secondary particles
  float getTofBeta(StPicoTrack const * const trk, 
		   StLorentzVectorF const & secondaryMother, StThreeVectorF const & secondaryVtx) const; 
  
  // -- calculate corrected beta of track -- for tertiary particles
  float getTofBeta(StPicoTrack const * const trk, 
		   StLorentzVectorF const & secondaryMother, StThreeVectorF const & secondaryVtx, 
		   StLorentzVectorF const & tertiaryMother,  StThreeVectorF const & tertiaryVtx) const;
  
  const float& getHypotheticalMass(int pidFlag)           const;

 private:
  
  StPicoCutsBase(StPicoCutsBase const &);       
  StPicoCutsBase& operator=(StPicoCutsBase const &); 

  StV0TofCorrection* mTOFCorr;  // TOF correction

  StThreeVectorF    mPrimVtx;   // primary vertex of current event
  const StPicoDst*  mPicoDst;   //! ptr to picoDst

  unsigned int mEventStatMax;   // number of event cuts
  
  float        mTOFResolution;  // TOF resolution = 0.013

  // -- bad run list
  TString mBadRunListFileName;
  std::vector<int> mVecBadRunList;

  // -- event cuts
  float mVzMax;
  float mVzVpdVzMax;
  UShort_t mTriggerWord;             // first five bits see http://rnc.lbl.gov/~xdong/SoftHadron/picoDst.html

  // -- tracking
  int   mNHitsFitMax;
  bool  mRequireHFT;
  float mNHitsFitnHitsMax;           

  float mPrimaryDCAtoVtxMax;         // used for primary selection for TOF Beta recalculation

  // -- acceptance - per particle type [ePicoPID]
  float mPtRange[kPicoPIDMax][2];

  // -- PID cuts - per particle type [ePicoPID]
  float mHypotheticalMass[kPicoPIDMax];        // hypothetical mass
  float mHypotheticalMass2[kPicoPIDMax];       // hypothetical mass squared

  float mTPCNSigmaMax[kPicoPIDMax];
  float mTOFDeltaOneOverBetaMax[kPicoPIDMax]; 

  float mPtotRangeTOF[kPicoPIDMax][2];         // momentum range [min,max], where TOF PID is applied
  float mPtotRangeHybridTOF[kPicoPIDMax][2];   // momentum range [min,max], where Hybrid TOF PID is applied

  ClassDef(StPicoCutsBase,1)
};

inline void StPicoCutsBase::setBadRunListFileName(const char* fileName) { mBadRunListFileName = fileName; }

inline void StPicoCutsBase::setCutVzMax(float f)              { mVzMax            = f; }
inline void StPicoCutsBase::setCutVzVpdVzMax(float f)         { mVzVpdVzMax       = f; }
inline void StPicoCutsBase::setCutTriggerWord(UShort_t us)    { mTriggerWord      = us; }

inline void StPicoCutsBase::setCutNHitsFitMax(int i)          { mNHitsFitMax      = i; }
inline void StPicoCutsBase::setCutRequireHFT(bool b)          { mRequireHFT       = b; }
inline void StPicoCutsBase::setCutNHitsFitnHitsMax(float f)   { mNHitsFitnHitsMax = f; }

inline void StPicoCutsBase::setCutPrimaryDCAtoVtxMax(float f) { mPrimaryDCAtoVtxMax = f; }

inline void StPicoCutsBase::setCutPtRange(float min, float max, int pidFlag)            { mPtRange[pidFlag][0] = min; 
                                                                                          mPtRange[pidFlag][1] = max; }
inline void StPicoCutsBase::setCutTPCNSigma(float f, int pidFlag)                       { mTPCNSigmaMax[pidFlag] = f; }
inline void StPicoCutsBase::setCutTOFNSigma(float f, int pidFlag)                       { mTOFDeltaOneOverBetaMax[pidFlag] = f*mTOFResolution;}
inline void StPicoCutsBase::setCutTOFDeltaOneOverBeta(float f, int pidFlag)             { mTOFDeltaOneOverBetaMax[pidFlag] = f;}
inline void StPicoCutsBase::setCutPtotRangeTOF(float min, float max, int pidFlag)       { mPtotRangeTOF[pidFlag][0] = min; 
                                                                                          mPtotRangeTOF[pidFlag][1] = max; }
inline void StPicoCutsBase::setCutPtotRangeHybridTOF(float min, float max, int pidFlag) { mPtotRangeHybridTOF[pidFlag][0] = min; 
                                                                                          mPtotRangeHybridTOF[pidFlag][1] = max; }

inline void StPicoCutsBase::setCutPionPtRange(float min, float max)              { setCutPtRange(min, max, StPicoCutsBase::kPion); }
inline void StPicoCutsBase::setCutTPCNSigmaPion(float f)                         { setCutTPCNSigma(f, StPicoCutsBase::kPion); }
inline void StPicoCutsBase::setCutTOFNSigmaPion(float f)                         { setCutTOFNSigma(f, StPicoCutsBase::kPion); }
inline void StPicoCutsBase::setCutTOFDeltaOneOverBetaPion(float f)               { setCutTOFDeltaOneOverBeta(f, StPicoCutsBase::kPion); }
inline void StPicoCutsBase::setCutPionPtotRangeTOF(float min, float max)         { setCutPtotRangeTOF(min, max, StPicoCutsBase::kPion); }
inline void StPicoCutsBase::setCutPionPtotRangeHybridTOF(float min, float max)   { setCutPtotRangeHybridTOF(min, max, StPicoCutsBase::kPion); }

inline void StPicoCutsBase::setCutKaonPtRange(float min, float max)              { setCutPtRange(min, max, StPicoCutsBase::kKaon); }
inline void StPicoCutsBase::setCutTPCNSigmaKaon(float f)                         { setCutTPCNSigma(f, StPicoCutsBase::kKaon); }
inline void StPicoCutsBase::setCutTOFNSigmaKaon(float f)                         { setCutTOFNSigma(f, StPicoCutsBase::kKaon); }
inline void StPicoCutsBase::setCutTOFDeltaOneOverBetaKaon(float f)               { setCutTOFDeltaOneOverBeta(f, StPicoCutsBase::kKaon); }
inline void StPicoCutsBase::setCutKaonPtotRangeTOF(float min, float max)         { setCutPtotRangeTOF(min, max, StPicoCutsBase::kKaon); }
inline void StPicoCutsBase::setCutKaonPtotRangeHybridTOF(float min, float max)   { setCutPtotRangeHybridTOF(min, max, StPicoCutsBase::kKaon); }

inline void StPicoCutsBase::setCutProtonPtRange(float min, float max)            { setCutPtRange(min, max, StPicoCutsBase::kProton); }
inline void StPicoCutsBase::setCutTPCNSigmaProton(float f)                       { setCutTPCNSigma(f, StPicoCutsBase::kProton); }
inline void StPicoCutsBase::setCutTOFNSigmaProton(float f)                       { setCutTOFNSigma(f, StPicoCutsBase::kProton); }
inline void StPicoCutsBase::setCutTOFDeltaOneOverBetaProton(float f)             { setCutTOFDeltaOneOverBeta(f, StPicoCutsBase::kProton); }
inline void StPicoCutsBase::setCutProtonPtotRangeTOF(float min, float max)       { setCutPtotRangeTOF(min, max, StPicoCutsBase::kProton); }
inline void StPicoCutsBase::setCutProtonPtotRangeHybridTOF(float min, float max) { setCutPtotRangeHybridTOF(min, max, StPicoCutsBase::kProton); }


inline const float&    StPicoCutsBase::getHypotheticalMass(int pidFlag)        const { return mHypotheticalMass[pidFlag]; }

// -- check for good hadrons in TPC - in ptRange
inline bool StPicoCutsBase::isTPCPion(StPicoTrack const * const trk)   const {return isTPCHadron(trk, StPicoCutsBase::kPion); }
inline bool StPicoCutsBase::isTPCKaon(StPicoTrack const * const trk)   const {return isTPCHadron(trk, StPicoCutsBase::kKaon); }
inline bool StPicoCutsBase::isTPCProton(StPicoTrack const * const trk) const {return isTPCHadron(trk, StPicoCutsBase::kProton); }

inline bool StPicoCutsBase::isTOFPion(StPicoTrack const *trk)   const { float tofBeta = getTofBeta(trk);  
                                                                        return isTOFHadron(trk, tofBeta, StPicoCutsBase::kPion); }
inline bool StPicoCutsBase::isTOFKaon(StPicoTrack const *trk)   const { float tofBeta = getTofBeta(trk);  
                                                                        return isTOFHadron(trk, tofBeta, StPicoCutsBase::kKaon); }
inline bool StPicoCutsBase::isTOFProton(StPicoTrack const *trk) const { float tofBeta = getTofBeta(trk);  
                                                                        return isTOFHadron(trk, tofBeta, StPicoCutsBase::kProton); }

inline bool StPicoCutsBase::isTOFPion(StPicoTrack const *trk,   float const & tofBeta) const { return isTOFHadron(trk, tofBeta, StPicoCutsBase::kPion); }
inline bool StPicoCutsBase::isTOFKaon(StPicoTrack const *trk,   float const & tofBeta) const { return isTOFHadron(trk, tofBeta, StPicoCutsBase::kKaon); }
inline bool StPicoCutsBase::isTOFProton(StPicoTrack const *trk, float const & tofBeta) const { return isTOFHadron(trk, tofBeta, StPicoCutsBase::kProton); }

inline bool StPicoCutsBase::isHybridTOFPion(StPicoTrack const *trk)   const { float tofBeta = getTofBeta(trk);  
                                                                            return isHybridTOFHadron(trk, tofBeta, StPicoCutsBase::kPion); }
inline bool StPicoCutsBase::isHybridTOFKaon(StPicoTrack const *trk)   const { float tofBeta = getTofBeta(trk);  
                                                                              return isHybridTOFHadron(trk, tofBeta, StPicoCutsBase::kKaon); }
inline bool StPicoCutsBase::isHybridTOFProton(StPicoTrack const *trk) const { float tofBeta = getTofBeta(trk);  
                                                                              return isHybridTOFHadron(trk, tofBeta, StPicoCutsBase::kProton); }

inline bool StPicoCutsBase::isHybridTOFPion(StPicoTrack const *trk,   float const & tofBeta) const { return isHybridTOFHadron(trk, tofBeta, StPicoCutsBase::kPion); }
inline bool StPicoCutsBase::isHybridTOFKaon(StPicoTrack const *trk,   float const & tofBeta) const { return isHybridTOFHadron(trk, tofBeta, StPicoCutsBase::kKaon); }
inline bool StPicoCutsBase::isHybridTOFProton(StPicoTrack const *trk, float const & tofBeta) const { return isHybridTOFHadron(trk, tofBeta, StPicoCutsBase::kProton); }

#endif
