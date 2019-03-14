#ifndef STMTDQAMAKER_HH
#define STMTDQAMAKER_HH

/***************************************************************************
 *
 * StMtdQAMaker - class to make MTD related QA plots
 * Author: Rongrong Ma
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class TH1F;
class TH2F;
class TProfile;

class StEvent;
class StVertex;
class StTrack;

class StMuDst;
class StMuTrack;
class StMtdTrigUtil;

class StPicoDst;
class StPicoTrack;
class StPicoMtdHit;

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t> IntVec;
#else
typedef vector<Int_t, allocator<Int_t>> IntVec;
#endif

class StMtdQAMaker : public StMaker {
 public:
  StMtdQAMaker(const Char_t *name = "MtdQAMaker");
  ~StMtdQAMaker();

  Int_t    Init();
  Int_t    Make();

  void     setMtdTrigUtil(StMtdTrigUtil* trig);
  void     setVertexMode(const Int_t mode);
  void     setMaxVtxZ(const Double_t max);
  void     setMaxVtxR(const Double_t max);
  void     setApplyMaxVtxDzCut(const Bool_t cut);
  void     setMaxVtxDz(const Double_t max);
  void     setTrackPtLimits(const Double_t min, const Double_t max);
  void     setTrackPhiLimits(const Double_t min, const Double_t max);
  void     setTrackEtaLimits(const Double_t min, const Double_t max);
  void     setMinNHitsFit(const Int_t min);
  void     setMinNHitsDedx(const Int_t min);
  void     setMinFitHitsFraction(const Double_t min);
  void     setMaxDca(const Double_t max);

  void     setMinMuonPt(const Double_t min);
  void     setNsigmaPiCut(const Double_t min, const Double_t max);
  void     setMuonDeltaZ(const Double_t min, const Double_t max);
  void     setMuonDeltaY(const Double_t min, const Double_t max);
  void     setMuonDeltaTof(const Double_t min, const Double_t max);
  void     setMtdHitTrigger(const Bool_t trig);

  void     setPrintMemory(const Bool_t pMem = kTRUE);
  void     setPrintCpu(const Bool_t pCpu = kTRUE);
  void     setPrintConfig(const Bool_t print = kTRUE);
  void     setTriggerIDs(const IntVec id);

 protected:
  void     bookHistos();
  void     printConfig();
  Int_t    processStEvent();
  Int_t    processMuDst();
  Int_t    processPicoDst();

  Int_t    getMtdHitTHUB(const Int_t backleg) const; // Return the THUB index for a particular backleg
  Int_t    getMtdHitIndex(const StPicoTrack *track);
  Int_t    getMtdPidTraitsIndex(const StPicoMtdHit *hit);
  Double_t rotatePhi(Double_t phi) const;
  void     addCutToHisto(TH1 *h, const Int_t bin, const char *label, const Float_t value = -9999999);

  Bool_t   isValidTrack(StTrack *t, StVertex *vtx) const ; // Check if a StTrack satisfies all the quality cuts
  Bool_t   isValidTrack(const StMuTrack *t) const ; // Check if a StMuTrack satisfies all the quality cuts
  Bool_t   isValidTrack(const StPicoTrack *t) const ; // Check if a StPicoTrack satisfies all the quality cuts

  Bool_t   isMuonCandidate(const StMuTrack *track);
  Bool_t   isMuonCandidate(const StPicoTrack *track);
  Bool_t   isMuonCandidate(const Double_t pt, const Double_t nSigmaPi, const Double_t dz, const Double_t dy, const Double_t dtof, const Bool_t isTrig);
  static const Int_t kNQTboard     = 8;

 private:
  StEvent          *mStEvent;                                  // Pointer to StEvent
  StMuDst          *mMuDst;                                    // Pointer to MuDst event
  StPicoDst        *mPicoDst;                                  // Pointer to PicoDst event
  StMtdTrigUtil    *mTrigUtil;                                 // Pointer to MTD trigger utility
  Int_t            mRunId;                                     // Run number
  Int_t            mRunYear;                                   // Run year
  TString          mCollisionSystem;                           // collision system
  Int_t            mVertexMode;                                // 0 - default; 1 - closest to VPD with positive ranking
  Bool_t           mPrintMemory;                               // Flag to print out memory usage
  Bool_t           mPrintCpu;                                  // Flag to print out CPU usage
  Bool_t           mPrintConfig;                               // Flag to print out task configuration
  IntVec           mTriggerIDs;                                // Valid trigger id collection that will be tested
  
  // track quality cuts
  Double_t         mMaxVtxZ;                                   // Maximum vertex z
  Double_t         mMaxVtxR;                                   // Maximum vertex r
  Bool_t           mApplyVtxDzCut;                             // switch for applying dz cut
  Double_t         mMaxVtxDz;                                  // Maximum dz between VPD and TPC
  Double_t         mMinTrkPt;                                  // Minimum track pt
  Double_t         mMaxTrkPt;                                  // Maximum track pt
  Double_t         mMinTrkPhi;                                 // Minimum track phi
  Double_t         mMaxTrkPhi;                                 // Maximum track phi
  Double_t         mMinTrkEta;                                 // Minimum track eta
  Double_t         mMaxTrkEta;                                 // Maximum track eta
  Int_t            mMinNHitsFit;                               // Minimum number of hits used for track fit
  Int_t            mMinNHitsDedx;                              // Minimum number of hits used for de/dx
  Double_t         mMinFitHitsFraction;                        // Minimum fraction of # of hits used for fit out of # of possible hits
  Double_t         mMaxDca;                                    // Maximum track dca

  // muon pid cuts
  Double_t         mMinMuonPt; 
  Double_t         mMinNsigmaPi;                               // Minimum nsigma for pion assumption
  Double_t         mMaxNsigmaPi;                               // Maximum nsigma for pion assumption
  Double_t         mMinMuonDeltaZ;                             
  Double_t         mMaxMuonDeltaZ;
  Double_t         mMinMuonDeltaY;
  Double_t         mMaxMuonDeltaY;
  Double_t         mMinMuonDeltaTof;
  Double_t         mMaxMuonDeltaTof;
  Bool_t           mMtdHitTrigger;

 
  // global properties
  TH1F             *mhEventStat;                               // Event statistics 
  TH1F             *mhEventCuts;                               // Analysis cuts used
  TH1F             *mhRunId;                                   // Run indices
  TH1F             *mhZdcRate;                                 // ZDC coincidence rate
  TH1F             *mhBbcRate;                                 // BBC coincidence rate

  // vertex
  TH2F             *mhVtxZvsVpdVzDefault;                      // TpcVz vs. VpdVz (default)
  TH1F             *mhVtxZDiffDefault;                         // TpcVz - VpdVz(default)
  TH2F             *mhVtxZvsVpdVzClosest;                      // TpcVz vs. VpdVz (closest)
  TH1F             *mhVtxZDiffClosest;                         // TpcVz - VpdVz (closest)
  TH1F             *mhVtxClosestIndex;                         // Index of postively ranked vertex that is closest to VPD vz
  TH2F             *mhVertexXY;                                // Correlation between vertex x and y
  TH2F             *mhVertexXZ;                                // Correlation between vertex x and z
  TH2F             *mhVertexYZ;                                // Correlation between vertex y and z
  TH1F             *mhVertexZ;                                 // Distribution of vertex z
  TH2F             *mhVtxZDiffVsTpcVz;                         // TpcVz - VpdVz vs. TpcVz
  TH1F             *mhVpdVz;                                   // Distribution of VpdVz
  TH1F             *mhVtxZDiff;                                // TpcVz - VpdVz

  /// reference multiplicity
  TH1F             *mhRefMult;                                 // RefMult distribution
  TH1F             *mhgRefMult;                                // gRefMult distribution
  TH2F             *mhgRefMultVsRefMult;
  TH2F             *mhTpcVzVsRefMult;
  TH2F             *mhDiffVzVsRefMult;
  TH2F             *mhZdcRateVsRefMult;
  TH2F             *mhBbcRateVsRefMult;
  TH2F             *mhTofMultVsRefMult;

  // Primary tracks
  TH1F             *mhNTrk;                                    // # of good tracks per event
  TH1F             *mhTrkPt;                                   // pt distribution of all good tracks
  TH2F             *mhTrkDcaVsPt;                              // Track dca distribution
  TH2F             *mhTrkPhiVsPt;                              // Track phi distribution
  TH2F             *mhTrkEtaVsPt;                              // Track eta distribution
  TH2F             *mhTrkPhiEta;                               // Track phi vs eta with p_T > 1 GeV/c
  TH2F             *mhTrkNHitsFitVsPt;                         // Track NHitsFit distribution
  TH2F             *mhTrkNHitsPossVsPt;                        // Track NHitsPoss distribution
  TH2F             *mhTrkNHitsDedxVsPt;                        // Track NHitsDedx distribtion
  TH2F             *mhTrkDedxVsMom;                            // Track Dedx distribution
  TH2F             *mhTrkDedxVsPhi;                            // Track Dedx vs. Phi (p_T > 1 GeV/c)
  TH2F             *mhTrkNsigmaPiVsMom;                        // Track NsigmaPi vs. momentum

  // TOF PID
  TH2F             *mhTrkBetaVsMom;                            // 1/beta vs. momentum
  TH2F             *mhTrkM2VsMom;                              // m2 vs. momentum
  TH2F             *mhTofMthTrkLocaly;                         // Projected y in TOF local coordinate for tracks matched to TOF hits
  TH2F             *mhTofMthTrkLocalz;                         // Projected z in TOF local coordinate for tracks matched to TOF hits

  // MTD trigger electronics
  TH2F             *mhMtdQTAdcAll;                             // Distribution of ADC per MTD QT channel
  TH2F             *mhMtdQTTacAll;                             // Distribution of TAC per MTD QT channel
  TH2F             *mhMtdQTAdcVsTacAll;                        // Correlation between ADC and TAC in MTD QT
  TH2F             *mhMtdQTJ2J3Diff;                           // Distribution of TAC difference between J3-J2 per MTD QT position
  TH2F             *mhMixMtdTacSumvsMxqMtdTacSum[kNQTboard][2];// Correlation between MT001 vs MT101
  TH2F             *mhMtdVpdTacDiffMT001;                      // Distribution of earliest 2 TAC of MTD-VPD for MT001
  TH2F             *mhMtdVpdTacDiffMT001Mth;                   // Distribution of earliest 2 TAC of MTD-VPD with matched tracks for MT001
  TH2F             *mhMtdVpdTacDiffMT001Muon;                  // Distribution of earliest 2 TAC of MTD-VPD with muon candidates for MT001
  TH2F             *mhMtdVpdTacDiffMT101;                      // Distribution of earliest 2 TAC of MTD-VPD for MT101
  TH2F             *mhMtdVpdTacDiffMT101Mth;                   // Distribution of earliest 2 TAC of MTD-VPD with matched tracks for MT101
  TH2F             *mhMtdVpdTacDiffMT101Muon;                  // Distribution of earliest 2 TAC of MTD-VPD with muon candidates for MT101
  TH1F             *mhNQtSignal;                               // Number of good QT signals
  TH1F             *mhNMT101Signal;                            // Number of good MT101 signals
  TH1F             *mhNTF201Signal;                            // Number of good TF201 signals

  // MTD hits
  TH1F             *mhMtdTriggerTime[2];                       // Distribution of MTD trigger time from THUB
  TH1F             *mhMtdNRawHits;                             // Number of MTD raw hits per event
  TH2F             *mhMtdRawHitMap;                            // Hit map of MTD raw hits
  TH2F             *mhMtdRawHitLeTime;                         // Leading time distribution of MTD raw hits
  TH2F             *mhMtdRawHitTrTime;                         // Trailing time distribution of MTD raw hits
  TH1F             *mhMtdRawHitLeNEast;                        // Number of MTD raw hits with leading-edge on east
  TH1F             *mhMtdRawHitLeNWest;                        // Number of MTD raw hits with leading-edge on west
  TH1F             *mhMtdRawHitTrNEast;                        // Number of MTD raw hits with trailing-edge on east
  TH1F             *mhMtdRawHitTrNWest;                        // Number of MTD raw hits with trailing-edge on west
  TH2F             *mhMtdRawHitLeNDiff;                        // Difference of the number of MTD raw hits with leading-edge on east and west
  TH2F             *mhMtdRawHitTrNDiff;                        // Difference of the number of MTD raw hits with trailing-edge on east and west

  TH1F             *mhMtdNHits;                                // Number of MTD hits per event
  TH2F             *mhMtdHitMap;                               // Hit map of MTD hits
  TH2F             *mhMtdHitLeTimeDiff;                        // Difference between leading time of MTD hits on east and west
  TH2F             *mhMtdHitTotWest;                           // Time-Over-Threshold distribution of MTD hits on west
  TH2F             *mhMtdHitTotEast;                           // Time-Over-Threshold distribution of MTD hits on east
  TH2F             *mhMtdHitTrigTime;                          // Difference between the MTD hit time, i.e. (west+east)/2, and trigger time from THUB
  TH2F             *mhMtdHitTrigTimeTrkMth;                    // MTD hit time with matched track

  TH1F             *mhMtdTrigNHits;                            // Number of triggering MTD hits 
  TH2F             *mhMtdTrigHitMap;                           // Hit map of trigger MTD hits
  TH1F             *mhMtdTrigMthNHits;                         // Number of triggering MTD hits with matched tracks
  TH2F             *mhMtdTrigMthHitMap;                        // Hit map of MTD hits with matched tracks

  // hit-track match
  TH1F             *mhMtdNMatchHits;                           // Number of MTD hits with matched tracks per event
  TH2F             *mhMtdMatchHitMap;                          // Hit map of MTD hits with matched tracks
  TH1F             *mhMtdMatchTrkPt;                           // pt distribution of matched tracks to MTD hits
  TH2F             *mhMtdMatchTrkPhiEta;                       // phi vs eta of matched tracks at primary vertex
  TH2F             *mhMtdMatchTrkPhiPt;                        // phi vs pt of matched tracks at primary vertex
  TH2F             *mhMtdMatchDzVsChan;                        // dz vs global channel id
  TH2F             *mhMtdMatchDzVsPtPos;                       // dz vs track pt (positive)
  TH2F             *mhMtdMatchDzVsPtNeg;                       // dz vs track pt (negative)
  TH2F             *mhMtdMatchDyVsChan;                        // dy vs global channel id
  TH2F             *mhMtdMatchDyVsPtPos;                       // dy vs track pt (positive)
  TH2F             *mhMtdMatchDyVsPtNeg;                       // dy vs track pt (negative)
  TH2F             *mhMtdMatchDtofVsPt;                        // dTof vs track pt
  TH2F             *mhMtdMatchMtdTofVsChan;                    // MTD time vs global channel id 
  TH2F             *mhMtdMatchExpTofVsChan;                    // TPC time vs global channel id
  TH2F             *mhMtdMatchDtofVsChan;                      // dTof vs global channel id 
  TH2F             *mhMtdMatchLocalyVsChan;                    // Projected y of matched tracks in local coordinates vs global channel id
  TH2F             *mhMtdMatchLocalzVsChan;                    // Projected z of matched tracks in local coordinates vs global channel id

  // Muon analysis
  TH1F             *mhNMuonPos;
  TH1F             *mhNMuonNeg;
  TH1F             *mhMuonPt;
  TH2F             *mhMuonPhiVsEta;
  TH2F             *mhMuonMap;
  TH1F             *mhNULpair;
  TH1F             *mhNLSpairPos;
  TH1F             *mhNLSpairNeg;
  TH2F             *mhInvMvsPtUL;
  TH2F             *mhInvMvsPtLSpos;
  TH2F             *mhInvMvsPtLSneg;
  TH1F             *mhInvMUL;
  TH1F             *mhInvMLSpos;
  TH1F             *mhInvMLSneg;

  // Run Dependence
  TProfile         *mhBBCrateVsRun;
  TProfile         *mhZDCrateVsRun;
  TProfile         *mhRefMultVsRun;
  TProfile         *mhgRefMultVsRun;
  TProfile         *mhTpcVxVsRun;
  TProfile         *mhTpcVyVsRun;
  TProfile         *mhTpcVzVsRun;
  TProfile         *mhVpdVzVsRun;
  TProfile         *mhDiffVzVsRun;
  TProfile         *mhpTrkPtVsRun;
  TProfile         *mhpTrkEtaVsRun;
  TProfile         *mhpTrkPhiVsRun;
  TProfile         *mhpTrkDcaVsRun;
  TProfile         *mhNHitsFitVsRun;
  TProfile         *mhNHitsPossVsRun;
  TProfile         *mhNHitsDedxVsRun;
  TProfile         *mhDedxVsRun;
  TProfile         *mhNsigmaPiVsRun;
  TProfile         *mhNsigmaEVsRun;
  TProfile         *mhNsigmaKVsRun;
  TProfile         *mhNsigmaPVsRun;
  TProfile         *mhBetaVsRun;
  TProfile         *mhNMtdHitsVsRun;
  TProfile         *mhNMtdTrigHitsVsRun;
  TProfile         *mhNMtdMthHitsVsRun;
  TProfile         *mhNMuonPosVsRun;
  TProfile         *mhNMuonNegVsRun;
  TProfile         *mhNMuonPairULVsRun;
  TProfile         *mhNMuonPairLSPosVsRun;
  TProfile         *mhNMuonPairLSNegVsRun;

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StMtdQAMaker, 3)
};
inline void StMtdQAMaker::setMtdTrigUtil(StMtdTrigUtil* trig)      { mTrigUtil = trig;         }
inline void StMtdQAMaker::setPrintMemory(const Bool_t pMem)        { mPrintMemory = pMem;      }
inline void StMtdQAMaker::setPrintCpu(const Bool_t pCpu)           { mPrintCpu = pCpu;         }
inline void StMtdQAMaker::setPrintConfig(const Bool_t print)       { mPrintConfig = print;     }
inline void StMtdQAMaker::setVertexMode(const Int_t mode)          { mVertexMode = mode;       }
inline void StMtdQAMaker::setApplyMaxVtxDzCut(const Bool_t cut)    { mApplyVtxDzCut = cut;     }
inline void StMtdQAMaker::setMaxVtxZ(const Double_t max)           { mMaxVtxZ = max;           }
inline void StMtdQAMaker::setMaxVtxR(const Double_t max)           { mMaxVtxR = max;           }
inline void StMtdQAMaker::setMaxVtxDz(const Double_t max)          { mMaxVtxDz = max;          }
inline void StMtdQAMaker::setMinNHitsFit(const Int_t min)          { mMinNHitsFit = min;       }
inline void StMtdQAMaker::setMinNHitsDedx(const Int_t min)         { mMinNHitsDedx = min;      }
inline void StMtdQAMaker::setMinFitHitsFraction(const Double_t min){ mMinFitHitsFraction = min;}
inline void StMtdQAMaker::setMaxDca(const Double_t max)            { mMaxDca = max;            }
inline void StMtdQAMaker::setTriggerIDs(const IntVec id)           { mTriggerIDs = id;         }
inline void StMtdQAMaker::setTrackPtLimits(const Double_t min, const Double_t max)
{  mMinTrkPt = min; mMaxTrkPt = max; }

inline void StMtdQAMaker::setTrackPhiLimits(const Double_t min, const Double_t max)
{  mMinTrkPhi = min; mMaxTrkPhi = max; }

inline void StMtdQAMaker::setTrackEtaLimits(const Double_t min, const Double_t max)
{  mMinTrkEta = min; mMaxTrkPhi = max;}

inline void StMtdQAMaker::setMinMuonPt(const Double_t min)
{  mMinMuonPt = min; }

inline void StMtdQAMaker::setNsigmaPiCut(const Double_t min, const Double_t max)
{  mMinNsigmaPi = min; mMaxNsigmaPi = max; }

inline void StMtdQAMaker::setMuonDeltaZ(const Double_t min, const Double_t max)
{ mMinMuonDeltaZ = min; mMaxMuonDeltaZ = max; }

inline void StMtdQAMaker::setMuonDeltaY(const Double_t min, const Double_t max)
{ mMinMuonDeltaY = min; mMaxMuonDeltaY = max; }

inline void StMtdQAMaker::setMuonDeltaTof(const Double_t min, const Double_t max)
{ mMinMuonDeltaTof = min; mMaxMuonDeltaTof = max; }

inline void StMtdQAMaker::setMtdHitTrigger(const Bool_t trig)
{ mMtdHitTrigger = trig; }


#endif
