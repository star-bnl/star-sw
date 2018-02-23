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
class TTree;
class TFile;

class StEvent;
class StTrack;
class StMtdHit;
class StVertex;

class StMuDst;
class StMuTrack;
class StMuMtdHit;
class StTriggerData;

#include "StPhysicalHelixD.hh"
#include "StThreeVector.hh"
#include "StMtdUtil/StMtdConstants.h"

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
  Int_t    InitRun(const Int_t runNumber);
  Int_t    Make();
  Int_t    Finish();

  Int_t    initHistos();
  Int_t    readQTTacOffsetFile();
  void     setCosmic(const Bool_t c);
  void     setFillQATree(const Bool_t fill = kFALSE);
  void     setOutTreeFileName(const Char_t *out);
  void     setVertexMode(const Int_t mode);
  void     setMaxVtxZ(const Double_t max);
  void     setMaxVtxDz(const Double_t max);
  void     setTrackPtLimits(const Double_t min, const Double_t max);
  void     setTrackPhiLimits(const Double_t min, const Double_t max);
  void     setTrackEtaLimits(const Double_t min, const Double_t max);
  void     setMinNHitsFit(const Int_t min);
  void     setMinNHitsDedx(const Int_t min);
  void     setMinFitHitsFraction(const Double_t min);
  void     setMaxDca(const Double_t max);
  void     setMatchToTof(const bool match);
  void     setNsigmaPiCut(const Double_t min, const Double_t max);
  void     setPrintMemory(const Bool_t pMem = kTRUE);
  void     setPrintCpu(const Bool_t pCpu = kTRUE);
  void     setPrintConfig(const Bool_t print = kTRUE);
  void     setTriggerIDs(const IntVec id);
  void     setApplyQTTacOffset(const bool apply);
  void     setFileQTTacOffset(const char* file);

 protected:
  void     printConfig();
  Int_t    processStEvent();
  Int_t    processMuDst();
  void     processTriggerData(StTriggerData *trigData);
  void     bookTree();
  void     bookHistos();
  void     fillHistos();

  Bool_t   isValidTrack(StTrack *t, StVertex *vtx) const ; // Check if a StTrack satisfies all the quality cuts
  Bool_t   isValidTrack(const StMuTrack *t) const ; // Check if a StMuTrack satisfies all the quality cuts

  Bool_t   propagateHelixToMtd(StPhysicalHelixD helix, Double_t &projPhi, Double_t &projZ) const; // Propagate a helix to MTD radius and return the projected position


  Int_t    getMtdHitTHUB(const Int_t backleg) const; // Return the THUB index for a particular backleg

  Double_t getMtdHitGlobalZ(StMuMtdHit *hit) const;  // Return the z position of a MTD hit in global coordinates in StEvent
  Double_t getMtdHitGlobalZ(StMtdHit *hit) const;    // Return the z position of a MTD hit in global coordinates in MuDst
  Double_t getMtdHitGlobalZ(Double_t leadingWestTime, Double_t leadingEastTime, Int_t module) const;

  Double_t getMtdHitGlobalPhi(StMuMtdHit *hit) const;  // Return the phi position of a MTD hit in global coordinates in StEvent
  Double_t getMtdHitGlobalPhi(StMtdHit *hit) const;    // Return the phi position of a MTD hit in global coordinates in MuDst
  Double_t getMtdHitGlobalPhi(Int_t backleg, Int_t module, Int_t channel) const;

  Int_t    getMtdBackleg(const Double_t projPhi) const; // Return the backleg index (1-30) for the given phi position in global coordinates
  Int_t    getMtdModule(const Double_t projZ) const;    // Return the module index (1-5) for the given z position in global coordinates
  Int_t    getMtdCell(const Double_t projPhi, const Double_t projZ) const; // Return the cell index (0-11) for given phi and z position in global coordinates
  Int_t    getMtdCell(const Double_t projPhi, const Int_t backleg, const Int_t module) const; // return the cell index (0-11) for given phi position and backleg, module indices
  void     getMtdPosFromProj(const Double_t projPhi, const Double_t projZ, Int_t &backleg, Int_t &module, Int_t &cell) const; // Return the backleg, module, cell indices for given phi and z position in global coordinates
  Double_t rotatePhi(Double_t phi) const;

  static const Int_t kMaxHits      = 1e6;
  static const Int_t kMaxTrack     = 1e5;
  static const Int_t kNTotalCells  = gMtdNBacklegs * gMtdNModules * gMtdNCells;
  static const Int_t kMaxPrepost   = 11;
  static const Int_t kMaxVpdChan   = 64;
  static const Int_t kMaxMtdQTchan = 32;
  static const Int_t kNQTboard     = 8;

  Int_t    mModuleToQT[gMtdNBacklegs][gMtdNModules];     // Map from module to QT board index
  Int_t    mModuleToQTPos[gMtdNBacklegs][gMtdNModules];  // Map from module to the position on QA board
  Int_t    mQTtoModule[kNQTboard][8];                    // Map from QA board to module index
  Int_t    mQTSlewBinEdge[kNQTboard][16][8];             // Slewing table for QT board       
  Int_t    mQTSlewCorr[kNQTboard][16][8];                // Slewing correction for QT board
  Int_t    mTrigQTpos[kNQTboard][2];                  // QT position of trigger signals


  struct StMtdQAData
  {

    // event information
    Int_t    runId;
    Int_t    eventId;
    Int_t    nTrigger;
    Int_t    triggerId[30];
    Int_t    refMult;
    Int_t    gRefMult;

    // vertex information
    Float_t  vertexX, vertexY, vertexZ;
    Float_t  vpdVz;

    // VPD information
    Int_t  pre;
    Int_t  post;
    Int_t  prepost;
    UShort_t vpdTacSum;
    UShort_t vpdHi[kMaxVpdChan];
    UShort_t mtdQTadc[kNQTboard][kMaxMtdQTchan/2];
    UShort_t mtdQTtac[kNQTboard][kMaxMtdQTchan/2];  
    UShort_t mtdQTtacSum[kNQTboard][kMaxMtdQTchan/4];
    UShort_t mtdQThigh2Pos[kNQTboard][2];
    UShort_t mixMtdTacSum[kNQTboard][2];
    UInt_t   TF201Bit;
    UInt_t   TF201Bit2;

    // TOF start time
    Int_t    tofStartTime;

    // Tracks
    Int_t    nGoodTrack;
    Double_t trkPt[kMaxTrack];
    Double_t trkEta[kMaxTrack];
    Double_t trkPhi[kMaxTrack];
    Double_t trkDca[kMaxTrack];
    Int_t    trkNHitsFit[kMaxTrack];
    Int_t    trkNHitsDedx[kMaxTrack];
    Double_t trkDedx[kMaxTrack];
    Double_t trkNsigmaPi[kMaxTrack];

    //== TOF hits matched to tracks
    Bool_t   isTrkTofMatched[kMaxTrack];
    Int_t    trkMthTofTray[kMaxTrack];
    Int_t    trkMthTofModule[kMaxTrack];
    Int_t    trkMthTofCell[kMaxTrack];
    Double_t trkMthTofLocaly[kMaxTrack];
    Double_t trkMthTofLocalz[kMaxTrack];

    //== Track position projected to MTD radius
    Bool_t   isTrkProjected[kMaxTrack];
    Double_t trkProjPhi[kMaxTrack];
    Double_t trkProjZ[kMaxTrack];
    Int_t    trkProjBackleg[kMaxTrack];
    Int_t    trkProjModule[kMaxTrack];
    Int_t    trkProjChannel[kMaxTrack];
    Bool_t   isTrkMtdMatched[kMaxTrack];
    Int_t    trkMthBackleg[kMaxTrack];
    Int_t    trkMthModule[kMaxTrack];
    Int_t    trkMthChannel[kMaxTrack];

    // MTD information
    Double_t mtdTriggerTime[2];  // trigger time

    //== MTD raw hits
    Int_t    nMtdRawHits;
    Int_t    mtdRawHitFlag[kMaxHits];
    Int_t    mtdRawHitBackleg[kMaxHits];  //1-30
    Int_t    mtdRawHitModule[kMaxHits];   //1-5
    Int_t    mtdRawHitChan[kMaxHits];     //1-120
    Double_t mtdRawHitTdc[kMaxHits];
    Double_t mtdRawHitTimdDiff[kMaxHits];

    //== MTD hits
    Int_t    nMtdHits;
    Int_t    mtdHitBackleg[kMaxHits];        //1-30
    Int_t    mtdHitModule[kMaxHits];         //1-5
    Int_t    mtdHitChan[kMaxHits];           //0-12
    Double_t mtdHitLeTimeWest[kMaxHits];
    Double_t mtdHitLeTimeEast[kMaxHits];
    Double_t mtdHitTotWest[kMaxHits];
    Double_t mtdHitTotEast[kMaxHits];
    Double_t mtdHitTrigTime[kMaxHits];
    Double_t mtdHitPhi[kMaxHits];
    Double_t mtdHitZ[kMaxHits];

    //== matched tracks
    Bool_t   isMatched[kMaxHits];
    Int_t    nMatchMtdHits;
    Double_t mtdMatchTrkPathLength[kMaxHits];
    Double_t mtdMatchTrkTof[kMaxHits];
    Double_t mtdMatchTrkExpTof[kMaxHits];
    Double_t mtdMatchTrkLocaly[kMaxHits];
    Double_t mtdMatchTrkLocalz[kMaxHits];
    Double_t mtdMatchTrkDeltay[kMaxHits];
    Double_t mtdMatchTrkDeltaz[kMaxHits];
    Double_t mtdMatchTrkProjPhi[kMaxHits];
    Double_t mtdMatchTrkProjZ[kMaxHits];
    Double_t mtdMatchTrkPt[kMaxHits];
    Double_t mtdMatchTrkDca[kMaxHits];
    Double_t mtdMatchTrCharge[kMaxHits];
    Double_t mtdMatchTrkPhi[kMaxHits];
    Double_t mtdMatchTrkEta[kMaxHits];
    Double_t mtdMatchTrkNsigmaPi[kMaxHits];
    Bool_t   mtdMatchTrkTofHit[kMaxHits];

    //== trigger hits
    Bool_t   isMtdTrig[kMaxHits];
  };


 private:
  Bool_t           mIsCosmic;                                  // Flag of cosmic or physics data
  StEvent          *mStEvent;                                  // Pointer to StEvent
  StMuDst          *mMuDst;                                    // Pointer to MuDst event
  Int_t            mVertexMode;                                // 0 - default; 1 - closest to VPD with positive ranking
  Int_t            mVertexIndex;                               // Index of selected vertex
  Int_t            mRunId;                                     // Run number
  Int_t            mRunYear;                                   // Run year
  TString          mCollisionSystem;                           // collision system
  Int_t            mStartRun;                                  // First run of the year
  Int_t            mEndRun;                                    // Last run of the year
  StTriggerData    *mTriggerData;                              // Pointer to the trigger data
  Bool_t           mMuDstIn;                                   // Flag to force running on MuDst
  Bool_t           mPrintMemory;                               // Flag to print out memory usage
  Bool_t           mPrintCpu;                                  // Flag to print out CPU usage
  Bool_t           mPrintConfig;                               // Flag to print out task configuration
  IntVec           mTriggerIDs;                                // Valid trigger id collection that will be tested
  Bool_t           mApplyQTTacOffset;                          // Flag to apply QT offset
  TString          mFileQTTacOffset;                           // File of QT offset
  Int_t            mQTTacOffset[kNQTboard][16];                // QT offset
  
  Double_t         mMaxVtxZ;                                   // Maximum vertex z
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
  Double_t         mMinNsigmaPi;                               // Minimum nsigma for pion assumption
  Double_t         mMaxNsigmaPi;                               // Maximum nsigma for pion assumption
  Bool_t           mMatchToTof;                                // Flag to require tracks matched to TOF hits to reject pileup
  UShort_t         mMtd_qt_tac_min;                            // Minimum QT tac
  UShort_t         mMtd_qt_tac_max;                            // Maximum QT tac
  UShort_t         mMtd_qt_tac_diff_range_abs;                 // Maximum difference between j2 and j3

  Bool_t           mHistoInit;                                 // Flag to histogram initilization
  Bool_t           mFillTree;                                  // Flag to fill the QA tree
  TFile            *fOutTreeFile;                              // Output file that the QA tree will be written to
  TString          mOutTreeFileName;                           // Name of the output file for the QA tree
  Double_t         mTrigTime[2];                               // Trigger time from THUB
 
  StMtdQAData      mMtdData;                                   // Structure to hold all the variables to fill the histograms
  TTree            *mQATree;                                   // Pointer to the QA tree
  TH1F             *mhEventTrig;                               // Event statistics 
  TH1F             *mhEventCuts;                               // Analysis cuts used
  TH1F             *mhRunId;                                   // Run indices
  TH1F             *mhRefMult;                                 // RefMult distribution
  TH1F             *mhgRefMult;                                // gRefMult distribution
  TH2F             *mhVertexXY;                                // Correlation between vertex x and y
  TH2F             *mhVertexXZ;                                // Correlation between vertex x and z
  TH2F             *mhVertexYZ;                                // Correlation between vertex y and z
  TH1F             *mhVertexZ;                                 // Distribution of vertex z
  TH2F             *mhVtxZvsVpdVzDefault;                      // Correlation between z of TPC and VPD vertices (default)
  TH1F             *mhVtxZDiffDefault;                         // Difference between z of TPC and VPD vertices (default)
  TH2F             *mhVtxZvsVpdVzClosest;                      // Correlation between z of TPC and VPD vertices (closest, positive ranking)
  TH1F             *mhVtxZDiffClosest;                         // Difference between z of TPC and VPD vertices (closest, positive ranking)
  TH1F             *mhVtxClosestIndex;                         // Index of postively ranked vertex that is closest to VPD vz
  TH1F             *mhTofStartTime;                            // Distribution of start time from BTOF
  TH2F             *mhVpdQTadc;                                // Distribution of ADC per VPD QT channel
  TH2F             *mhVpdQTtac;                                // Distribution of TAC per VPD QT channel

  // Primary tracks
  TH1F             *mhNTrk;                                    // # of good tracks per event
  TH1F             *mhTrkPt;                                   // pt distribution of all good tracks
  TH2F             *mhTrkDcaVsPt;                              // Track dca distribution
  TH2F             *mhTrkPhiVsPt;                              // Track phi distribution
  TH2F             *mhTrkEtaVsPt;                              // Track eta distribution
  TH2F             *mhTrkPhiEta;                               // Track phi vs eta with p_T > 1 GeV/c
  TH2F             *mhTrkNHitsFitVsPt;                         // Track NHitsFit distribution
  TH2F             *mhTrkNHitsDedxVsPt;                        // Track NHitsDedx distribtion
  TH2F             *mhTrkDedxVsPt;                             // Track Dedx distribution
  TH2F             *mhTrkNsigmaPiVsPt;                         // Track NsigmaPi vs. Pt
  TH2F             *mhTrkNsigmaPiVsPhi;                        // Track NsigmaPi vs. Phi (p_T > 1 GeV/c)
  TH2F             *mhTrkNsigmaPiVsEta;                        // Track NsigmaPi vs. eta (p_T > 1 GeV/c)
  TH2F             *mhTofMthTrkLocaly;                         // Projected y in TOF local coordinate for tracks matched to TOF hits
  TH2F             *mhTofMthTrkLocalz;                         // Projected z in TOF local coordinate for tracks matched to TOF hits
  TH2F             *mhMtdTrackProjMap;                         // Hit map of tracks that can be projected to MTD radius

  // MTD trigger electronics
  TH2F             *mhMtdQTAdcAll;                             // Distribution of ADC per MTD QT channel
  TH2F             *mhMtdQTAdcMth;                             // Distribution of ADC per MTD QT channel with matched tracks
  TH2F             *mhMtdQTAdcMthTof;                          // Distribution of ADC per MTD QT channel with matched TOF tracks
  TH2F             *mhMtdQTAdcMuon;                            // Distribution of ADC per MTD QT channel for muon candidates
  TH2F             *mhMtdQTTacAll;                             // Distribution of TAC per MTD QT channel
  TH2F             *mhMtdQTTacMth;                             // Distribution of TAC per MTD QT channel with matched tracks
  TH2F             *mhMtdQTTacMthTof;                          // Distribution of TAC per MTD QT channel with matched TOF tracks
  TH2F             *mhMtdQTTacMuon;                            // Distribution of TAC per MTD QT channel for muon candidates
  TH2F             *mhMtdQTAdcVsTacAll;                        // Correlation between ADC and TAC in MTD QT
  TH2F             *mhMtdQTJ2J3Diff;                           // Distribution of TAC difference between J3-J2 per MTD QT position
  TH2F             *mhMixMtdTacSumvsMxqMtdTacSum[kNQTboard][2];// Correlation between MT001 vs MT101
  TH2F             *mhMtdVpdTacDiffMT001;                      // Distribution of earliest 2 TAC of MTD-VPD for MT001
  TH2F             *mhMtdVpdTacDiffMT001Mth;                   // Distribution of earliest 2 TAC of MTD-VPD with matched tracks for MT001
  TH2F             *mhMtdVpdTacDiffMT001MthTof;                // Distribution of earliest 2 TAC of MTD-VPD with matched TOF tracks for MT001
  TH2F             *mhMtdVpdTacDiffMT001Muon;                  // Distribution of earliest 2 TAC of MTD-VPD with muon candidates for MT001
  TH2F             *mhMtdVpdTacDiffMT101;                      // Distribution of earliest 2 TAC of MTD-VPD for MT101
  TH2F             *mhMtdVpdTacDiffMT101Mth;                   // Distribution of earliest 2 TAC of MTD-VPD with matched tracks for MT101
  TH2F             *mhMtdVpdTacDiffMT101MthTof;                // Distribution of earliest 2 TAC of MTD-VPD with matched TOF tracks for MT101
  TH2F             *mhMtdVpdTacDiffMT101Muon;                  // Distribution of earliest 2 TAC of MTD-VPD with muon candidates for MT101

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
  TH2F             *mhMtdHitTrigTimeTrkMthTof;                 // MTD hit time with matched TOF track
  TH2F             *mhMtdHitTrigTimeMuon;                      // MTD hit time from muon candidates 
  TH2F             *mhMtdHitTrigTimeGoodQT;                    // MTD hit time with good QT signal
  TH2F             *mhMtdHitTrigTimeTrig;                      // MTD hit time from triggering hits
  TH2F             *mhMtdHitTrigTimeVsQtAdc[2];                // MTD hit time vs. the corresponding ADC values in the QT 
  TH2F             *mhMtdHitTrigTimeVsQtTac[2];                // MTD hit time vs. the corresponding TAC values in the QT

  TH1F             *mhMtdNMatchHits;                           // Number of MTD hits with matched tracks per event
  TH2F             *mhMtdMatchHitMap;                          // Hit map of MTD hits with matched tracks
  TH1F             *mhMtdMatchTrkPt;                           // pt distribution of matched tracks to MTD hits
  TH2F             *mhMtdMatchTrkPhiEta;                       // phi vs eta of matched tracks at primary vertex
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

  TH1F             *mhNQtSignal;                                // Number of good QT signals
  TH1F             *mhNMT101Signal;                             // Number of good MT101 signals
  TH1F             *mhNTF201Signal;                             // Number of good TF201 signals
  TH1F             *mhMtdTrigNHits;                             // Number of triggering MTD hits 
  TH2F             *mhMtdTrigHitMap;                            // Hit map of trigger MTD hits
  TH1F             *mhMtdTrigMthNHits;                          // Number of triggering MTD hits with matched tracks
  TH2F             *mhMtdTrigMthHitMap;                         // Hit map of MTD hits with matched tracks
 

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StMtdQAMaker, 3)
};

inline void StMtdQAMaker::setCosmic(const Bool_t c)                { mIsCosmic = c;            }
inline void StMtdQAMaker::setFillQATree(const Bool_t fill)         { mFillTree = fill;         }
inline void StMtdQAMaker::setOutTreeFileName(const Char_t *out)    { mOutTreeFileName = out;   }
inline void StMtdQAMaker::setPrintMemory(const Bool_t pMem)        { mPrintMemory = pMem;      }
inline void StMtdQAMaker::setPrintCpu(const Bool_t pCpu)           { mPrintCpu = pCpu;         }
inline void StMtdQAMaker::setPrintConfig(const Bool_t print)       { mPrintConfig = print;     }
inline void StMtdQAMaker::setVertexMode(const Int_t mode)          { mVertexMode = mode;       }
inline void StMtdQAMaker::setMaxVtxZ(const Double_t max)           { mMaxVtxZ = max;           }
inline void StMtdQAMaker::setMaxVtxDz(const Double_t max)          { mMaxVtxDz = max;          }
inline void StMtdQAMaker::setMinNHitsFit(const Int_t min)          { mMinNHitsFit = min;       }
inline void StMtdQAMaker::setMinNHitsDedx(const Int_t min)         { mMinNHitsDedx = min;      }
inline void StMtdQAMaker::setMinFitHitsFraction(const Double_t min){ mMinFitHitsFraction = min;}
inline void StMtdQAMaker::setMaxDca(const Double_t max)            { mMaxDca = max;            }
inline void StMtdQAMaker::setMatchToTof(const bool match)          { mMatchToTof = match;      }
inline void StMtdQAMaker::setTriggerIDs(const IntVec id)           { mTriggerIDs = id;         }
inline void StMtdQAMaker::setApplyQTTacOffset(const bool apply)    { mApplyQTTacOffset = apply;}
inline void StMtdQAMaker::setFileQTTacOffset(const char* file)     { mFileQTTacOffset = file;  }
inline void StMtdQAMaker::setTrackPtLimits(const Double_t min, const Double_t max){
  mMinTrkPt = min; mMaxTrkPt = max;
}
inline void StMtdQAMaker::setTrackPhiLimits(const Double_t min, const Double_t max){
  mMinTrkPhi = min; mMaxTrkPhi = max;
}
inline void StMtdQAMaker::setTrackEtaLimits(const Double_t min, const Double_t max){
  mMinTrkEta = min; mMaxTrkPhi = max;
}
inline void StMtdQAMaker::setNsigmaPiCut(const Double_t min, const Double_t max){
  mMinNsigmaPi = min; mMaxNsigmaPi = max;
}


#endif
