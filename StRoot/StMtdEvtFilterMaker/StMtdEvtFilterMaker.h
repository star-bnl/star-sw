/*!
 * \class StMtdEvtFilterMaker 
 * \author Rongrong Ma
 * \brief This class is used to check number of muon candidates in each
 * event. It runs both on StEvent and MuDst. The events containing the required
 * number of muon candidates, ususally two, are retained in the offline
 * reconstruction. Otherwise, the events are discarded to save prodction time
 * since they do not contain interested physics process.
 */

#ifndef STMTDEVTFILTERMAKER_HH
#define STMTDEVTFILTERMAKER_HH

#include "StMaker.h"
class TH1F;
class TH2F;

class StEvent;
class StTrack;
class StMtdHit;

class StMuDst;
class StMuTrack;
class StMuMtdHit;


class StMtdEvtFilterMaker : public StMaker {
 public:
  StMtdEvtFilterMaker(const Char_t *name = "StMtdEvtFilterMaker");
  ~StMtdEvtFilterMaker();

  Int_t    Init();
  Int_t    InitRun(const Int_t runNumber);
  Int_t    Make();


  void     setSaveHistos(const bool save)    { mSaveHistos = save; }
  bool     isRejectEvent();
  int      shouldHaveRejectEvent();

  bool     isMuonCandidate(StTrack *track);
  bool     isMuonCandidate(StMuTrack *track);

  // setting functions
  void     setMinTrackPt(const double min)            { mMinTrkPtAll = min;             }
  void     setMinLeadTrackPt(const double min)        { mMinTrkPtLead = min;            }
  void     setMinNHitsFit(const int min)              { mMinNHitsFit = min;             }
  void     setMinNHitsDedx(const int min)             { mMinNHitsDedx = min;            }
  void     setMinFitHitsFraction(const double min)    { mMinFitHitsFraction = min;      }
  void     setMaxDca(const double max)                { mMaxDca = max;                  }
  void     setMaxDeltaZ(const double max)             { mMaxDeltaZ = max;               }
  void     setMinMuonCandidates(const int min)        { nMinMuonCandidates = min;       }
  void     setNsigmaPiCut(const double min, const double max)
  { mMinNsigmaPi = min; mMaxNsigmaPi = max; }

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
  }

  typedef vector<int>  IntVec;
  

 private:
  void     checkTriggerIDs(const vector<unsigned int> triggers);

  int      processStEvent();
  int      processMuDst();
  void     bookHistos();

  bool     isValidTrack(StTrack *track);
  bool     isValidTrack(StMuTrack *track);

  StEvent        *mStEvent;                      // StEvent pointer
  StMuDst        *mMuDst;                        // MuDst pointer
  bool           mIsJpsiEvent;                   // Flag of interesting events
  IntVec         mTriggerIDs;                    // Di-muon trigger id
  IntVec         mOtherTrigIDs;                  // Single-muon and e-mu trigger id
  bool           mIsDiMuon;                      // Flag if a event is triggered by di-muon trigger
  bool           mIsDiMuonOnly;                  // Flag if a event is ONLY triggered by di-muon trigger

  double         mMinTrkPtAll;                   // Minimum pt for all tracks
  double         mMinTrkPtLead;                  // Minimum pt for leading tracks
  int            mMinNHitsFit;                   // Minimum number of hits used for track fit
  int            mMinNHitsDedx;                  // Minimum number of hits used for de/dx
  double         mMinFitHitsFraction;            // Minimum fraction of # of hits used for fit out of # of possible hits
  double         mMaxDca;                        // Maximum track dca
  double         mMinNsigmaPi;                   // Minimum nsigma for pion assumption
  double         mMaxNsigmaPi;                   // Maximum nsigma for pion assumption
  double         mMaxDeltaZ;                     // Maximum dz for track-hit pairs
  int            nMinMuonCandidates;             // Minimum number of muons


  // List of histograms
  bool           mSaveHistos;
  TH1F           *mhEventStat;
  TH1F           *mhNMuonCandidates;
  
  ClassDef(StMtdEvtFilterMaker, 0)
};


#endif


// $Id: StMtdEvtFilterMaker.h,v 1.3 2015/07/29 01:11:15 smirnovd Exp $
// $Log: StMtdEvtFilterMaker.h,v $
// Revision 1.3  2015/07/29 01:11:15  smirnovd
// C++11 requires a space between user-defined and string literals
//
// Revision 1.2  2015/04/23 21:10:19  marr
// 1. remove dz and pTlead cuts in the filtering by default
// 2. change the number scheme for shouldHaveRejectEvent()
//
// Revision 1.1  2015/04/07 14:10:37  jeromel
// First version of StMtdEvtFilterMaker - R.Ma - review closed 2015/04/06
//
//

