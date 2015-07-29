/*!
 * \class StMtdTrackingMaskMaker 
 * \author Rongrong Ma
 * \brief This class finds the MTD hits that actually fire the trigger,
 * and mask the correponding TPC sectors for tracking in the subsequent step.
 * It runs on both StEvent and MuDst.
 */


#ifndef STMTDTRACKINGMASKMAKER_HH
#define STMTDTRACKINGMASKMAKER_HH

#include "StMaker.h"
#include "StMtdUtil/StMtdConstants.h"

class TH1F;
class TH2F;

class StEvent;
class StTriggerData;
class StMtdHit;

class StMuDst;
class StMuMtdHit;

class StMtdTrackingMaskMaker : public StMaker {
 public:

  StMtdTrackingMaskMaker(const Char_t *name = "StMtdTrackingMaskMaker");
  ~StMtdTrackingMaskMaker();

  Int_t    Init();
  Int_t    InitRun(const Int_t runNumber);
  void 	   Clear(Option_t *option="");
  Int_t    Make();
  void     setSaveHistos(const bool save)    { mSaveHistos = save; }

  UInt_t   getTrackingMask()                 { return mTpcSectorsForTracking; }

  void     processTriggerData();
  void     findFriedTpcSector();

  bool     isMtdHitFiredTrigger(const StMtdHit *hit);
  bool     isMtdHitFiredTrigger(const StMuMtdHit *hit);
  bool     isQTFiredTrigger(const int qt, const int pos);

  void     determineTpcTrackingMask();
  void     findTpcSectorsForTracking(const double hit_phi, const int hit_module);

  typedef vector<int>  IntVec;
  IntVec   findWestTpcSectors(const double hit_phi);
  IntVec   findEastTpcSectors(const double hit_phi);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;}

 private:
  Int_t    processStEvent();
  Int_t    processMuDst();
  void     bookHistos();

  double   getMtdHitGlobalPhi(const int backleg, const int module, const int cell);
  double   rotatePhi(const double phi);

  StEvent        *mStEvent;                      // StEvent pointer
  StMuDst        *mMuDst;                        // MuDst pointer
  IntVec         mTriggerIDs;                    // Di-muon trigger id
  bool           mIsDiMuon;                      // Flag if a event is triggered by di-muon trigger
  StTriggerData  *mTrigData;                     // Trigger data pointer

  int            mModuleToQT[gMtdNBacklegs][gMtdNModules];     // Map from module to QT board index
  int            mModuleToQTPos[gMtdNBacklegs][gMtdNModules];  // Map from module to the position on QT board
  int            mQTtoModule[4][8];                            // Map from QA board to module index
  int            mQTSlewBinEdge[4][16][8];                     // Bin edges for online slewing correction for QT
  int            mQTSlewCorr[4][16][8];                        // Slewing correction values for QT
  int            mTrigQTpos[4][2];               // Channel fires trigger in each QT

  IntVec         mFiredSectors;
  UInt_t         mTpcSectorsForTracking;         // 24-bit mask for partial tracking (sector 1 is least significant bit)


  // List of histograms for QA
  bool           mSaveHistos;
  TH1F           *mhEventStat;
  TH1F           *mhNQTsignals;
  TH1F           *mhNMIXsignals;
  TH1F           *mhNMuons;
  TH1F           *mhNMtdHits;
  TH1F           *mhNTrigMtdHits;
  TH1F           *mhNTpcSectorForTracking; 

  ClassDef(StMtdTrackingMaskMaker, 0)
};


#endif


// $Id: StMtdTrackingMaskMaker.h,v 1.3 2015/07/29 01:11:16 smirnovd Exp $
// $Log: StMtdTrackingMaskMaker.h,v $
// Revision 1.3  2015/07/29 01:11:16  smirnovd
// C++11 requires a space between user-defined and string literals
//
// Revision 1.2  2015/05/01 21:37:21  marr
// Apply online slewing correction and position correction to QT data to make
// sure the correct trigger patches are found offline.
//
// Revision 1.1  2015/04/07 14:10:37  jeromel
// First version of StMtdEvtFilterMaker - R.Ma - review closed 2015/04/06
//
//
