// $Id: StMuEEmcCrateTimingMaker.h,v 1.1 2005/01/28 19:02:00 jwebb Exp $

/*
 * \class StMuEEmcCrateTimingMaker
 * \author Dave Relyea
 *
 */

#ifndef STAR_StMuEEmcCrateTimingMaker
#define STAR_StMuEEmcCrateTimingMaker

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "TFile.h"
#include "TTree.h"
#include "TH2.h"

class TObjArray  ;
class StMuDstMaker;
class StTriggerData ;
class EztEmcRawData;


class StMuEEmcCrateTimingMaker : public StMaker {
 public: 
  
  StMuEEmcCrateTimingMaker(StMuDstMaker* mudstmaker);
  virtual ~StMuEEmcCrateTimingMaker(){}
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  
  /// Select "mapmt" for processing mapmt scans, 
  /// "tower" for processing tower scans.
  void setFlavor(TString testFlavor = "tower") {mFlavor = testFlavor;}
  /// Specify the delay setting for this run.
  void setTiming(Float_t timeDelay) {mTimeDelay = timeDelay;}
  /// Sets the "phase", i.e. which channel in the crate or
  /// box we start with.
  void setPhase(Int_t phase) {mPhase = phase;}
  /// Output directory
  void setDirectory(TString directory) {mDirectory = directory;}

  /// Sets the number of channels above pedestal 
  void setNumberOfChannels( Int_t n=25 );
  /// Sets the number of sigma above pedestal (if set,
  /// overrides the nchannel cut above).
  void setNumberOfSigma( Int_t n=10 );

 private:
  const static int  MxMapmtFeeCh= (MaxMapmtCrates* MaxMapmtCrateCh / 16) + 1;
  // we're taking every sixteenth channel...
  StMuDstMaker* mMuDstMaker;
  TFile* mOutputFile; //!
  TTree* mOutputTree; //!
  TString mFlavor;
  TString mDirectory;

  // Integrate every 16th channel starting from mPhase
  Int_t mPhase; 
  Int_t mCycle; // = 16, hardcoded for now...

  // Number of channels above pedestal to cut (default mode)
  Int_t mNchannels;
  // Number of sigma above pedestal to cut (only if set)
  Float_t mNsigma;
                
  Float_t mTimeDelay;
  Float_t totalIntegral[MxMapmtFeeCh];
  Int_t kludge;
  TH2F* cratehist;

 public: 
  
  ClassDef(StMuEEmcCrateTimingMaker, 1)
};

inline void StMuEEmcCrateTimingMaker::setNumberOfChannels( Int_t n ) { mNchannels = n; }

#endif
