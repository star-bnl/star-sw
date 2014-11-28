// $Id: StMuEEmcCrateTimingMaker.h,v 1.4 2005/02/01 23:35:46 jwebb Exp $

/*
 * \class StMuEEmcCrateTimingMaker
 * \author Dave Relyea
 *
 * Class for generating EEMC timing scans.  The maker is configured to use
 * the ezTree branches in the muDst, without access to the database.  ADC
 * spectra are accumulated/integrated from a fixed cut above pedestal to
 * the maximum.  macros/makeTimingFiles.C runs the code.  macros/plotTiming.C
 * produces plots from the output.  
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
  virtual ~StMuEEmcCrateTimingMaker(){ /* nada */ }
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
  /// Sets the minimum number of counts in a channel to
  /// analysed
  void setMinCounts( Int_t min );

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
  /// Minimum number of counts
  Int_t mMinCounts;
                
  /// Timing, integrals, errors and explicitly save
  /// crate and channel IDs in the TTree
  Float_t mTimeDelay;
  Float_t totalIntegral[MxMapmtFeeCh];
  Float_t totalError[MxMapmtFeeCh];
  Int_t   channelIds[MxMapmtFeeCh];
  Int_t   crateIds[MxMapmtFeeCh];
  Int_t   kludge;
  TH2F* cratehist;  /// MxMapmtFeeCh x ADC

 public: 
  
  ClassDef(StMuEEmcCrateTimingMaker, 1)
};

inline void StMuEEmcCrateTimingMaker::setNumberOfChannels( Int_t n ) { mNchannels = n; }
inline void StMuEEmcCrateTimingMaker::setMinCounts( Int_t c ) { mMinCounts = c; }
#endif
