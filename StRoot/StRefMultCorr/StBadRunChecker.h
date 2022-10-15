#ifndef __StBadRunChecker_h__
#define __StBadRunChecker_h__

// C++ headers
#include <vector>
#include <map>

// ROOT headers
#include "TString.h"

// STAR headers
#include "BadRunSubSys.h"

//________________
class StBadRunChecker {
 public:
  /**
   * @brief Default constructor
   * 
   * Please follow the following naming convetions when calling StBadRunChecker (case insensitive).
   * So far, the available data sets are:
   * | RunYear  | CollisionMode | Energy    | Species |
   * | "run19"  | "col"         | "19.6"    | "auau"  |
   */
  StBadRunChecker(TString Run="run19",TString CollisionMode="col" ,TString RunEnergy="19.6",TString Species="auau");
  
  /// Default destructor
  virtual ~StBadRunChecker(); 
 
  /// Check if injection run
  Bool_t isInjection(const Int_t RunId);
  /// Check if bad run defined via TPC information
  Bool_t isBadRunTPC(const Int_t RunId);
  /// Check if bad run defined via TOF information
  Bool_t isBadRunbTOFStatus(const Int_t RunId);
  /// Check if bad run defined via TOF PID information
  Bool_t isBadRunbTOFPID(const Int_t RunId);
  /// Check if bad run defined via eTOF information
  Bool_t isBadRuneTOF(const Int_t RunId);
  /// Check if bad run defined via EPD information
  Bool_t isBadRunEPD(const Int_t RunId);
  /// Check if bad run defined via VPD information
  Bool_t isBadRunVPD(const Int_t RunId);
  /// Check if bad run defined via BEMC information
  Bool_t isBadRunBEMCStatus(const Int_t RunId);
  /// Check if bad run defined via BEMC PID information
  Bool_t isBadRunBEMCPID(const Int_t RunId);
  /// Check if bad run defined via BEMC trigger information
  Bool_t isBadRunBEMCTrigger(const Int_t RunId);
  /// Check if bad run defined via MTD information
  Bool_t isBadRunMTD(const Int_t RunId);
  /// Check if bad run
  Bool_t isBadRunAnalysis(const Int_t RunId);
  /// @brief Check if bad run via subsystem information
  ///   Eligible sub-system names (mSys) are the ones in the mSubSysName[12] array. 
  ///   Case insensitive; delimiter doesn't matter, comma, space... whichever you prefer.
  ///   Example: isBadRunSubSys(20056032,"TPC,etof,epd"); 
  ///   It will return 1 if 20056032 is marked bad by TPC OR eTOF OR EPD, return 0 otherwise.
  ///
  ///   Note: if a run is marked bad by "injection", it means this run is a run during injection,
  ///   we usually exclude those injection runs in the analysis. 
  ///
  /// @param RunId Run index
  /// @param mSys Subsystem name
  /// @return 
  Bool_t isBadRunSubSys(const Int_t RunId,TString mSys);

 private:
  
  /// Read bad runs from header file
  void readBadRunsFromHeaderFile();
  /// Bad run number list
  std::vector<Int_t> mBadRun_all; 
  /// Bad run list for sybsystem
  std::vector<std::vector<Int_t>> mBadRun_sub;
  /// Run range
  std::vector<Int_t> mRunRange;
  //const TString mSubSysName[12]={"Injection","TPC","bTOFStatus","bTOFPID","eTOF","EPD","VPD","BEMCStatus","BEMCPID","BEMCTrigger","MTD","Analysis"};

  /// Subsystem names
  TString mSubSysName[12]={"Injection","TPC","bTOFStatus","bTOFPID","eTOF","EPD","VPD","BEMCStatus","BEMCPID","BEMCTrigger","MTD","Analysis"};
  /// Run year
  TString mRun;
  /// Energy
  TString mEnergy;
  /// Collider mode
  TString mColMode;
  /// Particle species
  TString mSpecies;
  /// Run index
  Int_t mRunIndex;

  ClassDef(StBadRunChecker, 0)
};
#endif