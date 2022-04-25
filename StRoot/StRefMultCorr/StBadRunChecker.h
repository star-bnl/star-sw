#ifndef __StBadRunChecker_h__
#define __StBadRunChecker_h__

#include <vector>
#include <map>
#include "TString.h"

#include "BadRunSubSys.h"
//______________________________________________________________________________
class StBadRunChecker {
 public:
  StBadRunChecker(TString Run="run19",TString CollisionMode="col" ,TString RunEnergy="19.6",TString Species="auau");
  /*
  Please follow the following naming convetions when calling StBadRunChecker (case insensitive).
  So far, the available data sets are:
  | RunYear  | CollisionMode | Energy    | Species |
  | "run19"  | "col"         | "19.6"    | "auau"  |
  */
  virtual ~StBadRunChecker(); /// Default destructor
 
  // Bad run rejection
  Bool_t isInjection(const Int_t RunId) ;
  Bool_t isBadRunTPC(const Int_t RunId) ;
  Bool_t isBadRunbTOFStatus(const Int_t RunId) ;
  Bool_t isBadRunbTOFPID(const Int_t RunId) ;
  Bool_t isBadRuneTOF(const Int_t RunId) ;
  Bool_t isBadRunEPD(const Int_t RunId) ;
  Bool_t isBadRunVPD(const Int_t RunId) ;
  Bool_t isBadRunBEMCStatus(const Int_t RunId) ;
  Bool_t isBadRunBEMCPID(const Int_t RunId) ;
  Bool_t isBadRunBEMCTrigger(const Int_t RunId) ;
  Bool_t isBadRunMTD(const Int_t RunId) ;
  Bool_t isBadRunAnalysis(const Int_t RunId) ; 
  Bool_t isBadRunSubSys(const Int_t RunId,TString mSys) ;
  /*
  Eligible sub-system names (mSys) are the ones in the mSubSysName[12] array. 
  Case insensitive; delimiter doesn't matter, comma, space... whichever you prefer.
  Example: isBadRunSubSys(20056032,"TPC,etof,epd"); 
  It will return 1 if 20056032 is marked bad by TPC OR eTOF OR EPD, return 0 otherwise.
  Note: if a run is marked bad by "injection", it means this run is a run during injection,
  we usually exclude those injection runs in the analysis. 
  */

 private:
  void readBadRunsFromHeaderFile();
  std::vector<Int_t> mBadRun_all; /// Bad run number list
  std::vector<std::vector<Int_t>> mBadRun_sub;
  std::vector<Int_t> mRunRange;
  //const TString mSubSysName[12]={"Injection","TPC","bTOFStatus","bTOFPID","eTOF","EPD","VPD","BEMCStatus","BEMCPID","BEMCTrigger","MTD","Analysis"};
  TString mSubSysName[12]={"Injection","TPC","bTOFStatus","bTOFPID","eTOF","EPD","VPD","BEMCStatus","BEMCPID","BEMCTrigger","MTD","Analysis"};
  TString mRun;
  TString mEnergy;
  TString mColMode;
  TString mSpecies;
  Int_t mRunIndex;
  ClassDef(StBadRunChecker, 0)
};
#endif