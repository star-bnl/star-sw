// $Id: StAngleCorrMaker.h,v 1.8 2000/01/04 03:56:20 horsley Exp $
//
//

#ifndef StAngleCorrMaker_HH
#define StAngleCorrMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StAngleCorrMaker
//
// Description: 
// 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List:
//  Craig Olgivie, MIT 
//  Matt Horsley, Yale University
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
#include "StAngleCorrAnalysisManager.h"
#include "TString.h"
#include <vector>
#include "StAngleCorrFunction.h"

class StEvent;
class StRun;

class StAngleCorrMaker : public StMaker {

private:
  Bool_t drawinit;
  Char_t collectionName[256];
  StAngleCorrAnalysisManager corrAnalysis; 
  TString track1,track2;
  TFile* mOutput;

protected:
  
public:

                StAngleCorrMaker(const Char_t *name="angle corr maker");
  virtual ~StAngleCorrMaker();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Init();
                void  AddAnalysis(TString analysisName);
                void  SetCorrelationFunction(TString analysisName, TString functionName);
                void  SetMomentumCutsTrack1(TString analysisName, double lowerCut, double upperCut);
                void  SetMomentumCutsTrack2(TString analysisName, double lowerCut, double upperCut);
                void  SetPtCutsTrack1(TString analysisName, double lowerCut, double upperCut);
                void  SetPtCutsTrack2(TString analysisName, double lowerCut, double upperCut);
                void  SetChargeTrack1(TString analysisName, Int_t charge);
                void  SetChargeTrack2(TString analysisName, Int_t charge);
                void  SetRapidityCutsTrack1(TString analysisName, double lowerCut, double upperCut);
                void  SetRapidityCutsTrack2(TString analysisName, double lowerCut, double upperCut);	  
                void  SetFastestTrackAnalysis(TString analysisName, int fastAnalysis);
                void  SetSignalHist(TString analysisName, TH1D* sHist);
                void  SetBackgroundHist(TString analysisName, TH1D* bHist);
                void SetNBackgroundPairs(TString analysisName, int number, Double_t fraction);
                void SetNBackgroundEvents(TString analysisName, int number);
                void SetDiagnosticsON(TString analysisName);
                
  virtual Int_t Make();
  virtual Int_t Finish();

  ClassDef(StAngleCorrMaker, 1)
};

#endif

