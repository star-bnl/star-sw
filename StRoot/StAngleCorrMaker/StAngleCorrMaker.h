#ifndef StAngleCorrMaker_HH
#define StAngleCorrMaker_HH

#include "StMaker.h"
#include "TString.h"
#include "TFile.h"
#include "StAngleCorrAnalysisManager.h" 

#include "StAngleCorrFunction.h"
#include "StTrackCuts.h"
#include "StTrackForPool.h"
#include "StAngleCorrAnalysis.h"


class StAngleCorrMaker : public StMaker {

private:
  Bool_t drawinit;
  Char_t collectionName[256];
  
  StAngleCorrAnalysisManager corrAnalysis; //!  
  TFile* mOutput;
  TString track1,track2; 
  
public:
 
  StAngleCorrMaker(const Char_t *name="angleCorrMaker");
  virtual ~StAngleCorrMaker();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  
  void  AddAnalysis(TString analysisName);    
  void  SetCorrelationFunction(TString analysisName, TString functionName); 
  void  SetFastestTrackAnalysis(TString analysisName, int fastAnalysis);
  void  SetSignalHist(TString analysisName, TH1D* sHist);
  void  SetBackgroundHist(TString analysisName, TH1D* bHist);
  void  SetNBackgroundPairs(TString analysisName, int number);
  void  SetNBackgroundEvents(TString analysisName, int number);

  void  SetMomentumCutsTrack1(TString analysisName, double lowerCut, double upperCut);
  void  SetMomentumCutsTrack2(TString analysisName, double lowerCut, double upperCut);
  void  SetPtCutsTrack1(TString analysisName, double lowerCut, double upperCut);
  void  SetPtCutsTrack2(TString analysisName, double lowerCut, double upperCut);
  void  SetChargeTrack1(TString analysisName, Int_t charge);
  void  SetChargeTrack2(TString analysisName, Int_t charge);
  void  SetRapidityCutsTrack1(TString analysisName, double lowerCut, double upperCut);
  void  SetRapidityCutsTrack2(TString analysisName, double lowerCut, double upperCut);	 
  
  void  SetMultiplicityCuts(TString analysisName, double lowerCut, double upperCut);
 
  void SetDiagnosticsON(TString analysisName);
 
  
      virtual const char *GetCVS() const	{
	static const char cvs[]="Tag $Name:  $ $Id: StAngleCorrMaker.h,v 1.11 2000/02/20 16:53:28 horsley Exp $ built "__DATE__" "__TIME__ ;
	return cvs;
      }

 ClassDef(StAngleCorrMaker,1)
};

#endif

