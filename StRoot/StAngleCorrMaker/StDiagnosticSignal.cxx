#include "StDiagnosticSignal.h"
#include <TH3.h>
#include "StTrackForPool.h"

StDiagnosticSignal::StDiagnosticSignal() 
{
  Int_t NTPCBins=50;      Double_t lowerNTPCBin = 0.0;   Double_t upperNTPCBin = 50.0;
  Int_t NChi2Bins=50;  Double_t lowerChi2Bin = 0.0;  Double_t upperChi2Bin = 10.0;
  Int_t NRapBins=50;     Double_t lowerRapBin = 0.0;       Double_t upperRapBin = 2.0;
  Int_t NPBins=50;          Double_t lowerPBin = 0.0;           Double_t upperPBin = 10.0;
  Int_t NAngleBins=50;  Double_t lowerAngleBin = 0.0;   Double_t upperAngleBin = 180.0;

  signalCut1  = new TH3D("signalCut1","Signal Cuts: NTPC1 vs NTPC2 vs angle",NTPCBins,lowerNTPCBin,upperNTPCBin,NTPCBins,lowerNTPCBin,upperNTPCBin,NAngleBins,upperAngleBin,lowerAngleBin);
  signalCut2  = new TH3D("signalCut2","Signal Cuts: chi^2_1 vs chi^2_2 vs angle",NChi2Bins,lowerChi2Bin,upperChi2Bin,NChi2Bins,lowerChi2Bin,upperChi2Bin,NAngleBins,upperAngleBin,lowerAngleBin);
  // signalCut3  = new TH3D("signalCut3","Signal Cuts: chi_z1 vs chi_z2 vs angle",NChiZBins,lowerChiZBin,upperChiZBin,NChiZBins,lowerChiZBin,upperChiZBin,NAngleBins,upperAngleBin,lowerAngleBin);
  signalCut4  = new TH3D("signalCut4","Signal Cuts: rap1 vs rap2 vs angle",NRapBins,lowerRapBin,upperRapBin,NRapBins,lowerRapBin,upperRapBin,NAngleBins,upperAngleBin,lowerAngleBin);
  signalCut5  = new TH3D("signalCut5","Signal Cuts: p1 vs p2 vs angle",NPBins,lowerPBin,upperPBin,NPBins,lowerPBin,upperPBin,NAngleBins,upperAngleBin,lowerAngleBin);
  
}

StDiagnosticSignal::~StDiagnosticSignal() 
{
  
}

void 
StDiagnosticSignal::Write()
{
  // signalCut->Write();
}

void 
StDiagnosticSignal::SetCorrelationFunction(StAngleCorrFunction* func)
{
  corrFunc = func;
}

void 
StDiagnosticSignal::Fill(StTrackForPool* t1, StTrackForPool* t2) 
{
  Double_t weight = 1.0;
  Double_t corrValue = corrFunc->GetCorr(t1,t2);
  Double_t rap1,rap2,p1,p2;
  t1->GetPseudoRapidity(rap1);
  t2->GetPseudoRapidity(rap2);
  t1->GetMomentum(p1);
  t2->GetMomentum(p2);

  // fill histo's
  signalCut1->Fill(t1->GetNTPCPoints(),t2->GetNTPCPoints(),corrValue,weight);
  signalCut2->Fill(t1->GetChiSquared(),t2->GetChiSquared(),corrValue,weight);
  //signalCut3->Fill(t1->GetRChiSquaredZ(),t2->GetRChiSquaredZ(),corrValue,weight);
  signalCut4->Fill(rap1,rap2,corrValue,weight);
  signalCut5->Fill(p1,p2,corrValue,weight);
}


TString
StDiagnosticSignal::GetName() 
{
  TString name = "DiagnoseSignal";
  return name;
}
