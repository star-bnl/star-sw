#include "StDiagnosticBackground.h"
#include <TH3.h>
#include "StTrackForPool.h"


StDiagnosticBackground::StDiagnosticBackground() 
{
  
  Int_t NTPCBins=50;      
  Double_t lowerNTPCBin = 0.0;   
  Double_t upperNTPCBin = 50.0;

  Int_t NChiXYBins=50;  
  Double_t lowerChiXYBin = 0.0;  
  Double_t upperChiXYBin = 10.0;

  Int_t NChiZBins=50;   
  Double_t lowerChiZBin = 0.0;
  Double_t upperChiZBin = 10.0;

  Int_t NRapBins=50; 
  Double_t lowerRapBin = 0.0; 
  Double_t upperRapBin = 2.0;
  
  Int_t NPBins=50;  
  Double_t lowerPBin = 0.0;
  Double_t upperPBin = 10.0;

  Int_t NAngleBins=50;  
  Double_t lowerAngleBin = 0.0;  
  Double_t upperAngleBin = 180.0;

  backgroundCut1  = new TH3D("backgroundCut1","Background Cuts: NTPC1 vs NTPC2 vs angle",NTPCBins,lowerNTPCBin,upperNTPCBin,NTPCBins,lowerNTPCBin,upperNTPCBin,NAngleBins,upperAngleBin,lowerAngleBin);
  backgroundCut2  = new TH3D("backgroundCut2","Background Cuts: chi_xy1 vs chi_xy2 vs angle",NChiXYBins,lowerChiXYBin,upperChiXYBin,NChiXYBins,lowerChiXYBin,upperChiXYBin,NAngleBins,upperAngleBin,lowerAngleBin);
  backgroundCut3  = new TH3D("backgroundCut3","Background Cuts: chi_z1 vs chi_z2 vs angle",NChiZBins,lowerChiZBin,upperChiZBin,NChiZBins,lowerChiZBin,upperChiZBin,NAngleBins,upperAngleBin,lowerAngleBin);
  backgroundCut4  = new TH3D("backgroundCut4","Background Cuts: rap1 vs rap2 vs angle",NRapBins,lowerRapBin,upperRapBin,NRapBins,lowerRapBin,upperRapBin,NAngleBins,upperAngleBin,lowerAngleBin);
  backgroundCut5  = new TH3D("backgroundCut5","Background Cuts: p1 vs p2 vs angle",NPBins,lowerPBin,upperPBin,NPBins,lowerPBin,upperPBin,NAngleBins,upperAngleBin,lowerAngleBin);

}

StDiagnosticBackground::~StDiagnosticBackground() 
{
}

void 
StDiagnosticBackground::Write()
{
  //   backgroundCut1->Write();
}

void 
StDiagnosticBackground::SetCorrelationFunction(StAngleCorrFunction* func)
{
  corrFunc = func;
}

void 
StDiagnosticBackground::Fill(StTrackForPool* t1, StTrackForPool* t2) 
{
  
  Double_t weight = 1;
  Double_t corrValue = corrFunc->GetCorr(t1,t2);
  Double_t rap1,rap2,p1,p2;
  t1->GetPseudoRapidity(rap1);
  t2->GetPseudoRapidity(rap2);
  t1->GetMomentum(p1);
  t2->GetMomentum(p2);

  // fill histo's
  backgroundCut1->Fill(t1->GetNTPCPoints(),t2->GetNTPCPoints(),corrValue,weight);
  backgroundCut2->Fill(t1->GetRChiSquaredXY(),t2->GetRChiSquaredXY(),corrValue,weight);
  backgroundCut3->Fill(t1->GetRChiSquaredZ(),t2->GetRChiSquaredZ(),corrValue,weight);
  backgroundCut4->Fill(rap1,rap2,corrValue,weight);
  backgroundCut5->Fill(p1,p2,corrValue,weight);
}

TString
StDiagnosticBackground::GetName() 
{
  TString name = "DiagnoseBackground";
  return name;
}
