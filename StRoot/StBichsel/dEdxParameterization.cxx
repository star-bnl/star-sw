//#define PRINT 1
#define PRINT0 1
//#define PRINT2 1
//#define PRINT3 1
//#define NumDer
#include <assert.h>
#include <Stiostream.h>
#include "dEdxParameterization.h"   
#include "TSystem.h"
#include "TROOT.h"
#include "TDirectory.h"
#include "TF1.h"
#include "TError.h" 
#include "TMath.h"

#define  PrP(A)  cout << "\t" << (#A) << " = \t" << ( A )

ClassImp(dEdxParameterization)
//________________________________________________________________________________
dEdxParameterization::dEdxParameterization(const Char_t *Tag, Int_t keep3D,
					   const Double_t MostProbableZShift,
					   const Double_t AverageZShift,
					   const Double_t I70Shift,
					   const Double_t I60Shift):
  fTag (Tag), fP(0), fA(0), fI70 (0), fI60(0), fD(0),  
  fRms (0), fW(0), fPhi(0),
  fMostProbableZShift(MostProbableZShift),
  fAverageZShift(AverageZShift),
  fI70Shift(I70Shift),
  fI60Shift(I60Shift),
  fbgL10min(-1), fbgL10max(4),
  fdxL2min(-0.3), fdxL2max(3),
  fzmin(-4), fzmax(6)
{
  memset (fTrs, 0, sizeof(fTrs));
  TDirectory *dir = gDirectory;
  const Char_t                                   *rootf = "P10T.root";
  if (fTag.Contains("pai" ,TString::kIgnoreCase)) rootf = "PaiT.root";
  if (fTag.Contains("p10" ,TString::kIgnoreCase)) rootf = "P10T.root";
  if (fTag.Contains("bich",TString::kIgnoreCase)) rootf = "BichselT.root";
  static const Char_t *path  = ".:./StarDb/dEdxModel:./StarDb/global/dEdx:./StRoot/StBichsel:$STAR/StarDb/dEdxModel:$STAR/StarDb/global/dEdx:$STAR/StRoot/StBichsel";
  Char_t *file = gSystem->Which(path,rootf,kReadPermission);
  if (! file) Fatal("dEdxParameterization::GetFile","File %s has not been found in path %s",rootf,path);
  else        Warning("dEdxParameterization::GetFile","File %s has been found as %s",rootf,file);
  TFile       *pFile = new TFile(file);
  delete [] file;
  assert(pFile);
  fP   = (TProfile2D *) pFile->Get("bichP");   assert(fP);   fP->SetDirectory(0);
  fA   = (TProfile2D *) pFile->Get("bichA");   assert(fA);   fA->SetDirectory(0);
  fI70 = (TProfile2D *) pFile->Get("bichI70"); assert(fI70); fI70->SetDirectory(0);
  fI60 = (TProfile2D *) pFile->Get("bichI60"); assert(fI60); fI60->SetDirectory(0);
  fD   = (TProfile2D *) pFile->Get("bichD");   assert(fD);   fD->SetDirectory(0);
  fRms = (TProfile2D *) pFile->Get("bichRms"); assert(fRms); fRms->SetDirectory(0);
  fW   = (TProfile2D *) pFile->Get("bichW");   assert(fW);   fW->SetDirectory(0);
  fPhi = (TH3D       *) pFile->Get("bichPhi"); assert(fPhi); fPhi->SetDirectory(0);
  fbgL10min = fPhi->GetXaxis()->GetBinCenter(1) + 1e-7;
  fbgL10max = fPhi->GetXaxis()->GetBinCenter(fPhi->GetXaxis()->GetNbins()) - 1e-7;
  fdxL2min  = fPhi->GetYaxis()->GetBinCenter(1) + 1e-7;
  fdxL2max  = fPhi->GetYaxis()->GetBinCenter(fPhi->GetYaxis()->GetNbins()) - 1e-7;
  fzmin  = fPhi->GetZaxis()->GetBinCenter(1) + 1e-7;
  fzmax  = fPhi->GetZaxis()->GetBinCenter(fPhi->GetZaxis()->GetNbins()) - 1e-7;
  if (dir) dir->cd();
  for (Int_t i = 0; i<3; i++) {
    if (i == 0) fAXYZ[i] = fPhi->GetXaxis(); 
    if (i == 1) fAXYZ[i] = fPhi->GetYaxis(); 
    if (i == 2) fAXYZ[i] = fPhi->GetZaxis(); 
    fnBins[i] = fAXYZ[i]->GetNbins(); 
    fbinW[i]  = fAXYZ[i]->GetBinWidth(1);
#ifdef PRINT0
    PrP(i); PrP(fnBins[i]); PrP(fbinW[i]); cout << endl;
    assert(fnBins[i] !=1);
#endif
  }
  //  if (! keep3D) SafeDelete(fPhi);
  // set normalization factor to 2.3976 keV/cm at beta*gamma = 4;
  static const Double_t dEdxMIP = 2.39761562607903311; // [keV/cm]
  static const Double_t MIPBetaGamma10 = TMath::Log10(4.);
  //  fMostProbableZShift = TMath::Log(dEdxMIP) - Interpolation(fP,MIPBetaGamma10,1,0);
  //  fAverageZShift      = TMath::Log(dEdxMIP) - Interpolation(fA,MIPBetaGamma10,1,0);
  fI70Shift           *= dEdxMIP/GetI70(MIPBetaGamma10,1);
  fI60Shift           *= dEdxMIP/GetI60(MIPBetaGamma10,1);
  fMostProbableZShift  = TMath::Log(fI70Shift);
  fAverageZShift       = fMostProbableZShift;
  const Char_t *Names[KPidParticles+1] = {"e","proton","kaon","pi","mu","deuteron","triton","He3","alpha","all"};
  for (Int_t i = 0; i <= KPidParticles; i++) {
    TString name(Names[i]);
    const Char_t *type[6] = {"70p","70","70S","zp","z","zS"};
    for (Int_t j = 0; j < 6; j++) {
      fTrs[i][j] = (TH1D *) pFile->Get(name + type[j]);
      if (fTrs[i][j])  fTrs[i][j]->SetDirectory(0);
    }
  }
  delete pFile;  
}
//________________________________________________________________________________
dEdxParameterization::~dEdxParameterization() { 
  SafeDelete(fP);  
  SafeDelete(fA);  
  SafeDelete(fI70);
  SafeDelete(fI60);
  SafeDelete(fD);  
  SafeDelete(fRms);
  SafeDelete(fW);
  SafeDelete(fPhi);
  for (Int_t i = 0; i <= KPidParticles; i++) 
    for (Int_t j = 0; j < 6; j++) {SafeDelete(fTrs[i][j]);}
}    
//________________________________________________________________________________
void dEdxParameterization::Print() {
  PrP(fTag); cout << endl;
  PrP(fP); if (fP) PrP(fP->GetTitle()); cout << endl;
  PrP(fA); if (fA) PrP(fA->GetTitle()); cout << endl;
  PrP(fI70); if (fI70) PrP(fI70->GetTitle()); cout << endl;
  PrP(fI60); if (fI60) PrP(fI60->GetTitle()); cout << endl;
  PrP(fD); if (fD) PrP(fD->GetTitle()); cout << endl;
  PrP(fRms); if (fRms) PrP(fRms->GetTitle()); cout << endl;
  PrP(fW); if (fW) PrP(fW->GetTitle()); cout << endl;
  PrP(fPhi); if (fPhi) PrP(fPhi->GetTitle()); cout << endl;
  PrP(fMostProbableZShift); cout << endl;
  PrP(fAverageZShift); cout << endl;
  PrP(fI70Shift); cout << endl;
  PrP(fI60Shift); cout << endl;
} 
//________________________________________________________________________________
Double_t dEdxParameterization::MostProbableZCorrection(Double_t log10bg) {
  static const Double_t pars[2] = {-3.68846e-03, 4.72944e+00}; // FitHzAllHist012P05id  FitH + Prof 050905
  return pars[0]*TMath::Exp(-pars[1]*log10bg);
}//________________________________________________________________________________
Double_t dEdxParameterization::I70Correction(Double_t log10bg) {
  static const Double_t pars[2] = {-1.65714e-02, 3.27271e+00}; //  FitH70AllHist012P05id FitH + Prof 050905
  return TMath::Exp(pars[0]*TMath::Exp(-pars[1]*log10bg));
}
//________________________________________________________________________________
Double_t dEdxParameterization::Get(const TH1D *hist, Double_t log10bg) const {
  static TH1D *hsave = 0;
  static Double_t xmin = -100, xmax = 100;
  if (hist != hsave) {
    hsave = (TH1D *) hist;
    TAxis *x = hsave->GetXaxis();
    Int_t f = x->GetFirst(); 
    Int_t l = x->GetLast(); 
    
    xmin = x->GetBinUpEdge(f);
    xmax = x->GetBinLowEdge(l);
  }
  if (log10bg < xmin) log10bg = xmin;
  if (log10bg > xmax) log10bg = xmax;
  return hsave->Interpolate(log10bg);
}
// $Id: dEdxParameterization.cxx,v 1.19 2016/06/10 19:56:11 fisyak Exp $
// $Log: dEdxParameterization.cxx,v $
// Revision 1.19  2016/06/10 19:56:11  fisyak
// Fix covertry warning
//
// Revision 1.18  2015/12/24 00:16:26  fisyak
// Add TpcRS model and macros
//
