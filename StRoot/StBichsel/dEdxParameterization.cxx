//#define PRINT 1
#define PRINT0 1
//#define PRINT2 1
//#define PRINT3 1
//#define NumDer
#include <assert.h>
#include <iostream.h>
#include "dEdxParameterization.h"   
#include "TSystem.h"
#include "TROOT.h"
#include "TDirectory.h"
#include "TF1.h"
#include "TError.h"

#define  PrP(A)  cout << "\t" << (#A) << " = \t" << ( A )

ClassImp(dEdxParameterization);
//________________________________________________________________________________
dEdxParameterization::dEdxParameterization(const Char_t *Tag, 
					   const Double_t MostProbableZShift,
					   const Double_t AverageZShift,
					   const Double_t I70Shift,
					   const Double_t I60Shift):
  fTag (Tag), fP(0), fA(0), fI70 (0), fI60(0), fD(0),  
  fRms (0), fW(0), fPhi(0),
  fMostProbableZShift(MostProbableZShift),
  fAverageZShift(AverageZShift),
  fI70Shift(I70Shift),
  fI60Shift(I60Shift)
{
  TDirectory *dir = gDirectory;
  Char_t            *rootf = "BichselT.root";
  if (fTag == "pai") rootf = "PaiT.root";
  static Char_t *path  = ".:./StarDb/dEdxModel:./StarDb/global/dEdx:./StRoot/StBichsel:$STAR/StarDb/dEdxModel:$STAR/StarDb/global/dEdx:$STAR/StRoot/StBichsel";
  Char_t *file = gSystem->Which(path,rootf,kReadPermission);
  if (! file) Fatal("dEdxParameterization::GetFile","File %s has not been found in path %s",rootf,path);
  else        Warning("dEdxParameterization::GetFile","File %s has been found as %s",rootf,file);
  TFile       *pFile = new TFile(file);
  assert(pFile);
  fP   = (TProfile2D *) pFile->Get(Form("%sP",fTag.Data()));   assert(fP);   fP->SetDirectory(0);
  fA   = (TProfile2D *) pFile->Get(Form("%sA",fTag.Data()));   assert(fA);   fA->SetDirectory(0);
  fI70 = (TProfile2D *) pFile->Get(Form("%sI70",fTag.Data())); assert(fI70); fI70->SetDirectory(0);
  fI60 = (TProfile2D *) pFile->Get(Form("%sI60",fTag.Data())); assert(fI60); fI60->SetDirectory(0);
  fD   = (TProfile2D *) pFile->Get(Form("%sD",fTag.Data()));   assert(fD);   fD->SetDirectory(0);
  fRms = (TProfile2D *) pFile->Get(Form("%sRms",fTag.Data())); assert(fRms); fRms->SetDirectory(0);
  fW   = (TProfile2D *) pFile->Get(Form("%sW",fTag.Data()));   assert(fW);   fW->SetDirectory(0);
  fPhi = (TH3D       *) pFile->Get(Form("%sPhi",fTag.Data())); assert(fPhi); fPhi->SetDirectory(0);
  delete pFile;  
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
};
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
};    
//________________________________________________________________________________
Double_t    dEdxParameterization::Interpolation(Int_t Narg, TH1 *hist, Double_t *XYZ, Int_t kase) {
  assert(hist);
  Int_t Ndim = hist->GetDimension();
  assert (Ndim>=1 && Ndim <= 3);
  assert (Ndim == Narg);
#if defined(PRINT) || defined(PRINT2)
  cout << "Interpolation:";
  PrP(X); PrP(Y); PrP(Z); 
  cout << endl;
#endif
  Int_t iXYZ[3], ixyz[3];
  Double_t dXYZ[3], pXYZ[3];
  Int_t i;
  for (i = 0; i< Ndim; i++) {
    iXYZ[i] = fAXYZ[i]->FindBin(XYZ[i]); 
    if (iXYZ[i] < 2) iXYZ[i] = 2; 
    if (iXYZ[i] >= fnBins[i])  iXYZ[i] =  fnBins[i] - 1; 
    dXYZ[i] = (XYZ[i] - fAXYZ[i]->GetBinCenter(iXYZ[i]))/fbinW[i];
    if (dXYZ[i] >=0) ixyz[i] = iXYZ[i] + 1;
    else            {ixyz[i] = iXYZ[i] - 1; dXYZ[i] = - dXYZ[i];}
    pXYZ[i] = 1. - dXYZ[i];
    if (i == kase - 1) {// derivatives wrt X
      dXYZ[i] = 1./fbinW[i];
      if (ixyz[i] < iXYZ[i]) dXYZ[i] = - dXYZ[i];
      pXYZ[i] =    -dXYZ[i]; 
    }
  } 
  
#ifdef PRINT
  for (i = 0; i<Ndim; i++) {PrP(i); PrP(iXYZ[i]);} cout << endl;
  for (i = 0; i<Ndim; i++) {PrP(i); PrP(dXYZ[i]);} cout << endl;
  for (i = 0; i<Ndim; i++) {PrP(i); PrP(pXYZ[i]);} cout << endl;
  
  PrP(iXYZ[0]);PrP(iXYZ[1]);PrP(iXYZ[2]); PrP(hist->GetBinContent(iXYZ[0],iXYZ[1],iXYZ[2])); cout << endl; 
  PrP(ixyz[0]);PrP(iXYZ[1]);PrP(iXYZ[2]); PrP(hist->GetBinContent(ixyz[0],iXYZ[1],iXYZ[2])); cout << endl; 
  PrP(iXYZ[0]);PrP(ixyz[1]);PrP(iXYZ[2]); PrP(hist->GetBinContent(iXYZ[0],ixyz[1],iXYZ[2])); cout << endl; 
  PrP(ixyz[0]);PrP(ixyz[1]);PrP(iXYZ[2]); PrP(hist->GetBinContent(ixyz[0],ixyz[1],iXYZ[2])); cout << endl;
  PrP(iXYZ[0]);PrP(iXYZ[1]);PrP(ixyz[2]); PrP(hist->GetBinContent(iXYZ[0],iXYZ[1],ixyz[2])); cout << endl; 
  PrP(ixyz[0]);PrP(iXYZ[1]);PrP(ixyz[2]); PrP(hist->GetBinContent(ixyz[0],iXYZ[1],ixyz[2])); cout << endl; 
  PrP(iXYZ[0]);PrP(ixyz[1]);PrP(ixyz[2]); PrP(hist->GetBinContent(iXYZ[0],ixyz[1],ixyz[2])); cout << endl; 
  PrP(ixyz[0]);PrP(ixyz[1]);PrP(ixyz[2]); PrP(hist->GetBinContent(ixyz[0],ixyz[1],ixyz[2])); cout << endl;
#endif  
  Double_t Value = 0;
  switch (Ndim) {
  case 3:  Value = 
	     pXYZ[0]*pXYZ[1]*pXYZ[2]*hist->GetBinContent(iXYZ[0],iXYZ[1],iXYZ[2]) + 
	     dXYZ[0]*pXYZ[1]*pXYZ[2]*hist->GetBinContent(ixyz[0],iXYZ[1],iXYZ[2]) + 
	     pXYZ[0]*dXYZ[1]*pXYZ[2]*hist->GetBinContent(iXYZ[0],ixyz[1],iXYZ[2]) + 
	     dXYZ[0]*dXYZ[1]*pXYZ[2]*hist->GetBinContent(ixyz[0],ixyz[1],iXYZ[2]) +
	     pXYZ[0]*pXYZ[1]*dXYZ[2]*hist->GetBinContent(iXYZ[0],iXYZ[1],ixyz[2]) + 
	     dXYZ[0]*pXYZ[1]*dXYZ[2]*hist->GetBinContent(ixyz[0],iXYZ[1],ixyz[2]) + 
	     pXYZ[0]*dXYZ[1]*dXYZ[2]*hist->GetBinContent(iXYZ[0],ixyz[1],ixyz[2]) + 
	     dXYZ[0]*dXYZ[1]*dXYZ[2]*hist->GetBinContent(ixyz[0],ixyz[1],ixyz[2]);
  break;
  case 2: Value = 
	    pXYZ[0]*pXYZ[1]*hist->GetBinContent(iXYZ[0],iXYZ[1]) + 
	    dXYZ[0]*pXYZ[1]*hist->GetBinContent(ixyz[0],iXYZ[1]) + 
	    pXYZ[0]*dXYZ[1]*hist->GetBinContent(iXYZ[0],ixyz[1]) + 
	    dXYZ[0]*dXYZ[1]*hist->GetBinContent(ixyz[0],ixyz[1]);
  break;
  case 1: Value = 
	    pXYZ[0]*hist->GetBinContent(iXYZ[0]) + dXYZ[0]*hist->GetBinContent(ixyz[0]);
  break;
  default:
    assert(0);
  }
#if defined(PRINT) || defined(PRINT2)
  PrP(Value); cout << endl;
#endif
  return Value;
}
//________________________________________________________________________________
Double_t    dEdxParameterization::Interpolation(TH3 *hist, Double_t X, Double_t Y, Double_t Z, Int_t kase) {
  assert(hist);
#if defined(PRINT) || defined(PRINT2)
  cout << "Interpolation:";
  PrP(X); PrP(Y); PrP(Z); 
  cout << endl;
#endif
  Double_t XYZ[3];
  XYZ[0] = X;
  XYZ[1] = Y;
  XYZ[2] = Z;
  return Interpolation(3, hist, XYZ, kase);
}
//________________________________________________________________________________
Double_t    dEdxParameterization::Interpolation(TH2 *hist, Double_t X, Double_t Y, Int_t kase) {
  assert(hist);
#ifdef PRINT 
  cout << "Interpolation:";
  PrP(X); PrP(Y); cout << endl;
#endif
  Double_t XYZ[2];
  XYZ[0] = X;
  XYZ[1] = Y;
  return Interpolation(2, hist, XYZ, kase);
}
//________________________________________________________________________________
Double_t   dEdxParameterization:: Interpolation(TH1 *hist, Double_t X, Int_t kase) {
  assert(hist);
#ifdef PRINT 
  cout << "Interpolation:";
  PrP(X); cout << endl;
#endif
  Double_t XYZ[1];
  XYZ[0] = X;
  return Interpolation(1, hist, XYZ, kase);
}
