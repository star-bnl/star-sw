//#define PRINT 1
//#define PRINT2 1
//#define PRINT3 1
//#define NumDer
#include <assert.h>
#include <iostream.h>
#include "Bichsel.h"   
#include "TSystem.h"
#include "TROOT.h"
#include "TDirectory.h"
#include "TF1.h"
#include "TError.h"
TF1 *gFunc = 0;

#define  PrP(A)  cout << "\t" << (#A) << " = \t" << ( A )

ClassImp(Bichsel);
//________________________________________________________________________________
Bichsel::Bichsel():
fFile    (0),
fbichP   (0),  
fbichA   (0),  
fbichI70 (0),
fbichI60 (0),
fbichD   (0),  
fbichRms (0),
fbichW   (0),  
fbichPhi (0),
fFileName(0),
fnBins   (0),
fbinW    (0),
fMostProbableZShift(0),
fAverageZShft(0),
fI70Shft(1),
fI60Shift(1)
{//TFile *File, const Char_t *FileName) : fFileName(FileName){
  if (! fbichPhi) fbichPhi = GetHist3D("bichPhi");
  fnBins = new Int_t[3];
  fbinW  = new Double_t[3];
  for (Int_t i = 0; i<3; i++) {
    if (i == 0) fAXYZ[i] = fbichPhi->GetXaxis(); 
    if (i == 1) fAXYZ[i] = fbichPhi->GetYaxis(); 
    if (i == 2) fAXYZ[i] = fbichPhi->GetZaxis(); 
    fnBins[i] = fAXYZ[i]->GetNbins(); 
    fbinW[i]  = fAXYZ[i]->GetBinWidth(1);
    //#ifdef PRINT
    //    cout << "\ti =\t" << i << "\t Nbins =\t" << fnBins[i] << "\twidth =\t" << fbinW[i] << endl;
    PrP(i); PrP(fnBins[i]); PrP(fbinW[i]); cout << endl;
    assert(fnBins[i] !=1);
    //#endif
    fMostProbableZShift = 5.07402529167365057e-01;
    fAverageZShft       = 5.07402529167365057e-01;
    fI70Shft            = 9.16531837651389347e-01; 
    fI60Shift           = 9.75432754685096048e-01;
  }
};
//________________________________________________________________________________
Bichsel::~Bichsel() { 
  SafeDelete(fFile);
  SafeDelete(fbichP);  
  SafeDelete(fbichA);  
  SafeDelete(fbichI70);
  SafeDelete(fbichI60);
  SafeDelete(fbichD);  
  SafeDelete(fbichRms);
  SafeDelete(fbichW);
  SafeDelete(fFileName);
  SafeDelete(fbichPhi);
  SafeDelete(fnBins);
  SafeDelete(fbinW);
};    
//________________________________________________________________________________
TProfile2D *Bichsel::GetHist2D(const Char_t *HistName) {
  if (! fFile) GetFile();
  TProfile2D *hist = (TProfile2D *) fFile->Get(HistName);
  assert(hist);
  hist->SetDirectory(0);
  SafeDelete(fFile);
  return hist;
};
//________________________________________________________________________________
TH3D *Bichsel::GetHist3D(const Char_t *HistName) {
  if (! fFile) GetFile();
  TH3D *hist = (TH3D *) fFile->Get(HistName);
  assert(hist);
  hist->SetDirectory(0);
  SafeDelete(fFile);
  return hist;
};
//________________________________________________________________________________
void Bichsel::GetFile() {
  if (! fFileName) {
    static Char_t *rootf = "BichselT.root";
    static Char_t *path  = ".:./StRoot/StBichsel:$STAR/StRoot/StBichsel";
     Char_t *file = gSystem->Which(path,rootf,kReadPermission);
    if (! file) Fatal("Bichsel::GetFile","File %s has not been found in path %s",rootf,path);
    else        Warning("Bichsel::GetFile","File %s has been found as %s",rootf,file);
    fFileName = new TString(file);
  }
  TDirectory *dir = gDirectory;
#ifdef PRINT
  cout << "Save directory\t" << dir->GetName() << endl;
#endif
  fFile = new TFile(fFileName->Data());
  assert(fFile);
#ifdef PRINT
  cout << "cd to directory\t" << gDirectory->GetName() << endl;
#endif
  if (dir) dir->cd();
#ifdef PRINT
  cout << "cd to directory\t" << gDirectory->GetName() << endl;
#endif
}
//________________________________________________________________________________
Double_t    Bichsel::Interpolation(Int_t Narg, TH1 *hist, Double_t *XYZ, Int_t kase) {
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
Double_t    Bichsel::Interpolation(TH3 *hist, Double_t X, Double_t Y, Double_t Z, Int_t kase) {
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
Double_t    Bichsel::Interpolation(TH2 *hist, Double_t X, Double_t Y, Int_t kase) {
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
Double_t   Bichsel:: Interpolation(TH1 *hist, Double_t X, Int_t kase) {
  assert(hist);
#ifdef PRINT 
  cout << "Interpolation:";
  PrP(X); cout << endl;
#endif
  Double_t XYZ[1];
  XYZ[0] = X;
  return Interpolation(1, hist, XYZ, kase);
}
