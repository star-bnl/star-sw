//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Nov 29 16:25:03 2001 by ROOT version3.02/01)
//   from TTree Bichsel/H.Bichel Calculation Summary
//   found on file: dEdx2T.root
//////////////////////////////////////////////////////////


#ifndef Bichsel_h
#define Bichsel_h

#include "TROOT.h"
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile2D.h"
class Bichsel {
 private: 
  TFile       *fFile;     //! local TFile with histograms which keep H.Bichsel tables
  TProfile2D  *fbichP;    //! zm: The most probable value of log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fbichA;    //! mean_z: The average value of z = log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fbichI70;  //! I70: The average value after 30% truncation versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fbichI60;  //! I60: The average value after 40% truncation versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fbichD;    //! Delta_P : The most probable dE/dx versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fbichRms;  //! sigma_z : The RMS value of z = log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fbichW;    //! width : The RMS value of z = log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TH3D        *fbichPhi;  //! The Bichsel probability versus log10(beta*gamma) and log2(dx) and z
  TString     *fFileName; //!
  Int_t       *fnBins;    //! no. of bin for each dimension (log10(bg), log2(dx) and z = log(dE/dx))
  Double_t    *fbinW;     //! bin width
  TAxis       *fAXYZ[3];  //!
  Double_t    fMostProbableZShift;
  Double_t    fAverageZShft;
  Double_t    fI70Shft;
  Double_t    fI60Shift;
 public :
  Bichsel();
  virtual ~Bichsel();
  void        GetFile();
  TProfile2D *GetHist2D(const Char_t *HistName = "");
  TH3D       *GetHist3D(const Char_t *HistName = "");
  Double_t    Interpolation(Int_t Narg, TH1 *hist, Double_t *XYZ, Int_t kase = 0);
  Double_t    Interpolation(TH3 *hist, Double_t X, Double_t Y, Double_t Z, Int_t kase = 0);
  Double_t    Interpolation(TH2 *hist, Double_t X, Double_t Y, Int_t kase = 0);
  Double_t    Interpolation(TH1 *hist, Double_t X, Int_t kase = 0);
  TAxis      *GetAxis(Int_t i) {return fAXYZ[i];}
  Int_t       GetnBins(Int_t i) {return fnBins[i];}
  Double_t    GetbinW(Int_t i) {return fbinW[i];}
  TString    *GetFileName() {return fFileName;}
  Double_t    GetMostProbableZ(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    if (!fbichP) fbichP = GetHist2D("bichP"); 
    return fMostProbableZShift+Interpolation(fbichP,log10bg,log2dx,kase);
  }
  Double_t    GetAverageZ(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    if (!fbichA) fbichA = GetHist2D("bichA"); 
    return fAverageZShft+Interpolation(fbichA,log10bg,log2dx,kase);
  }
  Double_t    GetRmsZ(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    if (!fbichRms) fbichRms = GetHist2D("bichRms"); 
    return Interpolation(fbichRms,log10bg,log2dx,kase);
  }
  Double_t    GetI70(Double_t log10bg, Double_t log2dx, Int_t kase=0)  {
    if (!fbichI70) fbichI70 = GetHist2D("bichI70"); 
    return fI70Shft*Interpolation(fbichI70,log10bg,log2dx,kase);
  }
  Double_t    GetI60(Double_t log10bg, Double_t log2dx, Int_t kase=0)  {
    if (!fbichI60) fbichI60 = GetHist2D("bichI60"); 
    return fI60Shift*Interpolation(fbichI60,log10bg,log2dx,kase);
  }
  Double_t    GetMostProbabledEdx(Double_t log10bg, Double_t log2dx, Int_t kase = 0) {
    if (!fbichD) fbichD = GetHist2D("bichD"); 
    return Interpolation(fbichD,log10bg,log2dx,kase);
  }
  Double_t    GetdEdxWidth(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    if (!fbichW) fbichW = GetHist2D("bichW"); 
    return Interpolation(fbichW,log10bg,log2dx,kase);
  }
  Double_t    GetProbability(Double_t log10bg, Double_t log2dx, Double_t z, Int_t kase=0) {
    if (!fbichPhi) fbichPhi = GetHist3D("bichPhi"); 
    return Interpolation(fbichPhi,log10bg,log2dx,z,kase);}
  ClassDef(Bichsel,0)
};
#endif

