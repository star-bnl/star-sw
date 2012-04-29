
#ifndef dEdxParameterization_h
#define dEdxParameterization_h

#include "TROOT.h"
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile2D.h"
#include "TString.h"
//#include "Bichsel.h"
//#define __OWN_INTERPOLATION__
class dEdxParameterization {
 private: 
  TString      fTag;                 //! Tag for  root file (Bichsel or PAI)
  TProfile2D  *fP;                   //! zm: The most probable value of ::log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fA;                   //! mean_z: The average value of z = ::log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fI70;                 //! I70: The average value after 30% truncation versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fI60;                 //! I60: The average value after 40% truncation versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fD;                   //! Delta_P : The most probable dE/dx versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fRms;                 //! sigma_z : The RMS value of z = ::log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TProfile2D  *fW;                   //! width : The RMS value of z = ::log(dE/dx) versus log10(beta*gamma) and log2(dx)
  TH3D        *fPhi;                 //! The dEdxParameterization probability versus log10(beta*gamma) and log2(dx) and z
  Int_t        fnBins[3];            //! no. of bin for each dimension (log10(bg), log2(dx) and z = ::log(dE/dx))
  Double_t     fbinW[3];             //! bin width
  TAxis       *fAXYZ[3];             //!
  Double_t     fMostProbableZShift;  //!
  Double_t     fAverageZShift;       //!
  Double_t     fI70Shift;            //!
  Double_t     fI60Shift;            //!
  Double_t     fbgL10min;
  Double_t     fbgL10max;
  Double_t     fdxL2min;
  Double_t     fdxL2max;
  Double_t     fzmin;
  Double_t     fzmax;
 public :
    dEdxParameterization(const Char_t *Tag="p10", Int_t keep3D = 0,
			 const Double_t MostProbableZShift = 0,
			 const Double_t AverageZShift      = 0,
			 const Double_t I70Shift           = 1,
			 const Double_t I60Shift           = 1);
  virtual ~dEdxParameterization();
  Double_t    MostProbableZCorrection(Double_t log10bg);
  Double_t    I70Correction(Double_t log10bg);
#ifdef  __OWN_INTERPOLATION__
  Double_t    Interpolation(Int_t Narg, TH1 *hist, Double_t *XYZ, Int_t kase = 0);
  Double_t    Interpolation(TH3 *hist, Double_t X, Double_t Y, Double_t Z, Int_t kase = 0);
  Double_t    Interpolation(TH2 *hist, Double_t X, Double_t Y, Int_t kase = 0);
  Double_t    Interpolation(TH1 *hist, Double_t X, Int_t kase = 0);
  Double_t    GetMostProbableZ(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    return fMostProbableZShift+Interpolation(fP,log10bg,log2dx,kase);
  }
  Double_t    GetAverageZ(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    return fAverageZShift+MostProbableZCorrection(log10bg)+Interpolation(fA,log10bg,log2dx,kase);
  }
  Double_t    GetRmsZ(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    return Interpolation(fRms,log10bg,log2dx,kase);
  }
  Double_t    GetI70(Double_t log10bg, Double_t log2dx, Int_t kase=0)  {
    return fI70Shift*Interpolation(fI70,log10bg,log2dx,kase);
  }
  Double_t    GetI60(Double_t log10bg, Double_t log2dx, Int_t kase=0)  {
    return fI60Shift*Interpolation(fI60,log10bg,log2dx,kase);
  }
  Double_t    GetMostProbabledEdx(Double_t log10bg, Double_t log2dx, Int_t kase = 0) {
    return Interpolation(fD,log10bg,log2dx,kase);
  }
  Double_t    GetdEdxWidth(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    return Interpolation(fW,log10bg,log2dx,kase);
  }
  Double_t    GetProbability(Double_t log10bg, Double_t log2dx, Double_t z, Int_t kase=0) {
    return Interpolation(fPhi,log10bg,log2dx,z,kase);}
  Double_t    GetMostProbableZM(Double_t log10bg, Double_t log2dx) {
    return MostProbableZCorrection(log10bg)+GetMostProbableZ(log10bg,log2dx,kase);
  }
  Double_t    GetAverageZM(Double_t log10bg, Double_t log2dx, Int_t kase=0) {
    return MostProbableZCorrection(log10bg)+GetAverageZ(log10bg,log2dx,kase);
  }
  Double_t    GetI70M(Double_t log10bg, Double_t log2dx, Int_t kase=0)  {
    return I70Correction(log10bg)*GetI70(log10bg,log2dx,kase);
  }
#else /* !  __OWN_INTERPOLATION__ */ 
  Double_t    GetMostProbableZ(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fMostProbableZShift+fP->Interpolate(log10bg,log2dx);
  }
  Double_t    GetAverageZ(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fAverageZShift+MostProbableZCorrection(log10bg)+fA->Interpolate(log10bg,log2dx);
  }
  Double_t    GetRmsZ(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fRms->Interpolate(log10bg,log2dx);
  }
  Double_t    GetI70(Double_t log10bg, Double_t log2dx)  {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fI70Shift*fI70->Interpolate(log10bg,log2dx);
  }
  Double_t    GetI60(Double_t log10bg, Double_t log2dx)  {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fI60Shift*fI60->Interpolate(log10bg,log2dx);
  }
  Double_t    GetMostProbabledEdx(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fD->Interpolate(log10bg,log2dx);
  }
  Double_t    GetdEdxWidth(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return fW->Interpolate(log10bg,log2dx);
  }
  Double_t    GetProbability(Double_t log10bg, Double_t log2dx, Double_t z) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx  = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    z       = TMath::Max(fzmin, TMath::Min(fzmax,z));
    return fPhi->Interpolate(log10bg,log2dx,z);}
  Double_t    GetMostProbableZM(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return MostProbableZCorrection(log10bg)+GetMostProbableZ(log10bg,log2dx);
  }
  Double_t    GetAverageZM(Double_t log10bg, Double_t log2dx) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return MostProbableZCorrection(log10bg)+GetAverageZ(log10bg,log2dx);
  }
  Double_t    GetI70M(Double_t log10bg, Double_t log2dx)  {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    return I70Correction(log10bg)*GetI70(log10bg,log2dx);
  }
#endif /*  __OWN_INTERPOLATION__ */
  void        Print();
  const Char_t      *Tag() const {return    fTag.Data();}   
  const TProfile2D  *P()   const {return     fP;}     
  const TProfile2D  *A()   const {return     fA;}     
  const TProfile2D  *I70() const {return   fI70;}   
  const TProfile2D  *I60() const {return   fI60;}   
  const TProfile2D  *D()   const {return     fD;}     
  const TProfile2D  *Rms() const {return   fRms;}   
  const TProfile2D  *W()   const {return     fW;}     
  const TH3D        *Phi() const {return   fPhi;}     
  Double_t bgL10min() const {return fbgL10min;}
  Double_t bgL10max() const {return fbgL10max;}
  ClassDef(dEdxParameterization,0)
};
#endif

