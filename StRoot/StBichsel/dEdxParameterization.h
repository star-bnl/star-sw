
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
#include "StPidParticleDefinition.h"
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
  TH1D        *fTrs[KPidParticles+1][6]; //! Histograms from TpcRS dE/dx simulation for each particle type
 public :
    dEdxParameterization(const Char_t *Tag="p10", Int_t keep3D = 0,
			 const Double_t MostProbableZShift = 0,
			 const Double_t AverageZShift      = 0,
			 const Double_t I70Shift           = 1,
			 const Double_t I60Shift           = 1);
  virtual ~dEdxParameterization();
  Double_t    MostProbableZCorrection(Double_t log10bg);
  Double_t    I70Correction(Double_t log10bg);
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
  Double_t    GetProbability(Double_t log10bg, Double_t log2dx, Double_t z) {
    log10bg = TMath::Max(fbgL10min, TMath::Min(fbgL10max,log10bg));
    log2dx  = TMath::Max(fdxL2min, TMath::Min(fdxL2max,log2dx));
    z       = TMath::Max(fzmin, TMath::Min(fzmax,z));
    return fPhi->Interpolate(log10bg,log2dx,z);}
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
  const TH1D       *I70Trs(  Int_t part = KPidParticles) const {return fTrs[part][0];}  // Estimation for I70 from TpcRS
  const TH1D       *I70TrsB( Int_t part = KPidParticles) const {return fTrs[part][1];}  // Estimation for I70 - Bichsel from TpcRS
  const TH1D       *I70TrsS( Int_t part = KPidParticles) const {return fTrs[part][2];}  // Estimation for relative sigma beta*gamma dependence for I70 from TpcRS normalized to MIP
  const TH1D       *IfitTrs( Int_t part = KPidParticles) const {return fTrs[part][3];}  // Estimation for Ifit from TpcRS
  const TH1D       *IfitTrsB(Int_t part = KPidParticles) const {return fTrs[part][4];}  // Estimation for Ifit - Bichsel from TpcRS
  const TH1D       *IfitTrsS(Int_t part = KPidParticles) const {return fTrs[part][5];}  // Estimation for relative sigma beta*gamma dependence for Ifit from TpcRS normalized to MIP
  Double_t         Get(const TH1D *hist, Double_t log10bg) const;
  Double_t I70Trs  (Int_t part, Double_t log10bg) const {return Get(fTrs[part][0], log10bg);}  // Estimation for I70 from TpcRS
  Double_t I70TrsB (Int_t part, Double_t log10bg) const {return Get(fTrs[part][1], log10bg);}  // Estimation for I70 - Bichsel from TpcRS
  Double_t I70TrsS (Int_t part, Double_t log10bg) const {return Get(fTrs[part][2], log10bg);}  // Estimation for relative sigma beta*gamma dependence for I70 from TpcRS normalized to MIP
  Double_t IfitTrs (Int_t part, Double_t log10bg) const {return Get(fTrs[part][3], log10bg);}  // Estimation for Ifit from TpcRS
  Double_t IfitTrsB(Int_t part, Double_t log10bg) const {return Get(fTrs[part][4], log10bg);}  // Estimation for Ifit - Bichsel from TpcRS
  Double_t IfitTrsS(Int_t part, Double_t log10bg) const {return Get(fTrs[part][5], log10bg);}  // Estimation for relative sigma beta*gamma dependence for Ifit from TpcRS normalized to MIP

    
  ClassDef(dEdxParameterization,0)
};
// $Id: dEdxParameterization.h,v 1.10 2015/12/24 00:16:26 fisyak Exp $
// $Log: dEdxParameterization.h,v $
// Revision 1.10  2015/12/24 00:16:26  fisyak
// Add TpcRS model and macros
//
#endif
