//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Nov 29 16:25:03 2001 by ROOT version3.02/01)
//   from TTree Bichsel/H.Bichel Calculation Summary
//   found on file: dEdx2T.root
//////////////////////////////////////////////////////////


#ifndef Bichsel_h
#define Bichsel_h
//#define P03ia
#include "TString.h"
#include "dEdxParameterization.h"
class tpcCorrection_st;
class Bichsel {
 public: 
  enum EParTypes {kP10, kBichsel, kPAI, kTotal};
 private: 
  static TString        m_Tags[kTotal];
  Int_t                 m_Type;
  TString               m_Tag;
  static dEdxParameterization *m_dEdxParameterizations[kTotal]; //!
  dEdxParameterization *m_dEdxParameterization; //!
  static Bichsel       *fgBichsel; //! last instance          
 public:
  Bichsel(const Char_t *tag="P10", Int_t keep3D=0);
  virtual ~Bichsel() {fgBichsel = 0;};
  static Bichsel* Instance(const Char_t *tag="P10", Int_t keep3D=0);
  static Double_t GetdEdxResolution(Int_t k=1, Double_t TrackLengthInTPC=60);
  static Double_t GetdEdxResolution(Double_t *x, Double_t *p);
  static Double_t CalcCorrection(const tpcCorrection_st *cor,const Double_t x);
  static Double_t SumSeries(const Double_t &X,const Int_t &N,const Double_t *params);
  static void Clean();
  Double_t    GetMostProbableZ(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetMostProbableZ(log10bg,log2dx);
  }
  Double_t    GetMostProbableZM(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetMostProbableZM(log10bg,log2dx);
  }
  Double_t    GetAverageZ(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetAverageZ(log10bg,log2dx);
  }
  Double_t    GetAverageZM(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetAverageZM(log10bg,log2dx);
  }
  Double_t    GetRmsZ(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetRmsZ(log10bg,log2dx);
  }
  Double_t    GetI70(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetI70(log10bg,log2dx);
  }
  Double_t    GetI70M(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetI70M(log10bg,log2dx);
  }
  Double_t    GetI60(Double_t log10bg, Double_t log2dx = 1.)  {
    return m_dEdxParameterization->GetI60(log10bg,log2dx);
  }
  Double_t    GetMostProbabledEdx(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetMostProbabledEdx(log10bg,log2dx);
  }
  Double_t    GetdEdxWidth(Double_t log10bg, Double_t log2dx = 1.) {
    return m_dEdxParameterization->GetdEdxWidth(log10bg,log2dx);
  }
  Double_t    GetProbability(Double_t log10bg, Double_t log2dx, Double_t z) {
    return m_dEdxParameterization->GetProbability(log10bg,log2dx,z);}
  const dEdxParameterization *Parameterization() const {return m_dEdxParameterization;}
  virtual void Print();
  const Char_t      *Tag() const {return    m_dEdxParameterization->Tag();}   
  const TProfile2D  *P()   const {return     m_dEdxParameterization->P();}     
  const TProfile2D  *A()   const {return     m_dEdxParameterization->A();}     
  const TProfile2D  *I70() const {return   m_dEdxParameterization->I70();}   
  const TProfile2D  *I60() const {return   m_dEdxParameterization->I60();}   
  const TProfile2D  *D()   const {return     m_dEdxParameterization->D();}     
  const TProfile2D  *Rms() const {return   m_dEdxParameterization->Rms();}   
  const TProfile2D  *W()   const {return     m_dEdxParameterization->W();}     
  const TH3D        *Phi() const {return   m_dEdxParameterization->Phi();}     
  const TH1D       *I70Trs  (Int_t part = KPidParticles) const {return m_dEdxParameterization->I70Trs( part);}  // Estimation for I70 from TpcRS
  const TH1D       *I70TrsB (Int_t part = KPidParticles) const {return m_dEdxParameterization->I70TrsB(part);}  // Estimation for I70 - Bichsel from TpcRS
  const TH1D       *I70TrsS (Int_t part = KPidParticles) const {return m_dEdxParameterization->I70TrsS(part);}  // Estimation for relative sigma bg dependence for I70 from TpcRS normalized to MIP
  const TH1D       *IfitTrs (Int_t part = KPidParticles) const {return m_dEdxParameterization->IfitTrs(part);}  // Estimation for Ifit from TpcRS
  const TH1D       *IfitTrsB(Int_t part = KPidParticles) const {return m_dEdxParameterization->IfitTrsB(part);} // Estimation for Ifit - Bichsel from TpcRS
  const TH1D       *IfitTrsS(Int_t part = KPidParticles) const {return m_dEdxParameterization->IfitTrsS(part);} // Estimation for relative sigma bg dependence for Ifit from TpcRS normalized to MIP
  Double_t I70Trs  (Int_t part, Double_t log10bg) const {return m_dEdxParameterization->Get(I70Trs  (part), log10bg);}  // Estimation for I70 from TpcRS
  Double_t I70TrsB (Int_t part, Double_t log10bg) const {return m_dEdxParameterization->Get(I70TrsB (part), log10bg);}  // Estimation for I70 - Bichsel from TpcRS
  Double_t I70TrsS (Int_t part, Double_t log10bg) const {return m_dEdxParameterization->Get(I70TrsS (part), log10bg);}  // Estimation for relative sigma beta*gamma dependence for I70 from TpcRS normalized to MIP
  Double_t IfitTrs (Int_t part, Double_t log10bg) const {return m_dEdxParameterization->Get(IfitTrs (part), log10bg);}  // Estimation for Ifit from TpcRS
  Double_t IfitTrsB(Int_t part, Double_t log10bg) const {return m_dEdxParameterization->Get(IfitTrsB(part), log10bg);}  // Estimation for Ifit - Bichsel from TpcRS
  Double_t IfitTrsS(Int_t part, Double_t log10bg) const {return m_dEdxParameterization->Get(IfitTrsS(part), log10bg);}  // Estimation for relative sigma beta*gamma dependence for Ifit from TpcRS normalized to MIP

  ClassDef(Bichsel,0)
};
// $Id: Bichsel.h,v 1.15 2015/12/24 00:16:25 fisyak Exp $
// $Log: Bichsel.h,v $
// Revision 1.15  2015/12/24 00:16:25  fisyak
// Add TpcRS model and macros
//
#endif

