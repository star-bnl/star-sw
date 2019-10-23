// root.exe lBichsel.C BDiff.C+
#include "Names.h"
#include "StRoot/StBichsel/Bichsel.h"
#include "TMath.h"
#include "TF1.h"
#include "TH1.h"
#include "TRandom.h"
static Bichsel *m_Bichsel = 0;
//________________________________________________________________________________
void BDiff() {
  m_Bichsel = Bichsel::Instance();
#if 0
  Double_t pmin = 0.45;
  Double_t pmax = 0.55;
#else
  Double_t pmin = 0.35;
  Double_t pmax = 0.75;
#endif
  //                         pi p  k  e  d mu
  enum {nh = 6};
  Int_t index[nh] = {3, 1, 2, 0, 5, 4};
  Int_t kpi = 3;
  TH1D **hyps = new TH1D* [nh-1];
  for (Int_t h = 1; h < nh; h++) {
    Int_t i = index[h];
    hyps[i] = new TH1D(HistNameP[i],Names[i],340,-0.2,3.2);
  }
  //  TF1::InitStandardFunctions();
  TF1 *f = new TF1("pMom","TMath::Gaus(x,1.75843e-01,2.21705e-01)",pmin,pmax); 
  //  TF1 *f = new TF1("pMom","gaus",pmin,pmax,3); 
  // TF1 *f = new TF1("gaus","gaus",pmin,pmax); 
  //  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject("gaus");
  //  f->SetRange(pmin,pmax);
  // f->SetParameters(1., 1.75843e-01,2.21705e-01);
  for (Int_t iev = 0; iev < 100000; iev++) {
    //    Double_t p = pmin + (pmax - pmin)*gRandom->Rndm();
    Double_t p = f->GetRandom();
    Double_t bgpi = p/Masses[kpi];
    Double_t zpi  = m_Bichsel->GetMostProbableZ(TMath::Log10(bgpi),1.);
    for (Int_t h = 1; h < nh; h++) {
      Int_t i = index[h];
      Double_t bg = p/Masses[i];
      Double_t z  = m_Bichsel->GetMostProbableZ(TMath::Log10(bg),1.);
      hyps[i]->Fill(z-zpi);
    }
  }
  for (Int_t h = 1; h < nh; h++) {
    Int_t i = index[h];
    
    cout << "\t{\t" << hyps[i]->GetMean() << ",\t" <<  hyps[i]->GetRMS() << ",\t" << Masses[i] << ",\t\"" << PidNames[i] << "\"}," << endl;
  }
}
