class  Bichsel;
static Bichsel *m_Bichsel = 0;
#include "Names.h"
void BDiff() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
  Double_t pmin = 0.40;
  Double_t pmax = 0.50;
  Int_t nh = 6;
  //                pi e  p k  mu  d
  Int_t index[6] = {3, 0, 1, 2, 4, 5};
  Int_t kpi = 3;
  TH1D **hyps = new TH1D* [nh-1];
  for (Int_t h = 1; h < nh; h++) {
    Int_t i = index[h];
    hyps[i] = new TH1D(HistNameP[i],Names[i],6000,-1,5.);
  }
  for (Int_t iev = 0; iev < 100000; iev++) {
    Double_t p = pmin + (pmax - pmin)*gRandom->Rndm();
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
