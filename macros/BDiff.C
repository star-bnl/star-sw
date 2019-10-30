// root.exe lBichsel.C BDiff.C+
#include "Names.h"
#include "StRoot/StBichsel/Bichsel.h"
#include "StRoot/StBichsel/StdEdxModel.h"
#include "TMath.h"
#include "TF1.h"
#include "TH1.h"
#include "TRandom.h"
static Bichsel *m_Bichsel = 0;
//________________________________________________________________________________
Double_t gFunc(Double_t *x, Double_t *p = 0) {
  return TMath::Gaus(x[0], 1.75843e-01,2.21705e-01);
}
//________________________________________________________________________________
void BDiff() {
#ifdef __Bichsel__1   
  m_Bichsel = Bichsel::Instance();
#endif
#if 1
  // MIP from Heed bg = 3.77 => p_pion = 0.526
  Double_t pMIP = 0.526;
  Double_t pmin = pMIP - 0.05; // 0.45;
  Double_t pmax = pMIP + 0.05; // 0.55;
#else
  Double_t pmin = 0.35;
  Double_t pmax = 0.75;
#endif
  //                 pi p  k  e  d mu
  enum {nh = 6};
  Int_t index[nh] = {3, 1, 2, 0, 5, 4};
  Int_t kpi = 3;
  TH1D *hyps[nh] = {0};
  TH2F *hypsp[nh] = {0};
  for (Int_t h = 1; h < nh; h++) {
    Int_t i = index[h];
    hyps[i] = new TH1D(HistNameP[i],Names[i],340,-0.2,3.2);
    hypsp[i] = new TH2F(Form("%sP",HistNameP[i]),Names[i],40,pmin,pmax,340,-0.2,3.2);
  }
  TH2F *zpiP = new TH2F("zpiP","zP vs zpi",100,7.8,8.0,100,8,10);
  //  TF1::InitStandardFunctions();
  TF1 *f = new TF1("pMom",gFunc,pmin,pmax); 
  f->SetNpx(500);
  //  TF1 *f = new TF1("pMom","gaus",pmin,pmax,3); 
  // TF1 *f = new TF1("gaus","gaus",pmin,pmax); 
  //  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject("gaus");
  //  f->SetRange(pmin,pmax);
  // f->SetParameters(1., 1.75843e-01,2.21705e-01);
  for (Int_t iev = 0; iev < 10000; iev++) {
    //    Double_t p = pmin + (pmax - pmin)*gRandom->Rndm();
    Double_t p = f->GetRandom();
    Double_t bgpi = p/Masses[kpi];
#ifdef __Bichsel__1   
    Double_t zpi  = m_Bichsel->GetMostProbableZ(TMath::Log10(bgpi),1.);
#else
    Double_t dX = 2.0;
    Double_t dXLog = TMath::Log(dX);
    Double_t n_Ppi = dX*StdEdxModel::instance()->dNdx(bgpi);
    StdEdxModel::instance()->zMPV()->SetParameter(0,2);
    Double_t n_PLpi = TMath::Log(n_Ppi);
    Double_t logdEMPVpi = StdEdxModel::instance()->zMPV()->Eval(n_PLpi);
    Double_t zpi = logdEMPVpi - dXLog;
#endif
    for (Int_t h = 1; h < nh; h++) {
      Int_t i = index[h];
      Double_t bg = p/Masses[i];
#ifdef __Bichsel__1
      Double_t z  = m_Bichsel->GetMostProbableZ(TMath::Log10(bg),1.);
#else
      Double_t n_P = dX*StdEdxModel::instance()->dNdx(bg);
      Double_t n_PL = TMath::Log(n_P);
      Double_t logdEMPV = StdEdxModel::instance()->zMPV()->Eval(n_PL);
      Double_t z = logdEMPV - dXLog;
      if (h == 1) {
	zpiP->Fill(zpi, z);
#if 0
	cout << "p = " << p << "\tbgpi = " << bgpi << "\tn_Ppi = " << n_Ppi << "\tlogdEMPVpi = " << logdEMPVpi << "\tzpi = " << zpi << endl;
	cout                <<  "\t\tbg = " << bg << "\tn_P = " << n_P << "\tlogdEMPV = " << logdEMPV << "\tz = " << z << "\tdz = " << z - zpi <<  endl;
#endif
      }
#endif
      hyps[i]->Fill(z-zpi);
      hypsp[i]->Fill(p,z-zpi);
    }
  }
  for (Int_t h = 1; h < nh; h++) {
    Int_t i = index[h];
    
    cout << "\t{\t" << hyps[i]->GetMean() << ",\t" <<  hyps[i]->GetRMS() << ",\t" << Masses[i] << ",\t\"" << PidNames[i] << "\"}," << endl;
  }
}
