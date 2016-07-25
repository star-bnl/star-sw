#define NA27MuDst_cxx
#include "Riostream.h"
#include "NA27MuDst.h"
#include "TMath.h"
#include "TH3.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TLorentzVector.h"
void NA27MuDst::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L NA27MuDst.C
//      Root > NA27MuDst t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;
  const Int_t    nEta   =   20;
  const Double_t EtaMin =  -5.0;
  const Double_t EtaMax =  5.0 ;
  const Int_t    npT    =  100;
  const Double_t pTmin  =  0.00;
  const Double_t pTmax  = 10.00;
  static const Double_t amPi = 0.13956995;
  static const Double_t amP  = 0.93827231;
  static const Double_t amK  = 0.4936770;
  TH3D *pipi[3], *pipiw[3], *KK[3], *KKw[3], *KKp[3];
  for  (int i = 0; i < 3; i++) {
    TString name("pipi");
    TString title("");
    if (i == 0) {title = "pi+pi- eff. mass";}
    else 
      if (i == 1) {title = "pi-pi- eff. mass"; name += "NN";}
      else        {title = "pi+pi+ eff. mass"; name += "PP";}
    name += "Mass";
    pipi[i]  = new TH3D(name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax,200,0.26,2.260);
    name += "W";
    title += " weighted";
    pipiw[i]  = new TH3D(name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax,200,0.26,2.260);
    name = "KK";
    title = "";
    if (i == 0) {title = "pi+pi- eff. mass";}
    else 
      if (i == 1) {title = "pi-pi- eff. mass"; name += "NN";}
      else        {title = "pi+pi+ eff. mass"; name += "PP";}
    name += "Mass";
    KK[i] = new TH3D(name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax,200,0.95,1.45);
    name += "W";
    title += " weighted";
    KKw[i] = new TH3D(name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax,200,0.95,1.45);
    name += "P";
    title += "  with PiD";
    KKp[i] = new TH3D(name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax,200,0.95,1.45);
  }
  Int_t nentries = Int_t(fChain->GetEntriesFast());
  Int_t nbytes = 0, nb = 0;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    Int_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;
    //    cout << "no_trak\t" << no_trak << endl;
    if (no_trak < 2) continue;
    Double_t bmass = amP;
    if (krll < 300) bmass = amPi;
    TVector3 b(pxyzb[1],pxyzb[2],pxyzb[0]);
    Double_t pmom2 = b.Mag2(); 
    Double_t energy = TMath::Sqrt(bmass*bmass + pmom2);
    TLorentzVector beam(b,energy+amP);
    Double_t beta[3] = {-beam[0]/beam[3],-beam[1]/beam[3],-beam[2]/beam[3]};
    for (int i=0; i < no_trak - 1; i++) {
      if (wsp[i] <= 0.) continue;
      TVector3 tri(pyt[i],pzt[i],pxt[i]);
      pmom2 = tri.Mag2();
      energy = TMath::Sqrt(amPi*amPi + pmom2);
      TLorentzVector pi(tri,energy);
      energy = TMath::Sqrt(amK*amK + pmom2);
      TLorentzVector Ki(tri,energy);
      for (int j = i + 1; j < no_trak; j++) {
	Int_t k = 0;
	if (kchar[i] < 0 && kchar[j] < 0) k = 1;
	if (kchar[i] > 0 && kchar[j] > 0) k = 2;
	//	if (kchar[i]*kchar[j] >=0) continue;
	if (wsp[j] <= 0.) continue;
	TVector3 trj(pyt[j],pzt[j],pxt[j]);
	Double_t pmom2 = trj.Mag2();
	energy = TMath::Sqrt(amPi*amPi + pmom2);
	TLorentzVector pj(trj,energy);
	pj += pi;
	pj.Boost(beta[0],beta[1],beta[2]);
	Double_t M = pj.M();// cout << "M\t" << M;
	Double_t y = pj.Rapidity();// cout << "\ty\t" << y;
	Double_t pT = pj.Pt();     // cout << "\tpT\t" << pT << endl;
	pipi[k]->Fill(y,pT,M);
	Double_t w = eqmkb*wsp[i]*wsp[j];
	pipiw[k]->Fill(y,pT,M,w);
	// K+K-
	energy = TMath::Sqrt(amK*amK + pmom2);
	TLorentzVector Kj(trj,energy);
	Kj += Ki;
	Kj.Boost(beta[0],beta[1],beta[2]);
	M = Kj.M();       //cout << "M\t" << M;
	y = Kj.Rapidity();// cout << "\ty\t" << y;
	pT = Kj.Pt();     // cout << "\tpT\t" << pT << endl;
	KK[k]->Fill(y,pT,M);
	KKw[k]->Fill(y,pT,M,w);
	if (wpid[i] <= 0 || wpid[i] > 10 ||
	    wpid[j] <= 0 || wpid[j] > 10) continue;
	// Clvs[k]; e -> k = 0; pi -> 1; K -> 2; p -> 3;
	if (Clvs[i][2] < 0.1 || Clvs[j][2] < 0.1) continue;
	w *= wpid[i]*wpid[j];
	KKp[k]->Fill(y,pT,M,w);
      }
    }
    //    if (ientry > 10) break;
  }
}

