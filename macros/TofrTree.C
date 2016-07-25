#define TofrTree_cxx
#include "TofrTree.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "StBichsel/Bichsel.h"
//class Bichsel;
Bichsel *m_Bichsel = 0;
void Tofr() {
  if (!m_Bichsel || gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
    
  }
  TofrTree t;
  t.Loop();
}
void TofrTree::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L TofrTree.C
//      Root > TofrTree t
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

   Int_t nentries = Int_t(fChain->GetEntriesFast());
   Int_t nbytes = 0, nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      Int_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      Double_t pove   = pow(10.,x[0]);
      dEdx
	Double_t poverm = p/.140;
      //      Double_t val = m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),1.);
      Double_t val = 1.e-6*m_Bichsel->GetI70(TMath::Log10(poverm),1.);
      Double_t z = log(dEdx/val);
      cout << "p\t" << p << "\tdEdx\t" << dEdx << "\tpred\t" << val << "\tz\t" << z << endl;
   }
}
