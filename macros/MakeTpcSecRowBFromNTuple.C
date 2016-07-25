#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
class TpcSecRowCor_st;
TpcSecRowCor_st *rows[24];
class FitP {
   public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
//Declaration of leaves types
   Float_t         i;
   Float_t         j;
   Float_t         x;
   Float_t         y;
   Float_t         mean;
   Float_t         rms;
   Float_t         peak;
   Float_t         mu;
   Float_t         sigma;
   Float_t         entries;
   Float_t         chisq;
   Float_t         prob;
   Float_t         a0;
   Float_t         a1;
   Float_t         a2;
   Float_t         a3;
   Float_t         a4;

//List of branches
   TBranch        *b_i;   //!
   TBranch        *b_j;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_mean;   //!
   TBranch        *b_rms;   //!
   TBranch        *b_peak;   //!
   TBranch        *b_mu;   //!
   TBranch        *b_sigma;   //!
   TBranch        *b_entries;   //!
   TBranch        *b_chisq;   //!
   TBranch        *b_prob;   //!
   TBranch        *b_a0;   //!
   TBranch        *b_a1;   //!
   TBranch        *b_a2;   //!
   TBranch        *b_a3;   //!
   TBranch        *b_a4;   //!

   FitP(TTree *tree=0);
   ~FitP();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

FitP::FitP(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
     TString fName("SecRow3G2Hist416P03ia.root");
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject(fName.Data());
      if (f) cout << "found file " << fName << endl;
      else {
	f = new TFile(fName.Data());
	if (f) cout << "opened file " << fName << endl;
      }
      tree = (TTree*)gDirectory->Get("FitP");

   }
   Init(tree);
}

FitP::~FitP()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t FitP::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t FitP::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Int_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void FitP::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("i",&i);
   fChain->SetBranchAddress("j",&j);
   fChain->SetBranchAddress("x",&x);
   fChain->SetBranchAddress("y",&y);
   fChain->SetBranchAddress("mean",&mean);
   fChain->SetBranchAddress("rms",&rms);
   fChain->SetBranchAddress("peak",&peak);
   fChain->SetBranchAddress("mu",&mu);
   fChain->SetBranchAddress("sigma",&sigma);
   fChain->SetBranchAddress("entries",&entries);
   fChain->SetBranchAddress("chisq",&chisq);
   fChain->SetBranchAddress("prob",&prob);
   fChain->SetBranchAddress("a0",&a0);
   fChain->SetBranchAddress("a1",&a1);
   fChain->SetBranchAddress("a2",&a2);
   fChain->SetBranchAddress("a3",&a3);
   fChain->SetBranchAddress("a4",&a4);
   Notify();
}

Bool_t FitP::Notify()
{
   // Called when loading a new file.
   // Get branch pointers.
   b_i = fChain->GetBranch("i");
   b_j = fChain->GetBranch("j");
   b_x = fChain->GetBranch("x");
   b_y = fChain->GetBranch("y");
   b_mean = fChain->GetBranch("mean");
   b_rms = fChain->GetBranch("rms");
   b_peak = fChain->GetBranch("peak");
   b_mu = fChain->GetBranch("mu");
   b_sigma = fChain->GetBranch("sigma");
   b_entries = fChain->GetBranch("entries");
   b_chisq = fChain->GetBranch("chisq");
   b_prob = fChain->GetBranch("prob");
   b_a0 = fChain->GetBranch("a0");
   b_a1 = fChain->GetBranch("a1");
   b_a2 = fChain->GetBranch("a2");
   b_a3 = fChain->GetBranch("a3");
   b_a4 = fChain->GetBranch("a4");
   return kTRUE;
}

void FitP::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t FitP::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}

void FitP::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L FitP.C
//      Root > FitP t
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
      int ii = (int) i - 1;
      int jj = (int) j - 1;
      if (TMath::Abs(mu) < 0.5) {
	rows[ii]->GainScale[jj] = TMath::Exp(-mu);
	rows[ii]->GainRms[jj]   = sigma;
      }
      else {
	cout << "Skip Sector \t" << i << "\tRow\t" << j 
	     << "\twith correction\t" << mu << endl;
	rows[ii]->GainScale[jj] = 0;
	rows[ii]->GainRms[jj]   = 0;
      }
   }
}
//________________________________________________________________________________
St_TpcSecRowCor * MakeTpcSecRowB(const Char_t *TableName = "TpcSecRowB", Int_t d=2003016,Int_t t=0 ) {
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("St_TpcSecRowCor") < 0) gSystem->Load("St_Tables");
  St_TpcSecRowCor *secrow = new St_TpcSecRowCor(TableName,24); 
  TpcSecRowCor_st row;
  memset(&row, 0, secrow->GetRowSize());
  for (int i = 0; i < 24; i++) {secrow->AddAt(&row,i); rows[i] = secrow->GetTable(i);}
  FitP T;
  T.Loop();
  TDatime  time(d,t);
  TString filename(Form("%s.%08d.%06d",TableName,time.GetDate(),time.GetTime()));
  //  sprintf(filename,"./StarDb/Calibrations/tpc/TpcSecRowTest.%08d.%06d.C",time.GetDate(),time.GetTime());
  //  sprintf(filename,"TpcSecRow.%08d.%06d.root",time.GetDate(),time.GetTime());
  printf("Create %s\n",filename.Data());
#if 0
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directory " << dirname << " creation failed" << endl;
      cout << "Putting " << TableName << ".C in current directory" << endl;
      filename += ".C";
    }
  }
  ofstream *out = new ofstream(filename.Data());
  secrow->SavePrimitive(*out,"");
  delete out;
#else
  filename += ".root";
  TFile *f = new TFile(filename.Data(),"recreate");
  secrow->Write();
  delete f;
#endif
  delete [] rows;
  return secrow;
}
