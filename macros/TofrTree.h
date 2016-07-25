//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Mon Apr 14 15:55:02 2003 by ROOT version3.05/03)
//   from TTree TofrTree/TofrTree
//   found on file: Tofr_Sigma_New.root
//////////////////////////////////////////////////////////


#ifndef TofrTree_h
#define TofrTree_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

class TofrTree {
   public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
//Declaration of leaves types
   Int_t           trkid;
   Int_t           runId;
   Float_t         Vz;
   Float_t         dEdx;
   Float_t         adc;
   Float_t         T0;
   Float_t         tof;
   Float_t         time;
   Float_t         Pt;
   Float_t         p;
   Int_t           nhits;
   Float_t         L;
   Int_t           cell;
   Int_t           module;
   Int_t           tray;
   Int_t           charge;
   Int_t           Ieast;
   Int_t           Iwest;
   Float_t         xlocal;
   Float_t         ylocal;
   Float_t         zlocal;
   Int_t           Nch;
   Float_t         TdcSumWest;
   Float_t         TdcSumEast;
   Float_t         Tdif;
   Float_t         dca;
   Float_t         dca1;
   Float_t         Ieast1;
   Float_t         Iwest1;
   Float_t         mDelta;
   Int_t           ndedx;
   Float_t         error;
   Float_t         TdifE;
   Float_t         mSigmaElectron;
   Float_t         mSigmaPion;
   Float_t         mSigmaKaon;
   Float_t         mSigmaProton;
   Float_t         Tzero;
   Float_t         tofcorr;
   Float_t         tofcorr2;
   Float_t         tofcorr3;
   Float_t         tofcorr4;
   Float_t         tofcorr5;

//List of branches
   TBranch        *b_trkid;   //!
   TBranch        *b_runId;   //!
   TBranch        *b_Vz;   //!
   TBranch        *b_dEdx;   //!
   TBranch        *b_adc;   //!
   TBranch        *b_T0;   //!
   TBranch        *b_tof;   //!
   TBranch        *b_time;   //!
   TBranch        *b_Pt;   //!
   TBranch        *b_p;   //!
   TBranch        *b_nhits;   //!
   TBranch        *b_L;   //!
   TBranch        *b_cell;   //!
   TBranch        *b_module;   //!
   TBranch        *b_tray;   //!
   TBranch        *b_charge;   //!
   TBranch        *b_Ieast;   //!
   TBranch        *b_Iwest;   //!
   TBranch        *b_xlocal;   //!
   TBranch        *b_ylocal;   //!
   TBranch        *b_zlocal;   //!
   TBranch        *b_Nch;   //!
   TBranch        *b_TdcSumWest;   //!
   TBranch        *b_TdcSumEast;   //!
   TBranch        *b_Tdif;   //!
   TBranch        *b_dca;   //!
   TBranch        *b_dca1;   //!
   TBranch        *b_Ieast1;   //!
   TBranch        *b_Iwest1;   //!
   TBranch        *b_mDelta;   //!
   TBranch        *b_ndedx;   //!
   TBranch        *b_error;   //!
   TBranch        *b_TdifE;   //!
   TBranch        *b_mSigmaElectron;   //!
   TBranch        *b_mSigmaPion;   //!
   TBranch        *b_mSigmaKaon;   //!
   TBranch        *b_mSigmaProton;   //!
   TBranch        *b_Tzero;   //!
   TBranch        *b_tofcorr;   //!
   TBranch        *b_tofcorr2;   //!
   TBranch        *b_tofcorr3;   //!
   TBranch        *b_tofcorr4;   //!
   TBranch        *b_tofcorr5;   //!

   TofrTree(TTree *tree=0);
   ~TofrTree();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef TofrTree_cxx
TofrTree::TofrTree(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("Tofr_Sigma_New.root");
      if (!f) {
         f = new TFile("Tofr_Sigma_New.root");
      }
      tree = (TTree*)gDirectory->Get("TofrTree");

   }
   Init(tree);
}

TofrTree::~TofrTree()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t TofrTree::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t TofrTree::LoadTree(Int_t entry)
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

void TofrTree::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("trkid",&trkid);
   fChain->SetBranchAddress("runId",&runId);
   fChain->SetBranchAddress("Vz",&Vz);
   fChain->SetBranchAddress("dEdx",&dEdx);
   fChain->SetBranchAddress("adc",&adc);
   fChain->SetBranchAddress("T0",&T0);
   fChain->SetBranchAddress("tof",&tof);
   fChain->SetBranchAddress("time",&time);
   fChain->SetBranchAddress("Pt",&Pt);
   fChain->SetBranchAddress("p",&p);
   fChain->SetBranchAddress("nhits",&nhits);
   fChain->SetBranchAddress("L",&L);
   fChain->SetBranchAddress("cell",&cell);
   fChain->SetBranchAddress("module",&module);
   fChain->SetBranchAddress("tray",&tray);
   fChain->SetBranchAddress("charge",&charge);
   fChain->SetBranchAddress("Ieast",&Ieast);
   fChain->SetBranchAddress("Iwest",&Iwest);
   fChain->SetBranchAddress("xlocal",&xlocal);
   fChain->SetBranchAddress("ylocal",&ylocal);
   fChain->SetBranchAddress("zlocal",&zlocal);
   fChain->SetBranchAddress("Nch",&Nch);
   fChain->SetBranchAddress("TdcSumWest",&TdcSumWest);
   fChain->SetBranchAddress("TdcSumEast",&TdcSumEast);
   fChain->SetBranchAddress("Tdif",&Tdif);
   fChain->SetBranchAddress("dca",&dca);
   fChain->SetBranchAddress("dca1",&dca1);
   fChain->SetBranchAddress("Ieast1",&Ieast1);
   fChain->SetBranchAddress("Iwest1",&Iwest1);
   fChain->SetBranchAddress("mDelta",&mDelta);
   fChain->SetBranchAddress("ndedx",&ndedx);
   fChain->SetBranchAddress("error",&error);
   fChain->SetBranchAddress("TdifE",&TdifE);
   fChain->SetBranchAddress("mSigmaElectron",&mSigmaElectron);
   fChain->SetBranchAddress("mSigmaPion",&mSigmaPion);
   fChain->SetBranchAddress("mSigmaKaon",&mSigmaKaon);
   fChain->SetBranchAddress("mSigmaProton",&mSigmaProton);
   fChain->SetBranchAddress("Tzero",&Tzero);
   fChain->SetBranchAddress("tofcorr",&tofcorr);
   fChain->SetBranchAddress("tofcorr2",&tofcorr2);
   fChain->SetBranchAddress("tofcorr3",&tofcorr3);
   fChain->SetBranchAddress("tofcorr4",&tofcorr4);
   fChain->SetBranchAddress("tofcorr5",&tofcorr5);
   Notify();
}

Bool_t TofrTree::Notify()
{
   // Called when loading a new file.
   // Get branch pointers.
   b_trkid = fChain->GetBranch("trkid");
   b_runId = fChain->GetBranch("runId");
   b_Vz = fChain->GetBranch("Vz");
   b_dEdx = fChain->GetBranch("dEdx");
   b_adc = fChain->GetBranch("adc");
   b_T0 = fChain->GetBranch("T0");
   b_tof = fChain->GetBranch("tof");
   b_time = fChain->GetBranch("time");
   b_Pt = fChain->GetBranch("Pt");
   b_p = fChain->GetBranch("p");
   b_nhits = fChain->GetBranch("nhits");
   b_L = fChain->GetBranch("L");
   b_cell = fChain->GetBranch("cell");
   b_module = fChain->GetBranch("module");
   b_tray = fChain->GetBranch("tray");
   b_charge = fChain->GetBranch("charge");
   b_Ieast = fChain->GetBranch("Ieast");
   b_Iwest = fChain->GetBranch("Iwest");
   b_xlocal = fChain->GetBranch("xlocal");
   b_ylocal = fChain->GetBranch("ylocal");
   b_zlocal = fChain->GetBranch("zlocal");
   b_Nch = fChain->GetBranch("Nch");
   b_TdcSumWest = fChain->GetBranch("TdcSumWest");
   b_TdcSumEast = fChain->GetBranch("TdcSumEast");
   b_Tdif = fChain->GetBranch("Tdif");
   b_dca = fChain->GetBranch("dca");
   b_dca1 = fChain->GetBranch("dca1");
   b_Ieast1 = fChain->GetBranch("Ieast1");
   b_Iwest1 = fChain->GetBranch("Iwest1");
   b_mDelta = fChain->GetBranch("mDelta");
   b_ndedx = fChain->GetBranch("ndedx");
   b_error = fChain->GetBranch("error");
   b_TdifE = fChain->GetBranch("TdifE");
   b_mSigmaElectron = fChain->GetBranch("mSigmaElectron");
   b_mSigmaPion = fChain->GetBranch("mSigmaPion");
   b_mSigmaKaon = fChain->GetBranch("mSigmaKaon");
   b_mSigmaProton = fChain->GetBranch("mSigmaProton");
   b_Tzero = fChain->GetBranch("Tzero");
   b_tofcorr = fChain->GetBranch("tofcorr");
   b_tofcorr2 = fChain->GetBranch("tofcorr2");
   b_tofcorr3 = fChain->GetBranch("tofcorr3");
   b_tofcorr4 = fChain->GetBranch("tofcorr4");
   b_tofcorr5 = fChain->GetBranch("tofcorr5");
   return kTRUE;
}

void TofrTree::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t TofrTree::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef TofrTree_cxx

