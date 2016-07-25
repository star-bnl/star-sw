//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Wed Apr 17 16:25:04 2013 by ROOT version 5.34/05
// from TTree FitP/Fit results
// found on file: st_laser_adc_14072106_raw.RF.Fit.g1.LandauIFreQ.AllN.root
//////////////////////////////////////////////////////////

#ifndef LaserCluster_h
#define LaserCluster_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

// Fixed size dimensions of array or collections stored in the TTree if any.

class LaserCluster {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
   Float_t         sector;
   Float_t         row;
   Float_t         X;
   Float_t         Y;
   Float_t         zP;
   Float_t         eP;
   Float_t         NormLX;
   Float_t         dNormLX;
   Float_t         mvpX;
   Float_t         dmvpX;
   Float_t         sigmaLX;
   Float_t         dsigmaLX;
   Float_t         sigmaX;
   Float_t         dsigmaX;
   Float_t         pedX;
   Float_t         dpedX;
   Float_t         chisqX;
   Float_t         NDFX;
   Float_t         NormLY;
   Float_t         dNormLY;
   Float_t         muY;
   Float_t         dmuY;
   Float_t         sigmaY;
   Float_t         dsigmaY;
   Float_t         deltaY;
   Float_t         ddeltaY;
   Float_t         pedY;
   Float_t         dpedY;
   Float_t         chisqY;
   Float_t         NDFY;

   // List of branches
   TBranch        *b_sector;   //!
   TBranch        *b_row;   //!
   TBranch        *b_X;   //!
   TBranch        *b_Y;   //!
   TBranch        *b_zP;   //!
   TBranch        *b_eP;   //!
   TBranch        *b_NormLX;   //!
   TBranch        *b_dNormLX;   //!
   TBranch        *b_mvpX;   //!
   TBranch        *b_dmvpX;   //!
   TBranch        *b_sigmaLX;   //!
   TBranch        *b_dsigmaLX;   //!
   TBranch        *b_sigmaX;   //!
   TBranch        *b_dsigmaX;   //!
   TBranch        *b_pedX;   //!
   TBranch        *b_dpedX;   //!
   TBranch        *b_chisqX;   //!
   TBranch        *b_NDFX;   //!
   TBranch        *b_NormLY;   //!
   TBranch        *b_dNormLY;   //!
   TBranch        *b_muY;   //!
   TBranch        *b_dmuY;   //!
   TBranch        *b_sigmaY;   //!
   TBranch        *b_dsigmaY;   //!
   TBranch        *b_deltaY;   //!
   TBranch        *b_ddeltaY;   //!
   TBranch        *b_pedY;   //!
   TBranch        *b_dpedY;   //!
   TBranch        *b_chisqY;   //!
   TBranch        *b_NDFY;   //!

   LaserCluster(TTree *tree=0);
   virtual ~LaserCluster();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

#ifdef LaserCluster_cxx
LaserCluster::LaserCluster(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("st_laser_adc_14072106_raw.RF.Fit.g1.LandauIFreQ.AllN.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("st_laser_adc_14072106_raw.RF.Fit.g1.LandauIFreQ.AllN.root");
      }
      f->GetObject("FitP",tree);

   }
   Init(tree);
}

LaserCluster::~LaserCluster()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t LaserCluster::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t LaserCluster::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void LaserCluster::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normally not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("sector", &sector, &b_sector);
   fChain->SetBranchAddress("row", &row, &b_row);
   fChain->SetBranchAddress("X", &X, &b_X);
   fChain->SetBranchAddress("Y", &Y, &b_Y);
   fChain->SetBranchAddress("zP", &zP, &b_zP);
   fChain->SetBranchAddress("eP", &eP, &b_eP);
   fChain->SetBranchAddress("NormLX", &NormLX, &b_NormLX);
   fChain->SetBranchAddress("dNormLX", &dNormLX, &b_dNormLX);
   fChain->SetBranchAddress("mvpX", &mvpX, &b_mvpX);
   fChain->SetBranchAddress("dmvpX", &dmvpX, &b_dmvpX);
   fChain->SetBranchAddress("sigmaLX", &sigmaLX, &b_sigmaLX);
   fChain->SetBranchAddress("dsigmaLX", &dsigmaLX, &b_dsigmaLX);
   fChain->SetBranchAddress("sigmaX", &sigmaX, &b_sigmaX);
   fChain->SetBranchAddress("dsigmaX", &dsigmaX, &b_dsigmaX);
   fChain->SetBranchAddress("pedX", &pedX, &b_pedX);
   fChain->SetBranchAddress("dpedX", &dpedX, &b_dpedX);
   fChain->SetBranchAddress("chisqX", &chisqX, &b_chisqX);
   fChain->SetBranchAddress("NDFX", &NDFX, &b_NDFX);
   fChain->SetBranchAddress("NormLY", &NormLY, &b_NormLY);
   fChain->SetBranchAddress("dNormLY", &dNormLY, &b_dNormLY);
   fChain->SetBranchAddress("muY", &muY, &b_muY);
   fChain->SetBranchAddress("dmuY", &dmuY, &b_dmuY);
   fChain->SetBranchAddress("sigmaY", &sigmaY, &b_sigmaY);
   fChain->SetBranchAddress("dsigmaY", &dsigmaY, &b_dsigmaY);
   fChain->SetBranchAddress("deltaY", &deltaY, &b_deltaY);
   fChain->SetBranchAddress("ddeltaY", &ddeltaY, &b_ddeltaY);
   fChain->SetBranchAddress("pedY", &pedY, &b_pedY);
   fChain->SetBranchAddress("dpedY", &dpedY, &b_dpedY);
   fChain->SetBranchAddress("chisqY", &chisqY, &b_chisqY);
   fChain->SetBranchAddress("NDFY", &NDFY, &b_NDFY);
   Notify();
}

Bool_t LaserCluster::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void LaserCluster::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t LaserCluster::Cut(Long64_t /* entry */)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef LaserCluster_cxx
