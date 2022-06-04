#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//    ROOT5
#if defined(__CINT__) && !defined(__MAKECINT__)
// source code being actually interpreted by cint
#elif defined(__MAKECINT__)
// source code seen by rootcint only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
//    ROOT6 
#if defined(__CLING__) && !defined(__ROOTCLING__)
// source code being actually interpreted by Cling
#elif defined(__ROOTCLING__)
// source code seen by rootcling only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
#define __USE_ROOFIT__
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TEfficiency.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TPaletteAxis.h"
#include "TDirIter.h"
#endif
// Clusters
struct Name_t {
  const Char_t *histName;
  const Char_t *varName;
  const Char_t *cutName;
};
Name_t Names[24] = {
  {"PadFC",       "newP.pad:newP.row","oldP.sector<=0&&newP.sector!=20"},
  {"PminFC",     "newP.pmin:newP.row","oldP.sector<=0&&newP.sector!=20"},
  {"PmaxFC",     "newP.pmax:newP.row","oldP.sector<=0&&newP.sector!=20"},
  {"PadFCAll",    "newP.pad:newP.row","newP.sector!=20"},
  {"PminFCAll",  "newP.pmin:newP.row","newP.sector!=20"},
  {"PmaxFCAll",  "newP.pmax:newP.row","newP.sector!=20"},
  
  {"PadOC",       "oldP.pad:oldP.row","newP.sector<=0&&oldP.sector!=20"},
  {"PminOC",     "oldP.pmin:oldP.row","newP.sector<=0&&oldP.sector!=20"},
  {"PmaxOC",     "oldP.pmax:oldP.row","newP.sector<=0&&oldP.sector!=20"},
  {"PadOCAll",    "oldP.pad:oldP.row","oldP.sector!=20"},
  {"PminOCAll",  "oldP.pmin:oldP.row","oldP.sector!=20"},
  {"PmaxOCAll",  "oldP.pmax:oldP.row","oldP.sector!=20"},
  
  {"PadFC20",       "newP.pad:newP.row","oldP.sector<=0&&newP.sector==20"},
  {"PminFC20",     "newP.pmin:newP.row","oldP.sector<=0&&newP.sector==20"},
  {"PmaxFC20",     "newP.pmax:newP.row","oldP.sector<=0&&newP.sector==20"},
  {"PadFC20All",    "newP.pad:newP.row","newP.sector==20"},
  {"PminFC20All",  "newP.pmin:newP.row","newP.sector==20"},
  {"PmaxFC20All",  "newP.pmax:newP.row","newP.sector==20"},
  
  {"PadOC20",       "oldP.pad:oldP.row","newP.sector<=0&&oldP.sector==20"},
  {"PminOC20",     "oldP.pmin:oldP.row","newP.sector<=0&&oldP.sector==20"},
  {"PmaxOC20",     "oldP.pmax:oldP.row","newP.sector<=0&&oldP.sector==20"},
  {"PadOC20All",    "oldP.pad:oldP.row","oldP.sector==20"},
  {"PminOC20All",  "oldP.pmin:oldP.row","oldP.sector==20"},
  {"PmaxOC20All",  "oldP.pmax:oldP.row","oldP.sector==20"}
  
};
//________________________________________________________________________________
//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Fri Jun  3 17:33:01 2022 by ROOT version 5.34/39
// from TTree hitMateComp/hitMateComp
// found on file: trackMateFilest_physics_adc_20060069_raw_4500002.root
//////////////////////////////////////////////////////////

#ifndef hitMateComp_h
#define hitMateComp_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.
#include <TObject.h>

// Fixed size dimensions of array or collections stored in the TTree if any.

class hitMateComp {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
 //HitMatch        *HitMatch;
   UInt_t          fUniqueID;
   UInt_t          fBits;
   UInt_t          newP_fUniqueID;
   UInt_t          newP_fBits;
   Char_t          newP_begin;
   Int_t           newP_sector;
   Int_t           newP_row;
   Float_t         newP_x;
   Float_t         newP_y;
   Float_t         newP_z;
   Float_t         newP_q;
   Int_t           newP_adc;
   Float_t         newP_pad;
   Float_t         newP_timebucket;
   Int_t           newP_npads;
   Int_t           newP_ntbks;
   Int_t           newP_IdTruth;
   Int_t           newP_pmin;
   Int_t           newP_pmax;
   Int_t           newP_tmin;
   Int_t           newP_tmax;
   Float_t         newP_xL;
   Float_t         newP_yL;
   Float_t         newP_zL;
   Float_t         newP_dX;
   Int_t           newP_trigId;
   Int_t           newP_us;
   Int_t           newP_fl;
   Float_t         newP_time;
   Float_t         newP_timeb;
   Char_t          newP_end;
   UInt_t          oldP_fUniqueID;
   UInt_t          oldP_fBits;
   Char_t          oldP_begin;
   Int_t           oldP_sector;
   Int_t           oldP_row;
   Float_t         oldP_x;
   Float_t         oldP_y;
   Float_t         oldP_z;
   Float_t         oldP_q;
   Int_t           oldP_adc;
   Float_t         oldP_pad;
   Float_t         oldP_timebucket;
   Int_t           oldP_npads;
   Int_t           oldP_ntbks;
   Int_t           oldP_IdTruth;
   Int_t           oldP_pmin;
   Int_t           oldP_pmax;
   Int_t           oldP_tmin;
   Int_t           oldP_tmax;
   Float_t         oldP_xL;
   Float_t         oldP_yL;
   Float_t         oldP_zL;
   Float_t         oldP_dX;
   Int_t           oldP_trigId;
   Int_t           oldP_us;
   Int_t           oldP_fl;
   Float_t         oldP_time;
   Float_t         oldP_timeb;
   Char_t          oldP_end;

   // List of branches
   TBranch        *b_HitMatch_fUniqueID;   //!
   TBranch        *b_HitMatch_fBits;   //!
   TBranch        *b_HitMatch_newP_fUniqueID;   //!
   TBranch        *b_HitMatch_newP_fBits;   //!
   TBranch        *b_HitMatch_newP_begin;   //!
   TBranch        *b_HitMatch_newP_sector;   //!
   TBranch        *b_HitMatch_newP_row;   //!
   TBranch        *b_HitMatch_newP_x;   //!
   TBranch        *b_HitMatch_newP_y;   //!
   TBranch        *b_HitMatch_newP_z;   //!
   TBranch        *b_HitMatch_newP_q;   //!
   TBranch        *b_HitMatch_newP_adc;   //!
   TBranch        *b_HitMatch_newP_pad;   //!
   TBranch        *b_HitMatch_newP_timebucket;   //!
   TBranch        *b_HitMatch_newP_npads;   //!
   TBranch        *b_HitMatch_newP_ntbks;   //!
   TBranch        *b_HitMatch_newP_IdTruth;   //!
   TBranch        *b_HitMatch_newP_pmin;   //!
   TBranch        *b_HitMatch_newP_pmax;   //!
   TBranch        *b_HitMatch_newP_tmin;   //!
   TBranch        *b_HitMatch_newP_tmax;   //!
   TBranch        *b_HitMatch_newP_xL;   //!
   TBranch        *b_HitMatch_newP_yL;   //!
   TBranch        *b_HitMatch_newP_zL;   //!
   TBranch        *b_HitMatch_newP_dX;   //!
   TBranch        *b_HitMatch_newP_trigId;   //!
   TBranch        *b_HitMatch_newP_us;   //!
   TBranch        *b_HitMatch_newP_fl;   //!
   TBranch        *b_HitMatch_newP_time;   //!
   TBranch        *b_HitMatch_newP_timeb;   //!
   TBranch        *b_HitMatch_newP_end;   //!
   TBranch        *b_HitMatch_oldP_fUniqueID;   //!
   TBranch        *b_HitMatch_oldP_fBits;   //!
   TBranch        *b_HitMatch_oldP_begin;   //!
   TBranch        *b_HitMatch_oldP_sector;   //!
   TBranch        *b_HitMatch_oldP_row;   //!
   TBranch        *b_HitMatch_oldP_x;   //!
   TBranch        *b_HitMatch_oldP_y;   //!
   TBranch        *b_HitMatch_oldP_z;   //!
   TBranch        *b_HitMatch_oldP_q;   //!
   TBranch        *b_HitMatch_oldP_adc;   //!
   TBranch        *b_HitMatch_oldP_pad;   //!
   TBranch        *b_HitMatch_oldP_timebucket;   //!
   TBranch        *b_HitMatch_oldP_npads;   //!
   TBranch        *b_HitMatch_oldP_ntbks;   //!
   TBranch        *b_HitMatch_oldP_IdTruth;   //!
   TBranch        *b_HitMatch_oldP_pmin;   //!
   TBranch        *b_HitMatch_oldP_pmax;   //!
   TBranch        *b_HitMatch_oldP_tmin;   //!
   TBranch        *b_HitMatch_oldP_tmax;   //!
   TBranch        *b_HitMatch_oldP_xL;   //!
   TBranch        *b_HitMatch_oldP_yL;   //!
   TBranch        *b_HitMatch_oldP_zL;   //!
   TBranch        *b_HitMatch_oldP_dX;   //!
   TBranch        *b_HitMatch_oldP_trigId;   //!
   TBranch        *b_HitMatch_oldP_us;   //!
   TBranch        *b_HitMatch_oldP_fl;   //!
   TBranch        *b_HitMatch_oldP_time;   //!
   TBranch        *b_HitMatch_oldP_timeb;   //!
   TBranch        *b_HitMatch_oldP_end;   //!

   hitMateComp(TTree *tree=0);
   virtual ~hitMateComp();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

void hitMateComp::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L hitMateComp.C
//      Root > hitMateComp t
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
   TH1D *RN = new TH1D("RN","new clusters versus row",72,0.5,72.5);
   TH1D *RO = new TH1D("RO","old clusters versus row",72,0.5,72.5);
   TH1D *RON = new TH1D("RON","old&new clusters versus new row",72,0.5,72.5);
   TH1D *RNO = new TH1D("RNO","old&new clusters versus old row",72,0.5,72.5);
   TH2F *padD = new TH2F("padD","pad diff. new - old versus row",72,0.5,72.5,256,-2.0,2.0);
   TH2F *timD = new TH2F("timD","time diff. new - old versus row",72,0.5,72.5,256,-2.0,2.0);
   TH2F *adcR = new TH2F("adcR","log(adc_{new}/adc_{old} versus row",72,0.5,72.5,256,-2.0,2.0);
   TH2F *npadONi = new TH2F("npadONi","no. of pad new versus old inner",32,0.5,32.5,32,0.5,32.5);
   TH2F *npadONo = new TH2F("npadONo","no. of pad new versus old outer",32,0.5,32.5,32,0.5,32.5);  
   TH2F *ntimONi = new TH2F("ntimONi","no. of time buchets  new versus old inner",32,0.5,32.5,32,0.5,32.5);
   TH2F *ntimONo = new TH2F("ntimONo","no. of time buchets  new versus old outer",32,0.5,32.5,32,0.5,32.5); 
   Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
      if (oldP_sector > 0) {
	RO->Fill(oldP_row);
      }
      if (newP_sector > 0) {
	RN->Fill(newP_row);
      }
      if (oldP_sector <= 0 || newP_sector <= 0) continue;
      RNO->Fill(oldP_row);
      RON->Fill(newP_row);
      padD->Fill(newP_row, newP_pad - oldP_pad);
      timD->Fill(newP_row, newP_timebucket - oldP_timebucket);
      if (newP_adc > 0 && oldP_adc > 0) adcR->Fill(newP_row, TMath::Log(newP_adc/oldP_adc));
      if (newP_row < 40.5) {
	npadONi->Fill(oldP_npads,  newP_npads);
	ntimONi->Fill(oldP_ntbks,  newP_ntbks);
      } else {
	npadONo->Fill(oldP_npads,  newP_npads);
	ntimONo->Fill(oldP_ntbks,  newP_ntbks);
      }
   }
}
#define hitMateComp_cxx

#ifdef hitMateComp_cxx
hitMateComp::hitMateComp(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("trackMateFilest_physics_adc_20060069_raw_4500002.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("trackMateFilest_physics_adc_20060069_raw_4500002.root");
      }
      f->GetObject("hitMateComp",tree);

   }
   Init(tree);
}

hitMateComp::~hitMateComp()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t hitMateComp::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t hitMateComp::LoadTree(Long64_t entry)
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

void hitMateComp::Init(TTree *tree)
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

   fChain->SetBranchAddress("fUniqueID", &fUniqueID, &b_HitMatch_fUniqueID);
   fChain->SetBranchAddress("fBits", &fBits, &b_HitMatch_fBits);
   fChain->SetBranchAddress("newP.fUniqueID", &newP_fUniqueID, &b_HitMatch_newP_fUniqueID);
   fChain->SetBranchAddress("newP.fBits", &newP_fBits, &b_HitMatch_newP_fBits);
   fChain->SetBranchAddress("newP.begin", &newP_begin, &b_HitMatch_newP_begin);
   fChain->SetBranchAddress("newP.sector", &newP_sector, &b_HitMatch_newP_sector);
   fChain->SetBranchAddress("newP.row", &newP_row, &b_HitMatch_newP_row);
   fChain->SetBranchAddress("newP.x", &newP_x, &b_HitMatch_newP_x);
   fChain->SetBranchAddress("newP.y", &newP_y, &b_HitMatch_newP_y);
   fChain->SetBranchAddress("newP.z", &newP_z, &b_HitMatch_newP_z);
   fChain->SetBranchAddress("newP.q", &newP_q, &b_HitMatch_newP_q);
   fChain->SetBranchAddress("newP.adc", &newP_adc, &b_HitMatch_newP_adc);
   fChain->SetBranchAddress("newP.pad", &newP_pad, &b_HitMatch_newP_pad);
   fChain->SetBranchAddress("newP.timebucket", &newP_timebucket, &b_HitMatch_newP_timebucket);
   fChain->SetBranchAddress("newP.npads", &newP_npads, &b_HitMatch_newP_npads);
   fChain->SetBranchAddress("newP.ntbks", &newP_ntbks, &b_HitMatch_newP_ntbks);
   fChain->SetBranchAddress("newP.IdTruth", &newP_IdTruth, &b_HitMatch_newP_IdTruth);
   fChain->SetBranchAddress("newP.pmin", &newP_pmin, &b_HitMatch_newP_pmin);
   fChain->SetBranchAddress("newP.pmax", &newP_pmax, &b_HitMatch_newP_pmax);
   fChain->SetBranchAddress("newP.tmin", &newP_tmin, &b_HitMatch_newP_tmin);
   fChain->SetBranchAddress("newP.tmax", &newP_tmax, &b_HitMatch_newP_tmax);
   fChain->SetBranchAddress("newP.xL", &newP_xL, &b_HitMatch_newP_xL);
   fChain->SetBranchAddress("newP.yL", &newP_yL, &b_HitMatch_newP_yL);
   fChain->SetBranchAddress("newP.zL", &newP_zL, &b_HitMatch_newP_zL);
   fChain->SetBranchAddress("newP.dX", &newP_dX, &b_HitMatch_newP_dX);
   fChain->SetBranchAddress("newP.trigId", &newP_trigId, &b_HitMatch_newP_trigId);
   fChain->SetBranchAddress("newP.us", &newP_us, &b_HitMatch_newP_us);
   fChain->SetBranchAddress("newP.fl", &newP_fl, &b_HitMatch_newP_fl);
   fChain->SetBranchAddress("newP.time", &newP_time, &b_HitMatch_newP_time);
   fChain->SetBranchAddress("newP.timeb", &newP_timeb, &b_HitMatch_newP_timeb);
   fChain->SetBranchAddress("newP.end", &newP_end, &b_HitMatch_newP_end);
   fChain->SetBranchAddress("oldP.fUniqueID", &oldP_fUniqueID, &b_HitMatch_oldP_fUniqueID);
   fChain->SetBranchAddress("oldP.fBits", &oldP_fBits, &b_HitMatch_oldP_fBits);
   fChain->SetBranchAddress("oldP.begin", &oldP_begin, &b_HitMatch_oldP_begin);
   fChain->SetBranchAddress("oldP.sector", &oldP_sector, &b_HitMatch_oldP_sector);
   fChain->SetBranchAddress("oldP.row", &oldP_row, &b_HitMatch_oldP_row);
   fChain->SetBranchAddress("oldP.x", &oldP_x, &b_HitMatch_oldP_x);
   fChain->SetBranchAddress("oldP.y", &oldP_y, &b_HitMatch_oldP_y);
   fChain->SetBranchAddress("oldP.z", &oldP_z, &b_HitMatch_oldP_z);
   fChain->SetBranchAddress("oldP.q", &oldP_q, &b_HitMatch_oldP_q);
   fChain->SetBranchAddress("oldP.adc", &oldP_adc, &b_HitMatch_oldP_adc);
   fChain->SetBranchAddress("oldP.pad", &oldP_pad, &b_HitMatch_oldP_pad);
   fChain->SetBranchAddress("oldP.timebucket", &oldP_timebucket, &b_HitMatch_oldP_timebucket);
   fChain->SetBranchAddress("oldP.npads", &oldP_npads, &b_HitMatch_oldP_npads);
   fChain->SetBranchAddress("oldP.ntbks", &oldP_ntbks, &b_HitMatch_oldP_ntbks);
   fChain->SetBranchAddress("oldP.IdTruth", &oldP_IdTruth, &b_HitMatch_oldP_IdTruth);
   fChain->SetBranchAddress("oldP.pmin", &oldP_pmin, &b_HitMatch_oldP_pmin);
   fChain->SetBranchAddress("oldP.pmax", &oldP_pmax, &b_HitMatch_oldP_pmax);
   fChain->SetBranchAddress("oldP.tmin", &oldP_tmin, &b_HitMatch_oldP_tmin);
   fChain->SetBranchAddress("oldP.tmax", &oldP_tmax, &b_HitMatch_oldP_tmax);
   fChain->SetBranchAddress("oldP.xL", &oldP_xL, &b_HitMatch_oldP_xL);
   fChain->SetBranchAddress("oldP.yL", &oldP_yL, &b_HitMatch_oldP_yL);
   fChain->SetBranchAddress("oldP.zL", &oldP_zL, &b_HitMatch_oldP_zL);
   fChain->SetBranchAddress("oldP.dX", &oldP_dX, &b_HitMatch_oldP_dX);
   fChain->SetBranchAddress("oldP.trigId", &oldP_trigId, &b_HitMatch_oldP_trigId);
   fChain->SetBranchAddress("oldP.us", &oldP_us, &b_HitMatch_oldP_us);
   fChain->SetBranchAddress("oldP.fl", &oldP_fl, &b_HitMatch_oldP_fl);
   fChain->SetBranchAddress("oldP.time", &oldP_time, &b_HitMatch_oldP_time);
   fChain->SetBranchAddress("oldP.timeb", &oldP_timeb, &b_HitMatch_oldP_timeb);
   fChain->SetBranchAddress("oldP.end", &oldP_end, &b_HitMatch_oldP_end);
   Notify();
}

Bool_t hitMateComp::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void hitMateComp::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t hitMateComp::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef hitMateComp_cxx
//________________________________________________________________________________
TH2 *DrawRatio(TCanvas *c1, TH2F *P, TH2F *PAll) {
  
  TH2 *R = new TH2F(*P); // (TH2 *) P->Project3D("yz");
  R->SetName(Form("R%s",R->GetName()));
  //      TH2 *RAll = (TH2 *) PAll->Project3D("yz");
  //      R->Divide(RAll);
  R->Divide(PAll);
  R->SetStats(0);
  R->SetTitle(Form("Ratio of %s", P->GetName()));
  R->Draw("colz");
  c1->Update();
  TPaletteAxis *palette = (TPaletteAxis*)R->GetListOfFunctions()->FindObject("palette");
  if (palette) {
    palette->SetX2NDC(0.94);
    c1->Update();
  }
  return R;
}
//________________________________________________________________________________
void DrawAll() {
  for (Int_t l = 0; l < 4; l++) {
    for (Int_t k = 0; k < 3; k++) {
      TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(Form("c%i",l));
      if (! c1 ) c1 = new TCanvas(Form("c%i",Names[6*l+k]));
      c1->SetLogz(1);
      TH2F *P = (TH2F *) gDirectory->Get(Names[6*l+k].histName);
      TH2F *PAll = (TH2F *) gDirectory->Get(Names[6*l+k+3].histName);
      DrawRatio(c1, P, PAll);
      if (! P || ! PAll) continue;
      TH2 *R = DrawRatio(c1, P, PAll);
    }
  }
}
//________________________________________________________________________________
void TbyThitsPads() {
  TChain *tChain = 0;
  TFile *fOut = 0;
  //  fOut = new TFile("hit.root","update");
  if (! fOut) {
    TDirIter Dir("trackMateFilest_physics_adc*.root");
    TFile *f = 0;
    const Char_t *TreeName = "hitMateComp";
    tChain = new TChain(TreeName);
    Int_t NFiles = 0;
    ULong64_t nEvents = 0;
    ULong64_t nEvTot = 0;
    Char_t *file = 0;
    while ( (file = (Char_t *) Dir.NextFile()) ) {   
      f = new TFile(file);
      if (! f) {cout << "missing file " << file << endl; continue;}
      TTree *tree = (TTree *) f->Get(TreeName);
      cout << "#\t" << NFiles << "\t" << f->GetName();
      if (tree) {
	NFiles++;
	nEvents = tree->GetEntries();
	cout << "\tNo,Events = " << nEvents << endl;
	nEvTot += nEvents;
	tChain->Add(f->GetName());
      } else {
	cout << "\tTTree is missing" << endl;
      }
      delete f; 
    }
    cout	<< "chained " << NFiles  << " files \t" 
		<< "with total " << nEvTot << " events \t" 
		<< "chain returned pointer: " << tChain << endl;
    if (! fOut) fOut = new TFile("hit2D.root","recreate");
    TCanvas *c1 = new TCanvas();
    c1->SetLogz(1);
    for (Int_t k = 0; k < 24; k++) {
      TH2F *hist = (TH2F *) gDirectory->Get("Names[k].histName");
      if ( hist) continue;
      tChain->Draw(Form("%s>>%s(72,0.5,72.5,182,0.5,182.5)",Names[k].varName,Names[k].histName),Form("%s",Names[k].cutName),"goff",100000000);
      hist = (TH2F *) gDirectory->Get("Names[k].histName");
      if (! hist) continue;
      hist->SetXTitle("row");
      hist->SetYTitle("pad");
    }
  }
  DrawAll();
  fOut->Write();
}
//________________________________________________________________________________
void TbyThits() {
  TChain *tChain = 0;
  TDirIter Dir("trackMateFilest_physics_adc*.root");
  TFile *f = 0;
  const Char_t *TreeName = "hitMateComp";
  tChain = new TChain(TreeName);
  Int_t NFiles = 0;
  ULong64_t nEvents = 0;
  ULong64_t nEvTot = 0;
  Char_t *file = 0;
  while ( (file = (Char_t *) Dir.NextFile()) ) {   
    f = new TFile(file);
    if (! f) {cout << "missing file " << file << endl; continue;}
    TTree *tree = (TTree *) f->Get(TreeName);
      cout << "#\t" << NFiles << "\t" << f->GetName();
      if (tree) {
	NFiles++;
	nEvents = tree->GetEntries();
	cout << "\tNo,Events = " << nEvents << endl;
	nEvTot += nEvents;
	tChain->Add(f->GetName());
      } else {
	cout << "\tTTree is missing" << endl;
      }
      delete f; 
  }
  cout	<< "chained " << NFiles  << " files \t" 
	<< "with total " << nEvTot << " events \t" 
	<< "chain returned pointer: " << tChain << endl;
  TFile *fOut =  new TFile("hits.root","recreate");
  hitMateComp t(tChain);
  t.Loop();
  fOut->Write();
}
/*
// 2020 
c1->SetLogz();
gStyle->SetOptStat(0)
RPadFC->SetTitle("Ratio of lost Online Clusters wrt Offline ones")
RPadFC->SetXTitle("row")
RPadFC->SetYTitle("pad")
RPadFC->GetXaxis()->SetRange(1,45);
RPadFC->Draw("colz");

RPadOC->SetTitle("Ratio of lost Offline Clusters wrt Online ones")
RPadOC->SetXTitle("row")
RPadOC->SetYTitle("pad")
RPadOC->GetXaxis()->SetRange(1,45);
RPadOC->SetMaximum(1)
RPadOC->Draw("colz")

PadFCAll->SetTitle("All offline clusters")
PadFCAll->SetXTitle("row")
PadFCAll->SetYTitle("pad")
PadFCAll->GetXaxis()->SetRange(1,45);
PadFCAll->Draw("colz")

PadOCAll->SetTitle("All online clusters")
PadOCAll->SetXTitle("row")
PadOCAll->SetYTitle("pad")
PadOCAll->GetXaxis()->SetRange(1,45);
PadOCAll->Draw("colz")

PminFCAll->SetTitle("All offline clusters")
PminFCAll->SetXTitle("row")
PminFCAll->SetYTitle("min pad")
PminFCAll->GetXaxis()->SetRange(1,45);
PminFCAll->Draw("colz")

PminOCAll->SetTitle("All online clusters")
PminOCAll->SetXTitle("row")
PminOCAll->SetYTitle("min pad")
PminOCAll->GetXaxis()->SetRange(1,45);
PminOCAll->Draw("colz")

================================ Efficiencies ================================================
hitMateComp->Draw("newP.row>>RN(72,0.5,72.5)","newP.sector>0")
hitMateComp->Draw("oldP.row>>RO(72,0.5,72.5)","oldP.sector>0")
hitMateComp->Draw("oldP.row>>RON(72,0.5,72.5)","oldP.sector>0&&newP.sector>0")
TEfficiency *newE = new TEfficiency(*RON,*RO)
TEfficiency *oldE = new TEfficiency(*RON,*RN)

TH1F *frame = c1->DrawFrame(0.5,0.7,72.5,0.96)
frame->SetTitle("Old and New (TPC23) cluster efficiencies")
frame->SetXTitle("row")
newE->Draw("same")
TLegend *l = new TLegend(0.7,0.2,0.9,0.4)
l->AddEntry(newE,"new")
l->Draw()
oldE->SetMarkerColor(2)
oldE->Draw("same")
l->AddEntry(oldE,"old")


fl==0

hitMateComp->Draw("newP.row>>RN(72,0.5,72.5)","newP.sector>0&&newP.fl==0")
hitMateComp->Draw("oldP.row>>RO(72,0.5,72.5)","oldP.sector>0&&oldP.fl==0")
hitMateComp->Draw("oldP.row>>RON(72,0.5,72.5)","oldP.sector>0&&newP.sector>0&&newP.fl==0&&oldP.fl==0")
TEfficiency *newE = new TEfficiency(*RON,*RO)
TEfficiency *oldE = new TEfficiency(*RON,*RN)

TH1F *frame = c1->DrawFrame(0.5,0.7,72.5,0.96)
frame->SetTitle("Old and New (TPC23) cluster efficiencies")
frame->SetXTitle("row")
newE->Draw("same")
TLegend *l = new TLegend(0.7,0.2,0.9,0.4)
l->AddEntry(newE,"new")
l->Draw()
old->SetMarkerColor(2)
oldE->Draw("same")
l->AddEntry(oldE,"old")

================================================================================
c1->SetLogz(1);
 hitMateComp->Draw("newP.pad-oldP.pad:oldP.row>>padR(72,0.5,72.5,64,-0.5,0.5)","oldP.sector>0&&newP.sector>0&&newP.fl==0&&oldP.fl==0","colz")
  padR->SetXTitle("row")
 hitMateComp->Draw("newP.timebucket-oldP.timebucket:oldP.row>>timebucketR(72,0.5,72.5,64,-0.5,0.5)","oldP.sector>0&&newP.sector>0&&newP.fl==0&&oldP.fl==0","colz")
  timebucketR->SetXtitle("row")
  hitMateComp->Draw("TMath::Log(newP.adc/oldP.adc):oldP.row>>adcR(72,0.5,72.5,64,-0.5,0.5)","oldP.sector>0&&newP.sector>0&&newP.fl==0&&oldP.fl==0","colz")
 adcR->SetXTitle("row")
*/

