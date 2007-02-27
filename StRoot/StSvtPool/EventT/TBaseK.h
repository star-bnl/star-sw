//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Mon Feb  5 20:23:25 2007 by ROOT version 5.15/01
// from TTree T/TTree with SVT + SSD hits and tracks
// found on file: /star/data09/calib/fisyak/Pass112/TpcSsd/065/Event_6065045_raw_1010001.root
//////////////////////////////////////////////////////////

#ifndef TBase_h
#define TBase_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
   const Int_t kMaxfTracks = 10000;
   const Int_t kMaxfHits  = 100000;

class TBase {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leave types
 //EventT          *EventT;
   UInt_t          fUniqueID;
   UInt_t          fBits;
   UInt_t          fNPTracks;
   UInt_t          fNtrack;
   UInt_t          fNhit;
   UInt_t          fFlag;
   Int_t           fEvtHdr_fEvtNum;
   Int_t           fEvtHdr_fRun;
   Int_t           fEvtHdr_fDate;
   Double32_t      fEvtHdr_fField;
   Double32_t      fVertex[3];
   Double32_t      fCovariantMatrix[6];
   Int_t           fTracks_;
   UInt_t          fTracks_fUniqueID[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_fBits[kMaxfTracks];   //[fTracks_]
   Char_t          fTracks_beg[kMaxfTracks];   //[fTracks_]
   Double32_t      fTracks_fInvpT[kMaxfTracks];   //[fTracks_]
   Double32_t      fTracks_fTanL[kMaxfTracks];   //[fTracks_]
   Double32_t      fTracks_fPhi[kMaxfTracks];   //[fTracks_]
   Double32_t      fTracks_fRho[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_fNpoint[kMaxfTracks];   //[fTracks_]
   Short_t         fTracks_fValid[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_fNsp[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_fIdHitT[kMaxfTracks][1000];   //[fTracks_]
   Double32_t      fTracks_fdEdx[kMaxfTracks];   //[fTracks_]
   Double32_t      fTracks_fLength[kMaxfTracks];   //[fTracks_]
   Char_t          fTracks_end[kMaxfTracks];   //[fTracks_]
   Int_t           fHits_;
   UInt_t          fHits_fUniqueID[kMaxfHits];   //[fHits_]
   UInt_t          fHits_fBits[kMaxfHits];   //[fHits_]
   Char_t          fHits_start[kMaxfHits];   //[fHits_]
   Int_t           fHits_Id[kMaxfHits];   //[fHits_]
   Int_t           fHits_sector[kMaxfHits];   //[fHits_]
   Int_t           fHits_barrel[kMaxfHits];   //[fHits_]
   Int_t           fHits_layer[kMaxfHits];   //[fHits_]
   Int_t           fHits_ladder[kMaxfHits];   //[fHits_]
   Int_t           fHits_wafer[kMaxfHits];   //[fHits_]
   Int_t           fHits_hybrid[kMaxfHits];   //[fHits_]
   Int_t           fHits_rdo[kMaxfHits];   //[fHits_]
   Double32_t      fHits_xG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_yG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_zG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_xGC[kMaxfHits];   //[fHits_]
   Double32_t      fHits_yGC[kMaxfHits];   //[fHits_]
   Double32_t      fHits_zGC[kMaxfHits];   //[fHits_]
   Double32_t      fHits_xL[kMaxfHits];   //[fHits_]
   Double32_t      fHits_yL[kMaxfHits];   //[fHits_]
   Double32_t      fHits_zL[kMaxfHits];   //[fHits_]
   Double32_t      fHits_u[kMaxfHits];   //[fHits_]
   Double32_t      fHits_v[kMaxfHits];   //[fHits_]
   Double32_t      fHits_w[kMaxfHits];   //[fHits_]
   Double32_t      fHits_tuP[kMaxfHits];   //[fHits_]
   Double32_t      fHits_tvP[kMaxfHits];   //[fHits_]
   Double32_t      fHits_uP[kMaxfHits];   //[fHits_]
   Double32_t      fHits_vP[kMaxfHits];   //[fHits_]
   Double32_t      fHits_pT[kMaxfHits];   //[fHits_]
   Double32_t      fHits_pMom[kMaxfHits];   //[fHits_]
   Double32_t      fHits_xPG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_yPG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_zPG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_cxPG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_cyPG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_czPG[kMaxfHits];   //[fHits_]
   Double32_t      fHits_wGu[kMaxfHits];   //[fHits_]
   Double32_t      fHits_wGv[kMaxfHits];   //[fHits_]
   Double32_t      fHits_wGw[kMaxfHits];   //[fHits_]
   Double32_t      fHits_xPL[kMaxfHits];   //[fHits_]
   Double32_t      fHits_yPL[kMaxfHits];   //[fHits_]
   Double32_t      fHits_zPL[kMaxfHits];   //[fHits_]
   Double32_t      fHits_uM[kMaxfHits];   //[fHits_]
   Double32_t      fHits_vM[kMaxfHits];   //[fHits_]
   Double32_t      fHits_anode[kMaxfHits];   //[fHits_]
   Double32_t      fHits_timeb[kMaxfHits];   //[fHits_]
   Int_t           fHits_NoHitPerTrack[kMaxfHits];   //[fHits_]
   Char_t          fHits_end[kMaxfHits];   //[fHits_]
   TRef            fLastTrackT;
   TRef            fLastHitT;
   Bool_t          fIsValid;

   // List of branches
   TBranch        *b_EventT_fUniqueID;   //!
   TBranch        *b_EventT_fBits;   //!
   TBranch        *b_EventT_fNPTracks;   //!
   TBranch        *b_EventT_fNtrack;   //!
   TBranch        *b_EventT_fNhit;   //!
   TBranch        *b_EventT_fFlag;   //!
   TBranch        *b_EventT_fEvtHdr_fEvtNum;   //!
   TBranch        *b_EventT_fEvtHdr_fRun;   //!
   TBranch        *b_EventT_fEvtHdr_fDate;   //!
   TBranch        *b_EventT_fEvtHdr_fField;   //!
   TBranch        *b_EventT_fVertex;   //!
   TBranch        *b_EventT_fCovariantMatrix;   //!
   TBranch        *b_EventT_fTracks_;   //!
   TBranch        *b_fTracks_fUniqueID;   //!
   TBranch        *b_fTracks_fBits;   //!
   TBranch        *b_fTracks_beg;   //!
   TBranch        *b_fTracks_fInvpT;   //!
   TBranch        *b_fTracks_fTanL;   //!
   TBranch        *b_fTracks_fPhi;   //!
   TBranch        *b_fTracks_fRho;   //!
   TBranch        *b_fTracks_fNpoint;   //!
   TBranch        *b_fTracks_fValid;   //!
   TBranch        *b_fTracks_fNsp;   //!
   TBranch        *b_fTracks_fIdHitT;   //!
   TBranch        *b_fTracks_fdEdx;   //!
   TBranch        *b_fTracks_fLength;   //!
   TBranch        *b_fTracks_end;   //!
   TBranch        *b_EventT_fHits_;   //!
   TBranch        *b_fHits_fUniqueID;   //!
   TBranch        *b_fHits_fBits;   //!
   TBranch        *b_fHits_start;   //!
   TBranch        *b_fHits_Id;   //!
   TBranch        *b_fHits_sector;   //!
   TBranch        *b_fHits_barrel;   //!
   TBranch        *b_fHits_layer;   //!
   TBranch        *b_fHits_ladder;   //!
   TBranch        *b_fHits_wafer;   //!
   TBranch        *b_fHits_hybrid;   //!
   TBranch        *b_fHits_rdo;   //!
   TBranch        *b_fHits_xG;   //!
   TBranch        *b_fHits_yG;   //!
   TBranch        *b_fHits_zG;   //!
   TBranch        *b_fHits_xGC;   //!
   TBranch        *b_fHits_yGC;   //!
   TBranch        *b_fHits_zGC;   //!
   TBranch        *b_fHits_xL;   //!
   TBranch        *b_fHits_yL;   //!
   TBranch        *b_fHits_zL;   //!
   TBranch        *b_fHits_u;   //!
   TBranch        *b_fHits_v;   //!
   TBranch        *b_fHits_w;   //!
   TBranch        *b_fHits_tuP;   //!
   TBranch        *b_fHits_tvP;   //!
   TBranch        *b_fHits_uP;   //!
   TBranch        *b_fHits_vP;   //!
   TBranch        *b_fHits_pT;   //!
   TBranch        *b_fHits_pMom;   //!
   TBranch        *b_fHits_xPG;   //!
   TBranch        *b_fHits_yPG;   //!
   TBranch        *b_fHits_zPG;   //!
   TBranch        *b_fHits_cxPG;   //!
   TBranch        *b_fHits_cyPG;   //!
   TBranch        *b_fHits_czPG;   //!
   TBranch        *b_fHits_wGu;   //!
   TBranch        *b_fHits_wGv;   //!
   TBranch        *b_fHits_wGw;   //!
   TBranch        *b_fHits_xPL;   //!
   TBranch        *b_fHits_yPL;   //!
   TBranch        *b_fHits_zPL;   //!
   TBranch        *b_fHits_uM;   //!
   TBranch        *b_fHits_vM;   //!
   TBranch        *b_fHits_anode;   //!
   TBranch        *b_fHits_timeb;   //!
   TBranch        *b_fHits_NoHitPerTrack;   //!
   TBranch        *b_fHits_end;   //!
   TBranch        *b_EventT_fLastTrackT;   //!
   TBranch        *b_EventT_fLastHitT;   //!
   TBranch        *b_EventT_fIsValid;   //!

   TBase();

   TBase(TTree *tree);
   virtual ~TBase();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop(){};
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

//#endif

//#ifdef TBase_cxx

TBase::TBase(){} 

TBase::TBase(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("/star/data09/calib/fisyak/Pass112/TpcSsd/065/Event_6065045_raw_1010001.root");
      if (!f) {
         f = new TFile("/star/data09/calib/fisyak/Pass112/TpcSsd/065/Event_6065045_raw_1010001.root");
      }
      tree = (TTree*)gDirectory->Get("T");

   }
   Init(tree);
}

TBase::~TBase()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t TBase::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t TBase::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (!fChain->InheritsFrom(TChain::Class()))  return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void TBase::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normaly not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("fUniqueID", &fUniqueID, &b_EventT_fUniqueID);
   fChain->SetBranchAddress("fBits", &fBits, &b_EventT_fBits);
   fChain->SetBranchAddress("fNPTracks", &fNPTracks, &b_EventT_fNPTracks);
   fChain->SetBranchAddress("fNtrack", &fNtrack, &b_EventT_fNtrack);
   fChain->SetBranchAddress("fNhit", &fNhit, &b_EventT_fNhit);
   fChain->SetBranchAddress("fFlag", &fFlag, &b_EventT_fFlag);
   fChain->SetBranchAddress("fEvtHdr.fEvtNum", &fEvtHdr_fEvtNum, &b_EventT_fEvtHdr_fEvtNum);
   fChain->SetBranchAddress("fEvtHdr.fRun", &fEvtHdr_fRun, &b_EventT_fEvtHdr_fRun);
   fChain->SetBranchAddress("fEvtHdr.fDate", &fEvtHdr_fDate, &b_EventT_fEvtHdr_fDate);
   fChain->SetBranchAddress("fEvtHdr.fField", &fEvtHdr_fField, &b_EventT_fEvtHdr_fField);
   fChain->SetBranchAddress("fVertex[3]", fVertex, &b_EventT_fVertex);
   fChain->SetBranchAddress("fCovariantMatrix[6]", fCovariantMatrix, &b_EventT_fCovariantMatrix);
   fChain->SetBranchAddress("fTracks", &fTracks_, &b_EventT_fTracks_);
   fChain->SetBranchAddress("fTracks.fUniqueID", fTracks_fUniqueID, &b_fTracks_fUniqueID);
   fChain->SetBranchAddress("fTracks.fBits", fTracks_fBits, &b_fTracks_fBits);
   fChain->SetBranchAddress("fTracks.beg", fTracks_beg, &b_fTracks_beg);
   fChain->SetBranchAddress("fTracks.fInvpT", fTracks_fInvpT, &b_fTracks_fInvpT);
   fChain->SetBranchAddress("fTracks.fTanL", fTracks_fTanL, &b_fTracks_fTanL);
   fChain->SetBranchAddress("fTracks.fPhi", fTracks_fPhi, &b_fTracks_fPhi);
   fChain->SetBranchAddress("fTracks.fRho", fTracks_fRho, &b_fTracks_fRho);
   fChain->SetBranchAddress("fTracks.fNpoint", fTracks_fNpoint, &b_fTracks_fNpoint);
   fChain->SetBranchAddress("fTracks.fValid", fTracks_fValid, &b_fTracks_fValid);
   fChain->SetBranchAddress("fTracks.fNsp", fTracks_fNsp, &b_fTracks_fNsp);
   fChain->SetBranchAddress("fTracks.fIdHitT[1000]", fTracks_fIdHitT, &b_fTracks_fIdHitT);
   fChain->SetBranchAddress("fTracks.fdEdx", fTracks_fdEdx, &b_fTracks_fdEdx);
   fChain->SetBranchAddress("fTracks.fLength", fTracks_fLength, &b_fTracks_fLength);
   fChain->SetBranchAddress("fTracks.end", fTracks_end, &b_fTracks_end);
   fChain->SetBranchAddress("fHits", &fHits_, &b_EventT_fHits_);
   fChain->SetBranchAddress("fHits.fUniqueID", fHits_fUniqueID, &b_fHits_fUniqueID);
   fChain->SetBranchAddress("fHits.fBits", fHits_fBits, &b_fHits_fBits);
   fChain->SetBranchAddress("fHits.start", fHits_start, &b_fHits_start);
   fChain->SetBranchAddress("fHits.Id", fHits_Id, &b_fHits_Id);
   fChain->SetBranchAddress("fHits.sector", fHits_sector, &b_fHits_sector);
   fChain->SetBranchAddress("fHits.barrel", fHits_barrel, &b_fHits_barrel);
   fChain->SetBranchAddress("fHits.layer", fHits_layer, &b_fHits_layer);
   fChain->SetBranchAddress("fHits.ladder", fHits_ladder, &b_fHits_ladder);
   fChain->SetBranchAddress("fHits.wafer", fHits_wafer, &b_fHits_wafer);
   fChain->SetBranchAddress("fHits.hybrid", fHits_hybrid, &b_fHits_hybrid);
   fChain->SetBranchAddress("fHits.rdo", fHits_rdo, &b_fHits_rdo);
   fChain->SetBranchAddress("fHits.xG", fHits_xG, &b_fHits_xG);
   fChain->SetBranchAddress("fHits.yG", fHits_yG, &b_fHits_yG);
   fChain->SetBranchAddress("fHits.zG", fHits_zG, &b_fHits_zG);
   fChain->SetBranchAddress("fHits.xGC", fHits_xGC, &b_fHits_xGC);
   fChain->SetBranchAddress("fHits.yGC", fHits_yGC, &b_fHits_yGC);
   fChain->SetBranchAddress("fHits.zGC", fHits_zGC, &b_fHits_zGC);
   fChain->SetBranchAddress("fHits.xL", fHits_xL, &b_fHits_xL);
   fChain->SetBranchAddress("fHits.yL", fHits_yL, &b_fHits_yL);
   fChain->SetBranchAddress("fHits.zL", fHits_zL, &b_fHits_zL);
   fChain->SetBranchAddress("fHits.u", fHits_u, &b_fHits_u);
   fChain->SetBranchAddress("fHits.v", fHits_v, &b_fHits_v);
   fChain->SetBranchAddress("fHits.w", fHits_w, &b_fHits_w);
   fChain->SetBranchAddress("fHits.tuP", fHits_tuP, &b_fHits_tuP);
   fChain->SetBranchAddress("fHits.tvP", fHits_tvP, &b_fHits_tvP);
   fChain->SetBranchAddress("fHits.uP", fHits_uP, &b_fHits_uP);
   fChain->SetBranchAddress("fHits.vP", fHits_vP, &b_fHits_vP);
   fChain->SetBranchAddress("fHits.pT", fHits_pT, &b_fHits_pT);
   fChain->SetBranchAddress("fHits.pMom", fHits_pMom, &b_fHits_pMom);
   fChain->SetBranchAddress("fHits.xPG", fHits_xPG, &b_fHits_xPG);
   fChain->SetBranchAddress("fHits.yPG", fHits_yPG, &b_fHits_yPG);
   fChain->SetBranchAddress("fHits.zPG", fHits_zPG, &b_fHits_zPG);
   fChain->SetBranchAddress("fHits.cxPG", fHits_cxPG, &b_fHits_cxPG);
   fChain->SetBranchAddress("fHits.cyPG", fHits_cyPG, &b_fHits_cyPG);
   fChain->SetBranchAddress("fHits.czPG", fHits_czPG, &b_fHits_czPG);
   fChain->SetBranchAddress("fHits.wGu", fHits_wGu, &b_fHits_wGu);
   fChain->SetBranchAddress("fHits.wGv", fHits_wGv, &b_fHits_wGv);
   fChain->SetBranchAddress("fHits.wGw", fHits_wGw, &b_fHits_wGw);
   fChain->SetBranchAddress("fHits.xPL", fHits_xPL, &b_fHits_xPL);
   fChain->SetBranchAddress("fHits.yPL", fHits_yPL, &b_fHits_yPL);
   fChain->SetBranchAddress("fHits.zPL", fHits_zPL, &b_fHits_zPL);
   fChain->SetBranchAddress("fHits.uM", fHits_uM, &b_fHits_uM);
   fChain->SetBranchAddress("fHits.vM", fHits_vM, &b_fHits_vM);
   fChain->SetBranchAddress("fHits.anode", fHits_anode, &b_fHits_anode);
   fChain->SetBranchAddress("fHits.timeb", fHits_timeb, &b_fHits_timeb);
   fChain->SetBranchAddress("fHits.NoHitPerTrack", fHits_NoHitPerTrack, &b_fHits_NoHitPerTrack);
   fChain->SetBranchAddress("fHits.end", fHits_end, &b_fHits_end);
   fChain->SetBranchAddress("fLastTrackT", &fLastTrackT, &b_EventT_fLastTrackT);
   fChain->SetBranchAddress("fLastHitT", &fLastHitT, &b_EventT_fLastHitT);
   fChain->SetBranchAddress("fIsValid", &fIsValid, &b_EventT_fIsValid);
   Notify();
}

Bool_t TBase::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normaly not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void TBase::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t TBase::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
  if (entry);
   return 1;
}
#endif 
