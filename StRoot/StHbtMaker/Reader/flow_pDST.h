/*
Needed by StHbtFlowPicoReader. Describes the "empirical" (see the note below)
structure of STAR flow pico DST files. Valid as long as that structure is valid.
 */
//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Fri Apr 12 14:53:27 2002 by ROOT version3.02/07)
//   from TTree FlowTree/Flow Pico Tree
//   found on file: /auto/stardata/starspec2/flow_pDST_production/reco/ProductionMinBias/FullField/P02gc/2001/2269018/st_physics_2269018_raw_0001.flowpicoevent.root
//////////////////////////////////////////////////////////


#ifndef flow_pDST_h
#define flow_pDST_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
   const Int_t kMaxfTracks = 7000;

class flow_pDST {
   public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
//Declaration of leaves types
   UInt_t          fUniqueID;
   UInt_t          fBits;
   Int_t           mVersion;
   Int_t           mNtrack;
   Int_t           mEventID;
   Int_t           mRunID;
   Double_t        mMagneticField;
   Double_t        mCenterOfMassEnergy;
   Short_t         mBeamMassNumberEast;
   Short_t         mBeamMassNumberWest;
   UInt_t          mOrigMult;
   UInt_t          mL0TriggerWord;
   UInt_t          mUncorrNegMult;
   UInt_t          mUncorrPosMult;
   UInt_t          mMultEta;
   UInt_t          mCentrality;
   Float_t         mVertexX;
   Float_t         mVertexY;
   Float_t         mVertexZ;
   Float_t         mCTB;
   Float_t         mZDCe;
   Float_t         mZDCw;
   Int_t           fTracks_;
   UInt_t          fTracks_fUniqueID[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_fBits[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mPt[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mPtGlobal[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mEta[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mEtaGlobal[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDedx[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mPhi[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mPhiGlobal[kMaxfTracks];   //[fTracks_]
   Short_t         fTracks_mCharge[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDca[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDcaSigned[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDcaGlobal[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mChi2[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mFitPts[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mMaxPts[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mNhits[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mNdedxPts[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mTrackLength[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mPidPion[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mPidProton[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mPidKaon[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mPidDeuteron[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mPidElectron[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mMostLikelihoodPID[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mMostLikelihoodProb[kMaxfTracks];   //[fTracks_]
   Int_t           fTracks_mExtrapTag[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mElectronPositronProb[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mPionPlusMinusProb[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mKaonPlusMinusProb[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mProtonPbarProb[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDcaGlobalX[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDcaGlobalY[kMaxfTracks];   //[fTracks_]
   Float_t         fTracks_mDcaGlobalZ[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_mTopologyMap0[kMaxfTracks];   //[fTracks_]
   UInt_t          fTracks_mTopologyMap1[kMaxfTracks];   //[fTracks_]

//List of branches
   TBranch        *b_fUniqueID;   //!
   TBranch        *b_fBits;   //!
   TBranch        *b_mVersion;   //!
   TBranch        *b_mNtrack;   //!
   TBranch        *b_mEventID;   //!
   TBranch        *b_mRunID;   //!
   TBranch        *b_mMagneticField;   //!
   TBranch        *b_mCenterOfMassEnergy;   //!
   TBranch        *b_mBeamMassNumberEast;   //!
   TBranch        *b_mBeamMassNumberWest;   //!
   TBranch        *b_mOrigMult;   //!
   TBranch        *b_mL0TriggerWord;   //!
   TBranch        *b_mUncorrNegMult;   //!
   TBranch        *b_mUncorrPosMult;   //!
   TBranch        *b_mMultEta;   //!
   TBranch        *b_mCentrality;   //!
   TBranch        *b_mVertexX;   //!
   TBranch        *b_mVertexY;   //!
   TBranch        *b_mVertexZ;   //!
   TBranch        *b_mCTB;   //!
   TBranch        *b_mZDCe;   //!
   TBranch        *b_mZDCw;   //!
   TBranch        *b_fTracks_;   //!
   TBranch        *b_fTracks_fUniqueID;   //!
   TBranch        *b_fTracks_fBits;   //!
   TBranch        *b_fTracks_mPt;   //!
   TBranch        *b_fTracks_mPtGlobal;   //!
   TBranch        *b_fTracks_mEta;   //!
   TBranch        *b_fTracks_mEtaGlobal;   //!
   TBranch        *b_fTracks_mDedx;   //!
   TBranch        *b_fTracks_mPhi;   //!
   TBranch        *b_fTracks_mPhiGlobal;   //!
   TBranch        *b_fTracks_mCharge;   //!
   TBranch        *b_fTracks_mDca;   //!
   TBranch        *b_fTracks_mDcaSigned;   //!
   TBranch        *b_fTracks_mDcaGlobal;   //!
   TBranch        *b_fTracks_mChi2;   //!
   TBranch        *b_fTracks_mFitPts;   //!
   TBranch        *b_fTracks_mMaxPts;   //!
   TBranch        *b_fTracks_mNhits;   //!
   TBranch        *b_fTracks_mNdedxPts;   //!
   TBranch        *b_fTracks_mTrackLength;   //!
   TBranch        *b_fTracks_mPidPion;   //!
   TBranch        *b_fTracks_mPidProton;   //!
   TBranch        *b_fTracks_mPidKaon;   //!
   TBranch        *b_fTracks_mPidDeuteron;   //!
   TBranch        *b_fTracks_mPidElectron;   //!
   TBranch        *b_fTracks_mMostLikelihoodPID;   //!
   TBranch        *b_fTracks_mMostLikelihoodProb;   //!
   TBranch        *b_fTracks_mExtrapTag;   //!
   TBranch        *b_fTracks_mElectronPositronProb;   //!
   TBranch        *b_fTracks_mPionPlusMinusProb;   //!
   TBranch        *b_fTracks_mKaonPlusMinusProb;   //!
   TBranch        *b_fTracks_mProtonPbarProb;   //!
   TBranch        *b_fTracks_mDcaGlobalX;   //!
   TBranch        *b_fTracks_mDcaGlobalY;   //!
   TBranch        *b_fTracks_mDcaGlobalZ;   //!
   TBranch        *b_fTracks_mTopologyMap0;   //!
   TBranch        *b_fTracks_mTopologyMap1;   //!

   flow_pDST(TTree *tree=0);
   ~flow_pDST();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef flow_pDST_cxx
flow_pDST::flow_pDST(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("/auto/stardata/starspec2/flow_pDST_production/reco/ProductionMinBias/FullField/P02gc/2001/2269018/st_physics_2269018_raw_0001.flowpicoevent.root");
      if (!f) {
         f = new TFile("/auto/stardata/starspec2/flow_pDST_production/reco/ProductionMinBias/FullField/P02gc/2001/2269018/st_physics_2269018_raw_0001.flowpicoevent.root");
      }
      tree = (TTree*)gDirectory->Get("FlowTree");

   }
   Init(tree);
}

flow_pDST::~flow_pDST()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t flow_pDST::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t flow_pDST::LoadTree(Int_t entry)
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

void flow_pDST::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("fUniqueID",&fUniqueID);
   fChain->SetBranchAddress("fBits",&fBits);
   fChain->SetBranchAddress("mVersion",&mVersion);
   fChain->SetBranchAddress("mNtrack",&mNtrack);
   fChain->SetBranchAddress("mEventID",&mEventID);
   fChain->SetBranchAddress("mRunID",&mRunID);
   fChain->SetBranchAddress("mMagneticField",&mMagneticField);
   fChain->SetBranchAddress("mCenterOfMassEnergy",&mCenterOfMassEnergy);
   fChain->SetBranchAddress("mBeamMassNumberEast",&mBeamMassNumberEast);
   fChain->SetBranchAddress("mBeamMassNumberWest",&mBeamMassNumberWest);
   fChain->SetBranchAddress("mOrigMult",&mOrigMult);
   fChain->SetBranchAddress("mL0TriggerWord",&mL0TriggerWord);
   fChain->SetBranchAddress("mUncorrNegMult",&mUncorrNegMult);
   fChain->SetBranchAddress("mUncorrPosMult",&mUncorrPosMult);
   fChain->SetBranchAddress("mMultEta",&mMultEta);
   fChain->SetBranchAddress("mCentrality",&mCentrality);
   fChain->SetBranchAddress("mVertexX",&mVertexX);
   fChain->SetBranchAddress("mVertexY",&mVertexY);
   fChain->SetBranchAddress("mVertexZ",&mVertexZ);
   fChain->SetBranchAddress("mCTB",&mCTB);
   fChain->SetBranchAddress("mZDCe",&mZDCe);
   fChain->SetBranchAddress("mZDCw",&mZDCw);
   fChain->SetBranchAddress("fTracks",&fTracks_);
   fChain->SetBranchAddress("fTracks.fUniqueID",fTracks_fUniqueID);
   fChain->SetBranchAddress("fTracks.fBits",fTracks_fBits);
   fChain->SetBranchAddress("fTracks.mPt",fTracks_mPt);
   fChain->SetBranchAddress("fTracks.mPtGlobal",fTracks_mPtGlobal);
   fChain->SetBranchAddress("fTracks.mEta",fTracks_mEta);
   fChain->SetBranchAddress("fTracks.mEtaGlobal",fTracks_mEtaGlobal);
   fChain->SetBranchAddress("fTracks.mDedx",fTracks_mDedx);
   fChain->SetBranchAddress("fTracks.mPhi",fTracks_mPhi);
   fChain->SetBranchAddress("fTracks.mPhiGlobal",fTracks_mPhiGlobal);
   fChain->SetBranchAddress("fTracks.mCharge",fTracks_mCharge);
   fChain->SetBranchAddress("fTracks.mDca",fTracks_mDca);
   fChain->SetBranchAddress("fTracks.mDcaSigned",fTracks_mDcaSigned);
   fChain->SetBranchAddress("fTracks.mDcaGlobal",fTracks_mDcaGlobal);
   fChain->SetBranchAddress("fTracks.mChi2",fTracks_mChi2);
   fChain->SetBranchAddress("fTracks.mFitPts",fTracks_mFitPts);
   fChain->SetBranchAddress("fTracks.mMaxPts",fTracks_mMaxPts);
   fChain->SetBranchAddress("fTracks.mNhits",fTracks_mNhits);
   fChain->SetBranchAddress("fTracks.mNdedxPts",fTracks_mNdedxPts);
   fChain->SetBranchAddress("fTracks.mTrackLength",fTracks_mTrackLength);
   fChain->SetBranchAddress("fTracks.mPidPion",fTracks_mPidPion);
   fChain->SetBranchAddress("fTracks.mPidProton",fTracks_mPidProton);
   fChain->SetBranchAddress("fTracks.mPidKaon",fTracks_mPidKaon);
   fChain->SetBranchAddress("fTracks.mPidDeuteron",fTracks_mPidDeuteron);
   fChain->SetBranchAddress("fTracks.mPidElectron",fTracks_mPidElectron);
   fChain->SetBranchAddress("fTracks.mMostLikelihoodPID",fTracks_mMostLikelihoodPID);
   fChain->SetBranchAddress("fTracks.mMostLikelihoodProb",fTracks_mMostLikelihoodProb);
   fChain->SetBranchAddress("fTracks.mExtrapTag",fTracks_mExtrapTag);
   fChain->SetBranchAddress("fTracks.mElectronPositronProb",fTracks_mElectronPositronProb);
   fChain->SetBranchAddress("fTracks.mPionPlusMinusProb",fTracks_mPionPlusMinusProb);
   fChain->SetBranchAddress("fTracks.mKaonPlusMinusProb",fTracks_mKaonPlusMinusProb);
   fChain->SetBranchAddress("fTracks.mProtonPbarProb",fTracks_mProtonPbarProb);
   fChain->SetBranchAddress("fTracks.mDcaGlobalX",fTracks_mDcaGlobalX);
   fChain->SetBranchAddress("fTracks.mDcaGlobalY",fTracks_mDcaGlobalY);
   fChain->SetBranchAddress("fTracks.mDcaGlobalZ",fTracks_mDcaGlobalZ);
   fChain->SetBranchAddress("fTracks.mTopologyMap0",fTracks_mTopologyMap0);
   fChain->SetBranchAddress("fTracks.mTopologyMap1",fTracks_mTopologyMap1);
   Notify();
}

Bool_t flow_pDST::Notify()
{
   // Called when loading a new file.
   // Get branch pointers.
   b_fUniqueID = fChain->GetBranch("fUniqueID");
   b_fBits = fChain->GetBranch("fBits");
   b_mVersion = fChain->GetBranch("mVersion");
   b_mNtrack = fChain->GetBranch("mNtrack");
   b_mEventID = fChain->GetBranch("mEventID");
   b_mRunID = fChain->GetBranch("mRunID");
   b_mMagneticField = fChain->GetBranch("mMagneticField");
   b_mCenterOfMassEnergy = fChain->GetBranch("mCenterOfMassEnergy");
   b_mBeamMassNumberEast = fChain->GetBranch("mBeamMassNumberEast");
   b_mBeamMassNumberWest = fChain->GetBranch("mBeamMassNumberWest");
   b_mOrigMult = fChain->GetBranch("mOrigMult");
   b_mL0TriggerWord = fChain->GetBranch("mL0TriggerWord");
   b_mUncorrNegMult = fChain->GetBranch("mUncorrNegMult");
   b_mUncorrPosMult = fChain->GetBranch("mUncorrPosMult");
   b_mMultEta = fChain->GetBranch("mMultEta");
   b_mCentrality = fChain->GetBranch("mCentrality");
   b_mVertexX = fChain->GetBranch("mVertexX");
   b_mVertexY = fChain->GetBranch("mVertexY");
   b_mVertexZ = fChain->GetBranch("mVertexZ");
   b_mCTB = fChain->GetBranch("mCTB");
   b_mZDCe = fChain->GetBranch("mZDCe");
   b_mZDCw = fChain->GetBranch("mZDCw");
   b_fTracks_ = fChain->GetBranch("fTracks");
   b_fTracks_fUniqueID = fChain->GetBranch("fTracks.fUniqueID");
   b_fTracks_fBits = fChain->GetBranch("fTracks.fBits");
   b_fTracks_mPt = fChain->GetBranch("fTracks.mPt");
   b_fTracks_mPtGlobal = fChain->GetBranch("fTracks.mPtGlobal");
   b_fTracks_mEta = fChain->GetBranch("fTracks.mEta");
   b_fTracks_mEtaGlobal = fChain->GetBranch("fTracks.mEtaGlobal");
   b_fTracks_mDedx = fChain->GetBranch("fTracks.mDedx");
   b_fTracks_mPhi = fChain->GetBranch("fTracks.mPhi");
   b_fTracks_mPhiGlobal = fChain->GetBranch("fTracks.mPhiGlobal");
   b_fTracks_mCharge = fChain->GetBranch("fTracks.mCharge");
   b_fTracks_mDca = fChain->GetBranch("fTracks.mDca");
   b_fTracks_mDcaSigned = fChain->GetBranch("fTracks.mDcaSigned");
   b_fTracks_mDcaGlobal = fChain->GetBranch("fTracks.mDcaGlobal");
   b_fTracks_mChi2 = fChain->GetBranch("fTracks.mChi2");
   b_fTracks_mFitPts = fChain->GetBranch("fTracks.mFitPts");
   b_fTracks_mMaxPts = fChain->GetBranch("fTracks.mMaxPts");
   b_fTracks_mNhits = fChain->GetBranch("fTracks.mNhits");
   b_fTracks_mNdedxPts = fChain->GetBranch("fTracks.mNdedxPts");
   b_fTracks_mTrackLength = fChain->GetBranch("fTracks.mTrackLength");
   b_fTracks_mPidPion = fChain->GetBranch("fTracks.mPidPion");
   b_fTracks_mPidProton = fChain->GetBranch("fTracks.mPidProton");
   b_fTracks_mPidKaon = fChain->GetBranch("fTracks.mPidKaon");
   b_fTracks_mPidDeuteron = fChain->GetBranch("fTracks.mPidDeuteron");
   b_fTracks_mPidElectron = fChain->GetBranch("fTracks.mPidElectron");
   b_fTracks_mMostLikelihoodPID = fChain->GetBranch("fTracks.mMostLikelihoodPID");
   b_fTracks_mMostLikelihoodProb = fChain->GetBranch("fTracks.mMostLikelihoodProb");
   b_fTracks_mExtrapTag = fChain->GetBranch("fTracks.mExtrapTag");
   b_fTracks_mElectronPositronProb = fChain->GetBranch("fTracks.mElectronPositronProb");
   b_fTracks_mPionPlusMinusProb = fChain->GetBranch("fTracks.mPionPlusMinusProb");
   b_fTracks_mKaonPlusMinusProb = fChain->GetBranch("fTracks.mKaonPlusMinusProb");
   b_fTracks_mProtonPbarProb = fChain->GetBranch("fTracks.mProtonPbarProb");
   b_fTracks_mDcaGlobalX = fChain->GetBranch("fTracks.mDcaGlobalX");
   b_fTracks_mDcaGlobalY = fChain->GetBranch("fTracks.mDcaGlobalY");
   b_fTracks_mDcaGlobalZ = fChain->GetBranch("fTracks.mDcaGlobalZ");
   b_fTracks_mTopologyMap0 = fChain->GetBranch("fTracks.mTopologyMap0");
   b_fTracks_mTopologyMap1 = fChain->GetBranch("fTracks.mTopologyMap1");
   return kTRUE;
}

void flow_pDST::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t flow_pDST::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef flow_pDST_cxx

