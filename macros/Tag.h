//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Tue Oct  3 07:40:19 2000 by ROOT version2.25/03)
//   from TTree Tag/
//   found on file: Memory Directory
//////////////////////////////////////////////////////////


#ifndef Tag_h
#define Tag_h

#include <TChain.h>
#include <TFile.h>

class Tag {
   public :
   TTree          *fChain;   //pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //current Tree number in a TChain
//Declaration of leaves types
   Int_t           TpcTag_n_clus_tpc_tot;
   Int_t           TpcTag_n_clus_tpc_in[24];
   Int_t           TpcTag_n_clus_tpc_out[24];
   Int_t           TpcTag_n_pts_tpc_tot;
   Int_t           TpcTag_n_pts_tpc_in[24];
   Int_t           TpcTag_n_pts_tpc_out[24];
   Int_t           TpcTag_n_trk_tpc[2];
   Float_t         TpcTag_chrg_tpc_drift[10];
   Float_t         TpcTag_chrg_tpc_tot;
   Float_t         TpcTag_chrg_tpc_in[24];
   Float_t         TpcTag_chrg_tpc_out[24];
   Float_t         TpcTag_hit_frac_tpc[2];
   Float_t         TpcTag_avg_trkL_tpc[2];
   Float_t         TpcTag_res_pad_tpc[2];
   Float_t         TpcTag_res_drf_tpc[2];
   Float_t         FlowTag_qxa[6];
   Float_t         FlowTag_qxb[6];
   Float_t         FlowTag_qxc[6];
   Float_t         FlowTag_qxd[6];
   Float_t         FlowTag_qya[6];
   Float_t         FlowTag_qyb[6];
   Float_t         FlowTag_qyc[6];
   Float_t         FlowTag_qyd[6];
   Int_t           FlowTag_na[6];
   Int_t           FlowTag_nb[6];
   Int_t           FlowTag_nc[6];
   Int_t           FlowTag_nd[6];
   Float_t         FlowTag_mpta[6];
   Float_t         FlowTag_mptb[6];
   Float_t         FlowTag_mptc[6];
   Float_t         FlowTag_mptd[6];
   Int_t           StrangeTag_NV0;
   Int_t           StrangeTag_NbelowK0;
   Int_t           StrangeTag_NK0;
   Int_t           StrangeTag_NaboveK0;
   Int_t           StrangeTag_NbelowLa;
   Int_t           StrangeTag_NLa;
   Int_t           StrangeTag_NaboveLa;
   Int_t           StrangeTag_NbelowLb;
   Int_t           StrangeTag_NLb;
   Int_t           StrangeTag_NaboveLb;
   Int_t           StrangeTag_NbelowXi;
   Int_t           StrangeTag_NXi;
   Int_t           StrangeTag_NaboveXi;
   Float_t         StrangeTag_range;
   Float_t         ScaTag_scaAnalysisMatrix[144];
   Float_t         ScaTag_chargedParticles_Means[10];
   Float_t         ScaTag_chargedParticles_Sigmas[10];
   Int_t           PCollTag_chargedMultiplicity;
   Int_t           PCollTag_numberOfPhotonClusters;
   Int_t           PCollTag_numberOfElectronsInEmc;
   Int_t           PCollTag_numberOfMwpcHits;
   Int_t           PCollTag_numberOfPrimaryTracks;
   Float_t         PCollTag_vectorSumOfPt;
   Float_t         PCollTag_totalCharge;
   Float_t         PCollTag_pseudorapidityOfEvent;
   Float_t         PCollTag_primaryVertexX;
   Float_t         PCollTag_primaryVertexY;
   Float_t         PCollTag_primaryVertexZ;
   Float_t         PCollTag_zdc1Energy;
   Float_t         PCollTag_zdc2Energy;
   Int_t           EvtHddr_mRunNumber;
   Int_t           EvtHddr_mOldRunNumber;
   Int_t           EvtHddr_mId;
   Int_t           EvtHddr_mInputTriggerMask;
   Int_t           EvtHddr_mTriggerMask;
   Float_t         EvtHddr_mCenterOfMassEnergy;
   Int_t           EvtHddr_mAEast;
   Int_t           EvtHddr_mZEast;
   Int_t           EvtHddr_mAWest;
   Int_t           EvtHddr_mZWest;
   Float_t         EvtHddr_mLuminosity;
   Float_t         EvtHddr_mBeamPolarizationEast[3];
   Float_t         EvtHddr_mBeamPolarizationWest[3];
   Float_t         EvtHddr_mBImpact;
   Float_t         EvtHddr_mPhImpact;
   Int_t           EvtHddr_mGenerType;
   Int_t           EvtHddr_mBunchCrossingNumber;
   Int_t           EvtHddr_mIventNumber;
   Int_t           EvtHddr_mEventSize;
   Int_t           EvtHddr_mEventNumber;
   Double_t        EvtHddr_mEventTime;
   Double_t        EvtHddr_mProdTime;
   Char_t          EvtHddr_mEventType[16];

//List of branches
   TBranch        *b_TpcTag_n_clus_tpc_tot;
   TBranch        *b_TpcTag_n_clus_tpc_in;
   TBranch        *b_TpcTag_n_clus_tpc_out;
   TBranch        *b_TpcTag_n_pts_tpc_tot;
   TBranch        *b_TpcTag_n_pts_tpc_in;
   TBranch        *b_TpcTag_n_pts_tpc_out;
   TBranch        *b_TpcTag_n_trk_tpc;
   TBranch        *b_TpcTag_chrg_tpc_drift;
   TBranch        *b_TpcTag_chrg_tpc_tot;
   TBranch        *b_TpcTag_chrg_tpc_in;
   TBranch        *b_TpcTag_chrg_tpc_out;
   TBranch        *b_TpcTag_hit_frac_tpc;
   TBranch        *b_TpcTag_avg_trkL_tpc;
   TBranch        *b_TpcTag_res_pad_tpc;
   TBranch        *b_TpcTag_res_drf_tpc;
   TBranch        *b_FlowTag_qxa;
   TBranch        *b_FlowTag_qxb;
   TBranch        *b_FlowTag_qxc;
   TBranch        *b_FlowTag_qxd;
   TBranch        *b_FlowTag_qya;
   TBranch        *b_FlowTag_qyb;
   TBranch        *b_FlowTag_qyc;
   TBranch        *b_FlowTag_qyd;
   TBranch        *b_FlowTag_na;
   TBranch        *b_FlowTag_nb;
   TBranch        *b_FlowTag_nc;
   TBranch        *b_FlowTag_nd;
   TBranch        *b_FlowTag_mpta;
   TBranch        *b_FlowTag_mptb;
   TBranch        *b_FlowTag_mptc;
   TBranch        *b_FlowTag_mptd;
   TBranch        *b_StrangeTag_NV0;
   TBranch        *b_StrangeTag_NbelowK0;
   TBranch        *b_StrangeTag_NK0;
   TBranch        *b_StrangeTag_NaboveK0;
   TBranch        *b_StrangeTag_NbelowLa;
   TBranch        *b_StrangeTag_NLa;
   TBranch        *b_StrangeTag_NaboveLa;
   TBranch        *b_StrangeTag_NbelowLb;
   TBranch        *b_StrangeTag_NLb;
   TBranch        *b_StrangeTag_NaboveLb;
   TBranch        *b_StrangeTag_NbelowXi;
   TBranch        *b_StrangeTag_NXi;
   TBranch        *b_StrangeTag_NaboveXi;
   TBranch        *b_StrangeTag_range;
   TBranch        *b_ScaTag_scaAnalysisMatrix;
   TBranch        *b_ScaTag_chargedParticles_Means;
   TBranch        *b_ScaTag_chargedParticles_Sigmas;
   TBranch        *b_PCollTag_chargedMultiplicity;
   TBranch        *b_PCollTag_numberOfPhotonClusters;
   TBranch        *b_PCollTag_numberOfElectronsInEmc;
   TBranch        *b_PCollTag_numberOfMwpcHits;
   TBranch        *b_PCollTag_numberOfPrimaryTracks;
   TBranch        *b_PCollTag_vectorSumOfPt;
   TBranch        *b_PCollTag_totalCharge;
   TBranch        *b_PCollTag_pseudorapidityOfEvent;
   TBranch        *b_PCollTag_primaryVertexX;
   TBranch        *b_PCollTag_primaryVertexY;
   TBranch        *b_PCollTag_primaryVertexZ;
   TBranch        *b_PCollTag_zdc1Energy;
   TBranch        *b_PCollTag_zdc2Energy;
   TBranch        *b_EvtHddr_mRunNumber;
   TBranch        *b_EvtHddr_mOldRunNumber;
   TBranch        *b_EvtHddr_mId;
   TBranch        *b_EvtHddr_mInputTriggerMask;
   TBranch        *b_EvtHddr_mTriggerMask;
   TBranch        *b_EvtHddr_mCenterOfMassEnergy;
   TBranch        *b_EvtHddr_mAEast;
   TBranch        *b_EvtHddr_mZEast;
   TBranch        *b_EvtHddr_mAWest;
   TBranch        *b_EvtHddr_mZWest;
   TBranch        *b_EvtHddr_mLuminosity;
   TBranch        *b_EvtHddr_mBeamPolarizationEast;
   TBranch        *b_EvtHddr_mBeamPolarizationWest;
   TBranch        *b_EvtHddr_mBImpact;
   TBranch        *b_EvtHddr_mPhImpact;
   TBranch        *b_EvtHddr_mGenerType;
   TBranch        *b_EvtHddr_mBunchCrossingNumber;
   TBranch        *b_EvtHddr_mIventNumber;
   TBranch        *b_EvtHddr_mEventSize;
   TBranch        *b_EvtHddr_mEventNumber;
   TBranch        *b_EvtHddr_mEventTime;
   TBranch        *b_EvtHddr_mProdTime;
   TBranch        *b_EvtHddr_mEventType;

   Tag(TTree *tree=0);
   ~Tag();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef Tag_cxx
Tag::Tag(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("Memory Directory");
      if (!f) {
         f = new TFile("Memory Directory");
         f->cd("Rint:/");
      }
      tree = (TTree*)gDirectory->Get("Tag");

   }
   Init(tree);
}

Tag::~Tag()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t Tag::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t Tag::LoadTree(Int_t entry)
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

void Tag::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;

   fChain->SetBranchAddress("TpcTag.n_clus_tpc_tot",&n_clus_tpc_tot);
   fChain->SetBranchAddress("TpcTag.n_clus_tpc_in",n_clus_tpc_in);
   fChain->SetBranchAddress("TpcTag.n_clus_tpc_out",n_clus_tpc_out);
   fChain->SetBranchAddress("TpcTag.n_pts_tpc_tot",&n_pts_tpc_tot);
   fChain->SetBranchAddress("TpcTag.n_pts_tpc_in",n_pts_tpc_in);
   fChain->SetBranchAddress("TpcTag.n_pts_tpc_out",n_pts_tpc_out);
   fChain->SetBranchAddress("TpcTag.n_trk_tpc",n_trk_tpc);
   fChain->SetBranchAddress("TpcTag.chrg_tpc_drift",chrg_tpc_drift);
   fChain->SetBranchAddress("TpcTag.chrg_tpc_tot",&chrg_tpc_tot);
   fChain->SetBranchAddress("TpcTag.chrg_tpc_in",chrg_tpc_in);
   fChain->SetBranchAddress("TpcTag.chrg_tpc_out",chrg_tpc_out);
   fChain->SetBranchAddress("TpcTag.hit_frac_tpc",hit_frac_tpc);
   fChain->SetBranchAddress("TpcTag.avg_trkL_tpc",avg_trkL_tpc);
   fChain->SetBranchAddress("TpcTag.res_pad_tpc",res_pad_tpc);
   fChain->SetBranchAddress("TpcTag.res_drf_tpc",res_drf_tpc);
   fChain->SetBranchAddress("FlowTag.qxa",qxa);
   fChain->SetBranchAddress("FlowTag.qxb",qxb);
   fChain->SetBranchAddress("FlowTag.qxc",qxc);
   fChain->SetBranchAddress("FlowTag.qxd",qxd);
   fChain->SetBranchAddress("FlowTag.qya",qya);
   fChain->SetBranchAddress("FlowTag.qyb",qyb);
   fChain->SetBranchAddress("FlowTag.qyc",qyc);
   fChain->SetBranchAddress("FlowTag.qyd",qyd);
   fChain->SetBranchAddress("FlowTag.na",na);
   fChain->SetBranchAddress("FlowTag.nb",nb);
   fChain->SetBranchAddress("FlowTag.nc",nc);
   fChain->SetBranchAddress("FlowTag.nd",nd);
   fChain->SetBranchAddress("FlowTag.mpta",mpta);
   fChain->SetBranchAddress("FlowTag.mptb",mptb);
   fChain->SetBranchAddress("FlowTag.mptc",mptc);
   fChain->SetBranchAddress("FlowTag.mptd",mptd);
   fChain->SetBranchAddress("StrangeTag.NV0",&NV0);
   fChain->SetBranchAddress("StrangeTag.NbelowK0",&NbelowK0);
   fChain->SetBranchAddress("StrangeTag.NK0",&NK0);
   fChain->SetBranchAddress("StrangeTag.NaboveK0",&NaboveK0);
   fChain->SetBranchAddress("StrangeTag.NbelowLa",&NbelowLa);
   fChain->SetBranchAddress("StrangeTag.NLa",&NLa);
   fChain->SetBranchAddress("StrangeTag.NaboveLa",&NaboveLa);
   fChain->SetBranchAddress("StrangeTag.NbelowLb",&NbelowLb);
   fChain->SetBranchAddress("StrangeTag.NLb",&NLb);
   fChain->SetBranchAddress("StrangeTag.NaboveLb",&NaboveLb);
   fChain->SetBranchAddress("StrangeTag.NbelowXi",&NbelowXi);
   fChain->SetBranchAddress("StrangeTag.NXi",&NXi);
   fChain->SetBranchAddress("StrangeTag.NaboveXi",&NaboveXi);
   fChain->SetBranchAddress("StrangeTag.range",&range);
   fChain->SetBranchAddress("ScaTag.scaAnalysisMatrix",scaAnalysisMatrix);
   fChain->SetBranchAddress("ScaTag.chargedParticles_Means",chargedParticles_Means);
   fChain->SetBranchAddress("ScaTag.chargedParticles_Sigmas",chargedParticles_Sigmas);
   fChain->SetBranchAddress("PCollTag.chargedMultiplicity",&chargedMultiplicity);
   fChain->SetBranchAddress("PCollTag.numberOfPhotonClusters",&numberOfPhotonClusters);
   fChain->SetBranchAddress("PCollTag.numberOfElectronsInEmc",&numberOfElectronsInEmc);
   fChain->SetBranchAddress("PCollTag.numberOfMwpcHits",&numberOfMwpcHits);
   fChain->SetBranchAddress("PCollTag.numberOfPrimaryTracks",&numberOfPrimaryTracks);
   fChain->SetBranchAddress("PCollTag.vectorSumOfPt",&vectorSumOfPt);
   fChain->SetBranchAddress("PCollTag.totalCharge",&totalCharge);
   fChain->SetBranchAddress("PCollTag.pseudorapidityOfEvent",&pseudorapidityOfEvent);
   fChain->SetBranchAddress("PCollTag.primaryVertexX",&primaryVertexX);
   fChain->SetBranchAddress("PCollTag.primaryVertexY",&primaryVertexY);
   fChain->SetBranchAddress("PCollTag.primaryVertexZ",&primaryVertexZ);
   fChain->SetBranchAddress("PCollTag.zdc1Energy",&zdc1Energy);
   fChain->SetBranchAddress("PCollTag.zdc2Energy",&zdc2Energy);
   fChain->SetBranchAddress("EvtHddr.mRunNumber",&mRunNumber);
   fChain->SetBranchAddress("EvtHddr.mOldRunNumber",&mOldRunNumber);
   fChain->SetBranchAddress("EvtHddr.mId",&mId);
   fChain->SetBranchAddress("EvtHddr.mInputTriggerMask",&mInputTriggerMask);
   fChain->SetBranchAddress("EvtHddr.mTriggerMask",&mTriggerMask);
   fChain->SetBranchAddress("EvtHddr.mCenterOfMassEnergy",&mCenterOfMassEnergy);
   fChain->SetBranchAddress("EvtHddr.mAEast",&mAEast);
   fChain->SetBranchAddress("EvtHddr.mZEast",&mZEast);
   fChain->SetBranchAddress("EvtHddr.mAWest",&mAWest);
   fChain->SetBranchAddress("EvtHddr.mZWest",&mZWest);
   fChain->SetBranchAddress("EvtHddr.mLuminosity",&mLuminosity);
   fChain->SetBranchAddress("EvtHddr.mBeamPolarizationEast",mBeamPolarizationEast);
   fChain->SetBranchAddress("EvtHddr.mBeamPolarizationWest",mBeamPolarizationWest);
   fChain->SetBranchAddress("EvtHddr.mBImpact",&mBImpact);
   fChain->SetBranchAddress("EvtHddr.mPhImpact",&mPhImpact);
   fChain->SetBranchAddress("EvtHddr.mGenerType",&mGenerType);
   fChain->SetBranchAddress("EvtHddr.mBunchCrossingNumber",&mBunchCrossingNumber);
   fChain->SetBranchAddress("EvtHddr.mIventNumber",&mIventNumber);
   fChain->SetBranchAddress("EvtHddr.mEventSize",&mEventSize);
   fChain->SetBranchAddress("EvtHddr.mEventNumber",&mEventNumber);
   fChain->SetBranchAddress("EvtHddr.mEventTime",&mEventTime);
   fChain->SetBranchAddress("EvtHddr.mProdTime",&mProdTime);
   fChain->SetBranchAddress("EvtHddr.mEventType",mEventType);
}

Bool_t Tag::Notify()
{
//   called when loading a new file
//   get branch pointers
   b_TpcTag_n_clus_tpc_tot = fChain->GetBranch("TpcTag.n_clus_tpc_tot");
   b_TpcTag_n_clus_tpc_in = fChain->GetBranch("TpcTag.n_clus_tpc_in");
   b_TpcTag_n_clus_tpc_out = fChain->GetBranch("TpcTag.n_clus_tpc_out");
   b_TpcTag_n_pts_tpc_tot = fChain->GetBranch("TpcTag.n_pts_tpc_tot");
   b_TpcTag_n_pts_tpc_in = fChain->GetBranch("TpcTag.n_pts_tpc_in");
   b_TpcTag_n_pts_tpc_out = fChain->GetBranch("TpcTag.n_pts_tpc_out");
   b_TpcTag_n_trk_tpc = fChain->GetBranch("TpcTag.n_trk_tpc");
   b_TpcTag_chrg_tpc_drift = fChain->GetBranch("TpcTag.chrg_tpc_drift");
   b_TpcTag_chrg_tpc_tot = fChain->GetBranch("TpcTag.chrg_tpc_tot");
   b_TpcTag_chrg_tpc_in = fChain->GetBranch("TpcTag.chrg_tpc_in");
   b_TpcTag_chrg_tpc_out = fChain->GetBranch("TpcTag.chrg_tpc_out");
   b_TpcTag_hit_frac_tpc = fChain->GetBranch("TpcTag.hit_frac_tpc");
   b_TpcTag_avg_trkL_tpc = fChain->GetBranch("TpcTag.avg_trkL_tpc");
   b_TpcTag_res_pad_tpc = fChain->GetBranch("TpcTag.res_pad_tpc");
   b_TpcTag_res_drf_tpc = fChain->GetBranch("TpcTag.res_drf_tpc");
   b_FlowTag_qxa = fChain->GetBranch("FlowTag.qxa");
   b_FlowTag_qxb = fChain->GetBranch("FlowTag.qxb");
   b_FlowTag_qxc = fChain->GetBranch("FlowTag.qxc");
   b_FlowTag_qxd = fChain->GetBranch("FlowTag.qxd");
   b_FlowTag_qya = fChain->GetBranch("FlowTag.qya");
   b_FlowTag_qyb = fChain->GetBranch("FlowTag.qyb");
   b_FlowTag_qyc = fChain->GetBranch("FlowTag.qyc");
   b_FlowTag_qyd = fChain->GetBranch("FlowTag.qyd");
   b_FlowTag_na = fChain->GetBranch("FlowTag.na");
   b_FlowTag_nb = fChain->GetBranch("FlowTag.nb");
   b_FlowTag_nc = fChain->GetBranch("FlowTag.nc");
   b_FlowTag_nd = fChain->GetBranch("FlowTag.nd");
   b_FlowTag_mpta = fChain->GetBranch("FlowTag.mpta");
   b_FlowTag_mptb = fChain->GetBranch("FlowTag.mptb");
   b_FlowTag_mptc = fChain->GetBranch("FlowTag.mptc");
   b_FlowTag_mptd = fChain->GetBranch("FlowTag.mptd");
   b_StrangeTag_NV0 = fChain->GetBranch("StrangeTag.NV0");
   b_StrangeTag_NbelowK0 = fChain->GetBranch("StrangeTag.NbelowK0");
   b_StrangeTag_NK0 = fChain->GetBranch("StrangeTag.NK0");
   b_StrangeTag_NaboveK0 = fChain->GetBranch("StrangeTag.NaboveK0");
   b_StrangeTag_NbelowLa = fChain->GetBranch("StrangeTag.NbelowLa");
   b_StrangeTag_NLa = fChain->GetBranch("StrangeTag.NLa");
   b_StrangeTag_NaboveLa = fChain->GetBranch("StrangeTag.NaboveLa");
   b_StrangeTag_NbelowLb = fChain->GetBranch("StrangeTag.NbelowLb");
   b_StrangeTag_NLb = fChain->GetBranch("StrangeTag.NLb");
   b_StrangeTag_NaboveLb = fChain->GetBranch("StrangeTag.NaboveLb");
   b_StrangeTag_NbelowXi = fChain->GetBranch("StrangeTag.NbelowXi");
   b_StrangeTag_NXi = fChain->GetBranch("StrangeTag.NXi");
   b_StrangeTag_NaboveXi = fChain->GetBranch("StrangeTag.NaboveXi");
   b_StrangeTag_range = fChain->GetBranch("StrangeTag.range");
   b_ScaTag_scaAnalysisMatrix = fChain->GetBranch("ScaTag.scaAnalysisMatrix");
   b_ScaTag_chargedParticles_Means = fChain->GetBranch("ScaTag.chargedParticles_Means");
   b_ScaTag_chargedParticles_Sigmas = fChain->GetBranch("ScaTag.chargedParticles_Sigmas");
   b_PCollTag_chargedMultiplicity = fChain->GetBranch("PCollTag.chargedMultiplicity");
   b_PCollTag_numberOfPhotonClusters = fChain->GetBranch("PCollTag.numberOfPhotonClusters");
   b_PCollTag_numberOfElectronsInEmc = fChain->GetBranch("PCollTag.numberOfElectronsInEmc");
   b_PCollTag_numberOfMwpcHits = fChain->GetBranch("PCollTag.numberOfMwpcHits");
   b_PCollTag_numberOfPrimaryTracks = fChain->GetBranch("PCollTag.numberOfPrimaryTracks");
   b_PCollTag_vectorSumOfPt = fChain->GetBranch("PCollTag.vectorSumOfPt");
   b_PCollTag_totalCharge = fChain->GetBranch("PCollTag.totalCharge");
   b_PCollTag_pseudorapidityOfEvent = fChain->GetBranch("PCollTag.pseudorapidityOfEvent");
   b_PCollTag_primaryVertexX = fChain->GetBranch("PCollTag.primaryVertexX");
   b_PCollTag_primaryVertexY = fChain->GetBranch("PCollTag.primaryVertexY");
   b_PCollTag_primaryVertexZ = fChain->GetBranch("PCollTag.primaryVertexZ");
   b_PCollTag_zdc1Energy = fChain->GetBranch("PCollTag.zdc1Energy");
   b_PCollTag_zdc2Energy = fChain->GetBranch("PCollTag.zdc2Energy");
   b_EvtHddr_mRunNumber = fChain->GetBranch("EvtHddr.mRunNumber");
   b_EvtHddr_mOldRunNumber = fChain->GetBranch("EvtHddr.mOldRunNumber");
   b_EvtHddr_mId = fChain->GetBranch("EvtHddr.mId");
   b_EvtHddr_mInputTriggerMask = fChain->GetBranch("EvtHddr.mInputTriggerMask");
   b_EvtHddr_mTriggerMask = fChain->GetBranch("EvtHddr.mTriggerMask");
   b_EvtHddr_mCenterOfMassEnergy = fChain->GetBranch("EvtHddr.mCenterOfMassEnergy");
   b_EvtHddr_mAEast = fChain->GetBranch("EvtHddr.mAEast");
   b_EvtHddr_mZEast = fChain->GetBranch("EvtHddr.mZEast");
   b_EvtHddr_mAWest = fChain->GetBranch("EvtHddr.mAWest");
   b_EvtHddr_mZWest = fChain->GetBranch("EvtHddr.mZWest");
   b_EvtHddr_mLuminosity = fChain->GetBranch("EvtHddr.mLuminosity");
   b_EvtHddr_mBeamPolarizationEast = fChain->GetBranch("EvtHddr.mBeamPolarizationEast");
   b_EvtHddr_mBeamPolarizationWest = fChain->GetBranch("EvtHddr.mBeamPolarizationWest");
   b_EvtHddr_mBImpact = fChain->GetBranch("EvtHddr.mBImpact");
   b_EvtHddr_mPhImpact = fChain->GetBranch("EvtHddr.mPhImpact");
   b_EvtHddr_mGenerType = fChain->GetBranch("EvtHddr.mGenerType");
   b_EvtHddr_mBunchCrossingNumber = fChain->GetBranch("EvtHddr.mBunchCrossingNumber");
   b_EvtHddr_mIventNumber = fChain->GetBranch("EvtHddr.mIventNumber");
   b_EvtHddr_mEventSize = fChain->GetBranch("EvtHddr.mEventSize");
   b_EvtHddr_mEventNumber = fChain->GetBranch("EvtHddr.mEventNumber");
   b_EvtHddr_mEventTime = fChain->GetBranch("EvtHddr.mEventTime");
   b_EvtHddr_mProdTime = fChain->GetBranch("EvtHddr.mProdTime");
   b_EvtHddr_mEventType = fChain->GetBranch("EvtHddr.mEventType");
   return kTRUE;
}

void Tag::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t Tag::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef Tag_cxx

