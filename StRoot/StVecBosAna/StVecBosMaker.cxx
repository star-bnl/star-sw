#include <limits>
#include <string>

#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"
#include "TStopwatch.h"
#include "TString.h"

#include "StMessMgr.h"
#include "StThreeVectorF.hh"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StTriggerUtilities/L2Emulator/L2wAlgo/L2wResult2009.h"

#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

#include "StEmcRawMaker/defines.h"

#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StSpinPool/StSpinDbMaker/cstructs/spinConstDB.hh"
#include "StJetMaker/StJetMaker.h"
#include "StJetMaker/StJetReader.h"
#include "StJetMaker/StJetSkimEventMaker.h"

#include "Globals.h"
#include "WBosEvent.h"
#include "ZBosEvent.h"
#include "WeventDisplay.h"
#include "StVecBosMaker.h"

ClassImp(StVecBosMaker)


StVecBosMaker::StVecBosMaker(AnaOptions& anaInfo, const char *name, VecBosRootFile *vbFile): StMaker(name),
   mStopWatch(), mAnaOptions(&anaInfo), mStMuDstMaker(0), mStJetReader(0), mVecBosRootFile(vbFile),
   mJetTreeBranchName(), mJetTreeBranchNameNoEndcap(),
   mJets(0), mVecBosEvent(0), mVecBosTree(0), mTreeName("StVecBosMaker"),
   mNumInputEvents(0), mNumTrigEvents(0), mNumAcceptedEvents(0),
   mRunNo(0), nRun(0), mIsMc(0),
   Tfirst(numeric_limits<int>::max()), Tlast(numeric_limits<int>::min()),
   mL2BarrelTriggerId(0), mL2BarrelTriggerId2(0), mL2EndcapTriggerId(0), mL2EndcapTriggerId2(0),
   mParETOWScale(1.0), mParBTOWScale(1.0),   // for old the Endcap geometr you need ~1.3
   mStSpinDbMaker(0)
{
   mStMuDstMaker = (StMuDstMaker*) GetMaker("MuDst");

   if (!mStMuDstMaker) { // load tree if no MuDst
      Info("StVecBosMaker", "MuDst maker is not defined. Creating W tree");
      mTreeChain = new TChain("mVecBosTree", "W candidate events");
      index = 0;
   }

   // must have either MuDst or W tree
   assert(mStMuDstMaker || mTreeChain);

   mStJetReader = (StJetReader*) GetMaker("JetReader");

   if (!mStJetReader && mTreeChain) { // jet chain for reading W tree
      mJetTreeChain = new TChain("jet", "Jet Tree");
      indexJet = 0;
   }

   if (!mJetTreeChain && !mStJetReader)
      LOG_WARN << GetName() << Form("::constructor() NO JETS , W-algo is not working properly, continue") << endm;

   setHList(0);
   setHListTpc(0);
   setMC(0);
   setFindZ(0);

   // MC trigger simulator
   par_l0emulAdcThresh          = 30;
   par_l2emulSeedThresh         = 5.0;
   par_l2emulClusterThresh      = 12.0;

   // vertex
   mMinNumPileupVertices        = 3;    // to reject events w/o TPC, lower it for MC
   mCutVertexZ                  = 100;  // (cm)

   // towers
   par_kSigPed                  = 3;    // rawADC-ped cut off
   par_AdcThres                 = 8;    // ADC threshold to avoid correlated noise
   par_maxADC                   = 200.; // (adc chan) on the highest tower in events

   // Barrel params
   mMinBClusterEnergy           = 14.;  // GeV/c 2x2 cluster ET
   mMinBClusterEnergyIsoRatio   = 0.95; // ET ratio 2x2/4x4 cluster
   par_nearTotEtFrac            = 0.88; // ratio 2x2/near Tot ET
   mMinBTrackEta                = -1.5; // bracket acceptance
   mMaxBTrackEta                = 1.5;  // bracket acceptance
   par_ptBalance                = 14.;  // GeV, ele cluster vector + jet sum vector
   par_nFitPts                  = 15;   // hits on the track
   par_trackRin                 = 90;   // cm
   par_trackRout                = 160;  // cm
   par_highET                   = 25.;  // (GeV), cut-off for final Barrel W-cluster

   // Endcap Algo
   parE_trackEtaMin             = 0.7;  // avoid bad extrapolation to ESMD
   parE_clustET                 = 14.;  // (GeV/c) 2x1 cluster ET
   mMinEClusterEnergyIsoRatio   = 0.90; // ET ratio 2x2/4x4 cluster
   parE_nearTotEtFrac           = 0.85; // ratio 2x2/near Tot ET
   parE_delR3D                  = 10.;  // cm, dist between projected track and center of cluster
   mMinETrackEta                = 0.7;  // bracket acceptance
   mMaxETrackEta                = 2.5;  // bracket acceptance
   parE_ptBalance               = 14.;  // (GeV), ele cluster vector + jet sum vector

   // track
   parE_nFitPts                 = 5;    // hits on the track
   parE_nHitFrac                = 0.51;
   parE_trackRin                = 120;
   parE_trackRout               = 70;   // cm
   mMinETrackPt                 = 7.;   // GeV
   parE_nSmdStrip               = 20;
   parE_highET                  = 25.;  // (GeV), cut-off for final Endcap W-cluster

   hbxIdeal                     = 0;

   // irrelevant for W analysis
   par_DsmThres                 = 31;   // only for monitoring
   parE_DsmThres                = 31;   // only for monitoring
   par_maxDisplEve              = 1;    // # of displayed selected events

   use_gains_file               = 0;

   // Year dependent initialization
   if (mAnaOptions->fRhicRunId == 11) {
      mL2BarrelTriggerId  = 320801;
      mL2EndcapTriggerId  = 320851;
   }
   else if (mAnaOptions->fRhicRunId == 12) {
      mL2BarrelTriggerId  = 380209;
      mL2BarrelTriggerId2 = 380219;
      mL2EndcapTriggerId  = 380305;
   }
   else if (mAnaOptions->fRhicRunId == 13) {
      mL2BarrelTriggerId  = 430209;
      mL2BarrelTriggerId2 = 430229;
      mL2EndcapTriggerId  = 430305;
      mL2EndcapTriggerId2 = 430315;
   }
}


/**
 * This maker hook is called once before the processing
 */
Int_t StVecBosMaker::Init()
{
   if (mStMuDstMaker) {
      //only need DB tables for MuDst analysis
      mBarrelTables = new StBemcTables();
      mDbE = (StEEmcDb*) GetDataSet("StEEmcDb");
      assert(mDbE);
   }

   gBTowGeom       = StEmcGeom::instance("bemc");
   mBSmdGeom[kBSE] = StEmcGeom::instance("bsmde");
   mBSmdGeom[kBSP] = StEmcGeom::instance("bsmdp");

   mGeomEmc = new EEmcGeomSimple();
   mGeomSmd = EEmcSmdGeom::instance();
   InitGeom();

   mEventDisplay = new WeventDisplay(this, par_maxDisplEve);

   if (mIsMc) { // load vertex reweighting histo
      //TString filename="/star/u/stevens4/wAnalysis/efficXsec/zVertReweight.root";
      //TString filename="/star/u/fazio/offline/users/fazio/vbasym/zVertReweight.root";
      //TFile* reweightFile = new TFile(filename);
      //cout<<"Re-weighting vertex z distribution with '"<<nameReweight<<"' histo from file "<<endl<<filename<<endl;
      //hReweight = (TH1F*) reweightFile->Get(nameReweight);

      mMinNumPileupVertices = 1;
   }

   // tree only written during MuDst analysis
   if (mStMuDstMaker) {
      mTreeFile = new TFile(mTreeName, "recreate");
      mTreeFile->cd();
      mVecBosTree = new TTree("t", "mVecBosTree");

      if (mAnaOptions->GetBosonType() == kWBoson) {
         mVecBosEvent = new WBosEvent( mAnaOptions->GetTracksPtMin(), mAnaOptions->fRhicRunId );
         mVecBosTree->Branch("e", "WBosEvent", &mVecBosEvent, 128000, 0); // splitlevel=0. very important for custom streamers
      } else if (mAnaOptions->GetBosonType() == kZBoson) {
         mVecBosEvent = new ZBosEvent();
         mVecBosTree->Branch("e", "ZBosEvent", &mVecBosEvent, 128000, 0); // splitlevel=0. very important for custom streamers
      } else
         Warning("Init", "Unknown type of boson");
   }

   assert(HList);

   return StMaker::Init();
}


/** */
Int_t StVecBosMaker::InitRun(int runNo)
{
   Info("InitRun", "Called for run %d", runNo);

   //if (!mIsMc) assert(mRunNo == 0); // to prevent run merging - it was not tested

   if (mStMuDstMaker) {
      mBarrelTables->loadTables(this);
      mRunNo = runNo;
   }

   cout << Form("\n EtowScaleFact=%.2f  BtowScaleFacor=%.2f" , mParETOWScale, mParBTOWScale) << endl;

   if (mStMuDstMaker) {

      // initialization of TPC cuts is run dependent (only MuDst analysis)
      for (int iSector = 1; iSector <= mxTpcSec; iSector++)
      {
         float Rin   = par_trackRin;
         float Rout  = par_trackRout;
         float RinE  = parE_trackRin;
         float RoutE = parE_trackRout;

         // Rin changes

         //Run 9 (final)
         if (iSector == 4  && mRunNo >= 10090089) Rin = 125.;
         if (iSector == 11 && mRunNo >= 10083013) Rin = 125.;
         if (iSector == 15 && mRunNo >= 10088096 && mRunNo <= 10090112 ) Rin = 125.;

         //Run 11 ?? XXX:ds:

         //Run 12 (not final, need to identify where electronics died, JS)
         if ((iSector == 5 || iSector == 6 || iSector == 7 || iSector == 21) && (mRunNo >= 13000000 || mRunNo / 100 == 3) )
            Rin = 125.; // all have dead inner padrows, run#300+nn is for M-C generated by Jan

         // Rout changes

         //Run 9 (final)
         if (iSector == 5 && mRunNo >= 10098029) Rout = 140.;
         if (iSector == 6 ) Rout = 140.;
         if (iSector == 20 && mRunNo >= 10095120 && mRunNo <= 10099078 ) Rout = 140.;

         //Run 11 ?? XXX

         //Run 12 ??
         mTpcFilter[iSector-1].setCuts(par_nFitPts, VecBosEvent::sMinTrackHitFrac, Rin, Rout);
         mTpcFilter[iSector-1].init("iSector", iSector, HListTpc, true);

         mTpcFilterE[iSector-1].setCuts(parE_nFitPts, parE_nHitFrac, RinE, RoutE);
         mTpcFilterE[iSector-1].init("secEemcTr", iSector, HListTpc, false);
      }
   }

   // mStSpinDbMaker monitoring
   if (mStMuDstMaker && mStSpinDbMaker)
   {
      char txt[1000], txt0[100];
      sprintf(txt0, "bxIdeal%d", nRun);
      sprintf(txt,  "intended fill pattern  R%d-%d vs. bXing; ", mRunNo, nRun);
      //string str_txt = txt;
      //str_txt += mStSpinDbMaker->getV124comment();
      //Info("InitRun(...)", "v124comment: %s", mStSpinDbMaker->getV124comment());

      nRun++;

      Tfirst = int(2e9);
      Tlast  = -Tfirst;

      hbxIdeal = new TH1F(txt0, txt, 128, -0.5, 127.5);
      hbxIdeal->SetFillColor(kYellow);
      HList->Add(hbxIdeal);

      mStSpinDbMaker->print(0); // 0=short, 1=huge

      for (int bx = 0; bx < 120; bx++) {
         if (mStSpinDbMaker->isBXfilledUsingInternalBX(bx))
            hbxIdeal->Fill(bx);
      }
   } else {
      if (!mIsMc) Fatal("InitRun", "StSpinDbMaker is likely not set");
   }

   return kStOK;
}


Int_t StVecBosMaker::Finish()
{
   if (mStMuDstMaker) {
      cout << endl << "Output tree file: " << mTreeName << endl << endl;
      mTreeFile->Write();
      mTreeFile->Close();

      if (hbxIdeal) {
         char txt[1000];
         sprintf(txt, "events T = %d %d", Tfirst, Tlast);
         printf("Finish run=%d, events time range %s\n", mRunNo, txt);
         hbxIdeal->GetYaxis()->SetTitle(txt);
      }
   }

   return StMaker::Finish();
}


Int_t StVecBosMaker::FinishRun(int runNo)
{
   LOG_INFO << Form("::FinishRun(%d)", runNo) << endm;
   return kStOK;
}


void StVecBosMaker::Clear(const Option_t *)
{
   mVecBosEvent->Clear();
}


/**
 * This hook is called every event. The events are analyzed, preselected, and saved in the
 * #mVecBosTree tree.
 */
Int_t StVecBosMaker::Make()
{
   mStopWatch.Start(); // restart mStopWatch

   mNumInputEvents++;

   // standard MuDst analysis
   // We need both makers for proper analysis
   if (!mStMuDstMaker || !mStJetReader) {
     //mStopWatch.Stop();
     //return kStOK;
   }

   mVecBosEvent->InitUsing(mStMuDstMaker);

   int time = mVecBosEvent->time;

   Tlast  = (Tlast  < time) ? time : Tlast;
   Tfirst = (Tfirst > time) ? time : Tfirst;

   const char *afile = mStMuDstMaker->GetFile();

   if (mNumInputEvents % 200 == 0)
      Info("Make()", "nEve: inp=%d, trig=%d, accpt=%d, daqFile: %s\n", mNumInputEvents, mNumTrigEvents, mNumAcceptedEvents, afile);

   // First access calorimeter data
   int btowStat = ReadMuDstBTOW(); // get energy in BTOW
   int etowStat = ReadMuDstETOW(); // get energy in ETOW

   // Skip entire event if no energy in BTOW && ETOW
   if ( btowStat != 0 && etowStat != 0 ) {
     //Info("Make()", "No energy in neither BTOW nor ETOW. Skipping event...");
     //mStopWatch.Stop();
     //return kStOK;
   }

   int btrig = ReadMuDstBarrelTrig();
   int etrig = ReadMuDstEndcapTrig();

   // Skip entire event if no valid trig ID
   if ( btrig != 0 && etrig != 0 ) {
     //Info("Make()", "No trigger bit in neither BTOW nor ETOW. Skipping event...");
     //mStopWatch.Stop();
     //return kStOK;
   }

   mNumTrigEvents++;

   // Access other detectors and save info in the event
   ReadMuDstBSMD(); // get energy in BSMD
   ReadMuDstESMD(); // get energy in ESMD
   ReadMuDstEPRS(); // get energy in EPRS

   // Save all vertices from MuDst into event. Add tracks in the event. See the
   // function for the cuts imposed on the track quality
   ReadMuDstVerticesTracks();
   ReadMuDstJets(); // Get input jet info

   mVecBosEvent->Process();

   if (mIsMc) {
      mVecBosEvent->ProcessMC(mIsMc);
   }

   mVecBosEvent->SetCpuTimeEventAna( mStopWatch.CpuTime() );

   // Restart stopwatch
   mStopWatch.Continue();

   mVecBosEvent->SetCpuTimeHistFill( mStopWatch.CpuTime() );

   //XXX:sf:   if ( !mVecBosEvent->HasIsolatedTrack() ) {
   //XXX:sf:        return kStOK;  
   //XXX:sf:   }   //S.F. March 16, better to cut on the isolation later at the "vbana.c" level

   if (!mIsMc) {
      //m2011WlumiMaker = new St2011WlumiMaker();
   }

   // Write event to tree
   mVecBosTree->Fill();

   //XXX:ds: if (mVecBosEvent->GetNumVertices())
   //XXX:ds:    FillTowHit(true);  // fill 2D tower "hit" histos for vertex found and L2BW trigger (beam background analysis, remove any time JS)
   //XXX:ds: else
   //XXX:ds:    FillTowHit(false); // fill 2D tower "hit" histos for _no_ vertex and L2BW trigger (beam background analysis, remove any time JS)

   //XXX:ds: // Skip event w/o high Pt tracks
   //XXX:ds: if (mVecBosEvent->GetNumTracks() <= 0) {
   //XXX:ds:    Info("Make()", "No tracks found in the event. Skipping...");
   //XXX:ds:    return kStOK;
   //XXX:ds: }

   //for (uint iJet=0; iJet<mVecBosEvent->GetNumJets(); ++iJet)
   //{
   //   StJet *jet     = GetJet(iJet);
   //   float  jet_pt  = jet->Pt();
   //   float  jet_eta = jet->Eta();
   //   float  jet_phi = jet->Phi();
   //}

   mNumAcceptedEvents++;

   if (mNumAcceptedEvents == 1 || mNumAcceptedEvents % 1000 == 1 ) {
      Info("Make", "mNumAcceptedEvents: %d", mNumAcceptedEvents);
      mVecBosEvent->Print();
   }

   return kStOK;
}


/**
 * Initialized global variables with detector coordinates.
 */
void StVecBosMaker::InitGeom()
{
   // BTOW
   memset(gMapBTowEtaPhiBin2Id, 0, sizeof(gMapBTowEtaPhiBin2Id));

   // end of loop over towers
   for (int towerId = 1; towerId <= mxBtow; towerId++)
   {
      // querry BTOW geom
      int m, e, s;
      gBTowGeom->getBin(towerId, m, e, s);

      float eta, phi;
      gBTowGeom->getEta(m, e, eta);
      gBTowGeom->getPhi(m, s, phi); // -pi <= phi < pi

      int iEta, iPhi;
      assert(ConvertEtaPhi2Bins(eta, phi, iEta, iPhi)); // tower must be localized at the known position

      int IJ = iEta + iPhi * mxBTetaBin;
      assert(gMapBTowEtaPhiBin2Id[IJ] == 0); // avoid overlaping mapping
      gMapBTowEtaPhiBin2Id[IJ] = towerId;

      float x, y, z;
      assert( gBTowGeom->getXYZ(towerId, x, y, z) == 0);
      gBCalTowerCoords[towerId - 1] = TVector3(x, y, z);
   }

   // BSMD-E, -P
   for (int iep = 0; iep < mxBSmd; iep++) {
      for (int towerId = 1; towerId <= mxBStrips; towerId++) {
         float x, y, z;
         assert( mBSmdGeom[iep]->getXYZ(towerId, x, y, z) == 0);
         gBSmdStripCoords[iep][towerId - 1] = TVector3(x, y, z);
      }
   }

   // ETOW
   for (int isec = 0; isec < mxEtowSec; isec++) {
      for (int isub = 0; isub < mxEtowSub; isub++) {
         for (int ieta = 0; ieta < mxEtowEta; ieta++) {
            gETowCoords[isec * mxEtowSub + isub][ieta] = mGeomEmc->getTowerCenter(isec, isub, ieta);
         }
      }
   }
}


/** */
void StVecBosMaker::ReadMuDstJets()
{
   //if (mStJetReader == 0) {
   //   mVecBosEvent->mNJets = 0;
   //   return;
   //}

   StJets* stJets         = GetStJets(mJetTreeBranchName);
   StJets* stJetsNoEndcap = 0;// XXX GetStJets(mJetTreeBranchNameNoEndcap);

   mVecBosEvent->AddStJets(stJets, stJetsNoEndcap);

   //if (mStJetReader->getStJets(branchName)->eventId() != mVecBosEvent->GetEventId())
   if (stJets->eventId() != mVecBosEvent->GetEventId() || stJets->runId() != mVecBosEvent->GetRunId())
   {
      Error("ReadMuDstJets", "Jet and W run ids do not match: %12d, %12d",   stJets->runId(),   mVecBosEvent->GetRunId());
      Error("ReadMuDstJets", "Jet and W event ids do not match: %12d, %12d", stJets->eventId(), mVecBosEvent->GetEventId());
      Fatal("ReadMuDstJets", "Cannot proceed");
   }

   //if (stJetsNoEndcap->eventId() != mVecBosEvent->GetEventId() || stJetsNoEndcap->runId() != mVecBosEvent->GetRunId())
   //{
   //   Error("ReadMuDstJets", "Jet and W run ids do not match: %12d, %12d (no_endcap branch)",   stJetsNoEndcap->runId(),   mVecBosEvent->GetRunId());
   //   Error("ReadMuDstJets", "Jet and W event ids do not match: %12d, %12d (no_endcap branch)", stJetsNoEndcap->eventId(), mVecBosEvent->GetEventId());
   //   Fatal("ReadMuDstJets", "Cannot proceed");
   //}

   //mVecBosEvent->mNJets = stJets->nJets();
}


StJets* StVecBosMaker::GetStJets(const char* bname) const
{
  TBranch* branch = mStJetReader->tree()->GetBranch(bname);
  return branch ? *(StJets**) branch->GetAddress() : 0;
}


// Below is only used for Tree analysis
TClonesArray* StVecBosMaker::GetJetsTreeAnalysis(TString branchName)
{
   //if (mJetTreeChain == 0) {
   //   mVecBosEvent->GetNumJets() = 0;
   //   return 0;
   //}

   //cout<<"looking for matching jet event"<<endl;

   StJets *jetTmp = GetStJetsFromTree(branchName);

   while (jetTmp->eventId() != mVecBosEvent->GetEventId() || jetTmp->runId() != mVecBosEvent->GetRunId()) {
      mJetTreeChain->GetEntry(indexJet++);
      jetTmp = GetStJetsFromTree(branchName);
   }

   //cout<<"found matching jet event"<<endl;

   assert(jetTmp->eventId() == mVecBosEvent->GetEventId());
   assert(jetTmp->runId() == mVecBosEvent->GetRunId());
   //mVecBosEvent->mNJets = jetTmp->nJets();
   return jetTmp->jets();
}


StJets* StVecBosMaker::GetStJetsFromTree(TString branchName)
{
   TBranch *branch = mJetTreeChain->GetBranch(branchName);
   return branch ? *(StJets **)branch->GetAddress() : 0;
}


int StVecBosMaker::ReadMuDstBTOW()
{
   StMuEmcCollection *emc = mStMuDstMaker->muDst()->muEmcCollection();

   if (!emc) {
      gMessMgr->Warning() << "No EMC data for this event" << endm;
      return -4;
   }

   // kBTow: index for tower & preshower set to BTOW
   int n5  = 0, n0 = 0, n1 = 0, n2 = 0, n3 = 0, n4 = 0;
   int maxID = 0;
   double maxADC = 0, adcSum = 0;

   for (int softID=1; softID<=mxBtow; softID++)
   {
      float rawAdc = emc->getTowerADC(softID);

      if (rawAdc == 0) n0++;

      int statPed, statOfl, statGain;

      mBarrelTables->getStatus(BTOW, softID, statPed, "pedestal"); // official BTOW detector ID
      mBarrelTables->getStatus(BTOW, softID, statOfl);
      mBarrelTables->getStatus(BTOW, softID, statGain, "calib");

      if (statPed  != 1) { mVecBosEvent->bemc.statTile[kBTow][softID - 1] = 1; n1++; continue; }
      if (statOfl  != 1) { mVecBosEvent->bemc.statTile[kBTow][softID - 1] = 2; n2++; continue; }
      if (statGain != 1) { mVecBosEvent->bemc.statTile[kBTow][softID - 1] = 4; n3++; continue; }

      mVecBosEvent->bemc.statTile[kBTow][softID - 1] = 0 ;

      float ped, sigPed, gain;
      int capID = 0; // just one value for btow

      mBarrelTables->getPedestal(BTOW, softID, capID, ped, sigPed);
      mBarrelTables->getCalib(BTOW, softID, 1, gain);

      //if (use_gains_file == 1) {
      //   gain = gains_BTOW[softID];
      //}

      //printf("id=%d gain=%f\n", softID, gain);

      // method for shifting energy scale
      gain = gain * mParBTOWScale; //(default is mParBTOWScale=1)

      float adc = rawAdc - ped;

      if (adc > 0) n4++;
      if (adc < par_kSigPed * sigPed) continue;
      if (adc < par_AdcThres)         continue;

      n5++;
      mVecBosEvent->bemc.adcTile[kBTow][softID - 1] = adc;
      mVecBosEvent->bemc.eneTile[kBTow][softID - 1] = adc * gain;

      if (maxADC < adc) { maxID = softID; maxADC = adc;}

      adcSum += adc;
   }

   //printf("NNN %d %d %d %d %d %d id=%d\n",n0,n1,n2,n3,n4,n5,maxID);
   if (n0 == mxBtow) {
      Warning("ReadMuDstBTOW", "Missing BEMC info");
      return -1 ; // BTOW was not present in this events
   }

   mVecBosEvent->bemc.tileIn[kBTow] = 1; //tag usable data

   if (mNumInputEvents % 5000 == 1) {
      LOG_INFO << Form("unpackMuBTOW() dataIn=%d, nBbad: ped=%d stat=%d gain=%d ; nAdc: %d>0, %d>thres\n" \
                       "maxADC=%.0f softID=%d adcSum=%.0f",
                       mVecBosEvent->bemc.tileIn[kBTow], n1, n2, n3, n4, n5,
                       maxADC, maxID, adcSum
                      ) << endm;
   }

   mVecBosEvent->bemc.maxAdc = maxADC;

   if (maxADC < par_maxADC) {
      Warning("ReadMuDstBTOW", "Energy deposit is too small in BEMC");
      return -2 ; // not enough energy
   }

   return 0;
}


int StVecBosMaker::ReadMuDstETOW()
{
   StMuEmcCollection *emc = mStMuDstMaker->muDst()->muEmcCollection();

   if (!emc) {
      LOG_WARN << "No EMC data for this event" << endm;
      return -4;
   }

   mVecBosEvent->etow.etowIn = 1; //tag usable ETOW data
   const char *maxIdName = 0;
   double maxADC =  0, adcSum =  0;
   int    maxSec = -1, maxSub = -1, maxEta = -1;

   // Loop over all towers
   for (int i = 0; i < emc->getNEndcapTowerADC(); i++)
   {
      int sec, eta, sub, rawAdc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
      emc->getEndcapTowerADC(i, rawAdc, sec, sub, eta);

      const EEmcDbItem *eEmcDbItem = mDbE->getTile(sec, 'A' + sub - 1, eta, 'T');
      assert(eEmcDbItem); // it should never happened for muDst

      if (eEmcDbItem->fail ) continue; // drop not working channels

      int isec = eEmcDbItem->sec - 1;
      int isub = eEmcDbItem->sub - 'A';
      int ieta = eEmcDbItem->eta - 1;

      assert(isec >= 0 && isec < mxEtowSec); // check input is ok
      assert(isub >= 0 && isub < mxEtowSub);
      assert(ieta >= 0 && ieta < mxEtowEta);

      float adc = rawAdc - eEmcDbItem->ped; // ped subtracted ADC
      if (adc < par_kSigPed * eEmcDbItem->sigPed) continue;

      mVecBosEvent->etow.adc[isec * mxEtowSub + isub][ieta] = adc;

      if (eEmcDbItem->gain <= 0) continue; // drop channels w/o gains
      float ene = adc / eEmcDbItem->gain;

      //method for shifting energy scale
      ene *= mParETOWScale; //(default is mParETOWScale=1)
      mVecBosEvent->etow.ene[isec * mxEtowSub + isub][ieta] = ene;
      mVecBosEvent->etow.stat[isec * mxEtowSub + isub][ieta] = 0;

      if (maxADC < adc) { maxIdName = eEmcDbItem->name; maxADC = adc; maxSec = isec; maxSub = isub; maxEta = ieta;}
      adcSum += adc;

   }

   mVecBosEvent->etow.maxAdc = maxADC;
   mVecBosEvent->etow.maxSec = maxSec;
   mVecBosEvent->etow.maxSub = maxSub;
   mVecBosEvent->etow.maxEta = maxEta;

   if (maxADC < par_maxADC) return -2 ; // not enough energy

   return 0;
}


/**
 * Extracts the fired trigger ids for the BEMC and checks against the high p_T trigger defined by
 * the user. Also deals with the bunch crossing type, i.e. spin states and longitudinal vs
 * transverse polarization.
 *
 * @return zero on success
 */
int StVecBosMaker::ReadMuDstBarrelTrig()
{
   if (mIsMc) {
      // When the trigger emulator is ready, this should hook into that instead of the two functions
      // used below. For now, check that it passes both L0 and L2, and set the l2bitET flag to true
      // if so.

      //if (!passes_L0()) return -1;
      if (!passes_L2()) return -2;

      mVecBosEvent->l2bitET = true;
      return 0; // we haven't set everything, but it should be good enough for simu.
   }

   StMuEvent *stMuEvent = mStMuDstMaker->muDst()->event();

   //collect info for the luminosity monitor
   int highestT = 0;
   int highestM = 0;

   // Loop over trigger patches
   for (int m=0; m<300; m++) {
      int myT = stMuEvent->emcTriggerDetector().highTower(m);

      if  (myT > highestT) {
         highestT = myT;
         highestM = m;
      }
   }

   int highestPhi, tempPhi, tempEta;
   int awaySum[16];
   int totalSum = 0;

   memset(awaySum, 0, sizeof(awaySum));

   PatchToEtaPhi(highestM, &tempEta, &highestPhi);

   for (int m = 0; m < 300; m++)
   {
      int myT = stMuEvent->emcTriggerDetector().highTower(m);
      PatchToEtaPhi(m, &tempEta, &tempPhi);

      for (int away_width = 0; away_width < 16; away_width++) {
         if ((highestPhi + 30 - tempPhi) % 30 > (15 - away_width) &&
             (highestPhi + 30 - tempPhi) % 30 < (15 + away_width))
         {
            awaySum[away_width] += myT;
         }
      }
      totalSum += myT;
   }

   for (int i = 0; i < 16; i++)
      mVecBosEvent->trigAwaySum[i] = awaySum[i];

   mVecBosEvent->trigTotalSum = totalSum;

   StMuTriggerIdCollection* triggerIdCollection = &(stMuEvent->triggerIdCollection());
   assert(triggerIdCollection);
   const StTriggerId &l1 = triggerIdCollection->l1();
   vector<unsigned int> idL = l1.triggerIds();

   //printf("nTrig=%d, trigID: ",idL.size());
   for (unsigned int i = 0; i < idL.size(); i++) {
      char txt[100];
      sprintf(txt, "%d", idL[i]);
   }

   // Get bunch crossing info
   StL0Trigger *trig = &stMuEvent->l0Trigger();

   mVecBosEvent->bx48 = trig->bunchCrossingId();
   mVecBosEvent->bx7  = trig->bunchCrossingId7bit(mRunNo);

   // you do not want mix Long & Trans by accident
   // Removed the check mStSpinDbMaker->isValid()
   mVecBosEvent->bxStar48          = mStSpinDbMaker->BXstarUsingBX48(mVecBosEvent->bx48);
   mVecBosEvent->bxStar7           = mStSpinDbMaker->BXstarUsingBX7(mVecBosEvent->bx7);
   mVecBosEvent->mSpinPattern4Bits = mStSpinDbMaker->spin4usingBX48(mVecBosEvent->bx48);
   mVecBosEvent->mSpinDirection    = mStSpinDbMaker->isPolDirLong() ? polDirLong :
                                    (mStSpinDbMaker->isPolDirTrans() ? polDirTrans : -1);

   // Check trigger ID exists = fired
   if ( (mL2BarrelTriggerId  != 0 && !triggerIdCollection->nominal().isTrigger(mL2BarrelTriggerId)) &&
        (mL2BarrelTriggerId2 != 0 && !triggerIdCollection->nominal().isTrigger(mL2BarrelTriggerId2)) )
   {
      Warning("ReadMuDstBarrelTrig", "Trigger %d (and %d) not found", mL2BarrelTriggerId, mL2BarrelTriggerId2);
      return -2;
   }

   TArrayI &l2Array = stMuEvent->L2Result();
   LOG_DEBUG << Form("AccessL2Decision() from regular muDst: L2Array-size=%d", l2Array.GetSize()) << endm;

   unsigned int* trigL2Chunk = (unsigned int*) l2Array.GetArray();
   const int BEMCW_offset = 20; // valid only for 2009 & 2011 run

   L2wResult2009* l2wResult2009 = (L2wResult2009*) &trigL2Chunk[BEMCW_offset];

   mVecBosEvent->l2bitET  = (l2wResult2009->trigger & 2) > 0; // bit1=ET>thr
   mVecBosEvent->l2bitRnd = (l2wResult2009->trigger & 1) > 0; // bit0=rnd

   if ( (mVecBosEvent->l2bitRnd || mVecBosEvent->l2bitET) == 0) return -3; // L2W-algo did not accept this event

   if (mVecBosEvent->l2bitRnd) {

      for (int m = 0; m < 300; m++) {
         int val = stMuEvent->emcTriggerDetector().highTower(m);
      }
   }

   if (!mVecBosEvent->l2bitET) return -3; // drop L2W-random accepts

   // access L0-HT data
   int mxVal = -1;

   for (int m = 0; m < 300; m++) {
      int val = stMuEvent->emcTriggerDetector().highTower(m);

      if (mxVal < val) mxVal = val;
      if (val < par_DsmThres) continue;

      //printf("Fired L0 HT m=%d val=%d\n",m,val);
   }

   mVecBosEvent->bemc.maxHtDsm = mxVal;
   return 0;
}


// return non-zero on abort
int StVecBosMaker::ReadMuDstEndcapTrig()
{
   if (mIsMc) {
      if (mVecBosEvent->etow.maxAdc < 10. / 60.*4096)
         return -1; //L2 is HT
      mVecBosEvent->l2EbitET = true;
      return 0;
   }

   StMuEvent *stMuEvent = mStMuDstMaker->muDst()->event();
   StMuTriggerIdCollection *triggerIdCollection = &(stMuEvent->triggerIdCollection());

   assert(triggerIdCollection);

   const StTriggerId &l1 = triggerIdCollection->l1();
   vector<unsigned int> idL = l1.triggerIds();

   //printf("nTrig=%d, trigID: ",idL.size());
   for (unsigned int i = 0; i < idL.size(); i++) {
      char txt[100];
      sprintf(txt, "%d", idL[i]);
   }

   // Check trigger ID exists = fired
   if ( (mL2EndcapTriggerId  != 0 && !triggerIdCollection->nominal().isTrigger(mL2EndcapTriggerId)) &&
        (mL2EndcapTriggerId2 != 0 && !triggerIdCollection->nominal().isTrigger(mL2EndcapTriggerId2)) )
   {
     Warning("ReadMuDstEndcapTrig", "Trigger %d (and %d) not found", mL2EndcapTriggerId, mL2EndcapTriggerId2);
      return -2;
   }

   // need to get offset for 2011 run for EEMC
   struct  L2weResult2011 {
      unsigned char  trigger;     // bit0=rnd, bit1=ET>thr
      unsigned char  highestEt;   // cluster Et with 60Gev Max.  bits=Et*256/60
      unsigned short highestRDO;
   };

   TArrayI &l2Array = stMuEvent->L2Result();
   LOG_DEBUG << Form("AccessL2Decision() from regular muDst: L2Array-size=%d", l2Array.GetSize()) << endm;

   unsigned int* trigL2Chunk = (unsigned int*) l2Array.GetArray();
   const int EEMCW_offset = 35; // valid only for 2011 run

   L2weResult2011 *l2weResult2011 = (L2weResult2011*) &trigL2Chunk[EEMCW_offset];

   mVecBosEvent->l2EbitET  = (l2weResult2011->trigger & 2) > 0; // bit1=ET>thr
   mVecBosEvent->l2EbitRnd = (l2weResult2011->trigger & 1) > 0; // bit0=rnd,

   // hack to make the code work also for run 9 and early run 12
   // XXX:ds: What about run 11?
   if (mRunNo < 11000111 || mRunNo > 13000000) {
      mVecBosEvent->l2EbitET  = 1;
      mVecBosEvent->l2EbitRnd = 1;
   }

   if ( (mVecBosEvent->l2EbitRnd || mVecBosEvent->l2EbitET) == 0) return -3; // L2W-algo did not accept this event

   if (mVecBosEvent->l2EbitRnd) {
      for (int m = 0; m < 90; m++) {
         int val = stMuEvent->emcTriggerDetector().highTowerEndcap(m);
      }
   }

   if (!mVecBosEvent->l2EbitET) return -3; // drop L2W-random accepts

   // access L0-HT data
   int mxVal = -1;
   for (int m = 0; m < 90; m++)  {
      int val = stMuEvent->emcTriggerDetector().highTowerEndcap(m);
      if (mxVal < val) mxVal = val;
      if (val < parE_DsmThres) continue;
      //printf("Fired L0 EHT m=%d val=%d\n",m,val);
   }

   mVecBosEvent->etow.maxHtDsm = mxVal;
   return 0;
}


void StVecBosMaker::ReadMuDstBSMD()
{
   const char cPlane[mxBSmd] = {'E', 'P'};

   // Access to muDst
   StMuEmcCollection *emc = mStMuDstMaker->muDst()->muEmcCollection();

   if (!emc) {
      gMessMgr->Warning() << "No EMC data for this muDst event" << endm;
      return;
   }

   // BSMD
   for (int iEP = bsmde; iEP <= bsmdp; iEP++) // official BSMD plane IDs
   {
      int iep = iEP - 3;
      assert(bsmde == 3); // what a hack
      int nHits = emc->getNSmdHits(iEP);
      //printf("muDst BSMD-%c nHit=%d\n",cPlane[iep],nHits);
      int n5 = 0, n1 = 0, n2 = 0, n3 = 0, n4 = 0;

      for (int i = 0; i < nHits; i++)
      {
         StMuEmcHit *hit = emc->getSmdHit(i, iEP);
         float  adc  = hit->getAdc();
         int stripId = hit->getId();

         int statPed, statOfl, statGain;
         mBarrelTables->getStatus(iEP, stripId, statPed, "pedestal");
         mBarrelTables->getStatus(iEP, stripId, statOfl);
         mBarrelTables->getStatus(iEP, stripId, statGain, "calib");

         if (statPed != 1) { mVecBosEvent->bemc.statBsmd[iep][stripId - 1] = 1; n1++; continue; }
         if (statOfl != 1) { mVecBosEvent->bemc.statBsmd[iep][stripId - 1] = 2; n2++; continue; }
         if (statGain < 1 || statGain > 19) {
            mVecBosEvent->bemc.statBsmd[iep][stripId - 1] = 4; n3++; continue;
         }

         float pedRes, sigPed, gain;
         int capID = 0; // just one value for ped residua in pp500, 2009 run

         mBarrelTables->getPedestal(iEP, stripId, capID, pedRes, sigPed);
         mBarrelTables->getCalib(iEP, stripId, 1, gain);

         if (mIsMc) { // overwrite it based on genat DE & private calibration
            float par_bsmdAbsGain = 6e6; // tmp arbitrary absolute calib of bsmd, was 3e6
            float  de = hit->getEnergy();// Geant energy deposit (GeV)
            adc = de * par_bsmdAbsGain;
         }
         else { // correct for pedestal residua
            adc -= pedRes;

            if (adc > 0) n4++;
            if (adc < par_kSigPed * sigPed) continue;
         }

         n5++;
         assert(stripId >= 1 && stripId <= mxBStrips);
         int id0 = stripId - 1;
         mVecBosEvent->bemc.adcBsmd[iep][id0] = adc;

         //if(mNumInputEvents<3 || i <20 )printf("  i=%d, smd%c id=%d, m=%d
         // adc=%.3f pedRes=%.1f, sigP=%.1f stat: O=%d P=%d G=%d
         // gain=%.2f\n",i,cPlane[iep],stripId,1+id0/150,adc,pedRes,sigPed,
         // statOfl,statPed,statGain, gain);
      }

      if (mNumTrigEvents % 5000 == 1) {
         LOG_INFO << Form("unpackMuBSMD-%c() nBbad: ped=%d stat=%d gain=%d ; nAdc: %d>0, %d>thres",
                     cPlane[iep], n1, n2, n3, n4, n5) << endm;
      }
   }
}


void StVecBosMaker::ReadMuDstESMD()
{
   StMuEmcCollection *emc = mStMuDstMaker->muDst()->muEmcCollection();

   if (!emc) {
      LOG_WARN << "No EMC data for this event" << endm;
   }

   for (char uv = 'U'; uv <= 'V'; uv++)
   {
      int sec, strip;
      int nh = emc->getNEndcapSmdHits(uv);

      for (int i = 0; i < nh; i++) {
         StMuEmcHit *hit = emc->getEndcapSmdHit(uv, i, sec, strip);
         float rawAdc = hit->getAdc();
         const EEmcDbItem *eEmcDbItem = mDbE->getByStrip(sec, uv, strip);
         assert(eEmcDbItem); // it should never happened for muDst

         if (eEmcDbItem->fail )   continue; // drop broken channels
         if (eEmcDbItem->ped < 0) continue; // drop channels without peds

         float adc    = rawAdc - eEmcDbItem->ped; // ped subtracted ADC
         float sigPed = eEmcDbItem->sigPed;

         int isec     = sec - 1;
         int iuv      = eEmcDbItem->plane - 'U';
         int istr     = eEmcDbItem->strip - 1;

         assert(isec >= 0 && isec < mxEtowSec); //never trust the input
         assert(iuv  >= 0 && iuv  < mxEsmdPlane);
         assert(istr >= 0 && istr < mxEsmdStrip);

         if (eEmcDbItem->gain <= 0) continue; // drop channels w/o gains
         if (adc < par_kSigPed * sigPed) continue; //drop noise

         mVecBosEvent->esmd.adc[isec][iuv][istr] = adc;
         mVecBosEvent->esmd.ene[isec][iuv][istr] = adc / eEmcDbItem->gain;
      }
   }
}


void StVecBosMaker::ReadMuDstEPRS()
{
   StMuEmcCollection *emc = mStMuDstMaker->muDst()->muEmcCollection();

   if (!emc) {
      LOG_WARN << "No EMC data for this event" << endm;
   }

   int pNh = emc->getNEndcapPrsHits();

   for (int i = 0; i < pNh; i++) {
      int pre, sec, eta, sub; //muDst  ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post

      StMuEmcHit *hit = emc->getEndcapPrsHit(i, sec, sub, eta, pre);
      float rawAdc = hit->getAdc();
      //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method

      const EEmcDbItem *eEmcDbItem = mDbE->getTile(sec, sub - 1 + 'A', eta, pre - 1 + 'P');
      assert(eEmcDbItem); // it should never happened for muDst
      if (eEmcDbItem->fail ) continue; // drop not working channels

      int isec = eEmcDbItem->sec - 1;
      int isub = eEmcDbItem->sub - 'A';
      int ieta = eEmcDbItem->eta - 1;
      int ipre = pre - 1;
      int iphi = isec * mxEtowSub + isub;

      assert(isec >= 0 && isec < mxEtowSec); // check input is ok
      assert(isub >= 0 && isub < mxEtowSub);
      assert(ieta >= 0 && ieta < mxEtowEta);

      float adc = rawAdc - eEmcDbItem->ped; // ped subtracted ADC
      if (adc < par_kSigPed * eEmcDbItem->sigPed) continue;

      mVecBosEvent->eprs.adc[iphi][ieta][ipre] = adc;

      if (eEmcDbItem->gain <= 0) continue; // drop channels w/o gains

      mVecBosEvent->eprs.ene[isec * mxEtowSub + isub][ieta][ipre] = adc / eEmcDbItem->gain;
      mVecBosEvent->eprs.stat[isec * mxEtowSub + isub][ieta][ipre] = 0;
   }
}


/**
 * Saves all vertices. (Before: with (mRank>0 or EEMC matched) && z pos < mCutVertexZ)
 */
void StVecBosMaker::ReadMuDstVerticesTracks()
{
   int numOfPrimaryVertices = mStMuDstMaker->muDst()->numberOfPrimaryVertices();

   // XXX:ds: not sure I understand this cut
   //if (numOfPrimaryVertices < mMinNumPileupVertices) return;

   int nVerticesPosRank = 0;

   for (int iVertex=0; iVertex<numOfPrimaryVertices; iVertex++)
   {
      StMuPrimaryVertex *stMuVertex = mStMuDstMaker->muDst()->primaryVertex(iVertex);
      assert(stMuVertex);

      // Select current vertex
      mStMuDstMaker->muDst()->setVertexIndex(iVertex);

      float rank    = stMuVertex->ranking();
      float rankLog = 999;

      if (rank > 1e6)    rankLog = log(rank - 1e6) + 10;
      else if (rank > 0) rankLog = log(rank);
      else               rankLog = log(rank + 1e6) - 10;

      // Keep some neg. rank vertices for endcap if matched to ETOW
      // XXX:ds: if (rank <= 0 && stMuVertex->nEEMCMatch() <= 0) continue;

      const StThreeVectorF &vertexPosition = stMuVertex->position();

      // XXX:ds: if (fabs(vertexPosition.z()) > mCutVertexZ) continue;

      if (rank > 0) nVerticesPosRank++; // count vertices with rank>0

      VecBosVertex *vecBosVertex = mVecBosEvent->AddVertex(*stMuVertex);

      // Read tracks associated with this vertex
      ReadMuDstTracks(vecBosVertex);
   }

   if (mVecBosEvent->GetNumVertices() <= 0) return;

   // access L0-HT data
   StMuEvent *stMuEvent = mStMuDstMaker->muDst()->event();

   for (int m=0; m<300; m++) {
      int val = stMuEvent->emcTriggerDetector().highTower(m);

      if (val < par_DsmThres) continue;
   }

   for (int m=0; m<90; m++) {
      int val = stMuEvent->emcTriggerDetector().highTowerEndcap(m);

      if (val < parE_DsmThres) continue;
   }

   if (mVecBosEvent->GetNumVertices() <= 0) return;
}


/**
 * Extracts tracks from the mudst container.
 *
 * Previous requirements on tracks:
 * if (flag > 0) &&
 * (primary track has a global track) &&
 * (flag == 301 || 311) &&
 * track P_T >= 1
 *
 * Tracks which do not pass cuts on individual sectors, that is min number of
 * hits, fract of hits, min and max radius in transverse plane
 * from Pibero:
 * It looks like your global track is null. See this post:
 *
 * http://www.star.bnl.gov/HyperNews-star/get/mudst/53.html
 *
 * My reading of this hypernews says its just the way ITTF/MuDst
 * works. You can get a good primary track, but its global track
 * fails the chi2 fit. So the primary track is kept in the MuDst
 * but the global track is dropped. I would suggest you skip those
 * rare primary tracks that have no global tracks, that way you
 * still use most of the tracks in the MuDst. You don't need to
 * skip the entire event, just that track. I guess the down side
 * is you couldn't make a global DCA cut on those rare tracks, right?
 * I guess you could also request S&C to change ITTF/MuDst not to drop
 * the global track for every good primary track regardless of chi2.
 *  mFlag=zxyy, where  z = 1 for pile up track in TPC (otherwise 0)
 *                     x indicates the detectors included in the fit and
 *                    yy indicates the status of the fit.
 *  Positive mFlag values are good fits, negative values are bad fits.
 *
 *  The first digit indicates which detectors were used in the refit:
 *
 *      x=1 -> TPC only
 *      x=3 -> TPC       + primary vertex
 *      x=5 -> SVT + TPC
 *      x=6 -> SVT + TPC + primary vertex
 *      x=7 -> FTPC only
 *      x=8 -> FTPC      + primary
 *      x=9 -> TPC beam background tracks
 *
 *  The last two digits indicate the status of the refit:
 *      = +x01 -> good track
 *
 *      = -x01 -> Bad fit, outlier removal eliminated too many points
 *      = -x02 -> Bad fit, not enough points to fit
 *      = -x03 -> Bad fit, too many fit iterations
 *      = -x04 -> Bad Fit, too many outlier removal iterations
 *      = -x06 -> Bad fit, outlier could not be identified
 *      = -x10 -> Bad fit, not enough points to start
 *
 *      = +x11 -> Short track pointing to EEMC
 */
void StVecBosMaker::ReadMuDstTracks(VecBosVertex* vbVertex)
{
   // Get tracks from the current vertex set in ReadMuDstVerticesTracks
   Int_t nPrimaryTracks = mStMuDstMaker->muDst()->GetNPrimaryTrack();

   for (int iTrack=0; iTrack<nPrimaryTracks; iTrack++)
   {
      StMuTrack *primaryTrack = mStMuDstMaker->muDst()->primaryTracks(iTrack);

      mVecBosEvent->AddTrack(primaryTrack, vbVertex);

      //XXX:ds:if (primaryTrack->flag() <= 0) continue;

      const StMuTrack *globalTrack = primaryTrack->globalTrack();

      if (globalTrack == 0) continue; // see the reason at the end of this method

      // Keep list of all tracks for TPC cone sum in tree ana
      //mVecBosEvent->mVertices[iv].prTrList.push_back(primaryTrack);

      StThreeVectorF ro = globalTrack->lastPoint();

      // TPC+prim vertex tracks and short EEMC tracks
      //XXX:ds:if (primaryTrack->flag() != 301 && primaryTrack->flag() != 311) continue;

      float pt = primaryTrack->pt();

      //XXX:ds:if (pt < 1.0) continue;

      // Accepted tracks
      float hitFrac     = 1.*primaryTrack->nHitsFit() / primaryTrack->nHitsPoss();
      // Victor: in reality mChiSqXY is a normal Xi2 for track and mChiSqZ is Xi2 of fit to  primary vertex
      float globChi2dof = globalTrack->chi2();
      float dedx        = primaryTrack->dEdx() * 1e6;

      StThreeVectorF ri = globalTrack->firstPoint();

      // barrel algo track monitors
      if (mVecBosEvent->l2bitET && vbVertex->mRank > 0 && primaryTrack->flag() == 301)
      {
         //TPC sector dependent filter
         //XXX:ds: int secID = WtpcFilter::getTpcSec(ro.phi(), ro.pseudoRapidity());
         //XXX:ds: if (secID == 20) continue; //poorly calibrated sector for Run 9+11+12?
         //XXX:ds: if (mTpcFilter[secID - 1].accept(primaryTrack) == false) continue;
      }

      // endcap algo track monitors
      if (mVecBosEvent->l2EbitET && ro.pseudoRapidity() > parE_trackEtaMin)
      {
         // TPC sector dependent filter
         //XXX:ds:int secID = WtpcFilter::getTpcSec(ro.phi(), ro.pseudoRapidity());
         //XXX:ds:if ( mTpcFilterE[secID - 1].accept(primaryTrack) == false) continue;
      }

      bool barrelTrack = (mVecBosEvent->l2bitET && vbVertex->mRank > 0 && primaryTrack->flag() == 301 && pt > mVecBosEvent->sMinBTrackPt);

      bool endcapTrack = (mVecBosEvent->l2EbitET && ro.pseudoRapidity() > parE_trackEtaMin && pt > mMinETrackPt);

      //XXX:ds:if (!barrelTrack && !endcapTrack) continue;
   }

   if (mVecBosEvent->GetNumTracks() <= 0) return;
}


void StVecBosMaker::FillTowHit(bool hasVertices)
{
   if (!mVecBosEvent->l2bitET) return; //only barrel triggers

   //find highest rank vertex
   float maxRank   = 0;
   uint  maxRankId = 0;

   VecBosVertexPtrSetIter iVertex = GetVecBosEvent()->mVertices.begin();

   for (short iv=0 ; iVertex != GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &vertex = **iVertex;

      float rank = vertex.mRank;

      if (rank < 0) continue;

      if (rank > maxRank) {
         maxRank   = rank;
         maxRankId = iv;
      }
   }

   int bx7   = mVecBosEvent->bx7;
   int bxBin = -1;

   if (bx7 >= 0 && bx7 < 30) bxBin = 0;
   else if (bx7 < 40)        bxBin = 1;
   else if (bx7 < 110)       bxBin = 2;
   else if (bx7 < 120)       bxBin = 3;

   float Rcylinder  = gBTowGeom->Radius();
   float Rcylinder2 = Rcylinder*Rcylinder;

   // Loop barrel towers and fill histo
   for (int i = 0; i < mxBtow; i++)
   {
      float adc     = mVecBosEvent->bemc.adcTile[kBTow][i];
      bool  fillAdc = false;

      if (adc > 10) fillAdc = true; //~150 MeV threshold for tower firing

      if (hasVertices) {

         float ene  = mVecBosEvent->bemc.eneTile[kBTow][i];
         float delZ = 0; // XXX:ds gBCalTowerCoords[i].z() - mVecBosEvent->mVertices[maxRankId].z;
         float e2et = Rcylinder / sqrt(Rcylinder2 + delZ * delZ);
         float ET   = ene * e2et;

      }
   }

   //some lower threshold plots
   for (int isec = 0; isec < mxEtowSec; isec++)
   {
      for (int isub = 0; isub < mxEtowSub; isub++)
      {
         for (int ieta = 0; ieta < mxEtowEta; ieta++)
         {
            int   iPhi    = isec * mxEtowSub + isub;
            float adc     = mVecBosEvent->etow.adc[iPhi][ieta];
            bool  fillAdc = false;

            if (adc > 10) fillAdc = true; //~150 MeV threshold for tower firing

            if (hasVertices) {

               float ene  = mVecBosEvent->etow.ene[iPhi][ieta];
               float delZ = 0; // XXX:ds gETowCoords[iPhi][ieta].z() - mVecBosEvent->mVertices[maxRankId].z;
               float Rxy  = gETowCoords[iPhi][ieta].Perp();
               float e2et = Rxy / sqrt(Rxy * Rxy + delZ * delZ);
               float ET   = ene * e2et;

            }
         }
      }
   }
}


/**
 * In 2011, L2W fed off the BHT3 L0 trigger, which required a single high tower to have an ADC of greater than 30.  This
 * is the default threshold, but can be set from the macro if a different value is needed.
 */
bool StVecBosMaker::passes_L0()
{
   StMuEvent *stMuEvent = mStMuDstMaker->muDst()->event();

   for (int m = 0; m < 300; m++)
      if (stMuEvent->emcTriggerDetector().highTower(m) > par_l0emulAdcThresh) return true;

   return false;
}


/**
 * In 2011, the L2W trigger required a 2x2 patch of barrel towers where one tower has more than 5.0GeV and the sum of
 * all four is E_T>12.0GeV.  These thresholds are the defaults, but can be set from the macro if a different value is
 * needed.
 */
bool StVecBosMaker::passes_L2()
{
   for (int i = 0; i < mxBtow; i++)
   {
      // Zero means good
      if (mVecBosEvent->bemc.statTile[0][i] != 0 || mVecBosEvent->bemc.eneTile[0][i] <= par_l2emulSeedThresh)
         continue;

      int   ieta = -1;
      int   iphi = -1;
      float etaF = gBCalTowerCoords[i].Eta();
      float phiF = gBCalTowerCoords[i].Phi();

      ConvertEtaPhi2Bins(etaF, phiF, ieta, iphi);
      VecBosCluster c = mVecBosEvent->FindMaxBTow2x2(ieta, iphi, 0);

      if (c.ET > par_l2emulClusterThresh) return true;
   }

   return false;
}


void StVecBosMaker::CalcPtBalance()
{
   VecBosVertexPtrSetIter iVertex = GetVecBosEvent()->mVertices.begin();

   for ( ; iVertex != GetVecBosEvent()->mVertices.end(); ++iVertex)
   {
      VecBosVertex &vertex = **iVertex;

      for (uint iTrack = 0; iTrack < vertex.eleTrack.size(); iTrack++)
      {
         VecBosTrack &track = vertex.eleTrack[iTrack];

         if (track.HasCluster() == false) continue;

         // Loop over branch with EEMC
         //mJets = GetJets(mJetTreeBranchName);
         //if (mJetTreeChain) mJets = GetJetsTreeAnalysis(mJetTreeBranchName);

         // Add up all jets outside of nearDeltaR cone around the electron track
         // Loop over jets
         for (uint iJet = 0; iJet<mVecBosEvent->GetNumStJets(); iJet++)
         {
            StJet *jet = GetJet(iJet);
            TVector3 jetVec; //vector for jet momentum
            jetVec.SetPtEtaPhi(jet->Pt(), jet->Eta(), jet->Phi());

            if (jetVec.DeltaR(track.mP3AtDca) > mVecBosEvent->sMinTrackIsoDeltaR)
               track.ptBalance += jetVec;
         }

         TVector3 clustPt(track.mP3AtDca.X(), track.mP3AtDca.Y(), 0);
         clustPt.SetMag(track.mCluster2x2.ET);

         // Add electron energy. XXX:ds: Why is the energy transverse only?
         track.ptBalance  += clustPt;
         track.sPtBalance  = track.ptBalance.Perp();

         if (track.ptBalance.Dot(clustPt) < 0)
            track.sPtBalance *= -1.;

         // Loop over branch without EEMC
         //mJets = GetJets(mJetTreeBranchNameNoEndcap);
         //if (mJetTreeChain) mJets = GetJetsTreeAnalysis(mJetTreeBranchNameNoEndcap);

         // loop over jets
         for (uint iJet = 0; iJet < mVecBosEvent->GetNumStJetsNoEndcap(); iJet++)
         {
            StJet *jet = GetJet(iJet); // XXX:ds: Need different access method for noendcap jets
            TVector3 jetVec; //vector for jet momentum
            jetVec.SetPtEtaPhi(jet->Pt(), jet->Eta(), jet->Phi());

            if (jetVec.DeltaR(track.mP3AtDca) > mVecBosEvent->sMinTrackIsoDeltaR)
               track.ptBalance_noEEMC += jetVec;
         }

         track.ptBalance_noEEMC += clustPt;
         track.sPtBalance_noEEMC = track.ptBalance_noEEMC.Perp();

         if (track.ptBalance_noEEMC.Dot(clustPt) < 0)
            track.sPtBalance_noEEMC *= -1.;
      }
   }
}


void StVecBosMaker::AnalyzeESMD()
{
   if (!mVecBosEvent->l2EbitET) return;
   //Info("AnalyzeESMD", "");

   VecBosVertexPtrSetIter iVertex = GetVecBosEvent()->mVertices.begin();

   for (short iv=0 ; iVertex != GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &vertex = **iVertex;

      for (uint it = 0; it < vertex.eleTrack.size(); it++)
      {
         VecBosTrack &T = vertex.eleTrack[it];
         if (T.mMatchedTower.id >= 0) continue; //skip barrel towers
         if (T.HasCluster() == false) continue;

         assert(T.mCluster2x2.nTower > 0); // internal logical error

         // Id of strips pointed by prim and glob tracks in each plane
         int hitStrip[2]     = { -1, -1};
         int hitStripGlob[2] = { -1, -1};

         // Initialize shower shape histograms
         TH1F *esmdShowerHist[2];
         esmdShowerHist[0] = new TH1F(Form("esmdU%d", mVecBosEvent->GetEventId()), "esmdU", 41, -10.25, 10.25);
         esmdShowerHist[1] = new TH1F(Form("esmdV%d", mVecBosEvent->GetEventId()), "esmdV", 41, -10.25, 10.25);

         // Loop over planes
         for (int iuv = 0; iuv < 2; iuv++)
         {
            Float_t dca; //primary extrapolation to smd plane
            const StructEEmcStrip *stripPtr = mGeomSmd->getDca2Strip(iuv, T.mMatchedTower.R, &dca); // find pointed strip

            if (!stripPtr) { cout << "No Strip found" << endl; continue;}
            if (fabs(dca) > 0.5) { cout << "DCA to big" << endl; continue;}  // in cm

            Float_t dcaGlob; //global extrapolation to smd plane
            const StructEEmcStrip *stripPtrGlob = mGeomSmd->getDca2Strip(iuv, T.mMatchedTower.Rglob, &dcaGlob); // find pointed strip

            int maxStripId = -1; float maxE = -1;

            int stripId  = stripPtr->stripStructId.stripId;
            int sectorId = stripPtr->stripStructId.sectorId;

            T.hitSector          = sectorId;
            T.esmdGlobStrip[iuv] = stripPtrGlob->stripStructId.stripId - stripId;
            T.esmdDca[iuv]       = dca;
            T.esmdDcaGlob[iuv]   = dcaGlob;

            hitStrip[iuv]     = stripId;
            hitStripGlob[iuv] = stripPtrGlob->stripStructId.stripId;

            // set integration range for smd energy
            int str1 = stripId - parE_nSmdStrip; if (str1 < 1) str1 = 1;
            int str2 = stripId + parE_nSmdStrip; if (str2 > 288) str2 = 288;

            for (int istrip = str1; istrip <= str2; istrip++)
            {
               float ene = mVecBosEvent->esmd.ene[sectorId - 1][iuv][istrip - 1] * 1e3;
               esmdShowerHist[iuv]->SetBinContent(istrip - stripId + parE_nSmdStrip + 1, ene);
               T.esmdShower[iuv][istrip - stripId + parE_nSmdStrip] = ene;
               if (ene > maxE) { maxStripId = istrip; maxE = ene; }
               if (ene > 0) {
                  T.esmdE[iuv] += ene;
                  T.esmdNhit[iuv]++;
               }
            }

            // fit shower shape and fill shower properties
            TF1 *f = new TF1("f", "gaus", -5., 5.);
            f->SetParameter(1, 0);
            esmdShowerHist[iuv]->Fit(f, "RQ", "RQ", -5., 5.);
            T.esmdShowerCentroid[iuv] = f->GetParameter(1);
            T.esmdShowerWidth[iuv] = f->GetParameter(2);

            //get shower x-point from hitStrip + centroid of fit
            T.esmdXPcentroid = mGeomSmd->getIntersection(T.hitSector - 1, hitStrip[0] - 1 + (int)T.esmdShowerCentroid[0], hitStrip[1] - 1 + (int)T.esmdShowerCentroid[1]);
         }
      }
   }
}


void StVecBosMaker::AnalyzeEPRS()
{
   if (!mVecBosEvent->l2EbitET) return;
   // Info("AnalyzeEPRS");

   VecBosVertexPtrSetIter iVertex = GetVecBosEvent()->mVertices.begin();

   for (short iv=0 ; iVertex != GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &vertex = **iVertex;

      for (uint it = 0; it < vertex.eleTrack.size(); it++)
      {
         VecBosTrack &T = vertex.eleTrack[it];
         if (T.mMatchedTower.id >= 0) continue; // skip barrel towers
         if (T.HasCluster() == false) continue;
         assert(T.mCluster2x2.nTower > 0); // internal logical error

         //do some clustering of EPRS deposits and plot histos
      }
   }
}
