#include "Z0EventHContainer.h"

#include "TF1.h"
#include "TF2.h"

#include "ZBosEvent.h"
#include "TrackHContainer.h"


ClassImp(Z0EventHContainer)

using namespace std;


/** Default constructor. */
Z0EventHContainer::Z0EventHContainer() : PlotHelper()
{
   BookHists();
}


Z0EventHContainer::Z0EventHContainer(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


/** */
void Z0EventHContainer::BookHists()
{
   string shName;
   TH1*   h;

   Double_t xBinsPt2016Pr[4]  = {0, 5, 10, 25};
   Double_t xBinsRap2016Pr[4] = {-0.6, -0.2, 0.2, 0.6};

   fDir->cd();

   o["hRunId"]                 = h = new TH1I("hRunId", "; Run Id; Events", 20, 0, 20);
   h->SetOption("hist GRIDX");
   o["hZdcRate"]               = h = new TH1I("hZdcRate", "; ZDC Rate; Events", 50, 100e3, 200e3);
   h->SetOption("hist GRIDX");
   o["hNumJets"]               = h = new TH1I("hNumJets", "; Num. of Jets; Events", 15, 0, 15);
   h->SetOption("hist GRIDX");
   o["hNumJetsRecoil"]         = h = new TH1I("hNumJetsRecoil", "; Num. of Jets in Recoil; Events", 15, 0, 15);
   h->SetOption("hist GRIDX");
   o["hNumJetsWithIsoTrack"]   = h = new TH1I("hNumJetsWithIsoTrack", "; Num. of Jets w/ Iso Track; Events", 15, 0, 15);
   h->SetOption("hist GRIDX");
   o["hNumVertices"]           = h = new TH1I("hNumVertices", "; Num. of Vertices; Events", 10, 0, 10);
   h->SetOption("hist GRIDX");
   o["hNumGoodVertices"]       = h = new TH1I("hNumGoodVertices", "; Num. of Good Vertices; Events", 10, 0, 10);
   h->SetOption("hist GRIDX");
   o["hNumTracks"]             = h = new TH1I("hNumTracks", "; Num. of Tracks; Events", 50, 0, 250);
   h->SetOption("hist GRIDX");
   o["hNumGoodTracks"]         = h = new TH1I("hNumGoodTracks", "; Num. of Good Tracks; Events", 40, 0, 40);
   h->SetOption("hist GRIDX");
   o["hNumBTracks"]            = h = new TH1I("hNumBTracks", "; Num. of Barrel Tracks; Events", 40, 0, 40);
   h->SetOption("hist GRIDX");
   o["hNumETracks"]            = h = new TH1I("hNumETracks", "; Num. of Endcap Tracks; Events", 10, 0, 10);
   h->SetOption("hist GRIDX");
   o["hNumWithClusterTracks"]  = h = new TH1I("hNumWithClusterTracks", "; Num. of Tracks w/ Cluster; Events", 10, 0, 10);
   h->SetOption("hist GRIDX");
   o["hNumIsolatedTracks"]     = h = new TH1I("hNumIsolatedTracks", "; Num. of Isolated Tracks; Events", 10, 0, 10);
   h->SetOption("hist GRIDX");
   o["hNumCandidateTracks"]    = h = new TH1I("hNumCandidateTracks", "; Num. of Candidate Tracks; Events", 10, 0, 10);
   h->SetOption("hist GRIDX");
   o["hNumTracksWithBCluster"] = h = new TH1I("hNumTracksWithBCluster", "; Num. of Tracks with Barrel Cluster; Events", 5, 0, 5);
   h->SetOption("hist GRIDX");

   o["hCandidateTrackPt"]          = h = new TH1F("hCandidateTrackPt", "; Track P_T; Events", 80, 0, 80);
   h->SetOption("hist GRIDX GRIDY XY");

   o["hCandidate1Pt"]   = h = new TH1F("hCandidate1Pt", "; P_T; Events", 81, 0, 81);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hCandidate2Pt"]   = h = new TH1F("hCandidate2Pt", "; P_T; Events", 81, 0, 81);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hCandidate1Eta"]   = h = new TH1F("hCandidate1Eta", ";Candidate 1 - #eta; Events", 20, -2, 2);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hCandidate2Eta"]   = h = new TH1F("hCandidate2Eta", ";Candidate 2 - #eta; Events", 20, -2, 2);
   h->SetOption("hist GRIDX GRIDY XY");

   o["hZ0_MassInv"]   = h = new TH1F("hZ0_MassInv", "; M_{Z^{0}} (GeV/c^{2}); Events", 20, 70, 110);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Pt"]   = h = new TH1F("hZ0_Pt", "; Z^{0}-P_{T} (GeV/c); Events", 20, 0, 25);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Pt_zoomin"]   = h = new TH1F("hZ0_Pt_zoomin", "; Z^{0}-P_{T} (GeV/c); Events", 10, 0, 10);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Pt_2016Pr"]   = h = new TH1D("hZ0_Pt_2016Pr", "; Z^{0}-P_{T} (GeV/c); Events", 3, xBinsPt2016Pr);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Pz"]          = h = new TH1D("hZ0_Pz", "; Z^{0}-P_{z}; Events", 30, -60, 60);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Pz_zoomin"]   = h = new TH1D("hZ0_Pz_zoomin", "; Z^{0}-P_{z}; Events", 10, -20, 20);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Eta"]   = h = new TH1F("hZ0_Eta", "; Z^{0}- #eta; Events", 20, -4, 4);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Rapidity"]   = h = new TH1F("hZ0_Rapidity", "; Z^{0}- y; Events", 20, -2, 2);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Rapidity_2016Pr"]   = h = new TH1D("hZ0_Rapidity_2016Pr", "; Z^{0}- y; Events", 3, xBinsRap2016Pr);
   h->SetOption("hist GRIDX GRIDY XY");
   o["hZ0_Phi"]        = h = new TH1F("hZ0_Phi", "  ; Z^{0} #phi; Events", 16, -M_PI, M_PI);
   h->SetOption("hist GRIDX GRIDY XY");

   d["tracks"] = new TrackHContainer(new TDirectoryFile("tracks", "tracks", "", fDir));
}


/** */
void Z0EventHContainer::Fill(ProtoEvent &ev)
{
   ZBosEvent& event = (ZBosEvent&) ev;

   ((TH1*) o["hRunId"])                     ->Fill(event.GetRunId());
   ((TH1*) o["hZdcRate"])                   ->Fill(event.zdcRate);
   ((TH1*) o["hNumJets"])                   ->Fill(event.GetNumJets());
   ((TH1*) o["hNumJetsRecoil"])             ->Fill(event.GetNumJetsRecoil());
   ((TH1*) o["hNumJetsWithIsoTrack"])       ->Fill(event.GetNumJetsWithIsoTrack());
   ((TH1*) o["hNumVertices"])               ->Fill(event.GetNumVertices());
   ((TH1*) o["hNumGoodVertices"])           ->Fill(event.GetNumGoodVertices());
   ((TH1*) o["hNumTracks"])                 ->Fill(event.GetNumTracks());
   ((TH1*) o["hNumGoodTracks"])             ->Fill(event.GetNumGoodTracks());
   ((TH1*) o["hNumBTracks"])                ->Fill(event.GetNumBTracks());
   ((TH1*) o["hNumETracks"])                ->Fill(event.GetNumETracks());
   ((TH1*) o["hNumWithClusterTracks"])      ->Fill(event.GetNumWithClusterTracks());
   ((TH1*) o["hNumIsolatedTracks"])         ->Fill(event.GetNumIsolatedTracks());
   ((TH1*) o["hNumCandidateTracks"])        ->Fill(event.GetNumCandidateTracks());
   ((TH1*) o["hNumTracksWithBCluster"])     ->Fill(event.GetNumTracksWithBCluster());

   ((TH1*) o["hCandidate1Pt"])   ->Fill(event.GetCandidate1_P3().Pt());
   ((TH1*) o["hCandidate2Pt"])   ->Fill(event.GetCandidate2_P3().Pt());
   ((TH1*) o["hZ0_MassInv"])     ->Fill(event.GetVecBosonP4().M());
   ((TH1*) o["hZ0_Pt"])          ->Fill(event.GetVecBosonP4().Pt());
   ((TH1*) o["hZ0_Pt_zoomin"])   ->Fill(event.GetVecBosonP4().Pt());
   ((TH1*) o["hZ0_Pt_2016Pr"])   ->Fill(event.GetVecBosonP4().Pt());
   ((TH1*) o["hZ0_Pz"])          ->Fill(event.GetVecBosonP4().Pz());
   ((TH1*) o["hZ0_Pz_zoomin"])   ->Fill(event.GetVecBosonP4().Pz());

   if ( event.GetCandidate1_P3().Pt() && event.GetCandidate2_P3().Pt() ) {
      ((TH1*) o["hCandidate1Eta"])      ->Fill(event.GetCandidate1_P3().Eta());
      ((TH1*) o["hCandidate2Eta"])      ->Fill(event.GetCandidate2_P3().Eta());
      ((TH1*) o["hZ0_Eta"])             ->Fill(event.GetVecBosonP4().Eta());
      ((TH1*) o["hZ0_Rapidity"])        ->Fill(event.GetVecBosonP4().Rapidity());
      ((TH1*) o["hZ0_Rapidity_2016Pr"]) ->Fill(event.GetVecBosonP4().Rapidity());
      ((TH1*) o["hZ0_Phi"])             ->Fill(event.GetVecBosonP4().Phi());
   }

   d["tracks"]->Fill(ev);
}
