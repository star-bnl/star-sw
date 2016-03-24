#include "EventHContainer.h"

#include "TF1.h"
#include "TF2.h"

#include "WBosEvent.h"
#include "JetHContainer.h"
#include "TrackHContainer.h"


ClassImp(EventHContainer)

using namespace std;


/** Default constructor. */
EventHContainer::EventHContainer() : PlotHelper()
{
   BookHists();
}


EventHContainer::EventHContainer(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


/** */
void EventHContainer::BookHists()
{
   string shName;
   TH1*   h;

   fDir->cd();

   o["hRunId"]                 = h = new TH1I("hRunId", "; Run Id; Events", 60100, 12038000, 12098100);
   h->SetOption("hist GRIDX");
   o["hZdcRate"]               = h = new TH1I("hZdcRate", "; ZDC Rate; Events", 50, 100e3, 200e3);
   h->SetOption("hist GRIDX");
   o["hNumJets"]               = h = new TH1I("hNumJets", "; Num. of Jets; Events", 15, 0, 15);
   h->SetOption("hist GRIDX");

   o["hNumTracksRecoil"]       = h = new TH1I("hNumTracksRecoil", "; Num. of Tracks in Recoil; Events", 50, 0, 50); 
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

   o["hCandidateTrackEScaledPt"]   = h = new TH1F("hCandidateTrackEScaledPt", "; P_T; Events", 81, 0, 81);
   h->SetOption("hist GRIDX GRIDY XY");

   o["hJetRecoilPt"]               = h = new TH1F("hJetRecoilPt", "Recoil from Jets; Jet-based Recoil P_{T}; Events", 40, 0, 40);
   o["hJetRecoilPt_zoomin"]        = h = new TH1F("hJetRecoilPt_zoomin", "Recoil from Jets; Jet-based Recoil P_{T}; Events", 20, 0, 10);
   o["hTrackRecoilPt"]             = h = new TH1F("hTrackRecoilPt", "Recoil from Tracks: TPC+TOW; Track-based Recoil P_{T}; Events;", 40, 0, 40);
   o["hTrackRecoilTpcPt"]          = h = new TH1F("hTrackRecoilTpcPt", "Recoil from Tracks: TPC only; Track-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilWithNeutralsPt"] = h = new TH1F("hTrackRecoilWithNeutralsPt", "Recoil from Tracks: TPC+emCal (also trackless clusters) ; Track-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilWithNeutralsPtCorrected"]        = h = new TH1F("hTrackRecoilWithNeutralsPtCorrected", "Recoil from Tracks: TPC+emCal (CORRECTED) ; Track-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilWithNeutralsPt_zoomin"]          = h = new TH1F("hTrackRecoilWithNeutralsPt_zoomin", "Recoil from Tracks: TPC+emCal (also trackless clusters) ; Track-based Recoil P_{T}; Events", 20, 0, 10);
   o["hTrackRecoilWithNeutralsPtCorrected_zoomin"] = h = new TH1F("hTrackRecoilWithNeutralsPtCorrected_zoomin", "Recoil from Tracks: TPC+emCal (CORRECTED) ; Track-based Recoil P_{T}; Events", 20, 0, 10);

   o["hNumTracksRecoilVsTrackRecoilWithNeutralsPt"]           = h = new TH2F("hNumTracksRecoilVsTrackRecoilWithNeutralsPt", "; Num. of Tracks in Recoil; Track-based Recoil P_{T}", 50, 0, 50, 10, 0, 10); 
   h->SetOption("hist GRIDX");

   o["hNumTracksRecoilVsCandidateTrackEScaledPt"]           = h = new TH2F("hNumTracksRecoilVsCandidateTrackEScaledPt", "; Num. of Tracks in Recoil; Electron P_{T}", 50, 0, 50, 50, 0, 50); 
   h->SetOption("hist GRIDX");

   d["tracks"] = new TrackHContainer(new TDirectoryFile("tracks", "tracks", "", fDir));
   d["jets"]   = new JetHContainer(new TDirectoryFile("jets", "jets", "", fDir));
}


/** */
void EventHContainer::Fill(ProtoEvent &ev)
{
   WBosEvent& event = (WBosEvent&) ev;

   ((TH1*) o["hRunId"])                     ->Fill(event.GetRunId());
   ((TH1*) o["hZdcRate"])                   ->Fill(event.zdcRate);
   ((TH1*) o["hNumJets"])                   ->Fill(event.GetNumJets());
   ((TH1*) o["hNumTracksRecoil"])           ->Fill(event.mNumRecoilTracksTpc);
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

   //((TH1*) o["hCandidateTrackPt"])          ->Fill(event.GetElectronTrack().mP3AtDca.Pt());
   ((TH1*) o["hCandidateTrackEScaledPt"])   ->Fill(event.GetElectronP3().Pt());
   ((TH1*) o["hJetRecoilPt"])               ->Fill(event.GetJetRecoil().Pt());
   ((TH1*) o["hJetRecoilPt_zoomin"])        ->Fill(event.GetJetRecoil().Pt());
   ((TH1*) o["hTrackRecoilPt"])             ->Fill(event.GetTrackRecoil().Pt());
   ((TH1*) o["hTrackRecoilTpcPt"])          ->Fill(event.mP3TrackRecoilTpc.Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPt"]) ->Fill(event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPtCorrected"])->Fill(event.GetTrackRecoilTpcNeutralsCorrected().Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPt_zoomin"])->Fill(event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPtCorrected_zoomin"])->Fill(event.GetTrackRecoilTpcNeutralsCorrected().Pt());
   ((TH2*) o["hNumTracksRecoilVsTrackRecoilWithNeutralsPt"]) ->Fill(event.mNumRecoilTracksTpc, event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH2*) o["hNumTracksRecoilVsCandidateTrackEScaledPt"])   ->Fill(event.mNumRecoilTracksTpc, event.GetElectronP3().Pt());


   d["tracks"]->Fill(ev);
   d["jets"]->Fill(ev);
}
