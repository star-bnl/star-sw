#include "WBosEventHContainer.h"

#include "TF1.h"
#include "TF2.h"

#include "TrackHContainer.h"
#include "JetHContainer.h"
#include "WBosEvent.h"


ClassImp(WBosEventHContainer)

using namespace std;


/** Default constructor. */
WBosEventHContainer::WBosEventHContainer() : PlotHelper()
{
   BookHists();
}


WBosEventHContainer::WBosEventHContainer(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


/** */
void WBosEventHContainer::BookHists()
{
   string shName;
   TH1*   hist;

   fDir->cd();

   o["hRunId"]            = hist = new TH1I("hRunId", "; Run Id; Events", 61100, 12038000.5, 12099000.5);
   o["hElectronPt"]       = hist = new TH1D("hElectronPt", "; Electron P_{T}; Events", 20, 15, 55);
   o["hElectronPhi"]      = hist = new TH1D("hElectronPhi", "; Electron #phi; Events", 16, -M_PI, M_PI);
   o["hElectronPhi_shift"]      = hist = new TH1D("hElectronPhi_shift", "; Electron #phi; Events", 16, 0, 2*M_PI);
   o["hElectronEta"]      = hist = new TH1D("hElectronEta", "; Electron #eta; Events", 20, -2, 2);
   o["hNeutrinoPt"]       = hist = new TH1D("hNeutrinoPt", " ; Neutrino P_{T}; Events", 20, 15, 55);
   o["hNeutrinoPhi"]      = hist = new TH1D("hNeutrinoPhi", "; Neutrino #phi; Events", 16, -M_PI, M_PI);
   o["hNeutrinoEta"]      = hist = new TH1D("hNeutrinoEta", "; Neutrino #eta; Events", 20, -2, 2);
   o["hWBosonPt_bin1"]    = hist = new TH1D("hWBosonPt_bin1", ";  W Boson P_{T}; Events", 10, 0.5, 1);
   o["hWBosonPt_bin2"]    = hist = new TH1D("hWBosonPt_bin2", ";  W Boson P_{T}; Events", 10, 1, 2.5);
   o["hWBosonPt_bin3"]    = hist = new TH1D("hWBosonPt_bin3", ";  W Boson P_{T}; Events", 10, 2.5, 4);
   o["hWBosonPt_bin4"]    = hist = new TH1D("hWBosonPt_bin4", ";  W Boson P_{T}; Events", 10, 4, 5.5);
   o["hWBosonPt_bin5"]    = hist = new TH1D("hWBosonPt_bin5", ";  W Boson P_{T}; Events", 10, 5.5, 7);
   o["hWBosonPt_bin6"]    = hist = new TH1D("hWBosonPt_bin6", ";  W Boson P_{T}; Events", 10, 7, 10);
   o["hWBosonPt_ybin1"]   = hist = new TH1D("hWBosonPt_ybin1", "; W Boson P_{T}; Events", 20, 0, 20);
   o["hWBosonPt_ybin2"]   = hist = new TH1D("hWBosonPt_ybin2", "; W Boson P_{T}; Events", 20, 0, 20);
   o["hWBosonPt_ybin3"]   = hist = new TH1D("hWBosonPt_ybin3", "; W Boson P_{T}; Events", 20, 0, 20);
   o["hWBosonPt"]         = hist = new TH1D("hWBosonPt", "; W Boson P_{T}; Events", 20, 0, 20);
   o["hWBosonPt_zoomin"]  = hist = new TH1D("hWBosonPt_zoomin", "   ; W Boson P_{T}; Events", 20, 0, 10);
   o["hWBosonPz"]         = hist = new TH1D("hWBosonPz", "; W Boson P_{z}; Events", 30, -60, 60);
   o["hWBosonPz_zoomin"]  = hist = new TH1D("hWBosonPz_zoomin", "   ; W Boson P_{z}; Events", 10, -20, 20);
   o["hWBosonPhi"]        = hist = new TH1D("hWBosonPhi", "; W Boson #phi; Events", 16, -M_PI, M_PI);
   o["hWBosonPhi_shift"]        = hist = new TH1D("hWBosonPhi_shift", "; W Boson #phi; Events", 16, 0, 2*M_PI);
   o["hWBosonEta"]        = hist = new TH1D("hWBosonEta", "; W Boson #eta; Events", 20, -4, 4);
   o["hWBosonRapidity_bin1"]   = hist = new TH1D("hWBosonRapidity_bin1", "; W Boson Rapidity; Events", 10, -0.6, -0.2);
   o["hWBosonRapidity_bin2"]   = hist = new TH1D("hWBosonRapidity_bin2", "; W Boson Rapidity; Events", 10, -0.2, 0.2);
   o["hWBosonRapidity_bin3"]   = hist = new TH1D("hWBosonRapidity_bin3", "; W Boson Rapidity; Events", 10, 0.2, 0.6);
   o["hWBosonRapidity_ptbin1"] = hist = new TH1D("hWBosonRapidity_ptbin1", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonRapidity_ptbin2"] = hist = new TH1D("hWBosonRapidity_ptbin2", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonRapidity_ptbin3"] = hist = new TH1D("hWBosonRapidity_ptbin3", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonRapidity_ptbin4"] = hist = new TH1D("hWBosonRapidity_ptbin4", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonRapidity_ptbin5"] = hist = new TH1D("hWBosonRapidity_ptbin5", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonRapidity_ptbin6"] = hist = new TH1D("hWBosonRapidity_ptbin6", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonRapidity"]        = hist = new TH1D("hWBosonRapidity", "; W Boson Rapidity; Events", 20, -1.5, 1.5);
   o["hWBosonPzVsRapidity"]    = hist = new TH2D("hWBosonPzVsRapidity", "; W Boson P_{z}; W Boson Rapidity", 60, -60, 60, 40, -1, 1);
   hist->SetOption("colz LOGZ");
   o["hWBosonEtaVsElectronEta"]   = hist = new TH2D("hWBosonEtaVsElectronEta", "; W Boson #eta; Electron #eta", 20, -4, 4, 20, -2, 2);
   hist->SetOption("colz LOGZ");

   o["hWBosonMassInv"]    = hist = new TH1F("hWBosonMassInv", "; M_{W} (GeV/c^{2}); Events", 20, 70, 110);

   o["hJetRecoilPt"]               = hist = new TH1F("hJetRecoilPt", "Recoil from Jets; Jet-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilPt"]             = hist = new TH1F("hTrackRecoilPt", "Recoil from Tracks: TPC+TOW; Track-based Recoil P_{T}; Events;", 40, 0, 40);
   o["hTrackRecoilTpcPt"]          = hist = new TH1F("hTrackRecoilTpcPt", "Recoil from Tracks: TPC only; Track-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilWithNeutralsPt"] = hist = new TH1F("hTrackRecoilWithNeutralsPt", "Recoil from Tracks: TPC+emCal (also trackless clusters) ; Track-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilWithNeutralsPtCorrected"]        = hist = new TH1F("hTrackRecoilWithNeutralsPtCorrected", "Recoil from Tracks: TPC+emCal (CORRECTED) ; Track-based Recoil P_{T}; Events", 40, 0, 40);
   o["hTrackRecoilWithNeutralsPt_zoomin"]          = hist = new TH1F("hTrackRecoilWithNeutralsPt_zoomin", "Recoil from Tracks: TPC+emCal (also trackless clusters) ; Track-based Recoil P_{T}; Events", 20, 0, 10);
   o["hTrackRecoilWithNeutralsPtCorrected_zoomin"] = hist = new TH1F("hTrackRecoilWithNeutralsPtCorrected_zoomin", "Recoil from Tracks: TPC+emCal (CORRECTED) ; Track-based Recoil P_{T}; Events", 20, 0, 10);
   o["hPtBalanceFromTracksNeutrals"]        = hist = new TH1F("hPtBalanceFromTracksNeutrals", "P_{T}-balance from tracks; P_{T};", 40, 0, 60);
   o["hPtBalanceCosPhiFromTracksNeutrals"]  = hist = new TH1F("hPtBalanceCosPhiFromTracksNeutrals", "P_{T}-balance cos(#phi); P_{T};", 40, -100, 100);
   o["hPhiBalanceCosPhiFromTracksNeutrals"] = hist = new TH1F("hPhiBalanceCosPhiFromTracksNeutrals", "P_{T}-balance; #phi;", 40, -TMath::Pi(),TMath::Pi());
   o["hBalanceDeltaPhiFromTracksNeutrals"]  = hist = new TH1F("hBalanceDeltaPhiFromTracksNeutrals", "; #Delta #phi;", 40, -TMath::Pi(), TMath::Pi());
   o["hPtBalanceTracksNeutralsVsElecEt"]    = hist = new TH2F("hPtBalanceTracksNeutralsVsElecEt","; E_{T}^{electron}; P_{T}-balance cos(#phi)",40,0.,60.,40,-40,60);
   hist->SetOption("colz LOGZ");

   o["hPtBalanceFromTracks"]        = hist = new TH1F("hPtBalanceFromTracks", "P_{T}-balance from tracks; P_{T};", 40, 0, 60);
   o["hPtBalanceCosPhiFromTracks"]  = hist = new TH1F("hPtBalanceCosPhiFromTracks", "P_{T}-balance cos(#phi); P_{T};", 40, -100, 100);
   o["hPhiBalanceCosPhiFromTracks"] = hist = new TH1F("hPhiBalanceCosPhiFromTracks", "P_{T}-balance; #phi;", 40, -TMath::Pi(),TMath::Pi());
   o["hBalanceDeltaPhiFromTracks"]  = hist = new TH1F("hBalanceDeltaPhiFromTracks", "; #Delta #phi;", 40, -TMath::Pi(), TMath::Pi());
   o["hPtBalanceTracksVsElecEt"]    = hist = new TH2F("hPtBalanceTracksVsElecEt","; E_{T}^{electron}; P_{T}-balance cos(#phi)",40,0.,60.,40,-40,60);
   hist->SetOption("colz LOGZ");
   o["hPtBalanceFromJets"]          = hist = new TH1F("hPtBalanceFromJets", "Jets; P_{T}-balance from tracks; P_{T};", 40, 0, 60);
   o["hPtBalanceCosPhiFromJets"]    = hist = new TH1F("hPtBalanceCosPhiFromJets", "Jets; P_{T}-balance cos(#phi); P_{T};", 40, -100, 100);
   o["hPhiBalanceCosPhiFromJets"]   = hist = new TH1F("hPhiBalanceCosPhiFromJets", "Jets; P_{T}-balance; #phi;", 40, -TMath::Pi(),TMath::Pi());
   o["hBalanceDeltaPhiFromJets"]    = hist = new TH1F("hBalanceDeltaPhiFromJets", "Jets; #Delta #phi;", 40, -TMath::Pi(), TMath::Pi());
   o["hPtBalanceJetsVsElecEt"]      = hist = new TH2F("hPtBalanceJetsVsElecEt","Jets; E_{T}^{electron}; P_{T}-balance cos(#phi)",40,0.,60.,40,-40,60);
   hist->SetOption("colz LOGZ");

   d["track_candidate"] = new TrackHContainer(new TDirectoryFile("track_candidate", "track_candidate", "", fDir));
   d["jets_recoil"] = new JetHContainer(new TDirectoryFile("jets_recoil", "jets_recoil", "", fDir));
}


/** */
void WBosEventHContainer::Fill(ProtoEvent &ev)
{
   WBosEvent& event = (WBosEvent&) ev;

   ((TH1*) o["hRunId"])->Fill(event.GetRunId());
   ((TH1*) o["hElectronPt"])->Fill (event.GetElectronP3().Pt());
   ((TH1*) o["hElectronPhi"])->Fill(event.GetElectronP3().Phi());
   ((TH1*) o["hElectronPhi_shift"])->Fill(event.GetElectronP3().Phi());
   ((TH1*) o["hElectronEta"])->Fill(event.GetElectronP3().Eta());
   ((TH1*) o["hNeutrinoPt"])->Fill (event.GetNeutrinoP3().Pt());
   ((TH1*) o["hNeutrinoPhi"])->Fill(event.GetNeutrinoP3().Phi());
   ((TH1*) o["hNeutrinoEta"])->Fill(event.GetNeutrinoP3().Eta());
   ((TH1*) o["hWBosonPt_bin1"])  -> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPt_bin2"])  -> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPt_bin3"])  -> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPt_bin4"])  -> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPt_bin5"])  -> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPt_bin6"])  -> Fill (event.GetVecBosonP3().Pt());

   if (event.GetVecBosonP4().Rapidity() > -0.6 && event.GetVecBosonP4().Rapidity() < -0.2)
      ((TH1*) o["hWBosonPt_ybin1"])       -> Fill (event.GetVecBosonP3().Pt()); 
   if (event.GetVecBosonP4().Rapidity() > -0.2 && event.GetVecBosonP4().Rapidity() < 0.2)
      ((TH1*) o["hWBosonPt_ybin2"])       -> Fill (event.GetVecBosonP3().Pt());
   if (event.GetVecBosonP4().Rapidity() > 0.2 && event.GetVecBosonP4().Rapidity() < 0.6)
      ((TH1*) o["hWBosonPt_ybin3"])       -> Fill (event.GetVecBosonP3().Pt());

   ((TH1*) o["hWBosonPt"])       -> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPt_zoomin"])-> Fill (event.GetVecBosonP3().Pt());
   ((TH1*) o["hWBosonPz"])->Fill (event.GetVecBosonP4().Pz());
   ((TH1*) o["hWBosonPz_zoomin"])->Fill (event.GetVecBosonP4().Pz());
   ((TH1*) o["hWBosonPhi"])->Fill(event.GetVecBosonP3().Phi());
   ((TH1*) o["hWBosonPhi_shift"])->Fill(event.GetVecBosonP3().Phi());
   ((TH1*) o["hWBosonEta"])->Fill(event.GetVecBosonP3().Eta());
   ((TH1*) o["hWBosonRapidity_bin1"]) -> Fill(event.GetVecBosonP4().Rapidity());
   ((TH1*) o["hWBosonRapidity_bin2"]) -> Fill(event.GetVecBosonP4().Rapidity());
   ((TH1*) o["hWBosonRapidity_bin3"]) -> Fill(event.GetVecBosonP4().Rapidity());

   if (event.GetVecBosonP3().Pt() > 0.5 && event.GetVecBosonP3().Pt() < 1)
      ((TH1*) o["hWBosonRapidity_ptbin1"]) -> Fill(event.GetVecBosonP4().Rapidity());

   if (event.GetVecBosonP3().Pt() > 1 && event.GetVecBosonP3().Pt() < 2.5)
      ((TH1*) o["hWBosonRapidity_ptbin2"]) -> Fill(event.GetVecBosonP4().Rapidity());

   if (event.GetVecBosonP3().Pt() > 2.5 && event.GetVecBosonP3().Pt() < 4)
      ((TH1*) o["hWBosonRapidity_ptbin3"]) -> Fill(event.GetVecBosonP4().Rapidity());

   if (event.GetVecBosonP3().Pt() > 4 && event.GetVecBosonP3().Pt() < 5.5)
      ((TH1*) o["hWBosonRapidity_ptbin4"]) -> Fill(event.GetVecBosonP4().Rapidity());

   if (event.GetVecBosonP3().Pt() > 5.5 && event.GetVecBosonP3().Pt() < 7)
      ((TH1*) o["hWBosonRapidity_ptbin5"]) -> Fill(event.GetVecBosonP4().Rapidity());

   if (event.GetVecBosonP3().Pt() > 7 && event.GetVecBosonP3().Pt() < 10)
      ((TH1*) o["hWBosonRapidity_ptbin6"]) -> Fill(event.GetVecBosonP4().Rapidity());

   ((TH1*) o["hWBosonRapidity"])      -> Fill(event.GetVecBosonP4().Rapidity());
 
   ((TH2*) o["hWBosonPzVsRapidity"])  -> Fill(event.GetVecBosonP4().Pz(), event.GetVecBosonP4().Rapidity());
   ((TH2*) o["hWBosonEtaVsElectronEta"])->Fill(event.GetVecBosonP3().Eta(), event.GetElectronP3().Eta());
   ((TH1*) o["hWBosonMassInv"])->Fill(event.GetVecBosonP4().M());

   ((TH1*) o["hJetRecoilPt"])->Fill(event.GetJetRecoil().Pt());
   ((TH1*) o["hTrackRecoilPt"])->Fill(event.GetTrackRecoil().Pt());
   ((TH1*) o["hTrackRecoilTpcPt"])->Fill(event.mP3TrackRecoilTpc.Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPt"])->Fill(event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPtCorrected"])->Fill(event.GetTrackRecoilTpcNeutralsCorrected().Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPt_zoomin"])->Fill(event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH1*) o["hTrackRecoilWithNeutralsPtCorrected_zoomin"])->Fill(event.GetTrackRecoilTpcNeutralsCorrected().Pt());

   ((TH1*) o["hPtBalanceFromTracksNeutrals"])->Fill(event.mP3BalanceFromTracks.Pt());
   ((TH1*) o["hPtBalanceCosPhiFromTracksNeutrals"])->Fill(event.mPtBalanceCosPhiFromTracks);
   ((TH1*) o["hPhiBalanceCosPhiFromTracksNeutrals"])->Fill(event.mP3BalanceFromTracks.Phi());
   ((TH1*) o["hBalanceDeltaPhiFromTracksNeutrals"])->Fill(event.mBalanceDeltaPhiFromTracks);
   ((TH2*) o["hPtBalanceTracksNeutralsVsElecEt"])->Fill(event.GetElectronP3().Pt(), event.mPtBalanceCosPhiFromTracks);

   ((TH1*) o["hPtBalanceFromTracks"])->Fill(event.mP3BalanceFromTracks2.Pt());
   ((TH1*) o["hPtBalanceCosPhiFromTracks"])->Fill(event.mPtBalanceCosPhiFromTracks2);
   ((TH1*) o["hPhiBalanceCosPhiFromTracks"])->Fill(event.mP3BalanceFromTracks2.Phi());
   ((TH1*) o["hBalanceDeltaPhiFromTracks"])->Fill(event.mBalanceDeltaPhiFromTracks2);
   ((TH2*) o["hPtBalanceTracksVsElecEt"])->Fill(event.GetElectronP3().Pt(), event.mPtBalanceCosPhiFromTracks2);

   ((TH1*) o["hPtBalanceFromJets"])->Fill(event.mP3BalanceFromJets.Pt());
   ((TH1*) o["hPtBalanceCosPhiFromJets"])->Fill(event.mPtBalanceCosPhiFromJets);
   ((TH1*) o["hPhiBalanceCosPhiFromJets"])->Fill(event.mP3BalanceFromJets.Phi());
   ((TH1*) o["hBalanceDeltaPhiFromJets"])->Fill(event.mBalanceDeltaPhiFromJets);
   ((TH2*) o["hPtBalanceJetsVsElecEt"])->Fill(event.GetElectronP3().Pt(), event.mPtBalanceCosPhiFromJets);

   ((TrackHContainer*) d["track_candidate"])->Fill(event.GetElectronTrack());
   ((JetHContainer*) d["jets_recoil"])->Fill(event.mJetsRecoil);
}


/** */
void WBosEventHContainer::FillDerived()
{
   Info("FillDerived()", "Called");
}


/** */
void WBosEventHContainer::PostFill()
{
   Info("PostFill", "Called");
}
