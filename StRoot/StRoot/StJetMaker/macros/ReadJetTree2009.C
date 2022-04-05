//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 16 July 2012
//
// Sample jet tree reader
//

void ReadJetTree2009(int nentries = 1e6,
		     const char* jetfile  = "jets.root",
		     const char* skimfile = "skim.root",
		     const char* outfile  = "out.root")
{
  cout << "nentries = " << nentries << endl;
  cout << "jetfile  = " << jetfile  << endl;
  cout << "skimfile = " << skimfile << endl;
  cout << "outfile  = " << outfile  << endl;

  // Load libraries
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetSkimEvent");

  // Open jet & skim files
  TChain* jetChain = new TChain("jet");
  TChain* skimChain = new TChain("jetSkimTree");

  jetChain->Add(jetfile);
  skimChain->Add(skimfile);

  // Set jet buffer
  StJetEvent* jets = 0;
  jetChain->SetBranchAddress("ConeJets12",&jets);

  // Set skim buffer
  StJetSkimEvent* skim = 0;
  skimChain->SetBranchAddress("skimEventBranch",&skim);

  // Open output file and book histograms
  TFile* ofile = TFile::Open(outfile,"recreate");
  assert(ofile);

  TH1F* hTriggers = new TH1F("hTriggers",";trigger ID",5,0,5);
  TH1F* hVertexZ = new TH1F("hVertexZ",";z-vertex [cm]",100,-200,200);

  TH1F* hJetPt = new TH1F("hJetPt",";jet pt [GeV]",100,0,100);
  TH1F* hJetEta = new TH1F("hJetEta",";jet #eta",100,-1.5,1.5);
  TH1F* hJetPhi = new TH1F("hJetPhi",";jet #phi [radians]",100,-TMath::Pi(),TMath::Pi());
  TH1F* hJetRt = new TH1F("hJetRt",";R_{T}",101,0,1.01);
  TH1F* hNtracks = new TH1F("hNtracks",";track multiplicity",50,0,50);
  TH1F* hNtowers = new TH1F("hNtowers",";tower multiplicity",50,0,50);

  TH1F* hTrackPt = new TH1F("hTrackPt",";track pt [GeV]",100,0,100);
  TH1F* hTrackEta = new TH1F("hTrackEta",";track #eta",100,-1.5,1.5);
  TH1F* hTrackPhi = new TH1F("hTrackPhi",";track #phi [radians]",100,-TMath::Pi(),TMath::Pi());

  TH1F* hTowerPt = new TH1F("hTowerPt",";tower pt [GeV]",100,0,100);
  TH1F* hTowerEta = new TH1F("hTowerEta",";tower #eta",100,-1.5,1.5);
  TH1F* hTowerPhi = new TH1F("hTowerPhi",";tower #phi [radians]",100,-TMath::Pi(),TMath::Pi());

  // Event loop
  for (int iEntry = 0; iEntry < nentries; ++iEntry) {
    if (jetChain->GetEvent(iEntry) <= 0 || skimChain->GetEvent(iEntry) <= 0) break;

    // Should not be null
    assert(jets && skim);

    // Enforce event synchronization
    assert(jets->runId() == skim->runId() && jets->eventId() == skim->eventId());

    // Progress indicator
    if (iEntry % 1000 == 0) cout << iEntry << endl;

    // Trigger loop
    for (int i = 0; i < skim->triggers()->GetEntriesFast(); ++i) {
      StJetSkimTrig* trig = (StJetSkimTrig*)skim->triggers()->At(i);
      // Data only
      if (trig && trig->didFire()) {
	hTriggers->Fill(Form("%d",trig->trigId()),1);
      }
    } // End trigger loop

    // Vertex
    hVertexZ->Fill(jets->vertex()->position().z());

    // Jet loop
    for (int iJet = 0; iJet < jets->vertex()->numberOfJets(); ++iJet) {
      StJetCandidate* jet = jets->vertex()->jet(iJet);
      hJetPt->Fill(jet->pt());
      hJetEta->Fill(jet->eta());
      hJetPhi->Fill(jet->phi());
      hJetRt->Fill(jet->neutralFraction());
      hNtracks->Fill(jet->numberOfTracks());
      hNtowers->Fill(jet->numberOfTowers());
      // Track loop
      for (int iTrack = 0; iTrack < jet->numberOfTracks(); ++iTrack) {
	StJetTrack* track = jet->track(iTrack);
	hTrackPt->Fill(track->pt());
	hTrackEta->Fill(track->eta());
	hTrackPhi->Fill(track->phi());
      } // End track loop
      // Tower loop
      for (int iTower = 0; iTower < jet->numberOfTowers(); ++iTower) {
	StJetTower* tower = jet->tower(iTower);
	hTowerPt->Fill(tower->pt());
	hTowerEta->Fill(tower->eta());
	hTowerPhi->Fill(tower->phi());
      } // End tower loop
    } // End jet loop
  } // End event loop

  // Remove bins with no label
  hTriggers->LabelsDeflate();

  // Write and close output file
  ofile->Write();
  ofile->Close();
}
