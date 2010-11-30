//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 20 July 2010
//
// Simple macro to read jet trees in old format (StJets) and skim trees
//

void ReadStJets(int nevents = 10, const char* jetfile = "Jets_pt15_25_01.root", const char* skimfile = "Skim_pt15_25_01.root")
{
  // Load shared libraries
  gSystem->Load("StJets");
  gSystem->Load("StJetSkimEvent");

  // Create chains
  TChain* jetchain = new TChain("jet");
  TChain* skimchain = new TChain("jetSkimTree");

  // Add file lists
  jetchain->Add(jetfile);
  skimchain->Add(skimfile);

  // Setup skim event buffer
  StJetSkimEvent* skimevent = 0;
  skimchain->SetBranchAddress("skimEventBranch",&skimevent);

  // Event loop
  for (int iEvent = 0; iEvent < nevents; ++iEvent) {
    // Exit loop on end-of-file or error
    if (jetchain->GetEvent(iEvent) <= 0 || skimchain->GetEvent(iEvent) <= 0) break;

    // Loop over jet branches
    for (int iBranch = 0; iBranch < jetchain->GetNbranches(); ++iBranch) {
      TBranch* branch = (TBranch*)jetchain->GetListOfBranches()->At(iBranch);
      cout << "Branch #" << iBranch << ": " << branch->GetName() << endl;

      // Get jets
      StJets* stjets = *(StJets**)branch->GetAddress();

      // Jet branch and skim branch must be synchronized
      assert(stjets->runId() == skimevent->runId() && stjets->eventId() == skimevent->eventId());

      // Print
      cout << "iEvent = " << iEvent << endl;
      cout << "runId = " << stjets->runId() << endl;
      cout << "eventId = " << stjets->eventId() << endl;

      TVector3 vertex(skimevent->bestVert()->position());
      cout << "vx = " << vertex.x() << ", vy = " << vertex.y() << ", vz = " << vertex.z() << endl;

      // Loop over jets
      cout << "nJets = " << stjets->nJets() << endl;
      for (int iJet = 0; iJet < stjets->nJets(); ++iJet) {
	StJet* jet = (StJet*)stjets->jets()->At(iJet);
	cout << "Jet #" << iJet << ": jetPt = " << jet->jetPt << ", jetEta = " << jet->jetEta << ", jetPhi = " << jet->jetPhi
	     << ", nTracks = " << jet->nTracks << ", nBtowers = " << jet->nBtowers << ", nEtowers = " << jet->nEtowers
	     << endl;

	// Loop over tracks
	TObjArray tracks = stjets->tracks(iJet);
	if (strcmp(branch->GetName(),"PythiaConeJets") == 0) {
	  for (int iTrack = 0; iTrack < tracks.GetEntriesFast(); ++iTrack) {
	    TrackToJetIndex* track = (TrackToJetIndex*)tracks.At(iTrack);
	    cout << "Particle #" << iTrack << ": id = " << track->id() << ", pdg = " << track->pdg()
		 << ", status = " << track->status()
		 << ", pt = " << track->Pt() << ", eta = " << track->Eta() << ", phi = " << track->Phi()
		 << ", e = " << track->E() << ", m = " << track->M()
		 << endl;
	  } // End loop over tracks
	}
	else {
	  for (int iTrack = 0; iTrack < tracks.GetEntriesFast(); ++iTrack) {
	    TrackToJetIndex* track = (TrackToJetIndex*)tracks.At(iTrack);
	    cout << "Track #" << iTrack << ": id = " << track->trackId() << ", detId = " << track->detectorId()
		 << ", pt = " << track->Pt() << ", eta = " << track->Eta() << ", phi = " << track->Phi()
		 << endl;
	  } // End loop over tracks
	}

	// Loop over towers
	TObjArray towers = stjets->towers(iJet);
	for (int iTower = 0; iTower < towers.GetEntriesFast(); ++iTower) {
	  TowerToJetIndex* tower = (TowerToJetIndex*)towers.At(iTower);
	  cout << "Tower #" << iTower << ": id = " << tower->towerId() << ", detId = " << tower->detectorId()
	       << ", pt = " << tower->Pt() << ", eta = " << tower->Eta() << ", phi = " << tower->Phi()
	       << endl;
	} // End loop over towers
      }	// End loop over jets
    } // End loop over branches

    cout << endl;
  } // End event loop
}
