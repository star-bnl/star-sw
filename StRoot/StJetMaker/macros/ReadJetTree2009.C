#include <map>

void ReadJetTree2009(int nentries = 1e6,
		     const char* jetfile  = "st_physics_10143008_raw_6020001.jets.root",
		     const char* skimfile = "st_physics_10143008_raw_6020001.skim.root")
{
  cout << "nentries = " << nentries << endl;
  cout << "jetfile  = " << jetfile  << endl;
  cout << "skimfile = " << skimfile << endl;
  cout << endl;

  // Load libraries
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetSkimEvent");

  // Open jet & skim files
  TChain* jetChain = new TChain("jet");
  TChain* skimChain = new TChain("jetSkimTree");

  jetChain->Add(jetfile);
  skimChain->Add(skimfile);

  // Set jet buffer
  StJetEvent* jetEvent = 0;
  jetChain->SetBranchAddress("ConeJets12",&jetEvent);

  // Set skim buffer
  StJetSkimEvent* skimEvent = 0;
  skimChain->SetBranchAddress("skimEventBranch",&skimEvent);

  // Event loop
  for (int iEntry = 0; iEntry < nentries; ++iEntry) {
    if (jetChain->GetEvent(iEntry) <= 0 || skimChain->GetEvent(iEntry) <= 0) break;

    // Should not be null
    assert(jetEvent && skimEvent);

    // Enforce event synchronization
    assert(jetEvent->runId() == skimEvent->runId() && jetEvent->eventId() == skimEvent->eventId());

    //if (iEntry % 1000 == 0) cout << iEntry << endl;

    cout << "run: " << jetEvent->runId() << endl;
    cout << "event: " << jetEvent->eventId() << endl;

    // Print triggers
    cout << "triggers: ";
    TIter next(skimEvent->triggers());
    StJetSkimTrig* trig;
    while (trig = (StJetSkimTrig*)next()) {
      cout << trig->trigId() << " ";
    }
    cout << endl;

#if 0
    // Get 2009 pp200 JP1 trigger
    StJetSkimTrig* trig = skimEvent->trigger(240410);
    if (!trig) continue;
    if (!trig->didFire()) continue;
#endif

    // Get jet patches above JP1 threshold
    map<int,int> barrelJetPatches = skimEvent->barrelJetPatchesAboveTh(1);
    map<int,int> endcapJetPatches = skimEvent->endcapJetPatchesAboveTh(1);
    map<int,int> overlapJetPatches = skimEvent->overlapJetPatchesAboveTh(1);

    cout << "barrel jet patches above threshold: ";
    for (map<int,int>::const_iterator it = barrelJetPatches.begin(); it != barrelJetPatches.end(); ++it) {
      int jp = it->first;
      int adc = it->second;
      cout << "JP" << jp << "=" << adc << " ";
    }
    cout << endl;

    cout << "endcap jet patches above threshold: ";
    for (map<int,int>::const_iterator it = endcapJetPatches.begin(); it != endcapJetPatches.end(); ++it) {
      int jp = it->first;
      int adc = it->second;
      cout << "JP" << jp << "=" << adc << " ";
    }
    cout << endl;

    cout << "overlap jet patches above threshold: ";
    for (map<int,int>::const_iterator it = overlapJetPatches.begin(); it !=overlapJetPatches.end(); ++it) {
      int jp = it->first;
      int adc = it->second;
      cout << "JP" << jp << "=" << adc << " ";
    }
    cout << endl;

    int bbctimebin = skimEvent->bbcTimeBin() >> 9 & 0xf;
    cout << "bbctimebin: " << bbctimebin << endl;

    cout << "nvertices: " << jetEvent->numberOfVertices() << endl;

    for (int iVertex = 0; iVertex < jetEvent->numberOfVertices(); ++iVertex) {
      StJetVertex* vertex = jetEvent->vertex(iVertex);
      cout << "Vertex #" << iVertex
	   << ": vx=" << vertex->position().x()
	   << " vy=" << vertex->position().y()
	   << " vz=" << vertex->position().z() << endl;
      // Jet loop
      cout << "njets: " << vertex->numberOfJets() << endl;
      for (int iJet = 0; iJet < vertex->numberOfJets(); ++iJet) {
	StJetCandidate* jet = vertex->jet(iJet);
	cout << "Jet #" << iJet
	     << ": pt=" << jet->pt()
	     << " eta=" << jet->eta()
	     << " phi=" << jet->phi()
	     << " rt=" << jet->neutralFraction()
	     << " ntracks=" << jet->numberOfTracks()
	     << " ntowers=" << jet->numberOfTowers() << endl;
	// Track loop
	for (int iTrack = 0; iTrack < jet->numberOfTracks(); ++iTrack) {
	  StJetTrack* track = jet->track(iTrack);
	  cout << "Track #" << iTrack
	       << ": id=" << track->id()
	       << " pt=" << track->pt()
	       << " eta=" << track->eta()
	       << " phi=" << track->phi() << endl;
	} // End track loop
	// Tower loop
	for (int iTower = 0; iTower < jet->numberOfTowers(); ++iTower) {
	  StJetTower* tower = jet->tower(iTower);
	  cout << "Tower #" << iTower
	       << ": id=" << tower->id()
	       << " detid=" << tower->detectorId()
	       << " pt=" << tower->pt()
	       << " eta=" << tower->eta()
	       << " phi=" << tower->phi() << endl;
	} // End tower loop
      } // End jet loop
    } // End vertex loop
    cout << endl;
  } // End event loop
}
