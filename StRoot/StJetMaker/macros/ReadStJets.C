//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 20 July 2010
//
// Simple macro to read jet trees in old format (StJets) and skim trees
//

void ReadStJets(int nevents = 10, const char* jetfile = "blah.jets.root", const char* skimfile = "blah.skim.root")
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

      const float* pos = skimevent->bestVert()->position();
      cout << "vx = " << pos[0] << ", vy = " << pos[1] << ", vz = " << pos[2] << endl;
      cout << "nJets = " << stjets->nJets() << endl;

      // Loop over jets
      for (int iJet = 0; iJet < stjets->nJets(); ++iJet) {
	StJet* jet = (StJet*)stjets->jets()->At(iJet);
	cout << "Jet #" << iJet << ": jetPt = " << jet->jetPt << ", jetEta = " << jet->jetEta << ", jetPhi = " << jet->jetPhi
	     << ", nTracks = " << jet->nTracks << ", nBtowers = " << jet->nBtowers << ", nEtowers = " << jet->nEtowers
	     << endl;
      }	// End loop over jets
    } // End loop over branches

    cout << endl;
  } // End event loop
}
