//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 20 July 2010
//
// Simple macro to read Pythia record saved in .pythia.root files
// by the BFC trigger filter. The Pythia tree simply contains
// a StPythiaEvent object for each event.
//

void ReadPythia(int nevents = 10, const char* pythiafile = "jet_pp200_pythia_6422_perugia_0_pt2_3gev_2000evts_185.pythia.root")
{
  // Load shared library for StythiaEvent
  gSystem->Load("StJetSkimEvent");

  // Open pythia file(s)
  TChain* chain = new TChain("PythiaTree");
  chain->Add(pythiafile);

  // Set Pythia event buffer
  StPythiaEvent* pythiaEvent = 0;
  chain->SetBranchAddress("PythiaBranch",&pythiaEvent);

  // Event loop
  for (int iEvent = 0; iEvent < nevents; ++iEvent) {
    // Read event and exit event loop if end-of-file or error encountered
    if (chain->GetEvent(iEvent) <= 0) break;

    // Print Pythia event
    cout << "iEvent = " << iEvent << endl;
    cout << "runId = " << pythiaEvent->runId() << endl;
    cout << "eventId = " << pythiaEvent->eventId() << endl;
    cout << "processId = " << pythiaEvent->processId() << endl;
    const TVector3& v = pythiaEvent->vertex();
    cout << "vx = " << v.x() << ", vy = " << v.y() << ", vz = " << v.z() << endl;
    cout << "s = " << pythiaEvent->s() << endl;
    cout << "t = " << pythiaEvent->t() << endl;
    cout << "u = " << pythiaEvent->u() << endl;
    cout << "pt = " << pythiaEvent->pt() << endl;
    cout << "cosTheta = " << pythiaEvent->cosTheta() << endl;
    cout << endl;
  } // End event loop
}
