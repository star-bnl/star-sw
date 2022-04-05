//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 23 June 2010
//

void TestJpsiTopo(int nevents = 1e6, const char* mudstfile = "/star/data28/reco/ppProductionJPsi/FullField/P06id/2006/139/7139023/st_jpsi_7139023_raw_1130010.MuDst.root")
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");

  // Load shared libraries
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StTriggerUtilities");

  // Create chain
  StChain* chain = new StChain; 

  // MuDst reader
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,"",100000,"MuDst");

  // StMuDbReader...
  StMuDbReader* db = StMuDbReader::instance();

  // star database
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // Barrel ADC to energy maker
  StEmcADCtoEMaker* adc2e = new StEmcADCtoEMaker;

  // Trigger simulator
  StTriggerSimuMaker* trigSimu = new StTriggerSimuMaker;
  trigSimu->setMC(false);
  trigSimu->useBemc();
  trigSimu->bemc->setConfig(StBemcTriggerSimu::kOffline);

  // Initialize chain
  chain->Init();

  // Event loop
  for (int iEvent = 1; iEvent <= nevents; ++iEvent) {
    chain->Clear();
    int status = chain->Make(iEvent);
    if (status == kStSkip) continue;
    if (status % 10 == kStEOF || status % 10 == kStFatal) break;

    cout << "Run = " << chain->GetRunNumber() << ", Event = " << chain->GetEventNumber() << endl;

    // Test J/psi topology trigger in Run 6
    if (trigSimu->isTrigger(117705) || trigSimu->isTrigger(137705)) {
      // Print number of J/psi candidate BEMC tower pairs that could have fired the J/psi trigger
      cout << "Number of J/psi candidates = " << trigSimu->bemc->numberOfJpsiCandidates() << endl;
      // Loop over J/psi candidates
      for (int i = 0; i < trigSimu->bemc->numberOfJpsiCandidates(); ++i) {
	cout << "J/psi candidate #" << i
	     << ": towerId1 = " << trigSimu->bemc->jpsiCandidateFirstTowerId(i)
	     << ", towerId2 = " << trigSimu->bemc->jpsiCandidateSecondTowerId(i)
	     << endl;
      }	// End loop over J/psi candidates
    }

  } // End event loop
}
