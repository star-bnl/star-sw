#include <iostream>

class StMaker;
class StChain;
class StPicoDstReader;

const Char_t* testInFName = "/star/u/gnigmat/soft/u/centrality_definition/Centrality/input/st_physics_adc_19084053_raw_0000006.picoDst.root";

//________________
void runCentralityAnalyzer_Ru(const Char_t* inFileName = testInFName,
                           const Char_t* oFileName = "oTestAna.root") {

  std::cout << "Start running centrality analyzer" << std::endl;
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  // Load specific libraries
  gSystem->Load("StPicoEvent");
  gSystem->Load("StPicoDstMaker");
  gSystem->Load("StCentralityAnalyzer");

  // Create new chain
  StChain *chain = new StChain();

  StPicoDstReader *reader = new StPicoDstReader(inFileName);
  reader->Init();
  reader->SetStatus("*", 0);
  reader->SetStatus("Event*", 1);
  reader->SetStatus("Track*", 0);
  reader->SetStatus("BTofPidTraits*", 0);

  StCentralityAnalyzer *anaMaker = new StCentralityAnalyzer(reader, oFileName);
  anaMaker->setUsePileUp(true);
  anaMaker->setUseLumCorr(false);
  anaMaker->setUseVzCorr(false);
  anaMaker->setVtxZCut(-35., 25.);
  anaMaker->addTriggerId(600001);
  anaMaker->addTriggerId(600011);
  anaMaker->addTriggerId(600021);
  anaMaker->addTriggerId(600031);
  anaMaker->setRunIdParameters(57990, 19071030, 19129020);

  //These are found by the pileup code in ../QA/pileup
  //Ru pileup parameters
  //---------------------------------------------------
  //double a0=-1.1761972238142, a1=0.91190057314294, a2=-0.000361521536620541, a3=9.77684373134588e-07, a4=-1.28146895587406e-09;
  //double b0=13.5471357600988, b1=1.44271774447208, b2=-0.00288349264907301, b3=7.3544264332388e-06, b4=-7.54314488345979e-09;
  //double c0=-11.2648623623068, c1=0.419490120285247, c2=0.001856323147039, c3=-4.68448675939297e-06, c4=4.40012904437496e-09;
  //---------------------------------------------------
  //New parameters from corrected bad-runs list
  double a0=-1.17135278798859, a1=0.911627701794553, a2=-0.000359098306320787, a3=9.61800858474059e-07, a4=-1.25136113760776e-09;
  double b0=13.5426221755897, b1=1.44261201539344, b2=-0.00288428931227279, b3=7.35384541646783e-06, b4=-7.53407759526067e-09;
  double c0=-11.2591376113937, c1=0.419541462167548, c2=0.00185578651291454, c3=-4.68933832723005e-06, c4=4.4151761900593e-09;
  //---------------------------------------------------
  anaMaker->setPileUpParameters(a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,c0,c1,c2,c3,c4);
  
  //Ru luminosity correction parameters
  double lumcorr_a = -1.1624e-5;
  double lumcorr_b = 98.113;
  double lumcorr_bprime = 97.9927;
  //---------------------------------------------------
  //---------------------------------------------------
  anaMaker->setLumCorrParameters(lumcorr_a,lumcorr_b,lumcorr_bprime);

  //Vz correction parameters for Ru+Ru
  double vzCorPar0 = 302.019;
  double vzCorPar1 = 0.0270308;
  double vzCorPar2 = -0.0017795;
  double vzCorPar3 = 0.000243777;
  double vzCorPar4 = 1.18462e-05;
  double vzCorPar5 = -3.3121e-07;
  double vzCorPar6 = -1.22651e-08;

  anaMaker->setVzCorrParameters(vzCorPar0, vzCorPar1, vzCorPar2, vzCorPar3, vzCorPar4, vzCorPar5, vzCorPar6);

  //set shape correction weight index; 0: Ru, 1: Zr
  anaMaker->setShapeIndex(0);
  //---------------------------------------------------

  std::cout << "Initializing chain" << std::endl;
  // Check that all maker has been successfully initialized
  if( chain->Init() == kStErr ){
    std::cout << "Error during the chain initializtion. Exit. " << std::endl;
    return;
  }
  std::cout << "... done" << std::endl;

  int nEvents2Process = reader->chain()->GetEntries();
  std::cout << " Number of events in files: " << nEvents2Process << std::endl;

  // Processing events
  for (Int_t iEvent=0; iEvent<nEvents2Process; iEvent++) {
    
    if( iEvent % 1000 == 0 ) std::cout << "Macro: working on event: " << iEvent << std::endl;
    chain->Clear();

    // Check return code
    int iret = chain->Make();
    // Quit event processing if return code is not 0
    if (iret) { std::cout << "Bad return code!" << iret << endl; break; }
  } // for (Int_t iEvent=0; iEvent<nEvents2Process; iEvent++)
  std::cout << "Data have been processed, Master" << std::endl;

  std::cout << "Finalizing chain" << std::endl;
  // Finalize all makers in chain
  chain->Finish();
  std::cout << "... done" << std::endl;

	std::cout << "Centrality analyzer finished the work" << std::endl;
}
