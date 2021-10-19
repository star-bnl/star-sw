#include <iostream>

class StMaker;
class StChain;
class StPicoDstReader;

const Char_t* testInFName = "/star/u/gnigmat/soft/u/centrality_definition/Centrality/input/st_physics_adc_19084053_raw_0000006.picoDst.root";

//________________
void runCentralityAnalyzer(const Char_t* inFileName = testInFName,
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
  reader->SetStatus("Track*", 1);
  reader->SetStatus("BTofPidTraits*", 1);

  StCentralityAnalyzer *anaMaker = new StCentralityAnalyzer(reader, oFileName);
  anaMaker->setUsePileUp(false);
  anaMaker->setVtxZCut(-35., 25.);
  anaMaker->addTriggerId(600001);
  anaMaker->addTriggerId(600011);
  anaMaker->addTriggerId(600021);
  anaMaker->addTriggerId(600031);
  anaMaker->setRunIdParameters(57990, 19071030, 19129020);

  //These are found by the pileup code in ../QA/pileup
  //Test parameters
  double a0=-1.27785869748694, a1=0.918278154432662, a2=-0.000421651379949012, a3=1.20429516308073e-06, a4=-1.54521733920117e-09;
  double b0=13.2397261316822, b1=1.46248491801055, b2=-0.00310419986439369, b3=8.21875717651988e-06, b4=-8.61348326634967e-09;
  double c0=-11.2055760170529, c1=0.415626210975414, c2=0.00191757674104115, c3=-4.93833806589717e-06, c4=4.74283345256434e-09;
  anaMaker->setPileUpParameters(a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,c0,c1,c2,c3,c4);

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
