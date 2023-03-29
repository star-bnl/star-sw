#include <iostream>

class StMaker;
class StChain;
class StPicoDstReader;

const Char_t* testInFName = "/star/u/gnigmat/soft/u/centrality_definition/Centrality/input/st_physics_adc_19084053_raw_0000006.picoDst.root";

//________________
void runCentralityAnalyzer_Zr(const Char_t* inFileName = testInFName,
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
  //Zr pileup parameters
  //---------------------------------------------------
  //double a0=-1.21309838794601, a1=0.912951771896837, a2=-0.000367314265128479, a3=9.94347355669806e-07, a4=-1.31455343813894e-09;
  //double b0=13.9009773648019, b1=1.44056038211802, b2=-0.00289508482803666, b3=7.46976650036052e-06, b4=-7.76886643291828e-09;
  //double c0=-11.5092765154364, c1=0.422455652498953, c2=0.00184903353426273, c3=-4.6965709740072e-06, c4=4.43430319329608e-09;  
  //---------------------------------------------------
  //New parameters from corrected bad-runs list
  double a0=-1.19267140255075, a1=0.912703837475103, a2=-0.000366760799818202, a3=9.94532126858323e-07, a4=-1.31386727140715e-09;
  double b0=13.5244327901538, b1=1.4429201808933, b2=-0.002873496957537, b3=7.29172798142226e-06, b4=-7.45759942317285e-09;
  double c0=-11.2781454979572, c1=0.420728494449501, c2=0.00184005031913895, c3=-4.61008765754698e-06, c4=4.28291905929182e-09;
  //---------------------------------------------------  
  anaMaker->setPileUpParameters(a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,c0,c1,c2,c3,c4);

  //Zr luminosity correction parameters
  //---------------------------------------------------
  double lumcorr_a = -1.3917e-4;
  double lumcorr_b = 98.412;
  double lumcorr_bprime = 96.9914;
  //---------------------------------------------------
  anaMaker->setLumCorrParameters(lumcorr_a,lumcorr_b,lumcorr_bprime);

  //Vz correction parameters for Zr+Zr

  double vzCorPar0 = 300.296;
  double vzCorPar1 = 0.0358743;
  double vzCorPar2 = -0.000549242;
  double vzCorPar3 = 0.000232057;
  double vzCorPar4 = 5.86792e-06;
  double vzCorPar5 = -2.91945e-07;
  double vzCorPar6 = -6.33743e-09;

  anaMaker->setVzCorrParameters(vzCorPar0, vzCorPar1, vzCorPar2, vzCorPar3, vzCorPar4, vzCorPar5, vzCorPar6);

  //set shape correction weight index; 0: Ru, 1: Zr
  anaMaker->setShapeIndex(1);
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
