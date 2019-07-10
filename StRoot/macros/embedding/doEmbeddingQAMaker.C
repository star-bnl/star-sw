
//______________________________________________________________________
void doEmbeddingQAMaker(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const Char_t* inputFileList = "minimc.list",
    const Char_t* outputFileName = "", // Put the filename if you want to give some specific name, otherwise leave it blank.
    const Bool_t isSimulation = kTRUE,
    const Float_t vzCut = 30.0, 
    const Int_t refMultMinCut = 0,
    const Int_t refMultMaxCut = 1000,
    const Float_t ptMaxCut = 10.0,
    const Float_t ptMinCut = 0.1,
    const Float_t etaMaxCut = 1.5,
    const Float_t yMaxCut = 10.0
){
  const TString data = (isSimulation) ? "minimc tree" : "real data" ;
  const TString title = "Embedding QA from " + data ;

  gBenchmark->Start(title);

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingUtilities");

  // Set relevant cut parameters here used in the base QA
  // The default parameters can be found in the constructor of StRoot/StEmbeddingUtilities/StEmbeddingQAUtilities.cxx
  // You should have consistent parameters in both doEmbeddingQAMaker.C and drawEmbeddingQA.C
  // Below are the examples how to change the parameters
  // All values used here are default ones
  //
  // NOTE: In order to keep backward compatibility, the functions
  //   StEmbeddingQA::setZVertexCut()
  //   StEmbeddingQA::setRapidityCut()
  //   StEmbeddingQA::addTriggerIdCut()
  // were not removed. These functions can be still used instead of those from StEmbeddingQAUtilities.
  //
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  utility->setPtMinCut(ptMinCut);
  utility->setPtMaxCut(ptMaxCut);
  utility->setEtaCut(etaMaxCut);
  //  utility->setNHitCut(10);
  //  utility->setNHitToNPossCut(0.51);
  //  utility->setDcaCut(3.0);
  //  utility->setNSigmaCut(2.0);
  utility->setRapidityCut(yMaxCut);
  utility->setRefMultMinCut(refMultMinCut);
  utility->setRefMultMaxCut(refMultMaxCut);
  utility->setZVertexCut(vzCut);

  // FIXME: the trigger ID selections has to be hard-coded below!!!
  // Default is no trigger cut, you can add multiple trigger id's like
  //  utility->addTriggerIdCut(290001);
  //  utility->addTriggerIdCut(290004);
  // FIXME: using BTof PID in selecting real data primary tracks (turn this on ONLY for checking the dE/dx match between data and MC)
  //  utility->setBTofPid(kTRUE);

  StEmbeddingQA* maker = new StEmbeddingQA(year, production, isSimulation);

  maker->book(outputFileName);
  maker->run(inputFileList);
  maker->end();

  gBenchmark->Stop(title);
  gBenchmark->Show(title);
  gBenchmark->Reset();
}

//______________________________________________________________________
void doEmbeddingQA(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const TString inputFileList = "minimc.list"
){
  doEmbeddingQAMaker(year, production, inputFileList, "", kTRUE);
}

//______________________________________________________________________
void doRealDataQA(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const Char_t* inputFileList = "MuDst.list"
){
  doEmbeddingQAMaker(year, production, inputFileList, "", kFALSE);
}

//______________________________________________________________________
void doEmbeddingQAMakerOneFile(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const Char_t* inputFileName = "/star/institutions/lbl/hmasui/embedding/data/P08if/PiPlus_st_physics_8172100_raw_1020010.minimc.root",
    const Char_t* outputFileName = "",
    const Bool_t isSimulation = kTRUE,
    const Float_t vzCut = 30.0, 
    const Int_t refMultMinCut = 0, 
    const Int_t refMultMaxCut = 1000
){
  gBenchmark->Start("Embedding QA from minimc tree");

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingUtilities");

  StEmbeddingQA* maker = new StEmbeddingQA(year, production, isSimulation);
  maker->setZVertexCut(vzCut);
  maker->setRefMultMinCut(refMultMinCut);
  maker->setRefMultMaxCut(refMultMaxCut);

//  maker->setRapidityCut(1.0);
//  maker->addTriggerIdCut(210020);

  maker->book(outputFileName);
  maker->make(inputFileName, kTRUE);
  maker->end();

  gBenchmark->Stop("Embedding QA from minimc tree");
  gBenchmark->Show("Embedding QA from minimc tree");
  gBenchmark->Reset();
}

