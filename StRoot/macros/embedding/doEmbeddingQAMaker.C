
//______________________________________________________________________
void doEmbeddingQAMaker(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const Char_t* inputFileList = "minimc.list",
    const Char_t* outputFileName = "", // Put the filename if you want to give some specific name, otherwise leave it blank.
    const Bool_t isSimulation = kTRUE,
    const Float_t vzCut = 30.0
){
  const TString data = (isSimulation) ? "minimc tree" : "real data" ;
  const TString title = "Embedding QA from " + data ;

  gBenchmark->Start(title);

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingUtilities");

  StEmbeddingQA* maker = new StEmbeddingQA(year, production, isSimulation);

  /// Set z-vertex cut (default is 30cm unless otherwise specified)
  maker->setZVertexCut(vzCut);

  /// Set rapidity cut (default is 10). 
  // Uncomment next line and put the rapidity cut if you want to make the rapidity cut
//  maker->setRapidityCut(1.0);

  /// Set trigger id cut (default is no trigger id selection)
  //    NOTE: you can put multiple trigger id's like
  //     maker->addTriggerIdCut(290001);
  //     maker->addTriggerIdCut(290004);
  //
  // Uncomment next line and put the trigger id if you want to make the trigger id cut
//  maker->addTriggerIdCut(210020);

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
    const Float_t vzCut = 30.0
){
  gBenchmark->Start("Embedding QA from minimc tree");

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingUtilities");

  StEmbeddingQA* maker = new StEmbeddingQA(year, production, isSimulation);
  maker->setZVertexCut(vzCut);

//  maker->setRapidityCut(1.0);
//  maker->addTriggerIdCut(210020);

  maker->book(outputFileName);
  maker->make(inputFileName, kTRUE);
  maker->end();

  gBenchmark->Stop("Embedding QA from minimc tree");
  gBenchmark->Show("Embedding QA from minimc tree");
  gBenchmark->Reset();
}

