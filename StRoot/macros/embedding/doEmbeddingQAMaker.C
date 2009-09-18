
//______________________________________________________________________
void doEmbeddingQAMaker(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const TString particleName = "PiPlus",
    const Char_t* inputFileList = "minimc.list",
    const Char_t* outputFileName = "", // Put the filename if you want to give some specific name, otherwise leave it blank.
    const Bool_t isSimulation = kTRUE
){
  TString data = (isSimulation) ? "minimc tree" : "real data" ;
  TString title = "Embedding QA from " + data ;

  gBenchmark->Start(title);

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingQAMaker");

  StEmbeddingQAMaker* maker = new StEmbeddingQAMaker(year, production, particleName, isSimulation);
  maker->SetDebug(1);
  maker->Book(outputFileName);
  maker->Run(inputFileList);
  maker->End();

  gBenchmark->Stop(title);
  gBenchmark->Show(title);
  gBenchmark->Reset();
}

//______________________________________________________________________
void doEmbeddingQA(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const TString particleName = "PiPlus",
    const TString inputFileList = "minimc.list"
){
  doEmbeddingQAMaker(year, production, particleName, inputFileList, "", kTRUE);
}

//______________________________________________________________________
void doRealDataQA(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const TString particleName = "PiPlus",
    const Char_t* inputFileList = "MuDst.list"
){
  doEmbeddingQAMaker(year, production, particleName, inputFileList, "", kFALSE);
}

//______________________________________________________________________
void doEmbeddingQAMakerOneFile(
    const Char_t* inputFileName = "/star/institutions/lbl/hmasui/embedding/data/P08if/PiPlus_st_physics_8172100_raw_1020010.minimc.root",
    const Char_t* outputFileName = "qa_minimc.root"
){
  gBenchmark->Start("Embedding QA from minimc tree");

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingQAMaker");

  StEmbeddingQAMaker* maker = new StEmbeddingQAMaker();
  maker->SetDebug(1);
  maker->Book(outputFileName);
  maker->Make(inputFileName, kTRUE);
  maker->End();

  gBenchmark->Stop("Embedding QA from minimc tree");
  gBenchmark->Show("Embedding QA from minimc tree");
  gBenchmark->Reset();
}

