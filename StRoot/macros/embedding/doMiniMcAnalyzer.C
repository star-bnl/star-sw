
//______________________________________________________________________
void doMiniMcAnalyzer(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const TString particleName = "PiPlus",
    const Char_t* inputFileList = "MuDst.P08ic.list",
    const Char_t* outputFileName = "", // Put the filename if you want to give some specific name, otherwise leave it blank.
    const Bool_t isSimulation = kTRUE
){
  TString data = (isSimulation) ? "minimc tree" : "real data" ;
  TString title = "Embedding QA from " + data ;

  gBenchmark->Start(title);

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StMiniMcAnalyzer");

  StMiniMcAnalyzer* maker = new StMiniMcAnalyzer(year, production, particleName, isSimulation);
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
    const TString inputFileList = "minimc.PiPlus.list",
){
  doMiniMcAnalyzer(year, production, particleName, kTRUE, inputFileList, "");
}

//______________________________________________________________________
void doRealDataQA(
    const Int_t year = 2007,
    const TString production = "P08ic",
    const TString particleName = "PiPlus",
    const Char_t* inputFileList = "MuDst.P08ic.list",
){
  doMiniMcAnalyzer(year, production, particleName, kFALSE, inputFileList, "");
}

//______________________________________________________________________
void doMiniMcAnalyzerOneFile(
    const Char_t* inputFileName = "/star/institutions/lbl/hmasui/embedding/data/P08if/PiPlus_st_physics_8172100_raw_1020010.minimc.root",
    const Char_t* outputFileName = "ana_minimc.root"
){
  gBenchmark->Start("Embedding QA from minimc tree");

  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StMiniMcAnalyzer");

  StMiniMcAnalyzer* maker = new StMiniMcAnalyzer();
  maker->SetDebug(1);
  maker->Book(outputFileName);
  maker->Make(inputFileName, kTRUE);
  maker->End();

  gBenchmark->Stop("Embedding QA from minimc tree");
  gBenchmark->Show("Embedding QA from minimc tree");
  gBenchmark->Reset();
}

