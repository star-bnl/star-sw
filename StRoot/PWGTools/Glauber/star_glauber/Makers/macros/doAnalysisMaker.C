
//____________________________________________________________________________________________________
void doAnalysisMaker(
    const TString system = "AuAu_39GeV",
    const TString inputFileList = "tmp.list",
    const TString outputFile = "ana.root",
    const TString type = "default",
    const TString tabledir = "./table",
    const Bool_t doReweighting = kTRUE,
    const Bool_t doUnitWeight = kFALSE,
    const UShort_t debug = 0
){
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StGlauberUtilities");
  gSystem->Load("StCentralityMaker");
  gSystem->Load("StGlauberTree");
  gSystem->Load("StGlauberAnalysisMaker");

  StGlauberAnalysisMaker* maker = new StGlauberAnalysisMaker(type, system, outputFile, tabledir);
  if(debug) maker->DebugOn();
  if(doReweighting) maker->ReweightingOn() ;
  if(doUnitWeight) maker->UnitWeightOn() ;
  maker->Run(inputFileList);
  maker->Finish();
}

//____________________________________________________________________________________________________
void doAnalysisMakerOneFile(
    const TString system = "AuAu_39GeV",
    const TString inputFile = "icmaker.root",
    const TString outputFile = "ana.root",
    const TString type = "default",
    const TString abledir = "./table",
    const Bool_t doReweighting = kTRUE,
    const Bool_t doUnitWeight = kFALSE,
    const UShort_t debug = 3
){
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StGlauberUtilities");
  gSystem->Load("StCentralityMaker");
  gSystem->Load("StGlauberTree");
  gSystem->Load("StGlauberAnalysisMaker");

  StGlauberAnalysisMaker* maker = new StGlauberAnalysisMaker(type, system, outputFile, tabledir);
  if(debug) maker->DebugOn();
  if(doReweighting) maker->ReweightingOn() ;
  if(doUnitWeight) maker->UnitWeightOn() ;
  maker->RunFile(inputFile);
  maker->Finish();
}
