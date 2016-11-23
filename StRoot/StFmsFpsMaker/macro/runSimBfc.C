TString input_dir   = "fzd/";
TString output_dir   = "sim/";
TString input_chain = "sdt20141215,fzin,geant,evout,y2015,FieldOn,logger,MakeEvent,McEvout,IdTruth,ReverseField,db,fmsDB,fmsSim,FmsPoint,-tpcDB";

class StFmsSimulatorMaker;

void runSimBfc( Int_t nEvents=100, Int_t run=100, const char* pid="gamma", int merge=1,
		int print=0,
		TString myDir=input_dir, TString myOutDir=output_dir,
		TString myChain=input_chain, Int_t mnEvents=1){
    
  gROOT->LoadMacro("bfc.C");
  gROOT->Macro("loadMuDst.C");
  TString myDat=Form("test_%s_run%i.fzd",pid,run);
  bfc( -1, myChain, myDir+myDat );

  TString outfile = myOutDir + myDat.ReplaceAll(".fzd",".root");
  //TString outfile = myOutDir + chain->GetFileOut();    
  cout << "output file=" <<outfile<<endl;
  chain->SetOutputFile(outfile);
  
  StFmsDbMaker* fmsdb = (StFmsDbMaker*) chain->GetMaker("fmsDb");
  //fmsdb->readGainFromText();
  //fmsdb->forceUniformGain(0.038);
  //fmsdb->forceUniformGainCorrection(1.0);
  fmsdb->setDebug(2);
  fmsdb->readRecParamFromFile();

  StFmsFastSimulatorMaker *fmssim = (StFmsFastSimulatorMaker*) chain->GetMaker("fmsSim");
  fmssim->SetDebug();
  fmssim->setFpsNPhotonPerMIP(100.0);
  
  StFmsPointMaker* pointMaker = (StFmsPointMaker*) chain->GetMaker("StFmsPointMaker");
  pointMaker->setMergeSmallToLarge(merge);

  gSystem->Load("StFmsFpsMaker");
  StFmsFpsMaker* fmsfps = new StFmsFpsMaker(); 
  fmsfps->setPrint(4);
  outfile.ReplaceAll(".root",".fmsfps.root");
  fmsfps->setQA(outfile.Data());

  if(print>=9){
      gSystem->Load("StFmsEventDisplay");
      StFmsEventDisplay* fmsed = new StFmsEventDisplay();
      fmsed->setMaxEvents(100);
      //outfile.ReplaceAll(".fmsfps.root",".eventDisplay.pdf");
      outfile.ReplaceAll(".fmsfps.root",".eventDisplay.png");
      fmsed->setFileName(outfile.Data());
      if(print==10) fmsed->setFilter(1);
  }

  chain->Init();
  StMaker::lsMakers(chain);
  chain->EventLoop(mnEvents,nEvents);  
  chain->Finish(); 
}
