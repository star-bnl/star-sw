TString input_dir   = "./";
//TString input_chain = "sdt20120601,fzin,geant,evout,y2013,FieldOn,logger,MakeEvent,McEvout,IdTruth,ReverseField,db,fmsDB,-tpcDB";
//TString input_chain = "sdt20130312,fzin,geant,evout,y2013,FieldOn,logger,MakeEvent,McEvout,IdTruth,ReverseField,db,fmsDB,-tpcDB";
TString input_chain = "sdt20150101,fzin,geant,evout,y2013,FieldOn,logger,MakeEvent,McEvout,IdTruth,ReverseField,db,fmsDB,-tpcDB";

class StFmsSimulatorMaker;

void runTrgsim( Int_t nEvents=1000, Int_t run=1000, const char* pid="gamma",
		TString myDir=input_dir, TString myChain=input_chain, Int_t mnEvents=1){

  gROOT->LoadMacro("bfc.C");
  gROOT->Macro("loadMuDst.C");
  TString myDat=Form("test_%s_run%i.fzd",pid,run);
  bfc( -1, myChain, myDir+myDat );
  { 
    TString outfile = chain->GetFileOut();
    outfile.Prepend("reco_");
    chain->SetOutputFile(outfile);
  }

  StFmsDbMaker* fmsdb = (StFmsDbMaker*) chain->GetMaker("fmsDb");
  fmsdb->forceUniformGainCorrection(1.0);
  //fmsdb->forceUniformGain(0.038);
  fmsdb->setDebug(2);

  gSystem->Load("StFmsFastSimulatorMaker");
  StFmsFastSimulatorMaker *fmssim = new StFmsFastSimulatorMaker();
  fmssim->SetDebug();

  gSystem->Load("StFmsTriggerMaker");
  //StFmsTriggerMaker *fmstrg = new StFmsTriggerMaker();
  //fmstrg->useStEvent();  
  //fmstrg->forceRunNumber(16000000);
  //fmstrg->overwriteThr("FMS-JP2",30);
 
  /*  
  gSystem->Load("StFmsTrgEvalMaker");
  StFmsTrgEvalMaker* evl = new StFmsTrgEvalMaker();
  chain->GetFileOut();
  {
    TString outfile = chain->GetFileOut();
    outfile.ReplaceAll(".root","");
    evl->setFileName(outfile.Data());
  }
  */

  chain->Init();
  StMaker::lsMakers(chain);
  chain->EventLoop(mnEvents,nEvents);  
  chain->Finish(); 
}
