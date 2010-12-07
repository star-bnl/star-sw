
void evpMonitor(char *args)
{
  static int firsttime=1;
  printf("starting\n");

  if(firsttime) {
    printf("loading\n");
    firsttime=0;
    gROOT->Macro("Load.C");
    gSystem->Load("libQtRootGui.so");
    gSystem->Load("libQt3Support.so");
    gSystem->Load("RTS");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDbLib");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StEEmcPoolmuEztPanitkin");
    gSystem->Load("StBEMCPlots");
    gSystem->Load("StMcEvent");
    gSystem->Load("StPDFUtilities");    
    gSystem->Load("StJevpPlot");    
    gSystem->Load("StJevpBuilders");    
    gSystem->Load("StJevpServer");
    gSystem->Load("StJevpPresenter");
  }
  printf("calling main\n");
  EvpMonitor::main(args);
}
