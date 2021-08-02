#include <string.h>;

void testNative()
{
  static int firsttime=1;

  if(firsttime) {
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
    gSystem->Load("PDFUtil");    
    gSystem->Load("Jevp");    
  }
  printf("Run application...\n");
  MyMainFrame::example();
}
