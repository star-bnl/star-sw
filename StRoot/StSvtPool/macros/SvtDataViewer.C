//======================================================================
//
//   Macro to generate the SVT Data Viewer
//
//   author: Marcelo Munhoz
//
//======================================================================
//
{
  // Input initial Default file (can be edit)
  const char *MainFile="/dev/null";

  // Load libraries
    gSystem->Load("St_base");
    gSystem->Load("StUtilities");
    gSystem->Load("St_Tables");
    gSystem->Load("StChain");
    gSystem->Load("StBFChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("St_geom_Maker");
    gSystem->Load("StarClassLibrary");
    //gSystem->Load("StTrackFilter");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StTpcDb");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker");
    //gSystem->Load("StEventDisplayMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDAQMaker");
    gSystem->Load("StSvtClassLibrary");
    gSystem->Load("StSvtDaqMaker");
    gSystem->Load("StSvtCalibMaker");
    gSystem->Load("StSvtQAMaker");
    gSystem->Load("StSvtSeqAdjMaker");
    gSystem->Load("StSvtClusterMaker");

    gStyle->SetPalette(1);

    // Define chain to run BFC
    StChain  *chain=0;
    if (chain) delete chain;
    chain = new StChain("main");   // Create the main chain object

    // Define SVT chain
    chain->cd();
    StChain *chainSvt;
    chainSvt = new StChain("svt");
    chainSvt->SetDebug();
    chainSvt->cd();
   
    //Instantiate Makers for SVT chain
    StDAQMaker          *DAQMk       = new StDAQMaker("DAQInput",MainFile);
    StSvtDaqMaker       *SvtDaqMk    = new StSvtDaqMaker("SvtDaq","FULL","ZS");
    StSvtPedMaker       *SvtPedMk    = new StSvtPedMaker("SvtPed");
    StSvtBadAnodesMaker *SvtBadAnMk  = new StSvtBadAnodesMaker("SvtBadAn");
    StSvtQAMaker        *SvtQAMk     = new StSvtQAMaker("SvtQA");
    StSvtSeqAdjMaker    *SvtSeqAdjMk = new StSvtSeqAdjMaker("SvtSeqAdj");
    SvtSeqAdjMk->SetMinAdcLevels(3,2,5,0,10);
    SvtSeqAdjMk->SetLowInvProd(0);

    StSvtClusterMaker  *SvtCluMk  = new StSvtClusterMaker("SvtClu");
    StSvtClusterAnalysisMaker *SvtCluAnaMk = new StSvtClusterAnalysisMaker("SvtCluAna");

    //Setup graphical viewer
    StSvtGuiMonitor* main = new StSvtGuiMonitor("FULL", chainSvt, chain, gClient->GetRoot(),1,1);
    StSvtView* aGraphicMonitor = main->GetSvtView();
    StSvtMonitor* aMonitor = main->GetSvtMonitor();
    StSvtMenuBar* menuBar = main->GetSvtMenuBar();
    SvtQAMk->SetMonitor(aMonitor);
    
    aGraphicMonitor->DrawLadder(1,1);
    aGraphicMonitor->UpdateBarrelButtons(1,1);
    aGraphicMonitor->UpdateLadderButtons(1,1,1,1);
    aGraphicMonitor->SetInfoFile(MainFile);

    // Initialize Makers
    DAQMk->Init();
    SvtDaqMk->Init();
    //SvtDaqMk->SetSvtData();
    SvtPedMk->Init();
    SvtBadAnMk->Init();
    //SvtQAMk->SetSvtData();
    SvtQAMk->Init();
    SvtSeqAdjMk->Init();
    SvtCluMk->Init();
    SvtCluAnaMk->Init();
}
 


