// $Id: StTofAssociator.C,v 1.1 2004/04/25 18:52:02 geurts Exp $
// $Log: StTofAssociator.C,v $
// Revision 1.1  2004/04/25 18:52:02  geurts
// *** empty log message ***
//

class StChain;
StChain *chain=0;

void StTofAssociator(Int_t nevents=1,
		     const char *MainFile="/afs/rhic/star/data/samples/*.geant.root",
		     const char* histoFile="embedTOFp.root",
		     const char* dirHistoFile="/auto/u/geurts/Embedding/tofp")
{

  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDetectorDbMaker");

   gSystem->Load("StTpcDb");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker"); //not needed if event.root branch present
  gSystem->Load("StEmcUtil"); 

  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StTofpMcAnalysisMaker");

  // TOF related goodies
  gSystem->Load("StTofUtil");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  //--TOF

    
  chain = new StChain("StChain"); 
  //chain->SetDebug();
   
  // Now we add Makers to the chain...

  StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
  //ioMaker->SetDebug();
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
  //ioMaker->SetBranch("eventBranch",0,"r"); //activate geant Branch
  ioMaker->SetBranch("dstBranch",0,"r"); //activate Event Branch
  ioMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch

  // Note, the title "events" is used in the Association Maker, so don't change it.
  // StEventMaker is not needed for event.root files
  StEventMaker*       eventReader   = new StEventMaker("events","title");
  eventReader->doPrintMemoryInfo = kFALSE;
  StMcEventMaker*     mcEventReader = new StMcEventMaker; // Make an instance...
  mcEventReader->doPrintMemoryInfo = false;
  mcEventReader->doUseTpc  = true;
  mcEventReader->doUseSvt  = false;
  mcEventReader->doUseFtpc = false;
  mcEventReader->doUseRich = false;
  mcEventReader->doUseBemc = false;
  mcEventReader->doUseBsmd = false;

  StAssociationMaker* associator    = new StAssociationMaker;
  associator->doPrintMemoryInfo = false;

  // If you need to use L3 TRIGGER uncomment the line:
  // associator->useL3Trigger();
  //associator->SetDebug();
  //associator->doPrintMemoryInfo = kTRUE;
  StTofpMcAnalysisMaker*  examples      = new StTofpMcAnalysisMaker;
  examples->SetDebug();

  // Define the cuts for the Associations

  StMcParameterDB* parameterDB = StMcParameterDB::instance();  
  // TPC
  parameterDB->setXCutTpc(.5); // 5 mm
  parameterDB->setYCutTpc(.5); // 5 mm
  parameterDB->setZCutTpc(.5); // 5 mm
  parameterDB->setReqCommonHitsTpc(3); // Require 3 hits in common for tracks to be associated
  // FTPC
  parameterDB->setRCutFtpc(.3); // 3 mm
  parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
  parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
  // SVT
  parameterDB->setXCutSvt(.08); // 800 um
  parameterDB->setYCutSvt(.08); // 800 um
  parameterDB->setZCutSvt(.08); // 800 um
  parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated
    
  // TOF--
  char *db2 = "StarDb";
  if (gSystem->AccessPathName(db2) !=0) {
    printf("File %s does not exist\n",db2);
    db2 = "";
  }
  St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb",db2);
  dbMk->SetDateTime(010102,000);
  // --TOF
    
  // now execute the chain member functions

  chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
    
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && istat!=2) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
   if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
   iev++; goto EventLoop;
 } // Event Loop


  char histfile[200];
  sprintf(histfile,"%s%s",dirHistoFile,histoFile);
  TFile file(histfile,"RECREATE");

  TOrdCollection *histograms = new TOrdCollection;
  histograms->AddLast(examples->hMcElectron);
  histograms->AddLast(examples->hRcElectron);
  histograms->AddLast(examples->hMatchElectron);
  histograms->AddLast(examples->hMomResPtPion);
  histograms->AddLast(examples->hMultRef);
  histograms->AddLast(examples->hMcPionPlus);
  histograms->AddLast(examples->hRcPionPlus);
  histograms->AddLast(examples->hMatchPionPlus);
  histograms->AddLast(examples->hMcPionMin);
  histograms->AddLast(examples->hRcPionMin);
  histograms->AddLast(examples->hMatchPionMin);
  histograms->AddLast(examples->hMcKaonPlus);
  histograms->AddLast(examples->hRcKaonPlus);
  histograms->AddLast(examples->hMatchKaonPlus);
  histograms->AddLast(examples->hMcKaonMin);
  histograms->AddLast(examples->hRcKaonMin);
  histograms->AddLast(examples->hMatchKaonMin);
  histograms->AddLast(examples->hMcProton);
  histograms->AddLast(examples->hRcProton);
  histograms->AddLast(examples->hMatchProton);
  histograms->AddLast(examples->hMcAntiProton);
  histograms->AddLast(examples->hRcAntiProton);
  histograms->AddLast(examples->hMatchAntiProton);
  histograms->AddLast(examples->hTofpSlatIdA0);
  histograms->AddLast(examples->hTofpSlatIdA1);
  histograms->AddLast(examples->hTofpSlatIdB1);
  histograms->AddLast(examples->hTofpSlatIdD1);
  histograms->AddLast(examples->hTofpSlatIdD2);
  histograms->AddLast(examples->hTofpSlatIdE1);
  histograms->AddLast(examples->hTofpSlatIdE2);
  histograms->AddLast(examples->hTofpSlatIdE3);
  histograms->AddLast(examples->hTofpSlatIdE4);
  histograms->AddLast(examples->hTofpSlatIdE5);
  histograms->AddLast(examples->hTofpSlatIdF1);
  histograms->AddLast(examples->hTofpSlatHitVecSize);

  histograms->Write();

  file.Close();



  if(iev>200) chain->Finish(); // This should call the Finish() method in ALL makers,
  // comment it out if you want to keep the objects
  // available at the command line after running
  // the macro.

  // To look at the ntuple after the macro has executed:
  // f1 = new TFile("TrackMapNtuple.root");  //This opens the file, and loads the Ntuple
  // TrackNtuple->Draw("px:pxrec")  //Once loaded, the Ntuple is available by name.
  // To look at the Histograms once the Macro has executed:
  // TList* dList = chain->GetMaker("McAnalysis")->Histograms();
  // TH2F* hitRes = dList->At(0);  //or whatever index from 0 to 3
}

