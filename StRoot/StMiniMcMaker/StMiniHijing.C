//////////////////////////////////////////////////////////////////////
// $Id: StMiniHijing.C,v 1.3 2002/06/07 02:21:59 calderon Exp $
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file from hijing data, produces minimc.root file 
//               runs a chain with the makers: 
//                 StMcEventMaker,StAssociationMaker,
//                 StMiniMcEventMaker
// $Log: StMiniHijing.C,v $
// Revision 1.3  2002/06/07 02:21:59  calderon
// Protection against empty vector in findFirstLastHit
// $Log$ and $Id$ plus header comments for the macros
//
//////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

void StMiniHijing(Int_t nevents=3,
		  const char* MainFile=
		  "hijing_b0_3/rcf0147_01*geant.root",
		  const char* outDir = "./",
		  int commonHits=3)
{

  // Dynamically link needed shared libs
  //gSystem->Load("StarRoot");
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libglobal_Tables");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libtpc_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
    
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker"); 
  gSystem->Load("StEmcUtil"); 

  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");

  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StMiniMcMaker");
//   gSystem->Load("Common");

  chain = new StChain("StChain"); 
  chain->SetDebug();
  
  // Now we add Makers to the chain...
  
  StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
  ioMaker->SetDebug();
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
  ioMaker->SetBranch("dstBranch",0,"r"); //activate Event Branch
  // ioMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch
  
  //     const char *mainDB = "MySQL:Geometry_tpc";
  //     St_db_Maker *dbMk = new St_db_Maker("Geometry",mainDB);
  //     dbMk->SetDebug();
  
  //     const char *calibDB = "MySQL:Calibrations_tpc";
  //     St_db_Maker *calibMk = new St_db_Maker("Calibrations",calibDB);
  //     calibMk->SetDebug();
  
  //     StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  
  // Note, the title "events" is used in the Association Maker, so don't change it.
  StEventMaker*       eventReader   = new StEventMaker("events","title");
  eventReader->doPrintMemoryInfo = kFALSE;
  StMcEventMaker*     mcEventReader = new StMcEventMaker; // Make an instance...
  //     mcEventReader->doPrintMemoryInfo = kFALSE;
  //     mcEventReader->doUseTpc = kTRUE;
  //     mcEventReader->doUseSvt = kTRUE;
  //     mcEventReader->doUseFtpc = kTRUE;
  //     mcEventReader->doUseRich = kTRUE;
  StAssociationMaker* associator    = new StAssociationMaker;
  //associator->doPrintMemoryInfo = kTRUE;

  //
  // mdst of association stuff
  //
  StMiniMcMaker *krap = new StMiniMcMaker;
  krap->setDebug(1);
  krap->setGhost();
  krap->setOutDir(outDir);
  krap->setPtCut(1);
  krap->setFileName(MainFile);

  // Define the cuts for the Associations
  
  StMcParameterDB* parameterDB = StMcParameterDB::instance();  
  // TPC
  parameterDB->setXCutTpc(.5); // 5 mm
  parameterDB->setYCutTpc(.5); // 5 mm
  parameterDB->setZCutTpc(.5); // 5 mm
  parameterDB->setReqCommonHitsTpc(commonHits); // Require 3 hits in common for tracks to be associated
  // FTPC
  parameterDB->setRCutFtpc(.3); // 3 mm
  parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
  parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
  // SVT
  parameterDB->setXCutSvt(.08); // 800 um
  parameterDB->setYCutSvt(.08); // 800 um
  parameterDB->setZCutSvt(.08); // 800 um
  parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated
  
  
  // now execute the chain member functions
  
  //chain->PrintInfo();
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
 
 chain->Finish();
  
}

