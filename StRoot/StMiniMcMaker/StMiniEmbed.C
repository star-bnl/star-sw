//////////////////////////////////////////////////////////////////////
// $Id: StMiniEmbed.C,v 1.4 2002/06/11 19:09:34 calderon Exp $
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file from emedding data, produces minimc.root file 
//               runs a chain with the makers: 
//                 StMcEventMaker,StAssociationMaker,
//                 StMiniMcEventMaker
// Note: it is currently written to write into a specific directory structure in PDSF
//       so if one needs to run elsewhere, and the output directory doesn't have the same
//       lower level directory structure, no output files will be done.
// $Log: StMiniEmbed.C,v $
// Revision 1.4  2002/06/11 19:09:34  calderon
// Bug fix: the filename that was set in the macro was being overwritten
// in InitRun, so the emb80x string which was added to the filename was lost.
// This was fixed by not replacing the filename in InitRun and only replacing
// the current filename starting from st_physics.
//
// Revision 1.3  2002/06/07 02:21:48  calderon
// Protection against empty vector in findFirstLastHit
// $Log: StMiniEmbed.C,v $
// Revision 1.4  2002/06/11 19:09:34  calderon
// Bug fix: the filename that was set in the macro was being overwritten
// in InitRun, so the emb80x string which was added to the filename was lost.
// This was fixed by not replacing the filename in InitRun and only replacing
// the current filename starting from st_physics.
// and $Id: StMiniEmbed.C,v 1.4 2002/06/11 19:09:34 calderon Exp $ plus header comments for the macros
//
//////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

// const char* ffile="/auto/pdsfdv09/starprod/embedding/P01hj/HighpT_piminus_101/1243006_0003.26283/st_physics_1243006_raw_0003.dst.root";
const char* ffile="/beta/starprod/embedding/P02gd/Rev/Piminus_801_minbias/2254002_0021.21333/st_physics_2254002_raw_0021.geant.root";
void StMiniEmbed(Int_t nevents=2,
		 const char* MainFile=ffile,
		 const char* outDir = "./",
		 int commonHits=3)
{

  cout << "Using  : " << MainFile << endl;

  // Dynamically link needed shared libs
  //gSystem->Load("StarRoot");
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libglobal_Tables");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libtpc_Tables"); // needed for StTpcDb
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
  ioMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch
  
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

  StMiniMcMaker *krap = new StMiniMcMaker;
  krap->setDebug();
  TString outDirName = outDir;
  TString filename   = MainFile;
  TString embedrun   = MainFile;

  //
  // the string manipulations below are for use in PDSF, from
  // the /beta/starprod/embedding/ input directory
  // to the /auto/pdsfdv41/starprod/QA/McMiniDst/  output directory
  //
  if (filename.Contains("Rev"))
      outDirName.Append("RevFullField/");
  else
      outDirName.Append("FullField/");

  if (filename.Contains("Piminus"))
      outDirName.Append("PiMinus");
  if (filename.Contains("Piplus"))
      outDirName.Append("PiPlus");
  if (filename.Contains("KMinus"))
      outDirName.Append("KMinus");
  if (filename.Contains("KPlus"))
      outDirName.Append("KPlus");
  if (filename.Contains("Pbar"))
      outDirName.Append("Pbar");
  if (filename.Contains("Proton"))
      outDirName.Append("Proton");
  krap->setOutDir(outDirName.Data());
  int embedRunIndex = embedrun.Index("_",0);
  embedrun.Remove(0,embedRunIndex+1);
  embedRunIndex = embedrun.Index("_",0);
  embedrun.Remove(embedRunIndex);
  int fileBeginIndex = filename.Index("st_physics",0);
  filename.Remove(0,fileBeginIndex);
  filename.Prepend(embedrun);
  filename.Prepend("emb");
  krap->setFileName(filename);
  cout << "outdir : " << outDirName << endl;
  cout << "Output : " << filename << endl;

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
 
//  chain->Finish();
  
}

